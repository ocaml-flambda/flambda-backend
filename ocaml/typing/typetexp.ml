(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)

open Asttypes
open Jane_asttypes
open Misc
open Parsetree
open Typedtree
open Types
open Mode
open Ctype

exception Already_bound

type value_loc =
    Tuple | Poly_variant | Package_constraint | Object_field

type sort_loc =
    Fun_arg | Fun_ret

type cannot_quantify_reason =
  | Unified of type_expr
  | Univar
  | Scope_escape

(* a description of the jkind on an explicitly quantified universal
   variable, containing whether the jkind was a default
   (e.g. [let f : 'a. 'a -> 'a = ...]) or explicit
   (e.g. [let f : ('a : immediate). ...]) and what the jkind was;
   it is original as compared to the inferred jkind after processing
   the body of the type *)
type jkind_info = { original_jkind : jkind; defaulted : bool }

type error =
  | Unbound_type_variable of string * string list
  | No_type_wildcards
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * cannot_quantify_reason
  | Bad_univar_jkind of
      { name : string; jkind_info : jkind_info; inferred_jkind : jkind }
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unsupported_extension : _ Language_extension.t -> error
  | Polymorphic_optional_param
  | Non_value of
      {vloc : value_loc; typ : type_expr; err : Jkind.Violation.t}
  | Non_sort of
      {vloc : sort_loc; typ : type_expr; err : Jkind.Violation.t}
  | Bad_jkind_annot of type_expr * Jkind.Violation.t

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

module TyVarEnv : sig
  val reset : unit -> unit
  (* see mli file *)

  val is_in_scope : string -> bool

  val add : string -> type_expr -> unit
  (* add a global type variable to the environment *)

  val with_local_scope : (unit -> 'a) -> 'a
  (* see mli file *)

  type poly_univars
  val with_univars : poly_univars -> (unit -> 'a) -> 'a
  (* evaluate with a locally extended set of univars *)

<<<<<<< HEAD
  val make_poly_univars : string Location.loc list -> poly_univars
  (* a version of [make_poly_univars_jkinds] that doesn't take jkinds *)

  val make_poly_univars_jkinds :
    context:(string -> Jkind.annotation_context) ->
    (string Location.loc * jkind_annotation option) list -> poly_univars
  (* see mli file *)

  val check_poly_univars : Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  type policy
  val fixed_policy : policy (* no wildcards allowed *)
  val extensible_policy : policy (* common case *)
  val univars_policy : policy (* fresh variables are univars (in methods) *)
  val new_anon_var : Location.t -> Env.t -> Jkind.t -> policy -> type_expr
    (* create a new variable to represent a _; fails for fixed_policy *)
  val new_var : ?name:string -> Jkind.t -> policy -> type_expr
    (* create a new variable according to the given policy *)

  val add_pre_univar : type_expr -> policy -> unit
    (* remember that a variable might become a univar if it isn't unified;
       used for checking method types *)

  val collect_univars : (unit -> 'a) -> 'a * type_expr list
    (* collect univars during a computation; returns the univars.
       The wrapped computation should use [univars_policy].
       postcondition: the returned type_exprs are all Tunivar *)

  val reset_locals : ?univars:poly_univars -> unit -> unit
    (* clear out the local type variable env't; call this when starting
       a new e.g. type signature. Optionally pass some univars that
       are in scope. *)

  val lookup_local : string -> type_expr
    (* look up a local type variable; throws Not_found if it isn't in scope *)

  val remember_used : string -> type_expr -> Location.t -> unit
    (* remember that a given name is bound to a given type *)

  val globalize_used_variables : policy -> Env.t -> unit -> unit
   (* after finishing with a type signature, used variables are unified to the
      corresponding global type variables if they exist. Otherwise, in function
      of the policy, fresh used variables are either
        - added to the global type variable scope if they are not longer
        variables under the {!fixed_policy}
        - added to the global type variable scope under the {!extensible_policy}
        - expected to be collected later by a call to `collect_univar` under the
        {!universal_policy}
   *)

end = struct
  (** Map indexed by type variable names. *)
  module TyVarMap = Misc.Stdlib.String.Map

  let not_generic v = get_level v <> Btype.generic_level

  (* These are the type variables that were in scope before
     we started processing the current type.
  *)
  let type_variables = ref (TyVarMap.empty : type_expr TyVarMap.t)

  (* These are variables that have been used in the currently-being-checked
     type, possibly including the variables in [type_variables].
  *)
  let used_variables =
    ref (TyVarMap.empty : (type_expr * Location.t) TyVarMap.t)

  (* These are variables that will become univars when we're done with the
     current type. Used to force free variables in method types to become
     univars.
  *)
  let pre_univars = ref ([] : type_expr list)

  let reset () =
    reset_global_level ();
    Ctype.reset_reified_var_counter ();
    type_variables := TyVarMap.empty

  let is_in_scope name =
    TyVarMap.mem name !type_variables

  let add name v =
    assert (not_generic v);
    type_variables := TyVarMap.add name v !type_variables

  let narrow () =
    (increase_global_level (), !type_variables)

  let widen (gl, tv) =
    restore_global_level gl;
    type_variables := tv

  let with_local_scope f =
   let context = narrow () in
   Fun.protect
     f
     ~finally:(fun () -> widen context)

  (* throws Not_found if the variable is not in scope *)
  let lookup_global_type_variable name =
    TyVarMap.find name !type_variables

  let get_in_scope_names () =
    let add_name name _ l = if name = "_" then l else ("'" ^ name) :: l in
    TyVarMap.fold add_name !type_variables []

  (*****)
  type poly_univars = (string * type_expr * jkind_info) list

  (* These are variables we expect to become univars (they were introduced with
     e.g. ['a .]), but we need to make sure they don't unify first.  Why not
     just birth them as univars? Because they might successfully unify with a
     row variable in the ['a. < m : ty; .. > as 'a] idiom.  They are like the
     [used_variables], but will not be globalized in [globalize_used_variables].
  *)
  let univars = ref ([] : poly_univars)
  let assert_not_generic uvs =
    assert (List.for_all (fun (_name, v, _lay) -> not_generic v) uvs)

  let rec find_poly_univars name = function
    | [] -> raise Not_found
    | (n, t, _) :: rest ->
      if String.equal name n
      then t
      else find_poly_univars name rest

  let with_univars new_ones f =
    assert_not_generic new_ones;
    let old_univars = !univars in
    univars := new_ones @ !univars;
    Fun.protect
      f
      ~finally:(fun () -> univars := old_univars)

  let mk_poly_univars_triple_with_jkind ~context var jkind =
    let name = var.txt in
    let original_jkind = Jkind.of_annotation ~context:(context name) jkind in
    let jkind_info = { original_jkind; defaulted = false } in
    name, newvar ~name original_jkind, jkind_info

  let mk_poly_univars_triple_without_jkind var =
    let name = var.txt in
    let original_jkind = Jkind.value ~why:Univar in
    let jkind_info = { original_jkind; defaulted = true } in
    name, newvar ~name original_jkind, jkind_info

  let make_poly_univars vars =
    List.map mk_poly_univars_triple_without_jkind vars

  let make_poly_univars_jkinds ~context vars_jkinds =
    let mk_trip = function
        | (v, None) -> mk_poly_univars_triple_without_jkind v
        | (v, Some l) -> mk_poly_univars_triple_with_jkind ~context v l
    in
    List.map mk_trip vars_jkinds

  let check_poly_univars env loc vars =
    vars |> List.iter (fun (_, v, _) -> generalize v);
    vars |> List.map (fun (name, ty1,
                           ({ original_jkind; _ } as jkind_info)) ->
      let v = Btype.proxy ty1 in
      let cant_quantify reason =
        raise (Error (loc, env, Cannot_quantify(name, reason)))
      in
      begin match get_desc v with
      | Tvar { jkind } when not (Jkind.equate jkind original_jkind) ->
        let reason =
          Bad_univar_jkind { name; jkind_info; inferred_jkind = jkind }
        in
        raise (Error (loc, env, reason))
      | Tvar _ when get_level v <> Btype.generic_level ->
          cant_quantify Scope_escape
      | Tvar { name; jkind } ->
         set_type_desc v (Tunivar { name; jkind })
      | Tunivar _ ->
         cant_quantify Univar
      | _ ->
         cant_quantify (Unified v)
      end;
      v)

  let instance_poly_univars env loc vars =
    let vs = check_poly_univars env loc vars in
    vs |> List.iter (fun v ->
      match get_desc v with
      | Tunivar { name; jkind } ->
         set_type_desc v (Tvar { name; jkind })
      | _ -> assert false);
    vs

  (*****)
  let reset_locals ?univars:(uvs=[]) () =
    assert_not_generic uvs;
    univars := uvs;
    used_variables := TyVarMap.empty

  (* throws Not_found if the variable is not in scope *)
  let lookup_local name =
    try
      find_poly_univars name !univars
    with Not_found ->
      instance (fst (TyVarMap.find name !used_variables))
      (* This call to instance might be redundant; all variables
         inserted into [used_variables] are non-generic, but some
         might get generalized. *)

  let remember_used name v loc =
    assert (not_generic v);
    used_variables := TyVarMap.add name (v, loc) !used_variables


  type flavor = Unification | Universal
  type extensibility = Extensible | Fixed
  type policy = { flavor : flavor; extensibility : extensibility }

  let fixed_policy = { flavor = Unification; extensibility = Fixed }
  let extensible_policy = { flavor = Unification; extensibility = Extensible }
  let univars_policy = { flavor = Universal; extensibility = Extensible }

  let add_pre_univar tv = function
    | { flavor = Universal } ->
      assert (not_generic tv);
      pre_univars := tv :: !pre_univars
    | _ -> ()

  let collect_univars f =
    pre_univars := [];
    let result = f () in
    let univs =
      List.fold_left
        (fun acc v ->
           match get_desc v with
           | Tvar { name; jkind } when get_level v = Btype.generic_level ->
               set_type_desc v (Tunivar { name; jkind });
               v :: acc
           | _ -> acc)
        [] !pre_univars in
    result, univs

  let new_var ?name jkind policy =
    let tv = Ctype.newvar ?name jkind in
    add_pre_univar tv policy;
    tv

  let new_anon_var loc env jkind = function
    | { extensibility = Fixed } -> raise(Error(loc, env, No_type_wildcards))
    | policy -> new_var jkind policy

  let globalize_used_variables { flavor; extensibility } env =
    let r = ref [] in
    TyVarMap.iter
      (fun name (ty, loc) ->
        if flavor = Unification || is_in_scope name then
          let v = new_global_var (Jkind.any ~why:Dummy_jkind) in
          let snap = Btype.snapshot () in
          if try unify env v ty; true with _ -> Btype.backtrack snap; false
          then try
            r := (loc, v, lookup_global_type_variable name) :: !r
          with Not_found ->
            if extensibility = Fixed && Btype.is_Tvar ty then
              raise(Error(loc, env,
                          Unbound_type_variable ("'"^name,
                                                 get_in_scope_names ())));
            let v2 = new_global_var (Jkind.any ~why:Dummy_jkind) in
||||||| merged common ancestors
type variable_context = int * type_expr TyVarMap.t
=======
  val make_poly_univars : string list -> poly_univars
  (* see mli file *)

  val check_poly_univars : Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  type policy
  val fixed_policy : policy (* no wildcards allowed *)
  val extensible_policy : policy (* common case *)
  val univars_policy : policy (* fresh variables are univars (in methods) *)
  val new_any_var : Location.t -> Env.t -> policy -> type_expr
    (* create a new variable to represent a _; fails for fixed_policy *)
  val new_var : ?name:string -> policy -> type_expr
    (* create a new variable according to the given policy *)

  val add_pre_univar : type_expr -> policy -> unit
    (* remember that a variable might become a univar if it isn't unified;
       used for checking method types *)

  val collect_univars : (unit -> 'a) -> 'a * type_expr list
    (* collect univars during a computation; returns the univars.
       The wrapped computation should use [univars_policy].
       postcondition: the returned type_exprs are all Tunivar *)

  val reset_locals : ?univars:poly_univars -> unit -> unit
    (* clear out the local type variable env't; call this when starting
       a new e.g. type signature. Optionally pass some univars that
       are in scope. *)

  val lookup_local :
    row_context:type_expr option ref list -> string -> type_expr
    (* look up a local type variable; throws Not_found if it isn't in scope *)

  val remember_used : string -> type_expr -> Location.t -> unit
    (* remember that a given name is bound to a given type *)

  val globalize_used_variables : policy -> Env.t -> unit -> unit
   (* after finishing with a type signature, used variables are unified to the
      corresponding global type variables if they exist. Otherwise, in function
      of the policy, fresh used variables are either
        - added to the global type variable scope if they are not longer
        variables under the {!fixed_policy}
        - added to the global type variable scope under the {!extensible_policy}
        - expected to be collected later by a call to `collect_univar` under the
        {!universal_policy}
   *)

end = struct
  (** Map indexed by type variable names. *)
  module TyVarMap = Misc.Stdlib.String.Map

  let not_generic v = get_level v <> Btype.generic_level

  (* These are the "global" type variables: they were in scope before
     we started processing the current type.
  *)
  let type_variables = ref (TyVarMap.empty : type_expr TyVarMap.t)

  (* These are variables that have been used in the currently-being-checked
     type.
  *)
  let used_variables =
    ref (TyVarMap.empty : (type_expr * Location.t) TyVarMap.t)

  (* These are variables we expect to become univars (they were introduced with
     e.g. ['a .]), but we need to make sure they don't unify first.  Why not
     just birth them as univars? Because they might successfully unify with a
     row variable in the ['a. < m : ty; .. > as 'a] idiom.  They are like the
     [used_variables], but will not be globalized in [globalize_used_variables].
  *)
  type pending_univar = {
    univar: type_expr  (** the univar itself *);
    mutable associated: type_expr option ref list
     (** associated references to row variables that we want to generalize
       if possible *)
  }

  let univars = ref ([] : (string * pending_univar) list)
  let assert_univars uvs =
    assert (List.for_all (fun (_name, v) -> not_generic v.univar) uvs)

  (* These are variables that will become univars when we're done with the
     current type. Used to force free variables in method types to become
     univars.
  *)
  let pre_univars = ref ([] : type_expr list)

  let reset () =
    reset_global_level ();
    type_variables := TyVarMap.empty

  let is_in_scope name =
    TyVarMap.mem name !type_variables

  let add name v =
    assert (not_generic v);
    type_variables := TyVarMap.add name v !type_variables

  let narrow () =
    (increase_global_level (), !type_variables)

  let widen (gl, tv) =
    restore_global_level gl;
    type_variables := tv

  let with_local_scope f =
   let context = narrow () in
   Fun.protect
     f
     ~finally:(fun () -> widen context)

  (* throws Not_found if the variable is not in scope *)
  let lookup_global_type_variable name =
    TyVarMap.find name !type_variables

  let get_in_scope_names () =
    let add_name name _ l = if name = "_" then l else ("'" ^ name) :: l in
    TyVarMap.fold add_name !type_variables []

  (*****)
  type poly_univars = (string * pending_univar) list

  let with_univars new_ones f =
    assert_univars new_ones;
    let old_univars = !univars in
    univars := new_ones @ !univars;
    Fun.protect
      f
      ~finally:(fun () -> univars := old_univars)

  let make_poly_univars vars =
    let make name = { univar=newvar ~name (); associated = [] } in
    List.map (fun name -> name, make name ) vars

  let promote_generics_to_univars promoted vars =
      List.fold_left
        (fun acc v ->
           match get_desc v with
           | Tvar name when get_level v = Btype.generic_level ->
               set_type_desc v (Tunivar name);
               v :: acc
           | _ -> acc
        )
        promoted vars

  let check_poly_univars env loc vars =
    vars |> List.iter (fun (_, p) -> generalize p.univar);
    let univars =
      vars |> List.map (fun (name, {univar=ty1; _ }) ->
      let v = Btype.proxy ty1 in
      begin match get_desc v with
      | Tvar name when get_level v = Btype.generic_level ->
         set_type_desc v (Tunivar name)
      | _ ->
         raise (Error (loc, env, Cannot_quantify(name, v)))
      end;
      v)
    in
    (* Since we are promoting variables to univars in
       {!promote_generics_to_univars}, even if a row variable is associated with
       multiple univars we will promote it once, when checking the nearest
       univar associated to this row variable.
    *)
    let promote_associated acc (_,v) =
      let enclosed_rows = List.filter_map (!) v.associated in
      promote_generics_to_univars acc enclosed_rows
    in
    List.fold_left promote_associated univars vars

  let instance_poly_univars env loc vars =
    let vs = check_poly_univars env loc vars in
    vs |> List.iter (fun v ->
      match get_desc v with
      | Tunivar name ->
         set_type_desc v (Tvar name)
      | _ -> assert false);
    vs

  (*****)
  let reset_locals ?univars:(uvs=[]) () =
    assert_univars uvs;
    univars := uvs;
    used_variables := TyVarMap.empty

  let associate row_context p =
    let add l x = if List.memq x l then l else x :: l in
    p.associated <- List.fold_left add row_context p.associated

  (* throws Not_found if the variable is not in scope *)
  let lookup_local ~row_context name =
    try
      let p = List.assoc name !univars in
      associate row_context p;
      p.univar
    with Not_found ->
      instance (fst (TyVarMap.find name !used_variables))
      (* This call to instance might be redundant; all variables
         inserted into [used_variables] are non-generic, but some
         might get generalized. *)

  let remember_used name v loc =
    assert (not_generic v);
    used_variables := TyVarMap.add name (v, loc) !used_variables


  type flavor = Unification | Universal
  type extensibility = Extensible | Fixed
  type policy = { flavor : flavor; extensibility : extensibility }

  let fixed_policy = { flavor = Unification; extensibility = Fixed }
  let extensible_policy = { flavor = Unification; extensibility = Extensible }
  let univars_policy = { flavor = Universal; extensibility = Extensible }

  let add_pre_univar tv = function
    | { flavor = Universal } ->
      assert (not_generic tv);
      pre_univars := tv :: !pre_univars
    | _ -> ()

  let collect_univars f =
    pre_univars := [];
    let result = f () in
    let univs = promote_generics_to_univars [] !pre_univars in
    result, univs

  let new_var ?name policy =
    let tv = Ctype.newvar ?name () in
    add_pre_univar tv policy;
    tv

  let new_any_var loc env = function
    | { extensibility = Fixed } -> raise(Error(loc, env, No_type_wildcards))
    | policy -> new_var policy

  let globalize_used_variables { flavor; extensibility } env =
    let r = ref [] in
    TyVarMap.iter
      (fun name (ty, loc) ->
        if flavor = Unification || is_in_scope name then
          let v = new_global_var () in
          let snap = Btype.snapshot () in
          if try unify env v ty; true with _ -> Btype.backtrack snap; false
          then try
            r := (loc, v, lookup_global_type_variable name) :: !r
          with Not_found ->
            if extensibility = Fixed && Btype.is_Tvar ty then
              raise(Error(loc, env,
                          Unbound_type_variable ("'"^name,
                                                 get_in_scope_names ())));
            let v2 = new_global_var () in
>>>>>>> ocaml/5.1
            r := (loc, v, v2) :: !r;
            add name v2)
      !used_variables;
    used_variables := TyVarMap.empty;
    fun () ->
      List.iter
        (function (loc, t1, t2) ->
          try unify env t1 t2 with Unify err ->
            raise (Error(loc, env, Type_mismatch err)))
        !r
end

(* Support for first-class modules. *)

let transl_modtype_longident = ref (fun _ -> assert false)
let transl_modtype = ref (fun _ -> assert false)

let sort_constraints_no_duplicates loc env l =
  List.sort
    (fun (s1, _t1) (s2, _t2) ->
       if s1.txt = s2.txt then
         raise (Error (loc, env, Multiple_constraints_on_type s1.txt));
       compare s1.txt s2.txt)
    l

let create_package_mty loc p l =
  List.fold_left
    (fun mty (s, _) ->
      let d = {ptype_name = mkloc (Longident.last s.txt) s.loc;
               ptype_params = [];
               ptype_cstrs = [];
               ptype_kind = Ptype_abstract;
               ptype_private = Asttypes.Public;
               ptype_manifest = None;
               ptype_attributes = [];
               ptype_loc = loc} in
      Ast_helper.Mty.mk ~loc
        (Pmty_with (mty, [ Pwith_type ({ txt = s.txt; loc }, d) ]))
    )
    (Ast_helper.Mty.mk ~loc (Pmty_ident p))
    l

(* Translation of type expressions *)

<<<<<<< HEAD
||||||| merged common ancestors
let type_variables = ref (TyVarMap.empty : type_expr TyVarMap.t)
let univars        = ref ([] : (string * type_expr) list)
let pre_univars    = ref ([] : type_expr list)
let used_variables = ref (TyVarMap.empty : (type_expr * Location.t) TyVarMap.t)

let reset_type_variables () =
  reset_global_level ();
  Ctype.reset_reified_var_counter ();
  type_variables := TyVarMap.empty

let narrow () =
  (increase_global_level (), !type_variables)

let widen (gl, tv) =
  restore_global_level gl;
  type_variables := tv

=======
let generalize_ctyp typ = generalize typ.ctyp_type

>>>>>>> ocaml/5.1
let strict_ident c = (c = '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')

let validate_name = function
    None -> None
  | Some name as s ->
      if name <> "" && strict_ident name.[0] then s else None

<<<<<<< HEAD
let new_global_var ?name jkind =
  new_global_var ?name:(validate_name name) jkind
let newvar ?name jkind =
  newvar ?name:(validate_name name) jkind
||||||| merged common ancestors
let new_global_var ?name () =
  new_global_var ?name:(validate_name name) ()
let newvar ?name () =
  newvar ?name:(validate_name name) ()

let type_variable loc name =
  try
    TyVarMap.find name !type_variables
  with Not_found ->
    raise(Error(loc, Env.empty, Unbound_type_variable ("'" ^ name)))

=======
let new_global_var ?name () =
  new_global_var ?name:(validate_name name) ()
let newvar ?name () =
  newvar ?name:(validate_name name) ()
>>>>>>> ocaml/5.1

let valid_tyvar_name name =
  name <> "" && name.[0] <> '_'

let transl_type_param_var env loc attrs name_opt
      (jkind : jkind) (jkind_annot : const_jkind option) =
  let tvar = Ttyp_var (name_opt, jkind_annot) in
  let name =
    match name_opt with
    | None -> "_"
    | Some name ->
      if not (valid_tyvar_name name) then
        raise (Error (loc, Env.empty, Invalid_variable_name ("'" ^ name)));
      if TyVarEnv.is_in_scope name then
        raise Already_bound;
      name
  in
  let ty = new_global_var ~name jkind in
  Option.iter (fun name -> TyVarEnv.add name ty) name_opt;
  { ctyp_desc = tvar; ctyp_type = ty; ctyp_env = env;
    ctyp_loc = loc; ctyp_attributes = attrs }

let transl_type_param_jst env loc attrs path :
  Jane_syntax.Core_type.t -> _ =
  function
  | Jtyp_layout (Ltyp_var { name; jkind = annot }) ->
     let jkind =
       Jkind.of_annotation ~context:(Type_parameter (path, name)) annot
     in
     transl_type_param_var env loc attrs name jkind (Some annot.txt)
  | Jtyp_layout (Ltyp_poly _ | Ltyp_alias _) ->
    Misc.fatal_error "non-type-variable in transl_type_param_jst"

let transl_type_param env path styp =
  let loc = styp.ptyp_loc in
  match Jane_syntax.Core_type.of_ast styp with
  | Some (etyp, attrs) -> transl_type_param_jst env loc attrs path etyp
  | None ->
  (* Our choice for now is that if you want a parameter of jkind any, you have
   to ask for it with an annotation.  Some restriction here seems necessary
   for backwards compatibility (e.g., we wouldn't want [type 'a id = 'a] to
   have jkind any).  But it might be possible to infer any in some cases. *)
  let jkind = Jkind.of_new_sort ~why:Unannotated_type_parameter in
  let attrs = styp.ptyp_attributes in
  match styp.ptyp_desc with
    Ptyp_any -> transl_type_param_var env loc attrs None jkind None
  | Ptyp_var name ->
<<<<<<< HEAD
    transl_type_param_var env loc attrs (Some name) jkind None
||||||| merged common ancestors
      let ty =
        try
          if not (valid_tyvar_name name) then
            raise (Error (loc, Env.empty, Invalid_variable_name ("'" ^ name)));
          ignore (TyVarMap.find name !type_variables);
          raise Already_bound
        with Not_found ->
          let v = new_global_var ~name () in
            type_variables := TyVarMap.add name v !type_variables;
            v
      in
        { ctyp_desc = Ttyp_var name; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes; }
=======
      let ty =
          if not (valid_tyvar_name name) then
            raise (Error (loc, Env.empty, Invalid_variable_name ("'" ^ name)));
          if TyVarEnv.is_in_scope name then
            raise Already_bound;
          let v = new_global_var ~name () in
          TyVarEnv.add name v;
          v
      in
        { ctyp_desc = Ttyp_var name; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes; }
>>>>>>> ocaml/5.1
  | _ -> assert false

let transl_type_param env path styp =
  (* Currently useless, since type parameters cannot hold attributes
     (but this could easily be lifted in the future). *)
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> transl_type_param env path styp)

<<<<<<< HEAD
let get_type_param_jkind path styp =
  match Jane_syntax.Core_type.of_ast styp with
  | None -> Jkind.of_new_sort ~why:Unannotated_type_parameter
  | Some (Jtyp_layout (Ltyp_var { name; jkind }), _attrs) ->
    Jkind.of_annotation ~context:(Type_parameter (path, name)) jkind
  | Some _ -> Misc.fatal_error "non-type-variable in get_type_param_jkind"

let get_type_param_name styp =
  (* We don't need to check for jane-syntax here, just to get the
     name. *)
  match styp.ptyp_desc with
  | Ptyp_any -> None
  | Ptyp_var name -> Some name
  | _ -> Misc.fatal_error "non-type-variable in get_type_param_name"

let get_alloc_mode styp =
  let locality =
    match Builtin_attributes.has_local styp.ptyp_attributes with
    | Ok true -> Locality.Const.Local
    | Ok false -> Locality.Const.Global
    | Error () ->
      raise (Error(styp.ptyp_loc, Env.empty, Unsupported_extension Local))
  in
  let uniqueness =
    match Builtin_attributes.has_unique styp.ptyp_attributes with
    | Ok true -> Uniqueness.Const.Unique
    | Ok false -> Uniqueness.Const.Shared
    | Error () ->
      raise (Error(styp.ptyp_loc, Env.empty, Unsupported_extension Unique))
  in
  let linearity =
    match Builtin_attributes.has_once styp.ptyp_attributes with
    | Ok true -> Linearity.Const.Once
    | Ok false -> Linearity.Const.Many
    | Error () ->
      raise (Error(styp.ptyp_loc, Env.empty, Unsupported_extension Unique))
  in
  { locality = locality; uniqueness; linearity }

let rec extract_params styp =
  let final styp =
    [], styp, get_alloc_mode styp
  in
  match styp.ptyp_desc with
  | Ptyp_arrow (l, a, r) ->
      let arg_mode = get_alloc_mode a in
      let params, ret, ret_mode =
        if Builtin_attributes.has_curry r.ptyp_attributes then final r
        else extract_params r
      in
      (l, arg_mode, a) :: params, ret, ret_mode
  | _ -> final styp

let check_arg_type styp =
  if not (Language_extension.is_enabled Polymorphic_parameters) then begin
    match styp.ptyp_desc with
    | Ptyp_poly _ ->
        raise (Error (styp.ptyp_loc, Env.empty,
                      Unsupported_extension Polymorphic_parameters))
    | _ -> ()
  end

(* translate the ['a 'b ('c : immediate) .] part of a polytype,
   returning something suitable as the first argument of Ttyp_poly and
   a [poly_univars] *)
let transl_bound_vars : (_, _) Either.t -> _ =
  let mk_one v = v.txt, None in
  let mk_pair (v, l) = v.txt, Option.map Location.get_txt l in
  function
  | Left vars_only -> List.map mk_one vars_only,
                      TyVarEnv.make_poly_univars vars_only
  | Right vars_jkinds -> List.map mk_pair vars_jkinds,
                          TyVarEnv.make_poly_univars_jkinds
                            ~context:(fun v -> Univar v) vars_jkinds

let rec transl_type env policy mode styp =
||||||| merged common ancestors

let new_pre_univar ?name () =
  let v = newvar ?name () in pre_univars := v :: !pre_univars; v

type poly_univars = (string * type_expr) list
let make_poly_univars vars =
  List.map (fun name -> name, newvar ~name ()) vars

let check_poly_univars env loc vars =
  vars |> List.iter (fun (_, v) -> generalize v);
  vars |> List.map (fun (name, ty1) ->
    let v = Btype.proxy ty1 in
    begin match get_desc v with
    | Tvar name when get_level v = Btype.generic_level ->
       set_type_desc v (Tunivar name)
    | _ ->
       raise (Error (loc, env, Cannot_quantify(name, v)))
    end;
    v)

let instance_poly_univars env loc vars =
  let vs = check_poly_univars env loc vars in
  vs |> List.iter (fun v ->
    match get_desc v with
    | Tunivar name ->
       set_type_desc v (Tvar name)
    | _ -> assert false);
  vs


type policy = Fixed | Extensible | Univars

let rec transl_type env policy styp =
=======
let rec transl_type env ~policy ?(aliased=false) ~row_context styp =
>>>>>>> ocaml/5.1
  Builtin_attributes.warning_scope styp.ptyp_attributes
<<<<<<< HEAD
    (fun () -> transl_type_aux env policy mode styp)
||||||| merged common ancestors
    (fun () -> transl_type_aux env policy styp)
=======
    (fun () -> transl_type_aux env ~policy ~aliased ~row_context styp)
>>>>>>> ocaml/5.1

<<<<<<< HEAD
and transl_type_aux env policy mode styp =
||||||| merged common ancestors
and transl_type_aux env policy styp =
=======
and transl_type_aux env ~row_context ~aliased ~policy styp =
>>>>>>> ocaml/5.1
  let loc = styp.ptyp_loc in
  let ctyp ctyp_desc ctyp_type =
    { ctyp_desc; ctyp_type; ctyp_env = env;
      ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes }
  in
  match Jane_syntax.Core_type.of_ast styp with
  | Some (etyp, attrs) ->
    let desc, typ = transl_type_aux_jst env policy mode attrs loc etyp in
    ctyp desc typ
  | None ->
  match styp.ptyp_desc with
    Ptyp_any ->
<<<<<<< HEAD
     let ty =
       TyVarEnv.new_anon_var loc env (Jkind.any ~why:Wildcard) policy
     in
     ctyp (Ttyp_var (None, None)) ty
||||||| merged common ancestors
      let ty =
        if policy = Univars then new_pre_univar () else
          if policy = Fixed then
            raise (Error (styp.ptyp_loc, env, Unbound_type_variable "_"))
          else newvar ()
      in
      ctyp Ttyp_any ty
=======
      let ty = TyVarEnv.new_any_var styp.ptyp_loc env policy in
      ctyp Ttyp_any ty
>>>>>>> ocaml/5.1
  | Ptyp_var name ->
<<<<<<< HEAD
      let desc, typ = transl_type_var env policy styp.ptyp_loc name None in
      ctyp desc typ
  | Ptyp_arrow _ ->
      let args, ret, ret_mode = extract_params styp in
      let rec loop acc_mode args =
        match args with
        | (l, arg_mode, arg) :: rest ->
          check_arg_type arg;
          let arg_cty = transl_type env policy arg_mode arg in
          let acc_mode =
            Alloc.Const.join
              (Alloc.Const.close_over arg_mode)
              (Alloc.Const.partial_apply acc_mode)
          in
          let acc_mode =
            Alloc.Const.join acc_mode
              (Alloc.Const.min_with_uniqueness Uniqueness.Const.Shared)
          in
          let ret_mode =
            match rest with
            | [] -> ret_mode
            | _ :: _ -> acc_mode
          in
          let ret_cty = loop acc_mode rest in
          let arg_ty = arg_cty.ctyp_type in
          let arg_ty =
            if Btype.is_Tpoly arg_ty then arg_ty else newmono arg_ty
          in
          let arg_ty =
            if not (Btype.is_optional l) then arg_ty
            else begin
              if not (Btype.tpoly_is_mono arg_ty) then
                raise (Error (arg.ptyp_loc, env, Polymorphic_optional_param));
              newmono
                (newconstr Predef.path_option [Btype.tpoly_get_mono arg_ty])
            end
          in
          let arg_mode = Alloc.of_const arg_mode in
          let ret_mode = Alloc.of_const ret_mode in
          let arrow_desc = (l, arg_mode, ret_mode) in
          (* CR layouts v3: For now, we require function arguments and returns
             to have a representable jkind.  See comment in
             [Ctype.filter_arrow].  *)
          begin match
            Ctype.type_sort ~why:Function_argument env arg_ty,
            Ctype.type_sort ~why:Function_result env ret_cty.ctyp_type
          with
          | Ok _, Ok _ -> ()
          | Error e, _ ->
            raise (Error(arg.ptyp_loc, env,
                         Non_sort {vloc = Fun_arg; err = e; typ = arg_ty}))
          | _, Error e ->
            raise (Error(ret.ptyp_loc, env,
                         Non_sort
                           {vloc = Fun_ret; err = e; typ = ret_cty.ctyp_type}))
          end;
          let ty =
            newty (Tarrow(arrow_desc, arg_ty, ret_cty.ctyp_type, commu_ok))
          in
          ctyp (Ttyp_arrow (l, arg_cty, ret_cty)) ty
        | [] -> transl_type env policy ret_mode ret
      in
      loop mode args
||||||| merged common ancestors
    let ty =
      if not (valid_tyvar_name name) then
        raise (Error (styp.ptyp_loc, env, Invalid_variable_name ("'" ^ name)));
      begin try
        instance (List.assoc name !univars)
      with Not_found -> try
        instance (fst (TyVarMap.find name !used_variables))
      with Not_found ->
        let v =
          if policy = Univars then new_pre_univar ~name () else newvar ~name ()
        in
        used_variables := TyVarMap.add name (v, styp.ptyp_loc) !used_variables;
        v
      end
    in
    ctyp (Ttyp_var name) ty
  | Ptyp_arrow(l, st1, st2) ->
    let cty1 = transl_type env policy st1 in
    let cty2 = transl_type env policy st2 in
    let ty1 = cty1.ctyp_type in
    let ty1 =
      if Btype.is_optional l
      then newty (Tconstr(Predef.path_option,[ty1], ref Mnil))
      else ty1 in
    let ty = newty (Tarrow(l, ty1, cty2.ctyp_type, commu_ok)) in
    ctyp (Ttyp_arrow (l, cty1, cty2)) ty
=======
    let ty =
      if not (valid_tyvar_name name) then
        raise (Error (styp.ptyp_loc, env, Invalid_variable_name ("'" ^ name)));
      begin try
        TyVarEnv.lookup_local ~row_context:row_context name
      with Not_found ->
        let v = TyVarEnv.new_var ~name policy in
        TyVarEnv.remember_used name v styp.ptyp_loc;
        v
      end
    in
    ctyp (Ttyp_var name) ty
  | Ptyp_arrow(l, st1, st2) ->
    let cty1 = transl_type env ~policy ~row_context st1 in
    let cty2 = transl_type env ~policy ~row_context st2 in
    let ty1 = cty1.ctyp_type in
    let ty1 =
      if Btype.is_optional l
      then newty (Tconstr(Predef.path_option,[ty1], ref Mnil))
      else ty1 in
    let ty = newty (Tarrow(l, ty1, cty2.ctyp_type, commu_ok)) in
    ctyp (Ttyp_arrow (l, cty1, cty2)) ty
>>>>>>> ocaml/5.1
  | Ptyp_tuple stl ->
    assert (List.length stl >= 2);
<<<<<<< HEAD
    let ctys = List.map (transl_type env policy Alloc.Const.legacy) stl in
    List.iter (fun {ctyp_type; ctyp_loc} ->
      (* CR layouts v5: remove value requirement *)
      match
        constrain_type_jkind
          env ctyp_type (Jkind.value ~why:Tuple_element)
      with
      | Ok _ -> ()
      | Error e ->
        raise (Error(ctyp_loc, env,
                     Non_value {vloc = Tuple; err = e; typ = ctyp_type})))
      ctys;
||||||| merged common ancestors
    let ctys = List.map (transl_type env policy) stl in
=======
    let ctys = List.map (transl_type env ~policy ~row_context) stl in
>>>>>>> ocaml/5.1
    let ty = newty (Ttuple (List.map (fun ctyp -> ctyp.ctyp_type) ctys)) in
    ctyp (Ttyp_tuple ctys) ty
  | Ptyp_constr(lid, stl) ->
      let (path, decl) = Env.lookup_type ~loc:lid.loc lid.txt env in
      let stl =
        match stl with
        | [ {ptyp_desc=Ptyp_any} as t ] when decl.type_arity > 1 ->
            List.map (fun _ -> t) decl.type_params
        | _ -> stl
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
<<<<<<< HEAD
      let args = List.map (transl_type env policy Alloc.Const.legacy) stl in
||||||| merged common ancestors
      let args = List.map (transl_type env policy) stl in
=======
      let args = List.map (transl_type env ~policy ~row_context) stl in
>>>>>>> ocaml/5.1
      let params = instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if get_level ty = Btype.generic_level then unify_var else unify
      in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_param env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine stl args) params;
      let constr =
        newconstr path (List.map (fun ctyp -> ctyp.ctyp_type) args) in
      ctyp (Ttyp_constr (path, lid, args)) constr
  | Ptyp_object (fields, o) ->
      let ty, fields = transl_fields env ~policy ~row_context o fields in
      ctyp (Ttyp_object (fields, o)) (newobj ty)
  | Ptyp_class(lid, stl) ->
      let (path, decl) =
        let path, decl = Env.lookup_cltype ~loc:lid.loc lid.txt env in
        (path, decl.clty_hash_type)
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
<<<<<<< HEAD
      let args = List.map (transl_type env policy Alloc.Const.legacy) stl in
      let params = instance_list decl.type_params in
||||||| merged common ancestors
      let args = List.map (transl_type env policy) stl in
      let params = instance_list decl.type_params in
=======
      let args = List.map (transl_type env ~policy ~row_context) stl in
      let body = Option.get decl.type_manifest in
      let (params, body) = instance_parameterized_type decl.type_params body in
>>>>>>> ocaml/5.1
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_var env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine stl args) params;
      let ty_args = List.map (fun ctyp -> ctyp.ctyp_type) args in
      let ty = Ctype.apply ~use_current_level:true env params body ty_args in
      let ty = match get_desc ty with
<<<<<<< HEAD
        Tvariant row ->
          let fields =
            List.map
              (fun (l,f) -> l,
                match row_field_repr f with
                | Rpresent oty -> rf_either_of oty
                | _ -> f)
              (row_fields row)
          in
          (* NB: row is always non-static here; more is thus never Tnil *)
          let more =
            TyVarEnv.new_var (Jkind.value ~why:Row_variable) policy
          in
          let row =
            create_row ~fields ~more
              ~closed:true ~fixed:None ~name:(Some (path, ty_args))
          in
          newty (Tvariant row)
      | Tobject (fi, _) ->
          let _, tv = flatten_fields fi in
          TyVarEnv.add_pre_univar tv policy;
          ty
      | _ ->
          assert false
||||||| merged common ancestors
        Tvariant row ->
          let fields =
            List.map
              (fun (l,f) -> l,
                match row_field_repr f with
                | Rpresent oty -> rf_either_of oty
                | _ -> f)
              (row_fields row)
          in
          (* NB: row is always non-static here; more is thus never Tnil *)
          let more =
            if policy = Univars then new_pre_univar () else newvar () in
          let row =
            create_row ~fields ~more
              ~closed:true ~fixed:None ~name:(Some (path, ty_args)) in
          newty (Tvariant row)
      | Tobject (fi, _) ->
          let _, tv = flatten_fields fi in
          if policy = Univars then pre_univars := tv :: !pre_univars;
          ty
      | _ ->
          assert false
=======
        | Tobject (fi, _) ->
            let _, tv = flatten_fields fi in
            TyVarEnv.add_pre_univar tv policy;
            ty
        | _ ->
            assert false
>>>>>>> ocaml/5.1
      in
      ctyp (Ttyp_class (path, lid, args)) ty
  | Ptyp_alias(st, alias) ->
<<<<<<< HEAD
    let desc, typ = transl_type_alias env policy mode loc st (Some alias) None in
    ctyp desc typ
||||||| merged common ancestors
      let cty =
        try
          let t =
            try List.assoc alias !univars
            with Not_found ->
              instance (fst(TyVarMap.find alias !used_variables))
          in
          let ty = transl_type env policy st in
          begin try unify_var env t ty.ctyp_type with Unify err ->
            let err = Errortrace.swap_unification_error err in
            raise(Error(styp.ptyp_loc, env, Alias_type_mismatch err))
          end;
          ty
        with Not_found ->
          if !Clflags.principal then begin_def ();
          let t = newvar () in
          used_variables :=
            TyVarMap.add alias (t, styp.ptyp_loc) !used_variables;
          let ty = transl_type env policy st in
          begin try unify_var env t ty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
            raise(Error(styp.ptyp_loc, env, Alias_type_mismatch err))
          end;
          if !Clflags.principal then begin
            end_def ();
            generalize_structure t;
          end;
          let t = instance t in
          let px = Btype.proxy t in
          begin match get_desc px with
          | Tvar None -> set_type_desc px (Tvar (Some alias))
          | Tunivar None -> set_type_desc px (Tunivar (Some alias))
          | _ -> ()
          end;
          { ty with ctyp_type = t }
      in
      ctyp (Ttyp_alias (cty, alias)) cty.ctyp_type
=======
      let cty =
        try
          let t = TyVarEnv.lookup_local ~row_context alias in
          let ty = transl_type env ~policy ~aliased:true ~row_context st in
          begin try unify_var env t ty.ctyp_type with Unify err ->
            let err = Errortrace.swap_unification_error err in
            raise(Error(styp.ptyp_loc, env, Alias_type_mismatch err))
          end;
          ty
        with Not_found ->
          let t, ty =
            with_local_level_if_principal begin fun () ->
              let t = newvar () in
              TyVarEnv.remember_used alias t styp.ptyp_loc;
              let ty = transl_type env ~policy ~row_context st in
              begin try unify_var env t ty.ctyp_type with Unify err ->
                let err = Errortrace.swap_unification_error err in
                raise(Error(styp.ptyp_loc, env, Alias_type_mismatch err))
              end;
              (t, ty)
            end
            ~post: (fun (t, _) -> generalize_structure t)
          in
          let t = instance t in
          let px = Btype.proxy t in
          begin match get_desc px with
          | Tvar None -> set_type_desc px (Tvar (Some alias))
          | Tunivar None -> set_type_desc px (Tunivar (Some alias))
          | _ -> ()
          end;
          { ty with ctyp_type = t }
      in
      ctyp (Ttyp_alias (cty, alias)) cty.ctyp_type
>>>>>>> ocaml/5.1
  | Ptyp_variant(fields, closed, present) ->
      let name = ref None in
      let mkfield l f =
        newty (Tvariant (create_row ~fields:[l,f]
                           ~more:(newvar (Jkind.value ~why:Row_variable))
                           ~closed:true ~fixed:None ~name:None)) in
      let hfields = Hashtbl.create 17 in
      let add_typed_field loc l f =
        let h = Btype.hash_variant l in
        try
          let (l',f') = Hashtbl.find hfields h in
          (* Check for tag conflicts *)
          if l <> l' then raise(Error(styp.ptyp_loc, env, Variant_tags(l, l')));
          let ty = mkfield l f and ty' = mkfield l f' in
          if is_equal env false [ty] [ty'] then () else
          try unify env ty ty'
          with Unify _trace ->
            raise(Error(loc, env, Constructor_mismatch (ty,ty')))
        with Not_found ->
          Hashtbl.add hfields h (l,f)
      in
      let add_field row_context field =
        let rf_loc = field.prf_loc in
        let rf_attributes = field.prf_attributes in
        let rf_desc = match field.prf_desc with
        | Rtag (l, c, stl) ->
            name := None;
            let tl =
              Builtin_attributes.warning_scope rf_attributes
<<<<<<< HEAD
                (fun () ->
                   List.map (transl_type env policy Alloc.Const.legacy) stl)
||||||| merged common ancestors
                (fun () -> List.map (transl_type env policy) stl)
=======
                (fun () -> List.map (transl_type env ~policy ~row_context) stl)
>>>>>>> ocaml/5.1
            in
            List.iter (fun {ctyp_type; ctyp_loc} ->
              (* CR layouts: at some point we'll allow different jkinds in
                 polymorphic variants. *)
              match
                constrain_type_jkind env ctyp_type
                  (Jkind.value ~why:Polymorphic_variant_field)
              with
              | Ok _ -> ()
              | Error e ->
                raise (Error(ctyp_loc, env,
                             Non_value {vloc = Poly_variant; err = e;
                                        typ = ctyp_type})))
              tl;
            let f = match present with
              Some present when not (List.mem l.txt present) ->
                let ty_tl = List.map (fun cty -> cty.ctyp_type) tl in
                rf_either ty_tl ~no_arg:c ~matched:false
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, env,
                              Present_has_conjunction l.txt));
                match tl with [] -> rf_present None
                | st :: _ -> rf_present (Some st.ctyp_type)
            in
            add_typed_field styp.ptyp_loc l.txt f;
              Ttag (l,c,tl)
        | Rinherit sty ->
<<<<<<< HEAD
          let cty = transl_type env policy Alloc.Const.legacy sty in
||||||| merged common ancestors
            let cty = transl_type env policy sty in
=======
            let cty = transl_type env ~policy ~row_context sty in
>>>>>>> ocaml/5.1
            let ty = cty.ctyp_type in
            let nm =
              match get_desc cty.ctyp_type with
                Tconstr(p, tl, _) -> Some(p, tl)
              | _                 -> None
            in
            name := if Hashtbl.length hfields <> 0 then None else nm;
            let fl = match get_desc (expand_head env cty.ctyp_type), nm with
              Tvariant row, _ when Btype.static_row row ->
                row_fields row
            | Tvar _, Some(p, _) ->
                raise(Error(sty.ptyp_loc, env, Undefined_type_constructor p))
            | _ ->
                raise(Error(sty.ptyp_loc, env, Not_a_variant ty))
            in
            List.iter
              (fun (l, f) ->
                let f = match present with
                  Some present when not (List.mem l present) ->
                    begin match row_field_repr f with
                      Rpresent oty -> rf_either_of oty
                    | _ -> assert false
                    end
                | _ -> f
                in
                add_typed_field sty.ptyp_loc l f)
              fl;
              Tinherit cty
        in
        { rf_desc; rf_loc; rf_attributes; }
      in
      let more_slot = ref None in
      let row_context =
        if aliased then row_context else more_slot :: row_context
      in
      let tfields = List.map (add_field row_context) fields in
      let fields = List.rev (Hashtbl.fold (fun _ p l -> p :: l) hfields []) in
      begin match present with None -> ()
      | Some present ->
          List.iter
            (fun l -> if not (List.mem_assoc l fields) then
              raise(Error(styp.ptyp_loc, env, Present_has_no_type l)))
            present
      end;
      let name = !name in
      let make_row more =
        create_row ~fields ~more ~closed:(closed = Closed) ~fixed:None ~name
      in
      let more =
<<<<<<< HEAD
        if Btype.static_row
             (make_row (newvar (Jkind.value ~why:Row_variable)))
        then newty Tnil
        else TyVarEnv.new_var (Jkind.value ~why:Row_variable) policy
||||||| merged common ancestors
        if Btype.static_row (make_row (newvar ())) then newty Tnil else
        if policy = Univars then new_pre_univar () else newvar ()
=======
        if Btype.static_row (make_row (newvar ())) then newty Tnil else
           TyVarEnv.new_var policy
>>>>>>> ocaml/5.1
      in
      more_slot := Some more;
      let ty = newty (Tvariant (make_row more)) in
      ctyp (Ttyp_variant (tfields, closed, present)) ty
  | Ptyp_poly(vars, st) ->
<<<<<<< HEAD
      let desc, typ =
        transl_type_poly env policy mode styp.ptyp_loc (Either.Left vars) st
      in
      ctyp desc typ
||||||| merged common ancestors
      let vars = List.map (fun v -> v.txt) vars in
      begin_def();
      let new_univars = make_poly_univars vars in
      let old_univars = !univars in
      univars := new_univars @ !univars;
      let cty = transl_type env policy st in
      let ty = cty.ctyp_type in
      univars := old_univars;
      end_def();
      generalize ty;
      let ty_list = check_poly_univars env styp.ptyp_loc new_univars in
      let ty_list = List.filter (fun v -> deep_occur v ty) ty_list in
      let ty' = Btype.newgenty (Tpoly(ty, ty_list)) in
      unify_var env (newvar()) ty';
      ctyp (Ttyp_poly (vars, cty)) ty'
=======
      let vars = List.map (fun v -> v.txt) vars in
      let new_univars, cty =
        with_local_level begin fun () ->
          let new_univars = TyVarEnv.make_poly_univars vars in
          let cty = TyVarEnv.with_univars new_univars begin fun () ->
            transl_type env ~policy ~row_context st
          end in
          (new_univars, cty)
        end
        ~post:(fun (_,cty) -> generalize_ctyp cty)
      in
      let ty = cty.ctyp_type in
      let ty_list = TyVarEnv.check_poly_univars env styp.ptyp_loc new_univars in
      let ty_list = List.filter (fun v -> deep_occur v ty) ty_list in
      let ty' = Btype.newgenty (Tpoly(ty, ty_list)) in
      unify_var env (newvar()) ty';
      ctyp (Ttyp_poly (vars, cty)) ty'
>>>>>>> ocaml/5.1
  | Ptyp_package (p, l) ->
<<<<<<< HEAD
    (* CR layouts: right now we're doing a real gross hack where we demand
       everything in a package type with constraint be value.

       An alternative is to walk into the constrained module, using the
       longidents, and find the actual things that need jkind checking.
       See [Typemod.package_constraints_sig] for code that does a
       similar traversal from a longident.
    *)
    (* CR layouts: and in the long term, rewrite all of this to eliminate
       the [create_package_mty] hack that constructs fake source code. *)
      let l, mty = create_package_mty true styp.ptyp_loc env (p, l) in
      let mty = TyVarEnv.with_local_scope (fun () -> !transl_modtype env mty) in
||||||| merged common ancestors
      let l, mty = create_package_mty true styp.ptyp_loc env (p, l) in
      let z = narrow () in
      let mty = !transl_modtype env mty in
      widen z;
=======
      let loc = styp.ptyp_loc in
      let l = sort_constraints_no_duplicates loc env l in
      let mty = create_package_mty loc p l in
      let mty =
        TyVarEnv.with_local_scope (fun () -> !transl_modtype env mty) in
>>>>>>> ocaml/5.1
      let ptys = List.map (fun (s, pty) ->
<<<<<<< HEAD
                             s, transl_type env policy Alloc.Const.legacy pty
||||||| merged common ancestors
                             s, transl_type env policy pty
=======
                             s, transl_type env ~policy ~row_context pty
>>>>>>> ocaml/5.1
                          ) l in
<<<<<<< HEAD
      List.iter (fun (s,{ctyp_type=ty}) ->
        match
          Ctype.constrain_type_jkind env ty (Jkind.value ~why:Package_hack)
        with
        | Ok _ -> ()
        | Error e ->
          raise (Error(s.loc,env,
                       Non_value {vloc=Package_constraint; typ=ty; err=e})))
        ptys;
      let path = !transl_modtype_longident styp.ptyp_loc env p.txt in
||||||| merged common ancestors
      let path = !transl_modtype_longident styp.ptyp_loc env p.txt in
=======
      let path = !transl_modtype_longident loc env p.txt in
>>>>>>> ocaml/5.1
      let ty = newty (Tpackage (path,
                       List.map (fun (s, cty) -> (s.txt, cty.ctyp_type)) ptys))
      in
      ctyp (Ttyp_package {
            pack_path = path;
            pack_type = mty.mty_type;
            pack_fields = ptys;
            pack_txt = p;
           }) ty
  | Ptyp_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

<<<<<<< HEAD
and transl_type_aux_jst env policy mode _attrs loc :
      Jane_syntax.Core_type.t -> _ = function
  | Jtyp_layout typ -> transl_type_aux_jst_layout env policy mode loc typ

and transl_type_aux_jst_layout env policy mode loc :
      Jane_syntax.Layouts.core_type -> _ = function
  | Ltyp_var { name = None; jkind } ->
    let tjkind = Jkind.of_annotation ~context:(Type_wildcard loc) jkind in
    Ttyp_var (None, Some jkind.txt),
    TyVarEnv.new_anon_var loc env tjkind policy
  | Ltyp_var { name = Some name; jkind } ->
    transl_type_var env policy loc name (Some jkind)
  | Ltyp_poly { bound_vars; inner_type } ->
    transl_type_poly env policy mode loc (Either.Right bound_vars) inner_type
  | Ltyp_alias { aliased_type; name; jkind } ->
    transl_type_alias env policy mode loc aliased_type name (Some jkind)

and transl_type_var env policy loc name jkind_annot_opt =
  let print_name = "'" ^ name in
  if not (valid_tyvar_name name) then
    raise (Error (loc, env, Invalid_variable_name print_name));
  let of_annot = Jkind.of_annotation ~context:(Type_variable print_name) in
  let ty = try
      let ty = TyVarEnv.lookup_local name in
      begin match jkind_annot_opt with
      | None -> ()
      | Some jkind_annot ->
         let jkind = of_annot jkind_annot in
         match constrain_type_jkind env ty jkind with
         | Ok () -> ()
         | Error err ->
            raise (Error(jkind_annot.loc, env, Bad_jkind_annot (ty, err)))
      end;
      ty
    with Not_found ->
      let jkind = match jkind_annot_opt with
        | None -> Jkind.any ~why:Unification_var
        | Some jkind_annot -> of_annot jkind_annot
      in
      let ty = TyVarEnv.new_var ~name jkind policy in
      TyVarEnv.remember_used name ty loc;
      ty
  in
  Ttyp_var (Some name, Option.map Location.get_txt jkind_annot_opt), ty

and transl_type_poly env policy mode loc (vars : (_, _) Either.t) st =
  begin_def();
  let typed_vars, new_univars = transl_bound_vars vars in
  let cty = TyVarEnv.with_univars new_univars begin fun () ->
    transl_type env policy mode st
  end in
  let ty = cty.ctyp_type in
  end_def();
  generalize ty;
  let ty_list = TyVarEnv.check_poly_univars env loc new_univars in
  let ty_list = List.filter (fun v -> deep_occur v ty) ty_list in
  let ty' = Btype.newgenty (Tpoly(ty, ty_list)) in
  unify_var env (newvar (Jkind.any ~why:Dummy_jkind)) ty';
  Ttyp_poly (typed_vars, cty), ty'

and transl_type_alias env policy mode alias_loc styp name_opt jkind_annot_opt =
  let cty = match name_opt with
    | Some alias ->
      begin try
        let t = TyVarEnv.lookup_local alias in
        let cty = transl_type env policy mode styp in
        begin try unify_var env t cty.ctyp_type with Unify err ->
          let err = Errortrace.swap_unification_error err in
          raise(Error(alias_loc, env, Alias_type_mismatch err))
        end;
        begin match jkind_annot_opt with
        | None -> ()
        | Some jkind_annot ->
          let jkind =
            Jkind.of_annotation ~context:(Type_variable alias) jkind_annot
          in
          begin match constrain_type_jkind env t jkind with
          | Ok () -> ()
          | Error err ->
            raise (Error(jkind_annot.loc, env, Bad_jkind_annot(t, err)))
          end
        end;
        cty
      with Not_found ->
        if !Clflags.principal then begin_def ();
        let jkind =
          Jkind.(of_annotation_option_default
            ~default:(any ~why:Dummy_jkind)
            ~context:(Type_variable alias)
            jkind_annot_opt)
        in
        let t = newvar jkind in
        TyVarEnv.remember_used alias t alias_loc;
        let cty = transl_type env policy mode styp in
        begin try unify_var env t cty.ctyp_type with Unify err ->
          let err = Errortrace.swap_unification_error err in
          raise(Error(alias_loc, env, Alias_type_mismatch err))
        end;
        if !Clflags.principal then begin
          end_def ();
          generalize_structure t;
        end;
        let t = instance t in
        let px = Btype.proxy t in
        begin match get_desc px with
        | Tvar { name = None; jkind } ->
           set_type_desc px (Tvar { name = Some alias; jkind })
        | Tunivar { name = None; jkind } ->
           set_type_desc px (Tunivar {name = Some alias; jkind})
        | _ -> ()
        end;
        { cty with ctyp_type = t }
      end
    | None ->
      let cty = transl_type env policy mode styp in
      let cty_expr = cty.ctyp_type in
      let jkind_annot = match jkind_annot_opt with
        | None -> Misc.fatal_error "anonymous alias without layout annotation"
        | Some jkind_annot -> jkind_annot
      in
      let jkind =
          Jkind.of_annotation
            ~context:(Type_wildcard jkind_annot.loc) jkind_annot
      in
      begin match constrain_type_jkind env cty_expr jkind with
      | Ok () -> ()
      | Error err ->
        raise (Error(jkind_annot.loc, env,
                     Bad_jkind_annot(cty_expr, err)))
      end;
      cty
  in
  Ttyp_alias (cty, name_opt, Option.map Location.get_txt jkind_annot_opt),
  cty.ctyp_type

and transl_fields env policy o fields =
||||||| merged common ancestors
and transl_fields env policy o fields =
=======
and transl_fields env ~policy ~row_context o fields =
>>>>>>> ocaml/5.1
  let hfields = Hashtbl.create 17 in
  let add_typed_field loc l ty =
    try
      let ty' = Hashtbl.find hfields l in
      if is_equal env false [ty] [ty'] then () else
        try unify env ty ty'
        with Unify _trace ->
          raise(Error(loc, env, Method_mismatch (l, ty, ty')))
    with Not_found ->
      Hashtbl.add hfields l ty in
  let add_field {pof_desc; pof_loc; pof_attributes;} =
    let of_loc = pof_loc in
    let of_attributes = pof_attributes in
    let of_desc = match pof_desc with
    | Otag (s, ty1) -> begin
        let ty1 =
          Builtin_attributes.warning_scope of_attributes
<<<<<<< HEAD
            (fun () ->
               transl_type env policy Alloc.Const.legacy
                 (Ast_helper.Typ.force_poly ty1))
||||||| merged common ancestors
            (fun () -> transl_type env policy (Ast_helper.Typ.force_poly ty1))
=======
            (fun () -> transl_type env ~policy ~row_context
                (Ast_helper.Typ.force_poly ty1))
>>>>>>> ocaml/5.1
        in
        begin
          match
            constrain_type_jkind
              env ty1.ctyp_type (Jkind.value ~why:Object_field)
          with
          | Ok _ -> ()
          | Error e ->
            raise (Error(of_loc, env,
                         Non_value {vloc = Object_field; err = e;
                                    typ = ty1.ctyp_type}))
        end;
        let field = OTtag (s, ty1) in
        add_typed_field ty1.ctyp_loc s.txt ty1.ctyp_type;
        field
      end
    | Oinherit sty -> begin
<<<<<<< HEAD
        let cty = transl_type env policy Alloc.Const.legacy sty in
||||||| merged common ancestors
        let cty = transl_type env policy sty in
=======
        let cty = transl_type env ~policy ~row_context sty in
>>>>>>> ocaml/5.1
        let nm =
          match get_desc cty.ctyp_type with
            Tconstr(p, _, _) -> Some p
          | _                -> None in
        let t = expand_head env cty.ctyp_type in
        match get_desc t, nm with
          Tobject (tf, _), _
          when (match get_desc tf with Tfield _ | Tnil -> true | _ -> false) ->
            begin
              if opened_object t then
                raise (Error (sty.ptyp_loc, env, Opened_object nm));
              let rec iter_add ty =
                match get_desc ty with
                | Tfield (s, _k, ty1, ty2) ->
                    add_typed_field sty.ptyp_loc s ty1;
                    iter_add ty2
                | Tnil -> ()
                | _ -> assert false
              in
              iter_add tf;
              OTinherit cty
            end
        | Tvar _, Some p ->
            raise (Error (sty.ptyp_loc, env, Undefined_type_constructor p))
        | _ -> raise (Error (sty.ptyp_loc, env, Not_an_object t))
      end in
    { of_desc; of_loc; of_attributes; }
  in
  let object_fields = List.map add_field fields in
  let fields = Hashtbl.fold (fun s ty l -> (s, ty) :: l) hfields [] in
  let ty_init =
     match o with
     | Closed -> newty Tnil
<<<<<<< HEAD
     | Open -> TyVarEnv.new_var (Jkind.value ~why:Row_variable) policy
||||||| merged common ancestors
     match o, policy with
     | Closed, _ -> newty Tnil
     | Open, Univars -> new_pre_univar ()
     | Open, _ -> newvar () in
=======
     | Open -> TyVarEnv.new_var policy
>>>>>>> ocaml/5.1
  in
  let ty = List.fold_left (fun ty (s, ty') ->
      newty (Tfield (s, field_public, ty', ty))) ty_init fields in
  ty, object_fields

(* Make the rows "fixed" in this type, to make universal check easier *)
let rec make_fixed_univars ty =
  if Btype.try_mark_node ty then
    begin match get_desc ty with
    | Tvariant row ->
        let Row {fields; more; name; closed} = row_repr row in
        if Btype.is_Tunivar more then
          let fields =
            List.map
              (fun (s,f as p) -> match row_field_repr f with
                Reither (no_arg, tl, _m) ->
                  s, rf_either tl ~use_ext_of:f ~no_arg ~matched:true
              | _ -> p)
              fields
          in
          set_type_desc ty
            (Tvariant
               (create_row ~fields ~more ~name ~closed
                  ~fixed:(Some (Univar more))));
        Btype.iter_row make_fixed_univars row
    | _ ->
        Btype.iter_type_expr make_fixed_univars ty
    end

let transl_type env policy styp =
  transl_type env ~policy ~row_context:[] styp

let make_fixed_univars ty =
  make_fixed_univars ty;
  Btype.unmark_type ty

<<<<<<< HEAD
let create_package_mty = create_package_mty false

let transl_simple_type env ?univars ~closed mode styp =
  TyVarEnv.reset_locals ?univars ();
  let policy = TyVarEnv.(if closed then fixed_policy else extensible_policy) in
  let typ = transl_type env policy mode styp in
||||||| merged common ancestors
let create_package_mty = create_package_mty false

let globalize_used_variables env fixed =
  let r = ref [] in
  TyVarMap.iter
    (fun name (ty, loc) ->
      let v = new_global_var () in
      let snap = Btype.snapshot () in
      if try unify env v ty; true with _ -> Btype.backtrack snap; false
      then try
        r := (loc, v,  TyVarMap.find name !type_variables) :: !r
      with Not_found ->
        if fixed && Btype.is_Tvar ty then
          raise(Error(loc, env, Unbound_type_variable ("'"^name)));
        let v2 = new_global_var () in
        r := (loc, v, v2) :: !r;
        type_variables := TyVarMap.add name v2 !type_variables)
    !used_variables;
  used_variables := TyVarMap.empty;
  fun () ->
    List.iter
      (function (loc, t1, t2) ->
        try unify env t1 t2 with Unify err ->
          raise (Error(loc, env, Type_mismatch err)))
      !r

let transl_simple_type env ?univars:(uvs=[]) fixed styp =
  univars := uvs; used_variables := TyVarMap.empty;
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  globalize_used_variables env fixed ();
=======
let transl_simple_type env ?univars ~closed styp =
  TyVarEnv.reset_locals ?univars ();
  let policy = TyVarEnv.(if closed then fixed_policy else extensible_policy) in
  let typ = transl_type env policy styp in
>>>>>>> ocaml/5.1
  TyVarEnv.globalize_used_variables policy env ();
  make_fixed_univars typ.ctyp_type;
  typ

let transl_simple_type_univars env styp =
  TyVarEnv.reset_locals ();
<<<<<<< HEAD
  let typ, univs = TyVarEnv.collect_univars begin fun () ->
    begin_def ();
    let policy = TyVarEnv.univars_policy in
    let typ = transl_type env policy Alloc.Const.legacy styp in
    TyVarEnv.globalize_used_variables policy env ();
    end_def ();
    generalize typ.ctyp_type;
    typ
||||||| merged common ancestors
  univars := []; used_variables := TyVarMap.empty; pre_univars := [];
  begin_def ();
  let typ = transl_type env Univars styp in
  (* Only keep already global variables in used_variables *)
  let new_variables = !used_variables in
  used_variables := TyVarMap.empty;
  TyVarMap.iter
    (fun name p ->
      if TyVarMap.mem name !type_variables then
        used_variables := TyVarMap.add name p !used_variables)
    new_variables;
  globalize_used_variables env false ();
  end_def ();
  generalize typ.ctyp_type;
  let univs =
    List.fold_left
      (fun acc v ->
        match get_desc v with
          Tvar name when get_level v = Btype.generic_level ->
            set_type_desc v (Tunivar name); v :: acc
        | _ -> acc)
      [] !pre_univars
  in
=======
  let typ, univs =
    TyVarEnv.collect_univars begin fun () ->
      with_local_level ~post:generalize_ctyp begin fun () ->
        let policy = TyVarEnv.univars_policy in
        let typ = transl_type env policy styp in
        TyVarEnv.globalize_used_variables policy env ();
        typ
      end
>>>>>>> ocaml/5.1
  end in
  make_fixed_univars typ.ctyp_type;
    { typ with ctyp_type =
        instance (Btype.newgenty (Tpoly (typ.ctyp_type, univs))) }

<<<<<<< HEAD
let transl_simple_type_delayed env mode styp =
  TyVarEnv.reset_locals ();
  begin_def ();
  let policy = TyVarEnv.extensible_policy in
  let typ = transl_type env policy mode styp in
  end_def ();
  make_fixed_univars typ.ctyp_type;
  (* This brings the used variables to the global level, but doesn't link them
     to their other occurrences just yet. This will be done when [force] is
     called. *)
  let force = TyVarEnv.globalize_used_variables policy env in
  (* Generalizes everything except the variables that were just globalized. *)
  generalize typ.ctyp_type;
||||||| merged common ancestors
let transl_simple_type_delayed env styp =
  univars := []; used_variables := TyVarMap.empty;
  begin_def ();
  let typ = transl_type env Extensible styp in
  end_def ();
  make_fixed_univars typ.ctyp_type;
  (* This brings the used variables to the global level, but doesn't link them
     to their other occurrences just yet. This will be done when [force] is
     called. *)
  let force = globalize_used_variables env false in
  (* Generalizes everything except the variables that were just globalized. *)
  generalize typ.ctyp_type;
=======
let transl_simple_type_delayed env styp =
  TyVarEnv.reset_locals ();
  let typ, force =
    with_local_level begin fun () ->
      let policy = TyVarEnv.extensible_policy in
      let typ = transl_type env policy styp in
      make_fixed_univars typ.ctyp_type;
      (* This brings the used variables to the global level, but doesn't link
         them to their other occurrences just yet. This will be done when
         [force] is  called. *)
      let force = TyVarEnv.globalize_used_variables policy env in
      (typ, force)
    end
    (* Generalize everything except the variables that were just globalized. *)
    ~post:(fun (typ,_) -> generalize_ctyp typ)
  in
>>>>>>> ocaml/5.1
  (typ, instance typ.ctyp_type, force)

let transl_type_scheme_mono env styp =
  begin_def();
  let typ = transl_simple_type env ~closed:false Alloc.Const.legacy styp in
  end_def();
  (* This next line is very important: it stops [val] and [external]
     declarations from having undefaulted jkind variables. Without
     this line, we might accidentally export a jkind-flexible definition
     from a compilation unit, which would lead to miscompilation. *)
  remove_mode_and_jkind_variables typ.ctyp_type;
  generalize typ.ctyp_type;
  typ

let transl_type_scheme_poly env attrs loc vars inner_type =
  begin_def();
  let typed_vars, univars = transl_bound_vars vars in
  let typ =
    transl_simple_type env ~univars ~closed:true Alloc.Const.legacy inner_type
  in
  end_def();
  generalize typ.ctyp_type;
  let _ = TyVarEnv.instance_poly_univars env loc univars in
  { ctyp_desc = Ttyp_poly (typed_vars, typ);
    ctyp_type = typ.ctyp_type;
    ctyp_env = env;
    ctyp_loc = loc;
    ctyp_attributes = attrs }

let transl_type_scheme_jst env styp attrs loc : Jane_syntax.Core_type.t -> _ =
  function
  | Jtyp_layout (Ltyp_poly { bound_vars; inner_type }) ->
    transl_type_scheme_poly env attrs loc (Right bound_vars) inner_type
  | Jtyp_layout (Ltyp_var _ | Ltyp_alias _) ->
    transl_type_scheme_mono env styp

let transl_type_scheme env styp =
<<<<<<< HEAD
  TyVarEnv.reset ();
  match Jane_syntax.Core_type.of_ast styp with
  | Some (etyp, attrs) ->
    transl_type_scheme_jst env styp attrs styp.ptyp_loc etyp
  | None ->
||||||| merged common ancestors
  reset_type_variables();
=======
>>>>>>> ocaml/5.1
  match styp.ptyp_desc with
  | Ptyp_poly (vars, st) ->
<<<<<<< HEAD
    transl_type_scheme_poly env styp.ptyp_attributes
      styp.ptyp_loc (Either.Left vars) st
||||||| merged common ancestors
     begin_def();
     let vars = List.map (fun v -> v.txt) vars in
     let univars = make_poly_univars vars in
     let typ = transl_simple_type env ~univars true st in
     end_def();
     generalize typ.ctyp_type;
     let _ = instance_poly_univars env styp.ptyp_loc univars in
     { ctyp_desc = Ttyp_poly (vars, typ);
       ctyp_type = typ.ctyp_type;
       ctyp_env = env;
       ctyp_loc = styp.ptyp_loc;
       ctyp_attributes = styp.ptyp_attributes }
=======
     let vars = List.map (fun v -> v.txt) vars in
     let univars, typ =
       with_local_level begin fun () ->
         TyVarEnv.reset ();
         let univars = TyVarEnv.make_poly_univars vars in
         let typ = transl_simple_type env ~univars ~closed:true st in
         (univars, typ)
       end
       ~post:(fun (_,typ) -> generalize_ctyp typ)
     in
     let _ = TyVarEnv.instance_poly_univars env styp.ptyp_loc univars in
     { ctyp_desc = Ttyp_poly (vars, typ);
       ctyp_type = typ.ctyp_type;
       ctyp_env = env;
       ctyp_loc = styp.ptyp_loc;
       ctyp_attributes = styp.ptyp_attributes }
>>>>>>> ocaml/5.1
  | _ ->
<<<<<<< HEAD
    transl_type_scheme_mono env styp
||||||| merged common ancestors
     begin_def();
     let typ = transl_simple_type env false styp in
     end_def();
     generalize typ.ctyp_type;
     typ

=======
      with_local_level
        (fun () -> TyVarEnv.reset (); transl_simple_type env ~closed:false styp)
        ~post:generalize_ctyp

>>>>>>> ocaml/5.1

(* Error report *)

open Format
open Printtyp

let report_error env ppf = function
  | Unbound_type_variable (name, in_scope_names) ->
    fprintf ppf "The type variable %s is unbound in this type declaration.@ %a"
      name
      did_you_mean (fun () -> Misc.spellcheck in_scope_names name )
  | No_type_wildcards ->
    fprintf ppf "A type wildcard \"_\" is not allowed in this type declaration."
  | Undefined_type_constructor p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      path p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      longident lid expected provided
  | Bound_type_variable name ->
    fprintf ppf "Already bound type parameter %a" Printast.tyvar name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Unbound_row_variable lid ->
      (* we don't use "spellcheck" here: this error is not raised
         anywhere so it's unclear how it should be handled *)
      fprintf ppf "Unbound row variable in #%a" longident lid
  | Type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This type")
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf
        "@[<v>@[The constructor %s is missing from the upper bound@ \
         (between '<'@ and '>')@ of this polymorphic variant@ \
         but is present in@ its lower bound (after '>').@]@,\
         @[@{<hint>Hint@}: Either add `%s in the upper bound,@ \
         or remove it@ from the lower bound.@]@]"
         l l
  | Constructor_mismatch (ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        Printtyp.prepare_for_printing [ty; ty'];
        fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
          "This variant type contains a constructor"
          !Oprint.out_type (tree_of_typexp Type ty)
          "which should be"
           !Oprint.out_type (tree_of_typexp Type ty'))
  | Not_a_variant ty ->
      fprintf ppf
        "@[The type %a@ does not expand to a polymorphic variant type@]"
        Printtyp.type_expr ty;
      begin match get_desc ty with
        | Tvar { name = Some s } ->
           (* PR#7012: help the user that wrote 'Foo instead of `Foo *)
           Misc.did_you_mean ppf (fun () -> ["`" ^ s])
        | _ -> ()
      end
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]"
        lab1 lab2 "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, reason) ->
      fprintf ppf
        "@[<hov>The universal type variable %a cannot be generalized:@ "
        Printast.tyvar name;
      begin match reason with
      | Unified v ->
        fprintf ppf "it is bound to@ %a" Printtyp.type_expr v
      | Univar ->
        fprintf ppf "it is already bound to another variable"
      | Scope_escape ->
        fprintf ppf "it escapes its scope"
      end;
      fprintf ppf ".@]";
  | Bad_univar_jkind { name; jkind_info; inferred_jkind } ->
      fprintf ppf
        "@[<hov>The universal type variable %a was %s to have@ \
         layout %a, but was inferred to have %t.@]"
        Printast.tyvar name
        (if jkind_info.defaulted then "defaulted" else "declared")
        Jkind.format jkind_info.original_jkind
        (fun ppf -> match Jkind.get inferred_jkind with
           | Const c -> fprintf ppf "layout %s" (Jkind.string_of_const c)
           | Var _ -> fprintf ppf "a representable layout")
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a" longident s
  | Method_mismatch (l, ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        fprintf ppf "@[<hov>Method '%s' has type %a,@ which should be %a@]"
          l Printtyp.type_expr ty Printtyp.type_expr ty')
  | Opened_object nm ->
      fprintf ppf
        "Illegal open object type%a"
        (fun ppf -> function
             Some p -> fprintf ppf "@ %a" path p
           | None -> fprintf ppf "") nm
  | Not_an_object ty ->
      fprintf ppf "@[The type %a@ is not an object type@]"
        Printtyp.type_expr ty
  | Unsupported_extension ext ->
      let ext = Language_extension.to_string ext in
      fprintf ppf "@[The %s extension is disabled@ \
                   To enable it, pass the '-extension %s' flag@]" ext ext
  | Polymorphic_optional_param ->
      fprintf ppf "@[Optional parameters cannot be polymorphic@]"
  | Non_value {vloc; typ; err} ->
    let s =
      match vloc with
      | Tuple -> "Tuple element"
      | Poly_variant -> "Polymorphic variant constructor argument"
      | Package_constraint -> "Signature package constraint"
      | Object_field -> "Object field"
    in
    fprintf ppf "@[%s types must have layout value.@ \ %a@]"
      s (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf typ)) err
  | Non_sort {vloc; typ; err} ->
    let s =
      match vloc with
      | Fun_arg -> "Function argument"
      | Fun_ret -> "Function return"
    in
    fprintf ppf "@[%s types must have a representable layout.@ \ %a@]"
      s (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf typ)) err
  | Bad_jkind_annot(ty, violation) ->
    fprintf ppf "@[<b 2>Bad layout annotation:@ %a@]"
      (Jkind.Violation.report_with_offender
         ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) violation

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer ~loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
