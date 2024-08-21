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

(* Inclusion checks for the module language *)

open Misc
open Typedtree
open Types

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter


module Error = struct

  type functor_arg_descr =
    | Anonymous
    | Named of Path.t
    | Unit
    | Empty_struct
     (** For backward compatibility's sake, an empty struct can be implicitly
         converted to an unit module  *)

  type ('a,'b) diff = {got:'a; expected:'a; symptom:'b}
  type 'a core_diff =('a,unit) diff
  let diff x y s = {got=x;expected=y; symptom=s}
  let sdiff x y = {got=x; expected=y; symptom=()}

  type core_sigitem_symptom =
    | Value_descriptions of (value_description, Includecore.value_mismatch) diff
    | Type_declarations of (type_declaration, Includecore.type_mismatch) diff
    | Extension_constructors of
        (extension_constructor, Includecore.extension_constructor_mismatch) diff
    | Class_type_declarations of
        (class_type_declaration, Ctype.class_match_failure list) diff
    | Class_declarations of
        (class_declaration, Ctype.class_match_failure list) diff

  type core_module_type_symptom =
    | Not_an_alias
    | Not_an_identifier
    | Incompatible_aliases
    | Abstract_module_type
    | Unbound_module_path of Path.t

  type module_type_symptom =
    | Mt_core of core_module_type_symptom
    | Signature of signature_symptom
    | Functor of functor_symptom
    | Invalid_module_alias of Path.t
    | After_alias_expansion of module_type_diff


  and module_type_diff = (module_type, module_type_symptom) diff

  and functor_symptom =
    | Params of functor_params_diff
    | Result of module_type_diff

  and ('arg,'path) functor_param_symptom =
    | Incompatible_params of 'arg * functor_parameter
    | Mismatch of module_type_diff

  and arg_functor_param_symptom =
    (functor_parameter, Ident.t) functor_param_symptom

  and functor_params_diff = (functor_parameter list * module_type) core_diff

  and signature_symptom = {
    env: Env.t;
    missings: signature_item list;
    incompatibles: (Ident.t * sigitem_symptom) list;
  }
  and sigitem_symptom =
    | Core of core_sigitem_symptom
    | Module_type_declaration of
        (modtype_declaration, module_type_declaration_symptom) diff
    | Module_type of module_type_diff

  and module_type_declaration_symptom =
    | Illegal_permutation of Typedtree.module_coercion
    | Not_greater_than of module_type_diff
    | Not_less_than of module_type_diff
    | Incomparable of
        {less_than:module_type_diff; greater_than: module_type_diff}


  type compilation_unit_comparison =
    | Implementation_vs_interface
    | Argument_vs_parameter

  type all =
    | In_Compilation_unit of
        compilation_unit_comparison * (string, signature_symptom) diff
    | In_Signature of signature_symptom
    | In_Include_functor_signature of signature_symptom
    | In_Module_type of module_type_diff
    | In_Module_type_substitution of
        Ident.t * (Types.module_type,module_type_declaration_symptom) diff
    | In_Type_declaration of Ident.t * core_sigitem_symptom
    | In_Expansion of core_module_type_symptom

end

type mark =
  | Mark_both
  | Mark_positive
  | Mark_negative
  | Mark_neither

let negate_mark = function
  | Mark_both -> Mark_both
  | Mark_positive -> Mark_negative
  | Mark_negative -> Mark_positive
  | Mark_neither -> Mark_neither

let mark_positive = function
  | Mark_both | Mark_positive -> true
  | Mark_negative | Mark_neither -> false

(* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions ~loc env ~mark subst id vd1 vd2 =
  Cmt_format.record_value_dependency vd1 vd2;
  if mark_positive mark then
    Env.mark_value_used vd1.val_uid;
  let vd2 = Subst.value_description subst vd2 in
  try
    Ok (Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2)
  with Includecore.Dont_match err ->
    Error Error.(Core (Value_descriptions (diff vd1 vd2 err)))

(* Inclusion between type declarations *)

let type_declarations ~loc env ~mark subst id decl1 decl2 =
  let mark = mark_positive mark in
  if mark then
    Env.mark_type_used decl1.type_uid;
  let decl2 = Subst.type_declaration subst decl2 in
  match
    Includecore.type_declarations ~loc env ~mark
      (Ident.name id) decl1 (Path.Pident id) decl2
  with
  | None -> Ok Tcoerce_none
  | Some err ->
      Error Error.(Core(Type_declarations (diff decl1 decl2 err)))

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark  subst id ext1 ext2 =
  let mark = mark_positive mark in
  let ext2 = Subst.extension_constructor subst ext2 in
  match Includecore.extension_constructors ~loc env ~mark id ext1 ext2 with
  | None -> Ok Tcoerce_none
  | Some err ->
      Error Error.(Core(Extension_constructors(diff ext1 ext2 err)))

(* Inclusion between class declarations *)

let class_type_declarations ~loc env subst decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> Ok Tcoerce_none
  | reason ->
      Error Error.(Core(Class_type_declarations(diff decl1 decl2 reason)))

let class_declarations env subst decl1 decl2 =
  let decl2 = Subst.class_declaration subst decl2 in
  match Includeclass.class_declarations env decl1 decl2 with
    []     -> Ok Tcoerce_none
  | reason ->
     Error Error.(Core(Class_declarations(diff decl1 decl2 reason)))

(* Extract name, kind and ident from a signature item *)

type field_kind =
  | Field_value
  | Field_type
  | Field_exception
  | Field_typext
  | Field_module
  | Field_modtype
  | Field_class
  | Field_classtype



type field_desc = { name: string; kind: field_kind }

let kind_of_field_desc fd = match fd.kind with
  | Field_value -> "value"
  | Field_type -> "type"
  | Field_exception -> "exception"
  | Field_typext -> "extension constructor"
  | Field_module -> "module"
  | Field_modtype -> "module type"
  | Field_class -> "class"
  | Field_classtype -> "class type"

let field_desc kind id = { kind; name = Ident.name id }

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name =
  let open Subst.Lazy in
  function
    Sig_value(id, d, _) -> (id, d.val_loc, field_desc Field_value id)
  | Sig_type(id, d, _, _) -> (id, d.type_loc, field_desc Field_type  id )
  | Sig_typext(id, d, _, _) ->
      let kind =
        if Path.same d.ext_type_path Predef.path_exn
        then Field_exception
        else Field_typext
      in
      (id, d.ext_loc, field_desc kind id)
  | Sig_module(id, _, d, _, _) -> (id, d.md_loc, field_desc Field_module id)
  | Sig_modtype(id, d, _) -> (id, d.mtd_loc, field_desc Field_modtype id)
  | Sig_class(id, d, _, _) -> (id, d.cty_loc, field_desc Field_class id)
  | Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, field_desc Field_classtype id)

let is_runtime_component =
  let open Subst.Lazy in
  function
  | Sig_value(_,{val_kind = Val_prim _}, _)
  | Sig_type(_,_,_,_)
  | Sig_module(_,Mp_absent,_,_,_)
  | Sig_modtype(_,_,_)
  | Sig_class_type(_,_,_,_) -> false
  | Sig_value(_,_,_)
  | Sig_typext(_,_,_,_)
  | Sig_module(_,Mp_present,_,_,_)
  | Sig_class(_,_,_,_) -> true

let item_visibility =
  let open Subst.Lazy in
  function
  | Sig_value (_, _, vis)
  | Sig_type (_, _, _, vis)
  | Sig_typext (_, _, _, vis)
  | Sig_module (_, _, _, _, vis)
  | Sig_modtype (_, _, vis)
  | Sig_class (_, _, _, vis)
  | Sig_class_type (_, _, _, vis) -> vis


(* Print a coercion *)

let rec print_list pr ppf = function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; Format.fprintf ppf ";@ "; print_list pr ppf l
let print_list pr ppf l =
  Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
    Tcoerce_none -> pr "id"
  | Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
  | Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
  | Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Printtyp.raw_type_expr pc_type
  | Tcoerce_alias (_, p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]"
    (Ident.unique_name i) n print_coercion c

(* Simplify a structure coercion *)

let equal_module_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_module_path None env p1)
       (Env.normalize_module_path None env
          (Subst.module_path subst p2))

let equal_modtype_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_modtype_path env p1)
       (Env.normalize_modtype_path env
          (Subst.modtype_path subst p2))

let simplify_structure_coercion cc id_pos_list =
  let rec is_identity_coercion pos = function
  | [] ->
      true
  | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list)


(* Build a table of the components of sig1, along with their positions.
   The table is indexed by kind and name of component *)
let build_component_table pos_rep sg =
  let rec build_table nb_exported pos tbl = function
    [] -> nb_exported, pos, tbl
  | item :: rem ->
      let pos, nextpos =
        if is_runtime_component item then pos, pos + 1
        else -1, pos
      in
      match item_visibility item with
      | Hidden ->
          (* do not pair private items. *)
          build_table nb_exported nextpos tbl rem
      | Exported ->
          let (id, _loc, name) = item_ident_name item in
          build_table (nb_exported + 1) nextpos
            (FieldMap.add name (id, item, pos_rep pos id) tbl) rem
  in
  build_table 0 0 FieldMap.empty sg


(* Pair each component of sig2 with a component of sig1,
   identifying the names along the way.
   Return a coercion list indicating, for all run-time components
   of sig2, the position of the matching run-time components of sig1
   and the coercion to be applied to it. *)
let pair_components subst sig1_comps sig2 =
  let open Subst.Lazy in
  let rec pair subst paired unpaired = function
    | [] ->
      paired, unpaired, subst
  | item2 :: rem ->
      let (id2, _loc, name2) = item_ident_name item2 in
      let name2, report =
        match item2, name2 with
          Sig_type (_, {type_manifest_=None}, _, _), {name=s; kind=Field_type}
          when Btype.is_row_name s ->
            (* Do not report in case of failure,
               as the main type will generate an error *)
            { kind=Field_type; name=String.sub s 0 (String.length s - 4) },
            false
        | _ -> name2, true
      in
      begin match FieldMap.find name2 sig1_comps with
      | (id1, item1, pos1) ->
        let new_subst =
          match item2 with
            Sig_type _ ->
              Subst.add_type id2 (Path.Pident id1) subst
          | Sig_module _ ->
              Subst.add_module id2 (Path.Pident id1) subst
          | Sig_modtype _ ->
              Subst.add_modtype id2 (Mty_ident (Path.Pident id1)) subst
          | Sig_value _ | Sig_typext _
          | Sig_class _ | Sig_class_type _ ->
              subst
        in
        pair new_subst
          ((item1, item2, pos1) :: paired) unpaired rem
      | exception Not_found ->
        let unpaired =
          if report then
            item2 :: unpaired
          else unpaired in
        pair subst paired unpaired rem
      end
  in
  pair subst [] [] sig2


let retrieve_functor_params env mty =
  let rec retrieve_functor_params before env mty =
    match Mtype.scrape_alias env mty with
    | Mty_functor (p, res) ->
        retrieve_functor_params (p :: before) env res
    | Mty_ident _ | Mty_alias _ | Mty_signature _ | Mty_strengthen _ as res ->
        List.rev before, res
  in
  retrieve_functor_params [] env mty

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

(* When computing a signature difference, we need to distinguish between
   recoverable errors at the value level and unrecoverable errors at the type
   level that require us to stop the computation of the difference due to
   incoherent types.
*)
type 'a recoverable_error = { error: 'a; recoverable:bool }
let mark_error_as_recoverable r =
  Result.map_error (fun error -> { error; recoverable=true}) r
let mark_error_as_unrecoverable r =
  Result.map_error (fun error -> { error; recoverable=false}) r


module Sign_diff = struct
  type 'a t = {
    runtime_coercions: ('a * Typedtree.module_coercion) list;
    shape_map: Shape.Map.t;
    deep_modifications:bool;
    errors: (Ident.t * Error.sigitem_symptom) list;
    leftovers: ((Types.signature_item as 'it) * 'it * 'a) list
  }

  let empty = {
    runtime_coercions = [];
    shape_map = Shape.Map.empty;
    deep_modifications = false;
    errors = [];
    leftovers = []
  }

  let merge x y =
    {
      runtime_coercions = x.runtime_coercions @ y.runtime_coercions;
      shape_map = y.shape_map;
      (* the shape map is threaded the map during the difference computation,
          the last shape map contains all previous elements. *)
      deep_modifications = x.deep_modifications || y.deep_modifications;
      errors = x.errors @ y.errors;
      leftovers = x.leftovers @ y.leftovers
    }
end

(* Quickly compare module types without expanding them, succeeding only if mty1
  is a subtype of mty2 with no coercion  *)
let rec shallow_modtypes env subst mty1 mty2 =
  let open Subst.Lazy in
  let sub_aliasable a1 a2 =
    (* S with M (unaliasable) is not a subtype of S with M (aliasable) *)
    Aliasability.is_aliasable a1 || not (Aliasability.is_aliasable a2)
  in
  match mty1, mty2 with
  | Mty_alias p1, Mty_alias p2 ->
      not (Env.is_functor_arg p2 env) && equal_module_paths env p1 subst p2
  | Mty_ident p1, Mty_ident p2 ->
      equal_modtype_paths env p1 subst p2
  | Mty_strengthen (mty1,p1,a1), Mty_strengthen (mty2,p2,a2)
        when sub_aliasable a1 a2
              (* Destructive substitution can introduce this, similar to the
                 Mty_alias check *)
          && not (Aliasability.is_aliasable a2 && Env.is_functor_arg p2 env)
          && shallow_modtypes env subst mty1 mty2
          && shallow_module_paths env subst p1 mty2 p2 ->
      true
  | Mty_strengthen (mty1,_,_), mty2 ->
      (* S with M <= S *)
      shallow_modtypes env subst mty1 mty2
  | (Mty_alias _ | Mty_ident _ | Mty_signature _ | Mty_functor _), _  -> false

and shallow_module_paths env subst p1 mty2 p2 =
  equal_module_paths env p1 subst p2 ||
  (* This shortcut is a significant win in some cases. Note we don't apply it
     recursively as doing seems to be a net loss. *)
  match (Env.find_module_lazy p1 env).md_type with
    | Mty_strengthen (mty1,p1,_) ->
        shallow_modtypes env subst mty1 mty2
          && equal_module_paths env p1 subst p2
    | Mty_alias _ | Mty_ident _ | Mty_signature _ | Mty_functor _
    | exception Not_found -> false

(**
   In the group of mutual functions below, the [~in_eq] argument is [true] when
   we are in fact checking equality of module types.

   The module subtyping relation [A <: B] checks that [A.T = B.T] when [A]
   and [B] define a module type [T]. The relation [A.T = B.T] is equivalent
   to [(A.T <: B.T) and (B.T <: A.T)], but checking both recursively would lead
   to an exponential slowdown (see #10598 and #10616).
   To avoid this issue, when [~in_eq] is [true], we compute a coarser relation
   [A << B] which is the same as [A <: B] except that module types [T] are
   checked only for [A.T << B.T] and not the reverse.
   Thus, we can implement a cheap module type equality check [A.T = B.T] by
   computing [(A.T << B.T) and (B.T << A.T)], avoiding the exponential slowdown
   described above.
*)

let rec modtypes ~in_eq ~loc env ~mark subst mty1 mty2 shape =
  match try_modtypes ~in_eq ~loc env ~mark subst mty1 mty2 shape with
  | Ok _ as ok -> ok
  | Error reason ->
    let mty1 = Subst.Lazy.force_modtype mty1 in
    let mty2 =
      Subst.Lazy.force_modtype (Subst.Lazy.modtype Make_local subst mty2)
    in
    Error Error.(diff mty1 mty2 reason)

and try_modtypes ~in_eq ~loc env ~mark subst mty1 mty2 orig_shape =
  let open Subst.Lazy in
  (* Do a quick nominal comparison for simple types and if that fails, try to
      unfold one of them. For structured types, do a deep comparison. *)
  let is_alias = function
    | Mty_alias _ -> true
    | _ -> false
  in
  match mty1, mty2 with
  | _ when shallow_modtypes env subst mty1 mty2 ->
    Ok (Tcoerce_none, orig_shape)

  | (Mty_alias p1, _) when not (is_alias mty2) -> begin
    match
      Env.normalize_module_path (Some Location.none) env p1
    with
    | exception Env.Error (Env.Missing_module (_, _, path)) ->
        Error Error.(Mt_core(Unbound_module_path path))
    | p1 ->
        begin match Env.find_module_lazy p1 env with
        | md -> begin
            match strengthened_modtypes ~in_eq ~loc ~aliasable:true env ~mark
                    subst md.md_type p1 mty2 orig_shape
            with
            | Ok _ as x -> x
            | Error reason -> Error (Error.After_alias_expansion reason)
          end
        | exception Not_found ->
            Error (Error.Mt_core (Error.Unbound_module_path p1))
        end
    end
  | (Mty_signature sig1, Mty_signature sig2) ->
      begin match
        signatures ~in_eq ~loc env ~mark subst sig1 sig2 orig_shape
      with
      | Ok _ as ok -> ok
      | Error e -> Error (Error.Signature e)
      end

  | Mty_functor (param1, res1), Mty_functor (param2, res2) ->
      let cc_arg, env, subst =
        functor_param ~in_eq ~loc env ~mark:(negate_mark mark)
          subst param1 param2
      in
      let var, res_shape =
        match Shape.decompose_abs orig_shape with
        | Some (var, res_shape) -> var, res_shape
        | None ->
            (* Using a fresh variable with a placeholder uid here is fine: users
               will never try to jump to the definition of that variable.
               If they try to jump to the parameter from inside the functor,
               they will use the variable shape that is stored in the local
               environment.  *)
            let var, shape_var =
              Shape.fresh_var Uid.internal_not_actually_unique
            in
            var, Shape.app orig_shape ~arg:shape_var
      in
      let cc_res = modtypes ~in_eq ~loc env ~mark subst res1 res2 res_shape in
      begin match cc_arg, cc_res with
      | Ok Tcoerce_none, Ok (Tcoerce_none, final_res_shape) ->
          let final_shape =
            if final_res_shape == res_shape
            then orig_shape
            else Shape.abs var final_res_shape
          in
          Ok (Tcoerce_none, final_shape)
      | Ok cc_arg, Ok (cc_res, final_res_shape) ->
          let final_shape =
            if final_res_shape == res_shape
            then orig_shape
            else Shape.abs var final_res_shape
          in
          Ok (Tcoerce_functor(cc_arg, cc_res), final_shape)
      | _, Error {Error.symptom = Error.Functor Error.Params res; _} ->
          let got_params, got_res = res.got in
          let expected_params, expected_res = res.expected in
          let d = Error.sdiff
              (force_functor_parameter param1::got_params, got_res)
              (force_functor_parameter param2::expected_params, expected_res)
          in
          Error Error.(Functor (Params d))
      | Error _, _ ->
          let params1, res1 =
            retrieve_functor_params env (Subst.Lazy.force_modtype res1)
          in
          let params2, res2 =
            retrieve_functor_params env (Subst.Lazy.force_modtype res2)
          in
          let d = Error.sdiff
            (force_functor_parameter param1::params1, res1)
            (force_functor_parameter param2::params2, res2)
          in
          Error Error.(Functor (Params d))
      | Ok _, Error res ->
          Error Error.(Functor (Result res))
      end

  | _ ->
    let red =
      (* Try to reduce one of the two types *)
      match Mtype.reduce_lazy env mty1 with
      | Some mty1 -> Some (mty1,mty2)
      | None ->
          let mty2_red =
            Subst.Lazy.modtype Keep subst mty2
            |> Mtype.reduce_lazy env
          in
          match mty2_red with
          | Some mty2 -> Some (mty1,mty2)
          | None -> None
    in
    match red with
    | Some (mty1,mty2) ->
        try_modtypes ~in_eq ~loc env ~mark subst mty1 mty2 orig_shape
    | None ->
        (* Report error *)
        match mty1, mty2 with
        | _, Mty_strengthen (_,p,Aliasable) when Env.is_functor_arg p env ->
            Error (Error.Invalid_module_alias p)
        | (Mty_ident _ | Mty_strengthen _), _ ->
            Error (Error.Mt_core Abstract_module_type)
        | (Mty_alias _, Mty_alias p2) ->
            if Env.is_functor_arg p2 env then
              Error (Error.Invalid_module_alias p2)
            else
              Error Error.(Mt_core Incompatible_aliases)
        | Mty_functor _, _
        | _, Mty_functor _ ->
            let params1 =
              retrieve_functor_params env (Subst.Lazy.force_modtype mty1)
            in
            let params2 =
              retrieve_functor_params env (Subst.Lazy.force_modtype mty2)
            in
            let d = Error.sdiff params1 params2 in
            Error Error.(Functor (Params d))
        | _, (Mty_ident _ | Mty_strengthen _) ->
            Error Error.(Mt_core Not_an_identifier)
        | _, Mty_alias _ ->
            Error (Error.Mt_core Error.Not_an_alias)
        | (Mty_alias _ | Mty_signature _), _ ->
            Error (Error.Mt_core Abstract_module_type)

(* Functor parameters *)

and functor_param ~in_eq ~loc env ~mark subst param1 param2 =
  let open Subst.Lazy in
  match param1, param2 with
  | Unit, Unit ->
      Ok Tcoerce_none, env, subst
  | Named (name1, arg1), Named (name2, arg2) ->
      let arg2' = Subst.Lazy.modtype Keep subst arg2 in
      let cc_arg =
        match
          modtypes ~in_eq ~loc env ~mark Subst.identity arg2' arg1
                Shape.dummy_mod
        with
        | Ok (cc, _) -> Ok cc
        | Error err -> Error (Error.Mismatch err)
      in
      let env, subst = equate_one_functor_param subst env arg2' name1 name2 in
      cc_arg, env, subst
  | _, _ ->
      let param1 = force_functor_parameter param1 in
      let param2 = force_functor_parameter param2 in
      Error (Error.Incompatible_params (param1, param2)), env, subst

and equate_one_functor_param subst env arg2' name1 name2  =
  match name1, name2 with
  | Some id1, Some id2 ->
  (* two matching abstract parameters: we add one identifier to the
     environment and record the equality between the two identifiers
     in the substitution *)
      Env.add_module_lazy ~update_summary:false id1 Mp_present arg2' env,
      Subst.add_module id2 (Path.Pident id1) subst
  | None, Some id2 ->
      let id1 = Ident.rename id2 in
      Env.add_module_lazy ~update_summary:false id1 Mp_present arg2' env,
      Subst.add_module id2 (Path.Pident id1) subst
  | Some id1, None ->
      Env.add_module_lazy ~update_summary:false id1 Mp_present arg2' env, subst
  | None, None ->
      env, subst

and strengthened_modtypes ~in_eq ~loc ~aliasable env ~mark
    subst mty1 path1 mty2 shape =
  let mty1 = Mtype.strengthen_lazy ~aliasable mty1 path1 in
  modtypes ~in_eq ~loc env ~mark subst mty1 mty2 shape

and strengthened_module_decl ~loc ~aliasable env ~mark
    subst md1 path1 md2 shape =
  let md1 = Subst.Lazy.of_module_decl md1 in
  let md1 = Mtype.strengthen_lazy_decl ~aliasable md1 path1 in
  let mty2 = Subst.Lazy.of_modtype md2.md_type in
  modtypes ~in_eq:false ~loc env ~mark subst md1.md_type mty2 shape

(* Inclusion between signatures *)

and signatures ~in_eq ~loc env ~mark subst sig1 sig2 mod_shape =
  let open Subst.Lazy in
  (* Environment used to check inclusion of components *)
  let sig1 = force_signature_once sig1 in
  let sig2 = force_signature_once sig2 in
  let new_env =
    Env.add_signature_lazy sig1 (Env.in_signature true env) in
  (* Keep ids for module aliases *)
  let (id_pos_list,_) =
    List.fold_left
      (fun (l,pos) -> function
          Sig_module (id, Mp_present, _, _, _) ->
            ((id,pos,Tcoerce_none)::l , pos+1)
        | item -> (l, if is_runtime_component item then pos+1 else pos))
      ([], 0) sig1 in
  let exported_len1, runtime_len1, comps1 =
    build_component_table (fun pos _name -> pos) sig1
  in
  let exported_len2, runtime_len2 =
    List.fold_left (fun (el, rl) i ->
      let el = match item_visibility i with Hidden -> el | Exported -> el + 1 in
      let rl = if is_runtime_component i then rl + 1 else rl in
      el, rl
    ) (0, 0) sig2
  in
  (* Do the pairing and checking, and return the final coercion *)
  let paired, unpaired, subst = pair_components subst comps1 sig2 in
  let d =
    signature_components ~in_eq ~loc ~mark new_env subst mod_shape
      Shape.Map.empty
      (List.rev paired)
  in
  let open Sign_diff in
  match unpaired, d.errors, d.runtime_coercions, d.leftovers with
    | [], [], cc, [] ->
        let shape =
          if not d.deep_modifications && exported_len1 = exported_len2
          then mod_shape
          else Shape.str ?uid:mod_shape.Shape.uid d.shape_map
        in
        if runtime_len1 = runtime_len2 then (* see PR#5098 *)
          Ok (simplify_structure_coercion cc id_pos_list, shape)
        else
          Ok (Tcoerce_structure (cc, id_pos_list), shape)
    | missings, incompatibles, _runtime_coercions, _leftovers ->
        Error {
          Error.env=new_env;
          missings = List.map force_signature_item missings;
          incompatibles;
        }

(* Inclusion between signature components *)
and signature_components :
  'a. in_eq:_ -> loc:_ -> mark:_ -> _ -> _ -> _ -> _ -> (_ * _ * 'a) list -> 'a Sign_diff.t =
  fun ~in_eq ~loc ~mark env subst orig_shape shape_map paired ->
  let open Subst.Lazy in
  match paired with
  | [] -> Sign_diff.{ empty with shape_map }
  | (sigi1, sigi2, pos) :: rem ->
      let shape_modified = ref false in
      let id, item, shape_map, present_at_runtime =
        match sigi1, sigi2 with
        | Sig_value(id1, valdecl1, _) ,Sig_value(_id2, valdecl2, _) ->
            let item =
              value_descriptions ~loc env ~mark subst id1
                (Subst.Lazy.force_value_description valdecl1)
                (Subst.Lazy.force_value_description valdecl2)
            in
            let item = mark_error_as_recoverable item in
            let present_at_runtime = match valdecl2.val_kind with
              | Val_prim _ -> false
              | _ -> true
            in
            let shape_map = Shape.Map.add_value_proj shape_map id1 orig_shape in
            id1, item, shape_map, present_at_runtime
        | Sig_type(id1, tydec1, _, _), Sig_type(_id2, tydec2, _, _) ->
            let item =
              type_declarations ~loc env ~mark subst id1 tydec1 tydec2
            in
            let item = mark_error_as_unrecoverable item in
            (* Right now we don't filter hidden constructors / labels from the
            shape. *)
            let shape_map = Shape.Map.add_type_proj shape_map id1 orig_shape in
            id1, item, shape_map, false
        | Sig_typext(id1, ext1, _, _), Sig_typext(_id2, ext2, _, _) ->
            let item =
              extension_constructors ~loc env ~mark  subst id1 ext1 ext2
            in
            let item = mark_error_as_unrecoverable item in
            let shape_map =
              Shape.Map.add_extcons_proj shape_map id1 orig_shape
            in
            id1, item, shape_map, true
        | Sig_module(id1, pres1, mty1, _, _), Sig_module(_, pres2, mty2, _, _)
          -> begin
              let orig_shape =
                Shape.(proj orig_shape (Item.module_ id1))
              in
              let item =
                module_declarations ~in_eq ~loc env ~mark subst id1 mty1 mty2
                  orig_shape
              in
              let item, shape_map =
                match item with
                | Ok (cc, shape) ->
                    if shape != orig_shape then shape_modified := true;
                    let mod_shape = Shape.set_uid_if_none shape mty1.md_uid in
                    Ok cc, Shape.Map.add_module shape_map id1 mod_shape
                | Error diff ->
                    Error (Error.Module_type diff),
                    (* We add the original shape to the map, even though
                       there is a type error.
                       It could still be useful for merlin. *)
                    Shape.Map.add_module shape_map id1 orig_shape
              in
              let present_at_runtime, item =
                match pres1, pres2, mty1.md_type with
                | Mp_present, Mp_present, _ -> true, item
                | _, Mp_absent, _ -> false, item
                | Mp_absent, Mp_present, Mty_alias p1 ->
                    true, Result.map (fun i -> Tcoerce_alias (env, p1, i)) item
                | Mp_absent, Mp_present, _ -> assert false
              in
              let item = mark_error_as_unrecoverable item in
              id1, item, shape_map, present_at_runtime
            end
        | Sig_modtype(id1, info1, _), Sig_modtype(_id2, info2, _) ->
            let item =
              modtype_infos ~in_eq ~loc env ~mark  subst id1 info1 info2
            in
            let shape_map =
              Shape.Map.add_module_type_proj shape_map id1 orig_shape
            in
            let item = mark_error_as_unrecoverable item in
            id1, item, shape_map, false
        | Sig_class(id1, decl1, _, _), Sig_class(_id2, decl2, _, _) ->
            let item =
              class_declarations env subst decl1 decl2
            in
            let shape_map =
              Shape.Map.add_class_proj shape_map id1 orig_shape
            in
            let item = mark_error_as_unrecoverable item in
            id1, item, shape_map, true
        | Sig_class_type(id1, info1, _, _), Sig_class_type(_id2, info2, _, _) ->
            let item =
              class_type_declarations ~loc env subst info1 info2
            in
            let item = mark_error_as_unrecoverable item in
            let shape_map =
              Shape.Map.add_class_type_proj shape_map id1 orig_shape
            in
            id1, item, shape_map, false
        | _ ->
            assert false
      in
      let deep_modifications = !shape_modified in
      let first =
        match item with
        | Ok x ->
            let runtime_coercions =
              if present_at_runtime then [pos,x] else []
            in
            Sign_diff.{ empty with deep_modifications; runtime_coercions }
        | Error { error; recoverable=_ } ->
            Sign_diff.{ empty with errors=[id,error]; deep_modifications }
      in
      let continue = match item with
        | Ok _ -> true
        | Error x -> x.recoverable
      in
      let rest =
        if continue then
          signature_components ~in_eq ~loc ~mark env subst
            orig_shape shape_map rem
        else
          let rem = List.map
            (fun (x,y,z) ->
                Subst.Lazy.force_signature_item x,
                Subst.Lazy.force_signature_item y,
                z)
            rem
          in
          Sign_diff.{ empty with leftovers=rem }
       in
       Sign_diff.merge first rest

and module_declarations  ~in_eq ~loc env ~mark  subst id1 md1 md2 orig_shape =
  let open Subst.Lazy in
  Builtin_attributes.check_alerts_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);
  let p1 = Path.Pident id1 in
  if mark_positive mark then
    Env.mark_module_used md1.md_uid;
  strengthened_modtypes  ~in_eq ~loc ~aliasable:true env ~mark subst
    md1.md_type p1 md2.md_type orig_shape

(* Inclusion between module type specifications *)

and modtype_infos ~in_eq ~loc env ~mark subst id info1 info2 =
  let open Subst.Lazy in
  Builtin_attributes.check_alerts_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.Lazy.modtype_decl Keep subst info2 in
  let r =
    match (info1.mtd_type, info2.mtd_type) with
      (None, None) -> Ok Tcoerce_none
    | (Some _, None) -> Ok Tcoerce_none
    | (Some mty1, Some mty2) ->
        check_modtype_equiv ~in_eq ~loc env ~mark mty1 mty2
    | (None, Some mty2) ->
        let mty1 = Mty_ident(Path.Pident id) in
        check_modtype_equiv ~in_eq ~loc env ~mark mty1 mty2 in
  match r with
  | Ok _ as ok -> ok
  | Error e ->
      let info1 = Subst.Lazy.force_modtype_decl info1 in
      let info2 = Subst.Lazy.force_modtype_decl info2 in
      Error Error.(Module_type_declaration (diff info1 info2 e))

and check_modtype_equiv ~in_eq ~loc env ~mark mty1 mty2 =
  let c1 =
    modtypes ~in_eq:true ~loc env ~mark Subst.identity mty1 mty2 Shape.dummy_mod
  in
  let c2 =
    (* For nested module type paths, we check only one side of the equivalence:
       the outer module type is the one responsible for checking the other side
       of the equivalence.
     *)
    if in_eq then None
    else
      let mark = negate_mark mark in
      Some (
        modtypes ~in_eq:true ~loc env ~mark Subst.identity
          mty2 mty1 Shape.dummy_mod
      )
  in
  match c1, c2 with
  | Ok (Tcoerce_none, _), (Some Ok (Tcoerce_none, _)|None) -> Ok Tcoerce_none
  | Ok (c1, _), (Some Ok _ | None) ->
      (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
           print_coercion _c1 print_coercion _c2; *)
      Error Error.(Illegal_permutation c1)
  | Ok _, Some Error e -> Error Error.(Not_greater_than e)
  | Error e, (Some Ok _ | None) -> Error Error.(Not_less_than e)
  | Error less_than, Some Error greater_than ->
      Error Error.(Incomparable {less_than; greater_than})

let include_functor_signatures ~loc env ~mark subst sig1 sig2 mod_shape =
  let _, _, comps1 = build_component_table (fun _pos name -> name) sig1 in
  let paired, unpaired, subst = pair_components subst comps1 sig2 in
  let d = signature_components ~in_eq:false ~loc ~mark env subst mod_shape
            Shape.Map.empty
            (List.rev paired)
  in
  let open Sign_diff in
  match unpaired, d.errors, d.leftovers with
  | [], [], [] ->
     Ok d.runtime_coercions
  | missings, incompatibles, _leftovers ->
     let missings = List.map Subst.Lazy.force_signature_item missings in
     Error Error.{ env; missings; incompatibles }

let can_alias env path =
  let rec no_apply = function
    | Path.Pident _ -> true
    | Path.Pdot(p, _) | Path.Pextra_ty (p, _) -> no_apply p
    | Path.Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)


let signatures ~in_eq ~loc env ~mark subst sig1 sig2 mod_shape =
  let sig1 = Subst.Lazy.of_signature sig1 in
  let sig2 = Subst.Lazy.of_signature sig2 in
  signatures ~in_eq ~loc env ~mark subst sig1 sig2 mod_shape

let modtypes ~in_eq ~loc env ~mark subst mty1 mty2 shape =
  let mty1 = Subst.Lazy.of_modtype mty1 in
  let mty2 = Subst.Lazy.of_modtype mty2 in
  modtypes ~in_eq ~loc env ~mark subst mty1 mty2 shape

let strengthened_modtypes ~in_eq ~loc ~aliasable env ~mark
  subst mty1 path1 mty2 shape =
  let mty1 = Subst.Lazy.of_modtype mty1 in
  let mty2 = Subst.Lazy.of_modtype mty2 in
  strengthened_modtypes ~in_eq ~loc ~aliasable env ~mark subst mty1
    path1 mty2 shape

type explanation = Env.t * Error.all
exception Error of explanation

exception Apply_error of {
    loc : Location.t ;
    env : Env.t ;
    lid_app : Longident.t option ;
    mty_f : module_type ;
    args : (Error.functor_arg_descr * module_type) list ;
  }

let check_modtype_inclusion_raw ~loc env mty1 path1 mty2 =
  let aliasable = can_alias env path1 in
  strengthened_modtypes ~in_eq:false ~loc ~aliasable env ~mark:Mark_both
    Subst.identity mty1 path1 mty2 Shape.dummy_mod
  |> Result.map fst

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  match check_modtype_inclusion_raw ~loc env mty1 path1 mty2 with
  | Ok _ -> None
  | Error e -> Some (env, Error.In_Module_type e)

let check_functor_application_in_path
    ~errors ~loc ~lid_whole_app ~f0_path ~args
    ~arg_path ~arg_mty ~arg_mode ~param_mty env =
  Mode.Value.submode_exn arg_mode Mode.Value.legacy;
  match check_modtype_inclusion_raw ~loc env arg_mty arg_path param_mty with
  | Ok _ -> ()
  | Error _errs ->
      if errors then
        let prepare_arg (arg_path, arg_mty, _arg_mode) =
          let aliasable = can_alias env arg_path in
          let smd = Mtype.strengthen ~aliasable arg_mty arg_path in
          (Error.Named arg_path, smd)
        in
        let mty_f = (Env.find_module f0_path env).md_type in
        let args = List.map prepare_arg args in
        let lid_app = Some lid_whole_app in
        raise (Apply_error {loc; env; lid_app; mty_f; args})
      else
        raise Not_found

let () =
  Env.check_functor_application := check_functor_application_in_path


(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit0
    ~comparison env ~mark impl_name impl_sig intf_name intf_sig unit_shape =
  match
    signatures ~in_eq:false ~loc:(Location.in_file impl_name) env ~mark
      Subst.identity impl_sig intf_sig unit_shape
  with Result.Error reasons ->
    let diff = Error.diff impl_name intf_name reasons in
    let cdiff =
      Error.In_Compilation_unit(comparison, diff) in
    raise(Error(env, cdiff))
  | Ok x -> x

let compunit = compunit0 ~comparison:Implementation_vs_interface

(* Check that the interface of a compilation unit meets the interface of the
   parameter it's declared to be an argument for using [-as-argument-for] *)

let compunit_as_argument env arg_name arg_sig param_name param_sig =
  let cc, _shape =
    compunit0 env arg_name arg_sig param_name param_sig Shape.dummy_mod
      ~comparison:Argument_vs_parameter ~mark:Mark_positive
  in
  cc

(* Functor diffing computation:
   The diffing computation uses the internal typing function
 *)

module Functor_inclusion_diff = struct

  module Defs = struct
    type left = Types.functor_parameter
    type right = left
    type eq = Typedtree.module_coercion
    type diff = (Types.functor_parameter, unit) Error.functor_param_symptom
    type state = {
      res: module_type option;
      env: Env.t;
      subst: Subst.t;
    }
  end
  open Defs

  module Diff = Diffing.Define(Defs)

  let param_name = function
      | Named(x,_) -> x
      | Unit -> None

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) -> begin
        match param_name param1, param_name param2 with
        | None, None
          -> 0
        | Some n1, Some n2
          when String.equal (Ident.name n1) (Ident.name n2)
          -> 0
        | Some _, Some _ -> 1
        | Some _,  None | None, Some _ -> 1
      end



  let rec keep_expansible_param = function
    | Mty_ident _ | Mty_alias _ as mty -> Some mty
    | Mty_signature _ | Mty_functor _ -> None
    | Mty_strengthen (mty,_,_) -> keep_expansible_param mty

  let lookup_expansion { env ; res ; _ } = match res with
    | None -> None
    | Some res ->
        match retrieve_functor_params env res with
        | [], _ -> None
        | params, res ->
            let more = Array.of_list params  in
            Some (keep_expansible_param res, more)

  let expand_params state  =
    match lookup_expansion state with
    | None -> state, [||]
    | Some (res, expansion) -> { state with res }, expansion

  (* Whenever we have a named parameter that doesn't match it anonymous
     counterpart, we add it to the typing environment because it may
     contain useful abbreviations, but without adding any equations  *)
  let bind id arg state =
    let arg' = Subst.modtype Keep state.subst arg in
    let env = Env.add_module id Mp_present arg' state.env in
    { state with env }

  let rec update (d:Diff.change) st =
    match d with
    | Insert (Unit | Named (None,_))
    | Delete (Unit | Named (None,_))
    | Keep (Unit,_,_)
    | Keep (_,Unit,_) ->
        (* No named abstract parameters: we keep the same environment *)
        st, [||]
    | Insert (Named (Some id, arg)) | Delete (Named (Some id, arg)) ->
        (* one named parameter to bind *)
        st |> bind id arg |> expand_params
    | Change (delete, insert, _) ->
        (* Change should be delete + insert: we add both abstract parameters
           to the environment without equating them. *)
        let st, _expansion = update (Diffing.Delete delete) st in
        update (Diffing.Insert insert) st
    | Keep (Named (name1, _), Named (name2, arg2), _) ->
        let arg2 = Subst.Lazy.of_modtype arg2 in
        let arg = Subst.Lazy.modtype Keep st.subst arg2 in
        let env, subst =
          equate_one_functor_param st.subst st.env arg name1 name2
        in
        expand_params { st with env; subst }

  let diff env (l1,res1) (l2,_) =
    let module Compute = Diff.Left_variadic(struct
        let test st mty1 mty2 =
          let loc = Location.none in
          let res, _, _ =
            let mty1 = Subst.Lazy.of_functor_parameter mty1 in
            let mty2 = Subst.Lazy.of_functor_parameter mty2 in
            functor_param ~in_eq:false ~loc st.env ~mark:Mark_neither
              st.subst mty1 mty2
          in
          res
        let update = update
        let weight = weight
      end)
    in
    let param1 = Array.of_list l1 in
    let param2 = Array.of_list l2 in
    let state =
      { env; subst = Subst.identity; res = keep_expansible_param res1}
    in
    Compute.diff state param1 param2

end

module Functor_app_diff = struct
  module I = Functor_inclusion_diff
  module Defs= struct
    type left = Error.functor_arg_descr * Types.module_type
    type right = Types.functor_parameter
    type eq = Typedtree.module_coercion
    type diff = (Error.functor_arg_descr, unit) Error.functor_param_symptom
    type state = I.Defs.state
  end
  module Diff = Diffing.Define(Defs)

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) ->
        (* We assign a small penalty to named arguments with
           non-matching names *)
        begin
          let desc1 : Error.functor_arg_descr = fst param1 in
          match desc1, I.param_name param2 with
          | (Unit | Empty_struct | Anonymous) , None
            -> 0
          | Named (Path.Pident n1), Some n2
            when String.equal (Ident.name n1) (Ident.name n2)
            -> 0
          | Named _, Some _ -> 1
          | Named _,  None | (Unit | Empty_struct | Anonymous), Some _ -> 1
        end

  let update (d: Diff.change) (st:Defs.state) =
    let open Error in
    match d with
    | Insert (Unit|Named(None,_))
    | Delete _ (* delete is a concrete argument, not an abstract parameter*)
    | Keep ((Unit,_),_,_) (* Keep(Unit,_) implies Keep(Unit,Unit) *)
    | Keep (_,(Unit|Named(None,_)),_)
    | Change (_,(Unit|Named (None,_)), _ ) ->
        (* no abstract parameters to add, nor any equations *)
        st, [||]
    | Insert(Named(Some param, param_ty))
    | Change(_, Named(Some param, param_ty), _ ) ->
        (* Change is Delete + Insert: we add the Inserted parameter to the
           environnement to track equalities with external components that the
           parameter might add. *)
        let mty = Subst.modtype Keep st.subst param_ty in
        let env = Env.add_module ~arg:true param Mp_present mty st.env in
        I.expand_params { st with env }
    | Keep ((Named arg,  _mty) , Named (Some param, _param), _) ->
        let res =
          Option.map (fun res ->
              let scope = Ctype.create_scope () in
              let subst = Subst.add_module param arg Subst.identity in
              Subst.modtype (Rescope scope) subst res
            )
            st.res
        in
        let subst = Subst.add_module param arg st.subst in
        I.expand_params { st with subst; res }
    | Keep (((Anonymous|Empty_struct), mty),
            Named (Some param, _param), _) ->
        let mty' = Subst.modtype Keep st.subst mty in
        let env = Env.add_module ~arg:true param Mp_present mty' st.env in
        let res = Option.map (Mtype.nondep_supertype env [param]) st.res in
        I.expand_params { st with env; res}

  let diff env ~f ~args =
    let params, res = retrieve_functor_params env f in
    let module Compute = Diff.Right_variadic(struct
        let update = update
        let test (state:Defs.state) (arg,arg_mty) param =
          let loc = Location.none in
          let res = match (arg:Error.functor_arg_descr), param with
            | (Unit|Empty_struct), Unit -> Ok Tcoerce_none
            | Unit, Named _ | (Anonymous | Named _), Unit ->
                Result.Error (Error.Incompatible_params(arg,param))
            | ( Anonymous | Named _ | Empty_struct ), Named (_, param) ->
                match
                  modtypes ~in_eq:false ~loc state.env ~mark:Mark_neither
                    state.subst arg_mty param Shape.dummy_mod
                with
                | Error mty -> Result.Error (Error.Mismatch mty)
                | Ok (cc, _) -> Ok cc
          in
          res
        let weight = weight
      end)
    in
    let args = Array.of_list args in
    let params = Array.of_list params in
    let state : Defs.state =
      { env; subst = Subst.identity; res = I.keep_expansible_param res }
    in
    Compute.diff state args params

end

(* Hide the context and substitution parameters to the outside world *)

let modtypes_with_shape ~shape ~loc env ~mark mty1 mty2 =
  match modtypes ~in_eq:false ~loc env ~mark
          Subst.identity mty1 mty2 shape
  with
  | Ok (cc, shape) -> cc, shape
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))

let modtypes ~loc env ~mark mty1 mty2 =
  match modtypes ~in_eq:false ~loc env ~mark
          Subst.identity mty1 mty2 Shape.dummy_mod
  with
  | Ok (cc, _) -> cc
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))

let signatures env ~mark sig1 sig2 =
  match signatures ~in_eq:false ~loc:Location.none env ~mark
          Subst.identity sig1 sig2 Shape.dummy_mod
  with
  | Ok (cc, _) -> cc
  | Error reason -> raise (Error(env,Error.(In_Signature reason)))

let include_functor_signatures env ~mark sig1 sig2 =
  let sig1 = List.map Subst.Lazy.of_signature_item sig1 in
  let sig2 = List.map Subst.Lazy.of_signature_item sig2 in
  match include_functor_signatures ~loc:Location.none env ~mark
          Subst.identity sig1 sig2 Shape.dummy_mod
  with
  | Ok cc -> cc
  | Error reason -> raise (Error(env,Error.(In_Include_functor_signature reason)))

let type_declarations ~loc env ~mark id decl1 decl2 =
  match type_declarations ~loc env ~mark Subst.identity id decl1 decl2 with
  | Ok _ -> ()
  | Error (Error.Core reason) ->
      raise (Error(env,Error.(In_Type_declaration(id,reason))))
  | Error _ -> assert false

let strengthened_module_decl ~loc ~aliasable env ~mark md1 path1 md2 =
  match strengthened_module_decl ~loc ~aliasable env ~mark Subst.identity
    md1 path1 md2 Shape.dummy_mod with
  | Ok (x, _shape) -> x
  | Error mdiff ->
      raise (Error(env,Error.(In_Module_type mdiff)))

let expand_module_alias ~strengthen env path =
  try
    Mtype.find_type_of_module ~strengthen ~aliasable:true env path
  with Not_found ->
    raise (Error(env,In_Expansion(Error.Unbound_module_path path)))

let check_modtype_equiv ~loc env id mty1 mty2 =
  let mty1' = Subst.Lazy.of_modtype mty1 in
  let mty2' = Subst.Lazy.of_modtype mty2 in
  match check_modtype_equiv ~in_eq:false ~loc env ~mark:Mark_both mty1' mty2' with
  | Ok _ -> ()
  | Error e ->
      raise (Error(env,
                   Error.(In_Module_type_substitution (id,diff mty1 mty2 e)))
            )

let item_ident_name item = item_ident_name (Subst.Lazy.of_signature_item item)
let is_runtime_component item =
  is_runtime_component (Subst.Lazy.of_signature_item item)
