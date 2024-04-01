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

(* Substitutions *)

open Misc
open Path
open Types
open Btype

open Local_store

type jkind_error =
  | Unconstrained_jkind_variable

exception Error of Location.t * jkind_error

type type_replacement =
  | Path of Path.t
  | Type_function of { params : type_expr list; body : type_expr }

type additional_action =
  | Prepare_for_saving of (Location.t -> jkind -> jkind)
    (* The [Prepare_for_saving] function should be applied to all jkinds when
       saving; this commons them up, truncates their histories, and runs
       a check that all unconstrained variables have been defaulted to value. *)
  | Duplicate_variables
  | No_action

type t =
  { types: type_replacement Path.Map.t;
    modules: Path.t Path.Map.t;
    modtypes: module_type Path.Map.t;

    additional_action: additional_action;

    loc: Location.t option;
    mutable last_compose: (t * t) option  (* Memoized composition *)
  }
let identity =
  { types = Path.Map.empty;
    modules = Path.Map.empty;
    modtypes = Path.Map.empty;
    additional_action = No_action;
    loc = None;
    last_compose = None;
  }

let add_type_path id p s =
  { s with types = Path.Map.add id (Path p) s.types; last_compose = None }
let add_type id p s = add_type_path (Pident id) p s

let add_type_function id ~params ~body s =
  { s with types = Path.Map.add id (Type_function { params; body }) s.types;
    last_compose = None
  }

let add_module_path id p s =
  { s with modules = Path.Map.add id p s.modules; last_compose = None }
let add_module id p s = add_module_path (Pident id) p s

let add_modtype_path p ty s =
  { s with modtypes = Path.Map.add p ty s.modtypes; last_compose = None }
let add_modtype id ty s = add_modtype_path (Pident id) ty s

type additional_action_config =
  | Duplicate_variables
  | Prepare_for_saving

let with_additional_action (config : additional_action_config) s =
  (* CR layouts: it would be better to put all this stuff outside this
     function, but it's in here because we really want to tailor the reason
     to describe the module a symbol is imported from. But RAE's initial
     attempt to do this based on filename caused spurious "inconsistent
     assumption" errors that couldn't immediately be solved. Revisit
     with a better approach.

     We'll need to revisit the Note [Preparing_for_saving always the same]
     once we do this tailoring.
  *)
  let additional_action : additional_action =
    match config with
    | Duplicate_variables -> Duplicate_variables
    | Prepare_for_saving ->
        let reason = Jkind.Imported in
        let any = Jkind.of_const Any ~why:reason in
        let void = Jkind.of_const Void ~why:reason in
        let value = Jkind.of_const Value ~why:reason in
        let immediate = Jkind.of_const Immediate ~why:reason in
        let immediate64 = Jkind.of_const Immediate64 ~why:reason in
        let float64 = Jkind.of_const Float64 ~why:reason in
        let word = Jkind.of_const Word ~why:reason in
        let bits32 = Jkind.of_const Bits32 ~why:reason in
        let bits64 = Jkind.of_const Bits64 ~why:reason in
        let prepare_jkind loc lay =
          match Jkind.get lay with
          | Const Any -> any
          | Const Void -> void
          | Const Value -> value
          | Const Immediate -> immediate
          | Const Immediate64 -> immediate64
          | Const Float64 -> float64
          | Const Word -> word
          | Const Bits32 -> bits32
          | Const Bits64 -> bits64
          | Var _ -> raise(Error (loc, Unconstrained_jkind_variable))
        in
        Prepare_for_saving prepare_jkind
  in
  { s with additional_action; last_compose = None }

let apply_prepare_jkind s lay loc =
  match s.additional_action with
  | Prepare_for_saving prepare_jkind -> prepare_jkind loc lay
  | Duplicate_variables | No_action -> lay

let change_locs s loc = { s with loc = Some loc; last_compose = None }

let loc s x =
  match s.loc with
  | Some l -> l
  | None -> begin
      match s.additional_action with
      | Prepare_for_saving _ | Duplicate_variables ->
          if not !Clflags.keep_locs then Location.none else x
      | No_action -> x
    end

let remove_loc =
  let open Ast_mapper in
  {default_mapper with location = (fun _this _loc -> Location.none)}

let is_not_doc = function
  | {Parsetree.attr_name = {Location.txt = "ocaml.doc"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "ocaml.text"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "doc"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "text"}; _} -> false
  | _ -> true

let attrs s x =
  (* Now that we track [Duplicate_variables] and [Prepare_for_saving] as
     separate states, we should reconsider whether the [Duplicate_variables]
     callsites really need to scrub docs and locations. For now, we're keeping
     the scrubbing behavior for backward compatibility.
  *)
  let x =
    match s.additional_action with
    | Prepare_for_saving _ | Duplicate_variables ->
        if not !Clflags.keep_docs
        then List.filter is_not_doc x
        else x
    | No_action -> x
  in
  match s.additional_action with
  | Prepare_for_saving _ | Duplicate_variables ->
      if not !Clflags.keep_locs
      then remove_loc.Ast_mapper.attributes remove_loc x
      else x
  | No_action -> x

let rec module_path s path =
  try Path.Map.find path s.modules
  with Not_found ->
    match path with
    | Pident _ -> path
    | Pdot(p, n) ->
       Pdot(module_path s p, n)
    | Papply(p1, p2) ->
       Papply(module_path s p1, module_path s p2)
    | Pextra_ty _ ->
       fatal_error "Subst.module_path"

let modtype_path s path =
      match Path.Map.find path s.modtypes with
      | Mty_ident p -> p
      | Mty_alias _ | Mty_signature _ | Mty_functor _| Mty_strengthen _ ->
         fatal_error "Subst.modtype_path"
      | exception Not_found ->
         match path with
         | Pdot(p, n) ->
            Pdot(module_path s p, n)
         | Papply _ | Pextra_ty _ ->
            fatal_error "Subst.modtype_path"
         | Pident _ -> path

(* For values, extension constructors, classes and class types *)
let value_path s path =
  match path with
  | Pident _ -> path
  | Pdot(p, n) -> Pdot(module_path s p, n)
  | Papply _ | Pextra_ty _ -> fatal_error "Subst.value_path"

let rec type_path s path =
  match Path.Map.find path s.types with
  | Path p -> p
  | Type_function _ -> assert false
  | exception Not_found ->
     match path with
     | Pident _ -> path
     | Pdot(p, n) ->
        Pdot(module_path s p, n)
     | Papply _ ->
        fatal_error "Subst.type_path"
     | Pextra_ty (p, extra) ->
         match extra with
         | Pcstr_ty _ -> Pextra_ty (type_path s p, extra)
         | Pext_ty -> Pextra_ty (value_path s p, extra)

let to_subst_by_type_function s p =
  match Path.Map.find p s.types with
  | Path _ -> false
  | Type_function _ -> true
  | exception Not_found -> false

(* Special type ids for saved signatures *)

let new_id = s_ref (-1)
let reset_additional_action_type_id () = new_id := -1

let newpersty desc =
  decr new_id;
  create_expr
    desc ~level:generic_level ~scope:Btype.lowest_level ~id:!new_id

(* CR layouts: remove this. While we're still developing, though, it might
   be nice to get the location of this kind of error. *)
(* We use a ref instead of passing [loc] as an argument to [typexp]
   because the ref requires no modifications to the body of [typexp],
   reducing the chance of merge conflicts. This location is not critical --
   it just makes an error message more useful in case of a compiler bug.
   We may decide to get rid of this check someday, too.
*)
let location_for_jkind_check_errors = ref Location.none

let norm desc ~prepare_jkind =
  match desc with
  | Tvar { name; jkind } ->
      let loc = !location_for_jkind_check_errors in
      Tvar { name; jkind = prepare_jkind loc jkind }
  | Tunivar { name; jkind } ->
      let loc = !location_for_jkind_check_errors in
      Tunivar { name; jkind = prepare_jkind loc jkind }
    | desc -> desc

let ctype_apply_env_empty = ref (fun _ -> assert false)

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp copy_scope s ty =
  let should_duplicate_vars =
    match s.additional_action with
    | Duplicate_variables | Prepare_for_saving _ -> true
    | No_action -> false
  in
  let desc = get_desc ty in
  match desc with
    Tvar _ | Tunivar _ ->
      if should_duplicate_vars || get_id ty < 0 then
        let ty' =
          match s.additional_action with
          | Duplicate_variables -> newpersty desc
          | Prepare_for_saving prepare_jkind ->
              newpersty (norm desc ~prepare_jkind)
          | No_action -> newty2 ~level:(get_level ty) desc
        in
        For_copy.redirect_desc copy_scope ty (Tsubst (ty', None));
        ty'
      else ty
  | Tsubst (ty, _) ->
      ty
  | Tfield (m, k, _t1, _t2) when not should_duplicate_vars && m = dummy_method
      && field_kind_repr k <> Fabsent && get_level ty < generic_level ->
      (* do not copy the type of self when it is not generalized *)
      ty
(* cannot do it, since it would omit substitution
  | Tvariant row when not (static_row row) ->
      ty
*)
  | _ ->
    let tm = row_of_type ty in
    let has_fixed_row =
      not (is_Tconstr ty) && is_constr_row ~allow_ident:false tm in
    (* Make a stub *)
    let jkind = Jkind.any ~why:Dummy_jkind in
    let ty' =
      if should_duplicate_vars then newpersty (Tvar {name = None; jkind})
      else newgenstub ~scope:(get_scope ty) jkind
    in
    For_copy.redirect_desc copy_scope ty (Tsubst (ty', None));
    let desc =
      if has_fixed_row then
        match get_desc tm with (* PR#7348 *)
          Tconstr (Pdot(m,i), tl, _abbrev) ->
            let i' = String.sub i 0 (String.length i - 4) in
            Tconstr(type_path s (Pdot(m,i')), tl, ref Mnil)
        | _ -> assert false
      else match desc with
      | Tconstr (p, args, _abbrev) ->
         let args = List.map (typexp copy_scope s) args in
         begin match Path.Map.find p s.types with
         | exception Not_found -> Tconstr(type_path s p, args, ref Mnil)
         | Path _ -> Tconstr(type_path s p, args, ref Mnil)
         | Type_function { params; body } ->
            Tlink (!ctype_apply_env_empty params body args)
         end
      | Tpackage(p, fl) ->
          Tpackage(modtype_path s p,
                   List.map (fun (n, ty) -> (n, typexp copy_scope s ty)) fl)
      | Tobject (t1, name) ->
          let t1' = typexp copy_scope s t1 in
          let name' =
            match !name with
            | None -> None
            | Some (p, tl) ->
                if to_subst_by_type_function s p
                then None
                else Some (type_path s p, List.map (typexp copy_scope s) tl)
          in
          Tobject (t1', ref name')
      | Tvariant row ->
          let more = row_more row in
          let mored = get_desc more in
          (* We must substitute in a subtle way *)
          (* Tsubst takes a tuple containing the row var and the variant *)
          begin match mored with
            Tsubst (_, Some ty2) ->
              (* This variant type has been already copied *)
              (* Change the stub to avoid Tlink in the new type *)
              For_copy.redirect_desc copy_scope ty (Tsubst (ty2, None));
              Tlink ty2
          | _ ->
              let dup =
                should_duplicate_vars || get_level more = generic_level ||
                static_row row || is_Tconstr more in
              (* Various cases for the row variable *)
              let more' =
                match mored with
                  Tsubst (ty, None) -> ty
                | Tconstr _ | Tnil -> typexp copy_scope s more
                | Tunivar _ | Tvar _ ->
                    if should_duplicate_vars then newpersty mored
                    else if dup && is_Tvar more then newgenty mored
                    else more
                | _ -> assert false
              in
              (* Register new type first for recursion *)
              For_copy.redirect_desc copy_scope more
                (Tsubst (more', Some ty'));
              (* TODO: check if more' can be eliminated *)
              (* Return a new copy *)
              let row =
                copy_row (typexp copy_scope s) true row (not dup) more' in
              match row_name row with
              | Some (p, tl) ->
                  let name =
                    if to_subst_by_type_function s p then None
                    else Some (type_path s p, tl)
                  in
                  Tvariant (set_row_name row name)
              | None ->
                  Tvariant row
          end
      | Tfield(_label, kind, _t1, t2) when field_kind_repr kind = Fabsent ->
          Tlink (typexp copy_scope s t2)
      | _ -> copy_type_desc (typexp copy_scope s) desc
    in
    Transient_expr.set_stub_desc ty' desc;
    ty'

(* [loc] is different than [s.loc]:
     - [s.loc] is a way for the external client of the module to indicate the
       location of the copy.
     - [loc] is internally-populated and is the location of the AST construct
       that encloses the type (and is used only in errors in the jkind check).
*)
let typexp copy_scope s loc ty =
  location_for_jkind_check_errors := loc;
  typexp copy_scope s ty

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let loc = Option.value s.loc ~default:Location.none in
  For_copy.with_scope (fun copy_scope -> typexp copy_scope s loc ty)

let label_declaration copy_scope s l =
  {
    ld_id = l.ld_id;
    ld_mutable = l.ld_mutable;
    ld_global = l.ld_global;
    ld_jkind = apply_prepare_jkind s l.ld_jkind l.ld_loc;
    ld_type = typexp copy_scope s l.ld_loc l.ld_type;
    ld_loc = loc s l.ld_loc;
    ld_attributes = attrs s l.ld_attributes;
    ld_uid = l.ld_uid;
  }

let constructor_argument copy_scope s ca =
  {
    ca_type = typexp copy_scope s ca.ca_loc ca.ca_type;
    ca_loc = loc s ca.ca_loc;
    ca_global = ca.ca_global;
  }

let constructor_arguments copy_scope s = function
  | Cstr_tuple l ->
      Cstr_tuple (List.map (constructor_argument copy_scope s) l)
  | Cstr_record l ->
      Cstr_record (List.map (label_declaration copy_scope s) l)

let constructor_declaration copy_scope s c =
  {
    cd_id = c.cd_id;
    cd_args = constructor_arguments copy_scope s c.cd_args;
    cd_res = Option.map (typexp copy_scope s c.cd_loc) c.cd_res;
    cd_loc = loc s c.cd_loc;
    cd_attributes = attrs s c.cd_attributes;
    cd_uid = c.cd_uid;
  }

(* called only when additional_action is [Prepare_for_saving] *)
let constructor_tag ~prepare_jkind loc = function
  | Ordinary _ as tag -> tag
  | Extension (path, lays) ->
      Extension (path, Array.map (prepare_jkind loc) lays)

(* called only when additional_action is [Prepare_for_saving] *)
let variant_representation ~prepare_jkind loc = function
  | Variant_unboxed -> Variant_unboxed
  | Variant_boxed layss ->
    Variant_boxed (Array.map (Array.map (prepare_jkind loc)) layss)
  | Variant_extensible -> Variant_extensible

(* called only when additional_action is [Prepare_for_saving] *)
let record_representation ~prepare_jkind loc = function
  | Record_unboxed -> Record_unboxed
  | Record_inlined (tag, variant_rep) ->
    Record_inlined (constructor_tag ~prepare_jkind loc tag,
                    variant_representation ~prepare_jkind loc variant_rep)
  | Record_boxed lays ->
      Record_boxed (Array.map (prepare_jkind loc) lays)
  | Record_float -> Record_float
  | Record_ufloat -> Record_ufloat

let type_declaration' copy_scope s decl =
  { type_params = List.map (typexp copy_scope s decl.type_loc) decl.type_params;
    type_arity = decl.type_arity;
    type_kind =
      begin match decl.type_kind with
        Type_abstract r -> Type_abstract r
      | Type_variant (cstrs, rep) ->
          let rep =
            match s.additional_action with
            | No_action | Duplicate_variables -> rep
            | Prepare_for_saving prepare_jkind ->
                variant_representation ~prepare_jkind decl.type_loc rep
          in
          Type_variant (List.map (constructor_declaration copy_scope s) cstrs,
                        rep)
      | Type_record(lbls, rep) ->
          let rep =
            match s.additional_action with
            | No_action | Duplicate_variables -> rep
            | Prepare_for_saving prepare_jkind ->
                record_representation ~prepare_jkind decl.type_loc rep
          in
          Type_record (List.map (label_declaration copy_scope s) lbls, rep)
      | Type_open -> Type_open
      end;
    type_manifest =
      begin
        match decl.type_manifest with
          None -> None
        | Some ty -> Some(typexp copy_scope s decl.type_loc ty)
      end;
    type_jkind =
      begin
        match s.additional_action with
        | Prepare_for_saving prepare_jkind ->
            prepare_jkind decl.type_loc decl.type_jkind
        | Duplicate_variables | No_action -> decl.type_jkind
      end;
    (* CR layouts v10: Apply the substitution here, too *)
    type_jkind_annotation = decl.type_jkind_annotation;
    type_private = decl.type_private;
    type_variance = decl.type_variance;
    type_separability = decl.type_separability;
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = loc s decl.type_loc;
    type_attributes = attrs s decl.type_attributes;
    type_unboxed_default = decl.type_unboxed_default;
    type_uid = decl.type_uid;
  }

let type_declaration s decl =
  For_copy.with_scope (fun copy_scope -> type_declaration' copy_scope s decl)

let class_signature copy_scope s loc sign =
  { csig_self = typexp copy_scope s loc sign.csig_self;
    csig_self_row = typexp copy_scope s loc sign.csig_self_row;
    csig_vars =
      Vars.map
        (function (m, v, t) -> (m, v, typexp copy_scope s loc t))
        sign.csig_vars;
    csig_meths =
      Meths.map
        (function (p, v, t) -> (p, v, typexp copy_scope s loc t))
        sign.csig_meths;
  }

let rec class_type copy_scope s cty =
  let loc = Option.value s.loc ~default:Location.none in
  match cty with
  | Cty_constr (p, tyl, cty) ->
      let p' = type_path s p in
      let tyl' = List.map (typexp copy_scope s loc) tyl in
      let cty' = class_type copy_scope s cty in
      Cty_constr (p', tyl', cty')
  | Cty_signature sign ->
      Cty_signature (class_signature copy_scope s loc sign)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, typexp copy_scope s loc ty, class_type copy_scope s cty)

let class_declaration' copy_scope s decl =
  { cty_params = List.map (typexp copy_scope s decl.cty_loc) decl.cty_params;
    cty_variance = decl.cty_variance;
    cty_type = class_type copy_scope s decl.cty_type;
    cty_path = type_path s decl.cty_path;
    cty_new =
      begin match decl.cty_new with
      | None    -> None
      | Some ty -> Some (typexp copy_scope s decl.cty_loc ty)
      end;
    cty_loc = loc s decl.cty_loc;
    cty_attributes = attrs s decl.cty_attributes;
    cty_uid = decl.cty_uid;
  }

let class_declaration s decl =
  For_copy.with_scope (fun copy_scope -> class_declaration' copy_scope s decl)

let cltype_declaration' copy_scope s decl =
  { clty_params = List.map (typexp copy_scope s decl.clty_loc) decl.clty_params;
    clty_variance = decl.clty_variance;
    clty_type = class_type copy_scope s decl.clty_type;
    clty_path = type_path s decl.clty_path;
    clty_hash_type = type_declaration' copy_scope s decl.clty_hash_type ;
    clty_loc = loc s decl.clty_loc;
    clty_attributes = attrs s decl.clty_attributes;
    clty_uid = decl.clty_uid;
  }

let cltype_declaration s decl =
  For_copy.with_scope (fun copy_scope -> cltype_declaration' copy_scope s decl)

let class_type s cty =
  For_copy.with_scope (fun copy_scope -> class_type copy_scope s cty)


let extension_constructor' copy_scope s ext =
  { ext_type_path = type_path s ext.ext_type_path;
    ext_type_params =
      List.map (typexp copy_scope s ext.ext_loc) ext.ext_type_params;
    ext_args = constructor_arguments copy_scope s ext.ext_args;
    ext_arg_jkinds = begin match s.additional_action with
      | Prepare_for_saving prepare_jkind ->
          Array.map (prepare_jkind ext.ext_loc) ext.ext_arg_jkinds
      | Duplicate_variables | No_action -> ext.ext_arg_jkinds
    end;
    ext_constant = ext.ext_constant;
    ext_ret_type =
      Option.map (typexp copy_scope s ext.ext_loc) ext.ext_ret_type;
    ext_private = ext.ext_private;
    ext_attributes = attrs s ext.ext_attributes;
    ext_loc = begin match s.additional_action with
      | Prepare_for_saving _ | Duplicate_variables -> Location.none
      | No_action -> ext.ext_loc
    end;
    ext_uid = ext.ext_uid;
  }

let extension_constructor s ext =
  For_copy.with_scope
    (fun copy_scope -> extension_constructor' copy_scope s ext)


(* For every binding k |-> d of m1, add k |-> f d to m2
   and return resulting merged map. *)
let merge_path_maps f m1 m2 =
  Path.Map.fold (fun k d accu -> Path.Map.add k (f d) accu) m1 m2

let keep_latest_loc l1 l2 =
  match l2 with
  | None -> l1
  | Some _ -> l2

let type_replacement s = function
  | Path p -> Path (type_path s p)
  | Type_function { params; body } ->
    let loc = Option.value s.loc ~default:Location.none in
    For_copy.with_scope (fun copy_scope ->
     let params = List.map (typexp copy_scope s loc) params in
     let body = typexp copy_scope s loc body in
     Type_function { params; body })

type scoping =
  | Keep
  | Make_local
  | Rescope of int

module Wrap : sig
  type subst = t
  type 'a t

  val of_value : 'a -> 'a t
  val of_lazy : 'a Lazy.t -> 'a t
  val force : (scoping -> subst -> 'a -> 'a) -> 'a t -> 'a
  val substitute :
    compose:(subst -> subst -> subst) -> scoping -> subst -> 'a t -> 'a t
end = struct
  type subst = t

  (* We are lazy twice here - firstly, in converting an underlying type to Subst.Lazy.* and
     then, in applying the substitution which we try to accumulate. Note that there is a
     difference between not applying a substitution at all and applying an identity
     substitution as the latter renames - hence the option. *)
  type 'a t = ((scoping * subst) option * 'a Lazy.t, 'a) Lazy_backtrack.t

  let of_value = Lazy_backtrack.create_forced
  let of_lazy x = Lazy_backtrack.create (None, x)

  let substitute ~compose scoping s x =
    match Lazy_backtrack.get_contents x with
    | Left (None, x) ->
      Lazy_backtrack.create (Some (scoping, s), x)
    | Left (Some (scoping', s'), x) ->
        let scoping =
          match scoping', scoping with
          | sc, Keep -> sc
          | _, (Make_local|Rescope _) -> scoping
        in
        let s = compose s' s in
        Lazy_backtrack.create (Some (scoping, s), x)
    | Right x ->
        Lazy_backtrack.create (Some (scoping, s), Lazy.from_val x)

  let force f = Lazy_backtrack.force (fun (s, x) ->
    let x = Lazy.force x in
    match s with
    | Some (scoping, s) -> f scoping s x
    | None -> x)
end

module Lazy_types = Types.Make_wrapped(Wrap)
open Lazy_types

let rename_bound_idents scoping s sg =
  let rename =
    let open Ident in
    match scoping with
    | Keep -> (fun id -> create_scoped ~scope:(scope id) (name id))
    | Make_local -> Ident.rename
    | Rescope scope -> (fun id -> create_scoped ~scope (name id))
  in
  let rec rename_bound_idents s sg = function
    | [] -> sg, s
    | Sig_type(id, td, rs, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (Sig_type(id', td, rs, vis) :: sg)
          rest
    | Sig_module(id, pres, md, rs, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_module id (Pident id') s)
          (Sig_module (id', pres, md, rs, vis) :: sg)
          rest
    | Sig_modtype(id, mtd, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_modtype id (Types.Mty_ident(Pident id')) s)
          (Sig_modtype(id', mtd, vis) :: sg)
          rest
    | Sig_class(id, cd, rs, vis) :: rest ->
        (* cheat and pretend they are types cf. PR#6650 *)
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (Sig_class(id', cd, rs, vis) :: sg)
          rest
    | Sig_class_type(id, ctd, rs, vis) :: rest ->
        (* cheat and pretend they are types cf. PR#6650 *)
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (Sig_class_type(id', ctd, rs, vis) :: sg)
          rest
    | Sig_value(id, vd, vis) :: rest ->
        (* scope doesn't matter for value identifiers. *)
        let id' = Ident.rename id in
        rename_bound_idents s (Sig_value(id', vd, vis) :: sg) rest
    | Sig_typext(id, ec, es, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents s (Sig_typext(id',ec,es,vis) :: sg) rest
  in
  rename_bound_idents s [] sg

module To_lazy = Types.Map_wrapped(Types)(Lazy_types)

let to_lazy =
  let map_signature m sg =
    lazy (List.map (To_lazy.signature_item m) sg) |> Wrap.of_lazy
  in
  let map_type_expr _ = Wrap.of_value in
  To_lazy.{map_signature; map_type_expr}

let lazy_value_description = To_lazy.value_description to_lazy
let lazy_module_decl = To_lazy.module_declaration to_lazy
let lazy_functor_parameter = To_lazy.functor_parameter to_lazy
let lazy_modtype = To_lazy.module_type to_lazy
let lazy_modtype_decl = To_lazy.modtype_declaration to_lazy
let lazy_signature_item = To_lazy.signature_item to_lazy

module From_lazy = Types.Map_wrapped(Lazy_types)(Types)

let force_type_expr ty = Wrap.force (fun _ s ty ->
  let loc = Option.value s.loc ~default:Location.none in
  For_copy.with_scope (fun copy_scope -> typexp copy_scope s loc ty)) ty

let rec subst_lazy_value_description s descr =
  { val_type = Wrap.substitute ~compose Keep s descr.val_type;
    val_kind = descr.val_kind;
    val_loc = loc s descr.val_loc;
    val_attributes = attrs s descr.val_attributes;
    val_uid = descr.val_uid;
  }

and subst_lazy_module_decl scoping s md =
  let md_type = subst_lazy_modtype scoping s md.md_type in
  { md_type;
    md_attributes = attrs s md.md_attributes;
    md_loc = loc s md.md_loc;
    md_uid = md.md_uid }

and subst_lazy_modtype scoping s = function
  | Mty_ident p ->
      begin match Path.Map.find p s.modtypes with
       | mty -> lazy_modtype mty
       | exception Not_found ->
          begin match p with
          | Pident _ -> Mty_ident p
          | Pdot(p, n) ->
             Mty_ident(Pdot(module_path s p, n))
          | Papply _ | Pextra_ty _ ->
             fatal_error "Subst.modtype"
          end
      end
  | Mty_signature sg ->
      Mty_signature(subst_lazy_signature scoping s sg)
  | Mty_functor(Unit, res) ->
      Mty_functor(Unit, subst_lazy_modtype scoping s res)
  | Mty_functor(Named (None, arg), res) ->
      Mty_functor(Named (None, (subst_lazy_modtype scoping s) arg),
                   subst_lazy_modtype scoping s res)
  | Mty_functor(Named (Some id, arg), res) ->
      let id' = Ident.rename id in
      Mty_functor(Named (Some id', (subst_lazy_modtype scoping s) arg),
                  subst_lazy_modtype scoping (add_module id (Pident id') s) res)
  | Mty_alias p ->
      Mty_alias (module_path s p)
  | Mty_strengthen (mty, p, a) ->
      Mty_strengthen (subst_lazy_modtype scoping s mty, module_path s p, a)

and subst_lazy_modtype_decl scoping s mtd =
  { mtd_type = Option.map (subst_lazy_modtype scoping s) mtd.mtd_type;
    mtd_attributes = attrs s mtd.mtd_attributes;
    mtd_loc = loc s mtd.mtd_loc;
    mtd_uid = mtd.mtd_uid }

and subst_lazy_signature scoping s sg =
  Wrap.substitute ~compose scoping s sg

and force_signature_once sg =
  Wrap.force force_signature_once' sg

and force_signature_once' scoping s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (sg', s') = rename_bound_idents scoping s sg in
  (* ... then apply it to each signature component in turn *)
  For_copy.with_scope (fun copy_scope ->
    List.rev_map (subst_lazy_signature_item' copy_scope scoping s') sg'
  )

and subst_lazy_signature_item' copy_scope scoping s comp =
  match comp with
    Sig_value(id, d, vis) ->
      Sig_value(id, subst_lazy_value_description s d, vis)
  | Sig_type(id, d, rs, vis) ->
      Sig_type(id, type_declaration' copy_scope s d, rs, vis)
  | Sig_typext(id, ext, es, vis) ->
      Sig_typext(id, extension_constructor' copy_scope s ext, es, vis)
  | Sig_module(id, pres, d, rs, vis) ->
      Sig_module(id, pres, subst_lazy_module_decl scoping s d, rs, vis)
  | Sig_modtype(id, d, vis) ->
      Sig_modtype(id, subst_lazy_modtype_decl scoping s d, vis)
  | Sig_class(id, d, rs, vis) ->
      Sig_class(id, class_declaration' copy_scope s d, rs, vis)
  | Sig_class_type(id, d, rs, vis) ->
      Sig_class_type(id, cltype_declaration' copy_scope s d, rs, vis)

and modtype scoping s t =
  t |> lazy_modtype |> subst_lazy_modtype scoping s |> force_modtype

(* Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) *)

and compose s1 s2 =
  if s1 == identity then s2 else
  if s2 == identity then s1 else
  match s2.last_compose with
  | Some (t,s) when t == s1 -> s
  | _ ->
      let s =
        { types = merge_path_maps (type_replacement s2) s1.types s2.types;
          modules = merge_path_maps (module_path s2) s1.modules s2.modules;
          modtypes = merge_path_maps (modtype Keep s2) s1.modtypes s2.modtypes;
          additional_action = begin
            match s1.additional_action, s2.additional_action with
            | action, No_action | No_action, action -> action
            | Duplicate_variables, Duplicate_variables -> Duplicate_variables

            (* Preparing for saving runs a superset of the things involved with
               copying variables, so we prefer that if composing substitutions.
            *)
            | (Prepare_for_saving _ as prepare), Duplicate_variables
            | Duplicate_variables, (Prepare_for_saving _ as prepare)
                -> prepare

            (* Note [Preparing_for_saving always the same]
               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               The function we put in [Prepare_for_saving] is always the same,
               so we can take either.
            *)
            | (Prepare_for_saving _ as prepare1), Prepare_for_saving _
                -> prepare1
          end;
          loc = keep_latest_loc s1.loc s2.loc;
          last_compose = None
        }
      in
      s2.last_compose <- Some (s1,s); s

and from_lazy =
  let map_signature m sg =
    let items = force_signature_once sg in
    List.map (From_lazy.signature_item m) items
  in
  let map_type_expr _ = force_type_expr in
  From_lazy.{map_signature; map_type_expr}

and force_value_description vd = From_lazy.value_description from_lazy vd
and force_module_decl d = From_lazy.module_declaration from_lazy d
and force_functor_parameter x = From_lazy.functor_parameter from_lazy x
and force_modtype x = From_lazy.module_type from_lazy x
and force_modtype_decl x = From_lazy.modtype_declaration from_lazy x
and force_signature_item x = From_lazy.signature_item from_lazy x
and force_signature x = From_lazy.signature from_lazy x

let subst_lazy_signature_item scoping s comp =
  For_copy.with_scope
    (fun copy_scope -> subst_lazy_signature_item' copy_scope scoping s comp)

module Lazy = struct
  include Lazy_types

  let of_value x = Wrap.of_value x
  let of_lazy = Wrap.of_lazy
  let substitute s = Wrap.substitute ~compose Keep s

  let of_module_decl = lazy_module_decl
  let of_modtype = lazy_modtype
  let of_modtype_decl = lazy_modtype_decl
  let of_signature sg = Wrap.of_lazy (lazy (List.map lazy_signature_item sg))
  let of_signature_item = lazy_signature_item
  let of_functor_parameter = lazy_functor_parameter
  let of_value_description = lazy_value_description

  let module_decl = subst_lazy_module_decl
  let modtype = subst_lazy_modtype
  let modtype_decl = subst_lazy_modtype_decl
  let signature = subst_lazy_signature
  let signature_item = subst_lazy_signature_item
  let value_description = subst_lazy_value_description

  let force_module_decl = force_module_decl
  let force_modtype = force_modtype
  let force_modtype_decl = force_modtype_decl
  let force_signature = force_signature
  let force_signature_once = force_signature_once
  let force_signature_item = force_signature_item
  let force_functor_parameter = force_functor_parameter
  let force_value_description = force_value_description
  let force_type_expr = force_type_expr
end

let signature sc s sg =
  Lazy.(sg |> of_signature |> signature sc s |> force_signature)

let signature_item sc s comp =
  Lazy.(comp|> of_signature_item |> signature_item sc s |> force_signature_item)

let modtype_declaration sc s decl =
  Lazy.(decl |> of_modtype_decl |> modtype_decl sc s |> force_modtype_decl)

let module_declaration scoping s decl =
  Lazy.(decl |> of_module_decl |> module_decl scoping s |> force_module_decl)

let value_description s descr =
  Lazy.(descr |> of_value_description |> value_description s |> force_value_description)

(* Error report *)
open Format

let report_error ppf = function
  | Unconstrained_jkind_variable ->
      fprintf ppf
        "Unconstrained layout variable detected when saving artifacts of \
         compilation to disk.@ Please report this error to \
         the Jane Street compilers team.@ "

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
          None
    )
