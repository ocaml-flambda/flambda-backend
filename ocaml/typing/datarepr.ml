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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Layouts
open Types
open Btype

(* Simplified version of Ctype.free_vars *)
let free_vars ?(param=false) ty =
  let ret = ref TypeSet.empty in
  let rec loop ty =
    if try_mark_node ty then
      match get_desc ty with
      | Tvar _ ->
          ret := TypeSet.add ty !ret
      | Tvariant row ->
          iter_row loop row;
          if not (static_row row) then begin
            match get_desc (row_more row) with
            | Tvar _ when param -> ret := TypeSet.add ty !ret
            | _ -> loop (row_more row)
          end
      (* XXX: What about Tobject ? *)
      | _ ->
          iter_type_expr loop ty
  in
  loop ty;
  unmark_type ty;
  !ret

let newgenconstr path tyl = newgenty (Tconstr (path, tyl, ref Mnil))

let constructor_existentials cd_args cd_res =
  let tyl = tys_of_constr_args cd_args in
  let existentials =
    match cd_res with
    | None -> []
    | Some type_ret ->
        let arg_vars_set = free_vars (newgenty (Ttuple tyl)) in
        let res_vars = free_vars type_ret in
        TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  (tyl, existentials)

let constructor_args ~current_unit priv cd_args cd_res path rep =
  let tyl, existentials = constructor_existentials cd_args cd_res in
  match cd_args with
  | Cstr_tuple l -> existentials, l, None
  | Cstr_record lbls ->
      let arg_vars_set = free_vars ~param:true (newgenty (Ttuple tyl)) in
      let type_params = TypeSet.elements arg_vars_set in
      let arity = List.length type_params in
      let is_void_label lbl = Layout.is_void_defaulting lbl.ld_layout in
      let layout =
        Layout.for_boxed_record ~all_void:(List.for_all is_void_label lbls)
      in
      let tdecl =
        {
          type_params;
          type_arity = arity;
          type_kind = Type_record (lbls, rep);
          type_layout = layout;
          type_private = priv;
          type_manifest = None;
          type_variance = Variance.unknown_signature ~injective:true ~arity;
          type_separability = Types.Separability.default_signature ~arity;
          type_is_newtype = false;
          type_expansion_scope = Btype.lowest_level;
          type_loc = Location.none;
          type_attributes = [];
          type_unboxed_default = false;
          type_uid = Uid.mk ~current_unit;
        }
      in
      existentials,
      [ newgenconstr path type_params, Unrestricted ],
      Some tdecl

let constructor_descrs ~current_unit ty_path decl cstrs rep =
  let ty_res = newgenconstr ty_path decl.type_params in
  let cstr_arg_layouts : layout array array =
    match rep with
    | Variant_extensible -> assert false
    | Variant_boxed layouts -> layouts
    | Variant_unboxed -> [| [| decl.type_layout |] |]
  in
  let all_void layouts = Array.for_all Layout.is_void_defaulting layouts in
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  let cstr_constant =
    Array.map
      (fun layouts ->
         let all_void = all_void layouts in
         if all_void then incr num_consts else incr num_nonconsts;
         all_void)
      cstr_arg_layouts
  in
  let describe_constructor (src_index, const_tag, nonconst_tag, acc)
        {cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid} =
    let cstr_name = Ident.name cd_id in
    let cstr_res =
      match cd_res with
      | Some ty_res' -> ty_res'
      | None -> ty_res
    in
    let cstr_arg_layouts = cstr_arg_layouts.(src_index) in
    let cstr_constant = cstr_constant.(src_index) in
    let runtime_tag, const_tag, nonconst_tag =
      if cstr_constant
      then const_tag, 1 + const_tag, nonconst_tag
      else nonconst_tag, const_tag, 1 + nonconst_tag
    in
    let cstr_tag = Ordinary {src_index; runtime_tag} in
    let cstr_existentials, cstr_args, cstr_inlined =
      (* This is the representation of the inner record, IF there is one *)
      let record_repr = Record_inlined (cstr_tag, rep) in
      constructor_args ~current_unit decl.type_private cd_args cd_res
        (Path.Pdot (ty_path, cstr_name)) record_repr
    in
    let cstr =
      { cstr_name;
        cstr_res;
        cstr_existentials;
        cstr_args;
        cstr_arg_layouts;
        cstr_arity = List.length cstr_args;
        cstr_tag;
        cstr_repr = rep;
        cstr_constant;
        cstr_consts = !num_consts;
        cstr_nonconsts = !num_nonconsts;
        cstr_generalized = cd_res <> None;
        cstr_private = decl.type_private;
        cstr_loc = cd_loc;
        cstr_attributes = cd_attributes;
        cstr_inlined;
        cstr_uid = cd_uid;
      } in
    (src_index+1, const_tag, nonconst_tag, (cd_id, cstr) :: acc)
  in
  let (_,_,_,cstrs) = List.fold_left describe_constructor (0,0,0,[]) cstrs in
  List.rev cstrs

let extension_descr ~current_unit path_ext ext =
  let ty_res =
    match ext.ext_ret_type with
        Some type_ret -> type_ret
      | None -> newgenconstr ext.ext_type_path ext.ext_type_params
  in
  let cstr_tag = Extension (path_ext, ext.ext_arg_layouts) in
  let existentials, cstr_args, cstr_inlined =
    constructor_args ~current_unit ext.ext_private ext.ext_args ext.ext_ret_type
      path_ext (Record_inlined (cstr_tag, Variant_extensible))
  in
    { cstr_name = Path.last path_ext;
      cstr_res = ty_res;
      cstr_existentials = existentials;
      cstr_args;
      cstr_arg_layouts = ext.ext_arg_layouts;
      cstr_arity = List.length cstr_args;
      cstr_tag;
      cstr_repr = Variant_extensible;
      cstr_constant = ext.ext_constant;
      cstr_consts = -1;
      cstr_nonconsts = -1;
      cstr_private = ext.ext_private;
      cstr_generalized = ext.ext_ret_type <> None;
      cstr_loc = ext.ext_loc;
      cstr_attributes = ext.ext_attributes;
      cstr_inlined;
      cstr_uid = ext.ext_uid;
    }

let none =
  create_expr (Ttuple []) ~level:(-1) ~scope:Btype.generic_level ~id:(-1)
    (* Clearly ill-formed type *)

let dummy_label =
  { lbl_name = ""; lbl_res = none; lbl_arg = none;
    lbl_mut = Immutable; lbl_global = Unrestricted;
    lbl_layout = Layout.any ~why:Dummy_layout;
    lbl_num = -1; lbl_pos = -1; lbl_all = [||];
    lbl_repres = Record_unboxed;
    lbl_private = Public;
    lbl_loc = Location.none;
    lbl_attributes = [];
    lbl_uid = Uid.internal_not_actually_unique;
  }

let label_descrs ty_res lbls repres priv =
  let all_labels = Array.make (List.length lbls) dummy_label in
  let rec describe_labels num pos = function
      [] -> []
    | l :: rest ->
        let is_void = Layout.is_void_defaulting l.ld_layout  in
        let lbl =
          { lbl_name = Ident.name l.ld_id;
            lbl_res = ty_res;
            lbl_arg = l.ld_type;
            lbl_mut = l.ld_mutable;
            lbl_global = l.ld_global;
            lbl_layout = l.ld_layout;
            lbl_pos = if is_void then lbl_pos_void else pos;
            lbl_num = num;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv;
            lbl_loc = l.ld_loc;
            lbl_attributes = l.ld_attributes;
            lbl_uid = l.ld_uid;
          } in
        all_labels.(num) <- lbl;
        let pos = if is_void then pos else pos+1 in
        (l.ld_id, lbl) :: describe_labels (num+1) pos rest in
  describe_labels 0 0 lbls

exception Constr_not_found

let find_constr ~constant tag cstrs =
  try
    List.find
      (function
        | ({cstr_tag=Ordinary {runtime_tag=tag'}; cstr_constant},_) ->
          tag' = tag && cstr_constant = constant
        | ({cstr_tag=Extension _},_) -> false)
      cstrs
  with
  | Not_found -> raise Constr_not_found

let find_constr_by_tag ~constant tag cstrlist =
  fst (find_constr ~constant tag cstrlist)

let constructors_of_type ~current_unit ty_path decl =
  match decl.type_kind with
  | Type_variant (cstrs,rep) ->
     constructor_descrs ~current_unit ty_path decl cstrs rep
  | Type_record _ | Type_abstract | Type_open -> []

let labels_of_type ty_path decl =
  match decl.type_kind with
  | Type_record(labels, rep) ->
      label_descrs (newgenconstr ty_path decl.type_params)
        labels rep decl.type_private
  | Type_variant _ | Type_abstract | Type_open -> []
