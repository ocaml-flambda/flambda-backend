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
        let arg_vars_set =
          free_vars (newgenty (Ttuple (List.map (fun ty -> None, ty) tyl)))
        in
        let res_vars = free_vars type_ret in
        TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  (tyl, existentials)

let constructor_args ~current_unit priv cd_args cd_res path rep =
  let tyl, existentials = constructor_existentials cd_args cd_res in
  match cd_args with
  | Cstr_tuple l -> existentials, l, None
  | Cstr_record lbls ->
      let arg_vars_set =
        free_vars ~param:true
          (newgenty (Ttuple (List.map (fun ty -> None, ty) tyl)))
      in
      let type_params = TypeSet.elements arg_vars_set in
      let arity = List.length type_params in
      (* CR layouts v2.8: We could call [Jkind.normalize ~mode:Require_best] on this
         jkind, and plausibly gain some perf wins by building up smaller jkinds that are
         cheaper to deal with later. But doing so runs into some confusing mutual
         recursion that's non-trivial to debug. Reinvestigate later *)
      let jkind = Jkind.for_boxed_record lbls in
      let tdecl =
        {
          type_params;
          type_arity = arity;
          type_kind = Type_record (lbls, rep, None);
          type_jkind = jkind;
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
          type_unboxed_version = None;
        }
      in
      existentials,
      [
        {
          ca_type = newgenconstr path type_params;
          ca_sort = Jkind.Sort.Const.value;
          ca_modalities = Mode.Modality.Value.Const.id;
          ca_loc = Location.none
        }
      ],
      Some tdecl

let constructor_descrs ~current_unit ty_path decl cstrs rep =
  let ty_res = newgenconstr ty_path decl.type_params in
  let cstr_shapes_and_arg_jkinds =
    match rep, cstrs with
    | Variant_extensible, _ -> assert false
    | Variant_boxed x, _ -> x
    | Variant_unboxed, [{ cd_args }] ->
      (* CR layouts: It's tempting just to use [decl.type_jkind] here, instead
         of grabbing the jkind from the argument. However, doing so does not
         work, now that we say [@@unboxed] types are classified by a sort
         variable: it seems that the sort variable ends up getting copied
         into the argument kind and then defaulted prematurely, causing errors
         when the payload of the [@@unboxed] type is not a value. ccasinghino
         believes that the choice of using [decl.type_jkind] vs the algorithm
         written here should be irrelevant, and so would like to understand
         this interaction better. *)
      begin match cd_args with
      | Cstr_tuple [{ ca_sort = sort }]
      | Cstr_record [{ ld_sort = sort }] ->
        [| Constructor_uniform_value, [| sort |] |]
      | Cstr_tuple ([] | _ :: _) | Cstr_record ([] | _ :: _) ->
        Misc.fatal_error "Multiple arguments in [@@unboxed] variant"
      end
    | Variant_unboxed, ([] | _ :: _) ->
      Misc.fatal_error "Multiple or 0 constructors in [@@unboxed] variant"
    | Variant_with_null, _ ->
      (* CR layouts v3.5: this hardcodes ['a or_null]. Fix when we allow
         users to write their own null constructors. *)
      (* CR layouts v3.3: generalize to [any]. *)
      [| Constructor_uniform_value, [| |]
       ; Constructor_uniform_value, [| Jkind.Sort.Const.value |] |]
  in
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  let cstr_constant =
    Array.map
      (fun (_, sorts) ->
         let all_void = Array.for_all Jkind.Sort.Const.all_void sorts in
         if all_void then incr num_consts else incr num_nonconsts;
         all_void)
      cstr_shapes_and_arg_jkinds
  in
  let describe_constructor (src_index, const_tag, nonconst_tag, acc)
        {cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid} =
    let cstr_name = Ident.name cd_id in
    let cstr_res =
      match cd_res with
      | Some ty_res' -> ty_res'
      | None -> ty_res
    in
    let cstr_shape, _ = cstr_shapes_and_arg_jkinds.(src_index) in
    let cstr_constant = cstr_constant.(src_index) in
    let runtime_tag, const_tag, nonconst_tag =
      if cstr_constant
      then const_tag, 1 + const_tag, nonconst_tag
      else nonconst_tag, const_tag, 1 + nonconst_tag
    in
    let cstr_tag =
      match rep, cstr_constant with
      | Variant_with_null, true -> Null
      | _, _ ->  Ordinary {src_index; runtime_tag}
    in
    let cstr_existentials, cstr_args, cstr_inlined =
      (* This is the representation of the inner record, IF there is one *)
      let record_repr = Record_inlined (cstr_tag, cstr_shape, rep) in
      constructor_args ~current_unit decl.type_private cd_args cd_res
        Path.(Pextra_ty (ty_path, Pcstr_ty cstr_name)) record_repr
    in
    let cstr =
      { cstr_name;
        cstr_res;
        cstr_existentials;
        cstr_args;
        cstr_arity = List.length cstr_args;
        cstr_tag;
        cstr_repr = rep;
        cstr_shape = cstr_shape;
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
  let cstr_tag = Extension path_ext in
  let existentials, cstr_args, cstr_inlined =
    constructor_args ~current_unit ext.ext_private ext.ext_args ext.ext_ret_type
      Path.(Pextra_ty (path_ext, Pext_ty))
      (Record_inlined (cstr_tag, ext.ext_shape, Variant_extensible))
  in
    { cstr_name = Path.last path_ext;
      cstr_res = ty_res;
      cstr_existentials = existentials;
      cstr_args;
      cstr_arity = List.length cstr_args;
      cstr_tag;
      cstr_repr = Variant_extensible;
      cstr_shape = ext.ext_shape;
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

let dummy_label (type rep) (record_form : rep record_form)
    : rep gen_label_description =
  let repres : rep = match record_form with
  | Legacy -> Record_unboxed
  | Unboxed_product -> Record_unboxed_product
  in
  { lbl_name = ""; lbl_res = none; lbl_arg = none;
    lbl_mut = Immutable; lbl_modalities = Mode.Modality.Value.Const.id;
    lbl_sort = Jkind.Sort.Const.void;
    lbl_pos = -1; lbl_all = [||];
    lbl_repres = repres;
    lbl_private = Public;
    lbl_loc = Location.none;
    lbl_attributes = [];
    lbl_uid = Uid.internal_not_actually_unique;
  }

let label_descrs record_form ty_res lbls repres priv =
  let all_labels = Array.make (List.length lbls) (dummy_label record_form) in
  let rec describe_labels pos = function
      [] -> []
    | l :: rest ->
        let lbl =
          { lbl_name = Ident.name l.ld_id;
            lbl_res = ty_res;
            lbl_arg = l.ld_type;
            lbl_mut = l.ld_mutable;
            lbl_modalities = l.ld_modalities;
            lbl_sort = l.ld_sort;
            lbl_pos = pos;
            lbl_all = all_labels;
            lbl_repres = repres;
            lbl_private = priv;
            lbl_loc = l.ld_loc;
            lbl_attributes = l.ld_attributes;
            lbl_uid = l.ld_uid;
          } in
        all_labels.(pos) <- lbl;
        (l.ld_id, lbl) :: describe_labels (pos+1) rest in
  describe_labels 0 lbls

exception Constr_not_found

let find_constr ~constant tag cstrs =
  try
    List.find
      (function
        | (({cstr_tag=Ordinary {runtime_tag=tag'}; cstr_constant},_),_) ->
          tag' = tag && cstr_constant = constant
        | (({cstr_tag=Null; cstr_constant}, _),_) ->
          tag = -1 && cstr_constant = constant
        | (({cstr_tag=Extension _},_),_) -> false)
      cstrs
  with
  | Not_found -> raise Constr_not_found

let find_constr_by_tag ~constant tag cstrlist =
  fst (fst (find_constr ~constant tag cstrlist))

let constructors_of_type ~current_unit ty_path decl =
  match decl.type_kind with
  | Type_variant (cstrs, rep, _) ->
     constructor_descrs ~current_unit ty_path decl cstrs rep
  | Type_record _ | Type_record_unboxed_product _ | Type_abstract _
  | Type_open -> []

let labels_of_type ty_path decl =
  match decl.type_kind with
  | Type_record(labels, rep, _) ->
      label_descrs Legacy (newgenconstr ty_path decl.type_params)
        labels rep decl.type_private
  | Type_record_unboxed_product _
  | Type_variant _ | Type_abstract _ | Type_open -> []

let unboxed_labels_of_type ty_path decl =
  match decl.type_kind with
  | Type_record_unboxed_product(labels, rep, _) ->
      label_descrs Unboxed_product (newgenconstr ty_path decl.type_params)
        labels rep decl.type_private
  | Type_record _
  | Type_variant _ | Type_abstract _ | Type_open -> []
