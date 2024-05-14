(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_iterator

let err = Syntaxerr.ill_formed_ast

let empty_record loc = err loc "Records cannot be empty."
let invalid_tuple loc = err loc "Tuples must have at least 2 components."
let unlabeled_labeled_tuple_typ loc =
  err loc "Labeled tuple types must have at least one labeled component."
let unlabeled_labeled_tuple_exp loc =
  err loc "Labeled tuples must have at least one labeled component."
let unlabeled_labeled_tuple_pat loc =
  err loc
    "Closed labeled tuple patterns must have at least one labeled component."
let empty_open_labeled_tuple_pat loc =
  err loc "Open labeled tuple patterns must have at least one component."
let short_closed_labeled_tuple_pat loc =
  err loc "Closed labeled tuple patterns must have at least 2 components."
let no_args loc = err loc "Function application with no argument."
let empty_let loc = err loc "Let with no bindings."
let empty_type loc = err loc "Type declarations cannot be empty."
let complex_id loc = err loc "Functor application not allowed here."
let module_type_substitution_missing_rhs loc =
  err loc "Module type substitution with no right hand side"
let empty_comprehension loc = err loc "Comprehension with no clauses"
let no_val_params loc = err loc "Functions must have a value parameter."

let non_jane_syntax_function loc =
  err loc "Functions must be constructed using Jane Street syntax."

let simple_longident id =
  let rec is_simple = function
    | Longident.Lident _ -> true
    | Longident.Ldot (id, _) -> is_simple id
    | Longident.Lapply _ -> false
  in
  if not (is_simple id.txt) then complex_id id.loc

let labeled_tuple_without_label lt =
  List.for_all (fun (lbl,_) -> Option.is_none lbl) lt

let iterator =
  let super = Ast_iterator.default_iterator in
  let type_declaration self td =
    super.type_declaration self td;
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_record [] -> empty_record loc
    | _ -> ()
  in
  let jtyp _self loc (jtyp : Jane_syntax.Core_type.t) =
    match jtyp with
    | Jtyp_layout (Ltyp_var _ | Ltyp_poly _ | Ltyp_alias _) -> ()
    | Jtyp_tuple ([] | [_]) -> invalid_tuple loc
    | Jtyp_tuple l ->
      if labeled_tuple_without_label l then unlabeled_labeled_tuple_typ loc
  in
  let typ self ty =
    super.typ self ty;
    let loc = ty.ptyp_loc in
    match Jane_syntax.Core_type.of_ast ty with
    | Some (jtyp_, _attrs) -> jtyp self ty.ptyp_loc jtyp_
    | None ->
    match ty.ptyp_desc with
    | Ptyp_tuple ([] | [_]) -> invalid_tuple loc
    | Ptyp_package (_, cstrs) ->
      List.iter (fun (id, _) -> simple_longident id) cstrs
    | _ -> ()
  in
  let jpat _self loc (jpat : Jane_syntax.Pattern.t) =
    match jpat with
    | Jpat_immutable_array (Iapat_immutable_array _)-> ()
    | Jpat_layout (Lpat_constant _) -> ()
    | Jpat_tuple lt -> begin
        match lt with
        | ([], Open) -> empty_open_labeled_tuple_pat loc
        | (([] | [_]), Closed) ->
          short_closed_labeled_tuple_pat loc
        | (l, Closed) ->
          if labeled_tuple_without_label l then unlabeled_labeled_tuple_pat loc
        | (_ :: _, Open) -> ()
      end
  in
  let pat self pat =
    begin match pat.ppat_desc with
    | Ppat_construct (_, Some (_, ({ppat_desc = Ppat_tuple _} as p)))
      when Builtin_attributes.explicit_arity pat.ppat_attributes ->
        super.pat self p (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.pat self pat
    end;
    let loc = pat.ppat_loc in
    match Jane_syntax.Pattern.of_ast pat with
    | Some (jpat_, _attrs) -> jpat self pat.ppat_loc jpat_
    | None ->
    match pat.ppat_desc with
    | Ppat_tuple ([] | [_]) -> invalid_tuple loc
    | Ppat_record ([], _) -> empty_record loc
    | Ppat_construct (id, _) -> simple_longident id
    | Ppat_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | _ -> ()
  in
  let n_ary_function loc (params, _constraint, body) =
    let open Jane_syntax.N_ary_functions in
    match body with
    | Pfunction_cases _ -> ()
    | Pfunction_body _ ->
        if
          not (
            List.exists
              (function
                | { pparam_desc = Pparam_val _ } -> true
                | { pparam_desc = Pparam_newtype _ } -> false)
              params)
        then no_val_params loc
  in
  let jexpr _self loc (jexp : Jane_syntax.Expression.t) =
    match jexp with
    | Jexp_n_ary_function n_ary -> n_ary_function loc n_ary
    | Jexp_comprehension
        ( Cexp_list_comprehension {clauses = []; body = _}
        | Cexp_array_comprehension (_, {clauses = []; body = _}) )
      ->
        empty_comprehension loc
    | Jexp_tuple lt -> begin
        match lt with
        | [] | [_] -> invalid_tuple loc
        | l ->
          if labeled_tuple_without_label l then unlabeled_labeled_tuple_exp loc
      end
    | Jexp_comprehension _
    | Jexp_immutable_array _
    | Jexp_layout _
    | Jexp_modes _
      -> ()
  in
  let expr self exp =
    begin match exp.pexp_desc with
    | Pexp_construct (_, Some ({pexp_desc = Pexp_tuple _} as e))
      when Builtin_attributes.explicit_arity exp.pexp_attributes ->
        super.expr self e (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.expr self exp
    end;
    let loc = exp.pexp_loc in
    match Jane_syntax.Expression.of_ast exp with
    | Some (jexp, _attrs) -> jexpr self exp.pexp_loc jexp
    | None ->
    match exp.pexp_desc with
    | Pexp_tuple ([] | [_]) -> invalid_tuple loc
    | Pexp_record ([], _) -> empty_record loc
    | Pexp_apply (_, []) -> no_args loc
    | Pexp_let (_, [], _) -> empty_let loc
    | Pexp_ident id
    | Pexp_construct (id, _)
    | Pexp_field (_, id)
    | Pexp_setfield (_, id, _)
    | Pexp_new id -> simple_longident id
    | Pexp_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | Pexp_fun _ | Pexp_function _ -> non_jane_syntax_function loc
    | _ -> ()
  in
  let extension_constructor self ec =
    super.extension_constructor self ec;
    match ec.pext_kind with
    | Pext_rebind id -> simple_longident id
    | _ -> ()
  in
  let class_expr self ce =
    super.class_expr self ce;
    let loc = ce.pcl_loc in
    match ce.pcl_desc with
    | Pcl_apply (_, []) -> no_args loc
    | Pcl_constr (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_type self mty =
    super.module_type self mty;
    match mty.pmty_desc with
    | Pmty_alias id -> simple_longident id
    | _ -> ()
  in
  let open_description self opn =
    super.open_description self opn
  in
  let with_constraint self wc =
    super.with_constraint self wc;
    match wc with
    | Pwith_type (id, _)
    | Pwith_module (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_expr self me =
    super.module_expr self me;
    match me.pmod_desc with
    | Pmod_ident id -> simple_longident id
    | _ -> ()
  in
  let structure_item self st =
    super.structure_item self st;
    let loc = st.pstr_loc in
    match st.pstr_desc with
    | Pstr_type (_, []) -> empty_type loc
    | Pstr_value (_, []) -> empty_let loc
    | _ -> ()
  in
  let signature_item self sg =
    super.signature_item self sg;
    let loc = sg.psig_loc in
    match sg.psig_desc with
    | Psig_type (_, []) -> empty_type loc
    | Psig_modtypesubst {pmtd_type=None; _ } ->
        module_type_substitution_missing_rhs loc
    | _ -> ()
  in
  let row_field self field =
    super.row_field self field;
    let loc = field.prf_loc in
    match field.prf_desc with
    | Rtag _ -> ()
    | Rinherit _ ->
      if field.prf_attributes = []
      then ()
      else err loc
          "In variant types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let object_field self field =
    super.object_field self field;
    let loc = field.pof_loc in
    match field.pof_desc with
    | Otag _ -> ()
    | Oinherit _ ->
      if field.pof_attributes = []
      then ()
      else err loc
          "In object types, attaching attributes to inherited \
           subtypes is not allowed."
  in
  let attribute self attr =
    (* The change to `self` here avoids registering attributes within attributes
       for the purposes of warning 53, while keeping all the other invariant
       checks for attribute payloads.  See comment on [attr_tracking_time] in
       [builtin_attributes.mli]. *)
    super.attribute { self with attribute = super.attribute } attr;
    Builtin_attributes.(register_attr Invariant_check attr.attr_name)
  in
  { super with
    type_declaration
  ; typ
  ; pat
  ; expr
  ; extension_constructor
  ; class_expr
  ; module_expr
  ; module_type
  ; open_description
  ; with_constraint
  ; structure_item
  ; signature_item
  ; row_field
  ; object_field
  ; attribute
  }

let structure st = iterator.structure iterator st
let signature sg = iterator.signature iterator sg
