(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)

open Parsetree
open Ast_helper
open Location

module String = Misc.Stdlib.String

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  modes : mapper -> modes -> modes;
  modalities : mapper -> modalities -> modalities;
  binding_op: mapper -> binding_op -> binding_op;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constant: mapper -> constant -> constant;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  jkind_annotation:
    mapper -> Jane_syntax.Jkind.t -> Jane_syntax.Jkind.t;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_substitution: mapper -> module_substitution -> module_substitution;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_declaration: mapper -> open_declaration -> open_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_extension: mapper -> type_extension -> type_extension;
  type_exception: mapper -> type_exception -> type_exception;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;

  expr_jane_syntax:
    mapper -> Jane_syntax.Expression.t -> Jane_syntax.Expression.t;
  extension_constructor_jane_syntax:
    mapper ->
    Jane_syntax.Extension_constructor.t -> Jane_syntax.Extension_constructor.t;
  module_type_jane_syntax: mapper
    -> Jane_syntax.Module_type.t -> Jane_syntax.Module_type.t;
  pat_jane_syntax: mapper -> Jane_syntax.Pattern.t -> Jane_syntax.Pattern.t;
  signature_item_jane_syntax: mapper ->
    Jane_syntax.Signature_item.t -> Jane_syntax.Signature_item.t;
  structure_item_jane_syntax: mapper ->
    Jane_syntax.Structure_item.t -> Jane_syntax.Structure_item.t;
  typ_jane_syntax: mapper -> Jane_syntax.Core_type.t -> Jane_syntax.Core_type.t;
}

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}
let map_loc_txt sub f {loc; txt} =
  {loc = sub.location sub loc; txt = f sub txt}

module C = struct
  (* Constants *)

  let map sub c = match c with
    | Pconst_integer _
    | Pconst_char _
    | Pconst_float _
      -> c
    | Pconst_string (s, loc, quotation_delimiter) ->
        let loc = sub.location sub loc in
        Const.string ~loc ?quotation_delimiter s
end

module T = struct
  (* Type expressions for the core language *)

  module LT = Jane_syntax.Labeled_tuples

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    let loc = sub.location sub prf_loc in
    let attrs = sub.attributes sub prf_attributes in
    let desc = match prf_desc with
      | Rtag (l, b, tl) -> Rtag (map_loc sub l, b, List.map (sub.typ sub) tl)
      | Rinherit t -> Rinherit (sub.typ sub t)
    in
    Rf.mk ~loc ~attrs desc

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    let loc = sub.location sub pof_loc in
    let attrs = sub.attributes sub pof_attributes in
    let desc = match pof_desc with
      | Otag (l, t) -> Otag (map_loc sub l, sub.typ sub t)
      | Oinherit t -> Oinherit (sub.typ sub t)
    in
    Of.mk ~loc ~attrs desc

  let var_jkind sub (name, jkind_opt) =
    let name = map_loc sub name in
    let jkind_opt =
      map_opt (map_loc_txt sub sub.jkind_annotation) jkind_opt
    in
    (name, jkind_opt)

  let map_bound_vars sub bound_vars = List.map (var_jkind sub) bound_vars

  let map_jst_layouts sub :
        Jane_syntax.Layouts.core_type -> Jane_syntax.Layouts.core_type =
    function
    | Ltyp_var { name; jkind } ->
      let jkind = map_loc_txt sub sub.jkind_annotation jkind in
      Ltyp_var { name; jkind }
    | Ltyp_poly { bound_vars; inner_type } ->
      let bound_vars = map_bound_vars sub bound_vars in
      let inner_type = sub.typ sub inner_type in
      Ltyp_poly { bound_vars; inner_type }
    | Ltyp_alias { aliased_type; name; jkind } ->
      let aliased_type = sub.typ sub aliased_type in
      let jkind = map_loc_txt sub sub.jkind_annotation jkind in
      Ltyp_alias { aliased_type; name; jkind }

  let map_jst_labeled_tuple sub : LT.core_type -> LT.core_type = function
    (* CR labeled tuples: Eventually mappers may want to see the labels. *)
    | tl -> List.map (map_snd (sub.typ sub)) tl

  let map_jst sub : Jane_syntax.Core_type.t -> Jane_syntax.Core_type.t =
    function
    | Jtyp_layout typ -> Jtyp_layout (map_jst_layouts sub typ)
    | Jtyp_tuple x -> Jtyp_tuple (map_jst_labeled_tuple sub x)

  let map sub ({ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs}
                 as typ) =
    let open Typ in
    let loc = sub.location sub loc in
    match Jane_syntax.Core_type.of_ast typ with
    | Some (jtyp, attrs) -> begin
        let attrs = sub.attributes sub attrs in
        let jtyp = sub.typ_jane_syntax sub jtyp in
        Jane_syntax.Core_type.core_type_of jtyp ~loc ~attrs
    end
    | None ->
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2, m1, m2) ->
        arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2) (sub.modes sub m1) (sub.modes sub m2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
        object_ ~loc ~attrs (List.map (object_field sub) l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs
                             (List.map (map_loc sub) sl) (sub.typ sub t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
          (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_type_declaration sub
     ({ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} as tyd) =
    let loc = sub.location sub ptype_loc in
    let jkind, ptype_attributes =
      match Jane_syntax.Layouts.of_type_declaration tyd with
      | None -> None, ptype_attributes
      | Some (jkind, attributes) ->
          let jkind = map_loc_txt sub sub.jkind_annotation jkind in
          Some jkind, attributes
    in
    let attrs = sub.attributes sub ptype_attributes in
    Jane_syntax.Layouts.type_declaration_of ~loc ~attrs (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ~manifest:(map_opt (sub.typ sub) ptype_manifest)
      ~jkind
      ~docs:Docstrings.empty_docs
      ~text:None

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_constructor_argument sub x =
    let pca_type = sub.typ sub x.pca_type in
    let pca_loc = sub.location sub x.pca_loc in
    let pca_modalities = sub.modalities sub x.pca_modalities in
    { pca_type; pca_loc; pca_modalities }

  let map_constructor_arguments sub = function
    | Pcstr_tuple l -> Pcstr_tuple (List.map (map_constructor_argument sub) l)
    | Pcstr_record l ->
        Pcstr_record (List.map (sub.label_declaration sub) l)

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_loc;
       ptyext_attributes} =
    let loc = sub.location sub ptyext_loc in
    let attrs = sub.attributes sub ptyext_attributes in
    Te.mk ~loc ~attrs
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private

  let map_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    let loc = sub.location sub ptyexn_loc in
    let attrs = sub.attributes sub ptyexn_attributes in
    Te.mk_exception ~loc ~attrs
      (sub.extension_constructor sub ptyexn_constructor)

  let map_extension_constructor_jst sub :
    Jane_syntax.Extension_constructor.t -> Jane_syntax.Extension_constructor.t =
    function
    | Jext_layout (Lext_decl(vars, args, res)) ->
      let vars = map_bound_vars sub vars in
      let args = map_constructor_arguments sub args in
      let res = map_opt (sub.typ sub) res in
      Jext_layout (Lext_decl(vars, args, res))

  let map_extension_constructor_kind sub = function
      Pext_decl(vars, ctl, cto) ->
        Pext_decl(List.map (map_loc sub) vars,
                  map_constructor_arguments sub ctl,
                  map_opt (sub.typ sub) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
     ({pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} as ext) =
    let loc = sub.location sub pext_loc in
    let name = map_loc sub pext_name in
    match Jane_syntax.Extension_constructor.of_ast ext with
    | Some (jext, attrs) ->
      let attrs = sub.attributes sub attrs in
      let jext = sub.extension_constructor_jane_syntax sub jext in
      Jane_syntax.Extension_constructor.extension_constructor_of
        ~loc ~name ~attrs jext
    | None ->
    let attrs = sub.attributes sub pext_attributes in
    Te.constructor ~loc ~attrs
      name
      (map_extension_constructor_kind sub pext_kind)

end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcty_open (o, ct) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_type sub ct)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
    | Pctf_val (s, m, v, t) ->
        val_ ~loc ~attrs (map_loc sub s) m v (sub.typ sub t)
    | Pctf_method (s, p, v, t) ->
        method_ ~loc ~attrs (map_loc sub s) p v (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub.typ sub pcsig_self)
      (List.map (sub.class_type_field sub) pcsig_fields)
end

let map_functor_param sub = function
  | Unit -> Unit
  | Named (s, mt) -> Named (map_loc sub s, sub.module_type sub mt)

module MT = struct
  (* Type expressions for the module language *)

  let map sub
        ({pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} as mty) =
    let open Mty in
    let loc = sub.location sub loc in
    match Jane_syntax.Module_type.of_ast mty with
    | Some (jmty, attrs) -> begin
        let attrs = sub.attributes sub attrs in
        Jane_syntax.Module_type.mty_of ~loc ~attrs
          (sub.module_type_jane_syntax sub jmty)
      end
    | None ->
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (param, mt) ->
        functor_ ~loc ~attrs
          (map_functor_param sub param)
          (sub.module_type sub mt)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub.module_type sub mt)
          (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_modtype (lid, mty) ->
        Pwith_modtype (map_loc sub lid, sub.module_type sub mty)
    | Pwith_typesubst (lid, d) ->
        Pwith_typesubst (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)
    | Pwith_modtypesubst (lid, mty) ->
        Pwith_modtypesubst (map_loc sub lid, sub.module_type sub mty)

  module L = Jane_syntax.Layouts

  let map_sig_layout sub : L.signature_item -> L.signature_item =
    function
    | Lsig_kind_abbrev (name, jkind) ->
        Lsig_kind_abbrev (
          map_loc sub name,
          map_loc_txt sub sub.jkind_annotation jkind
        )

  let map_signature_item_jst sub :
    Jane_syntax.Signature_item.t -> Jane_syntax.Signature_item.t =
    function
    | Jsig_layout sigi ->
        Jsig_layout (map_sig_layout sub sigi)

  let map_signature_item sub ({psig_desc = desc; psig_loc = loc} as sigi) =
    let open Sig in
    let loc = sub.location sub loc in
    match Jane_syntax.Signature_item.of_ast sigi with
    | Some jsigi -> begin
        match sub.signature_item_jane_syntax sub jsigi with
        | Jsig_layout sigi ->
            Jane_syntax.Layouts.sig_item_of ~loc sigi
    end
    | None ->
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type (rf, l) ->
        type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Psig_typesubst l ->
        type_subst ~loc (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_modsubst x -> mod_subst ~loc (sub.module_substitution sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_modtypesubst x ->
        modtype_subst ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include (x, moda) ->
        include_ ~loc ~modalities:(sub.modalities sub moda)
          (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)

  let map_jane_syntax sub :
        Jane_syntax.Module_type.t -> Jane_syntax.Module_type.t = function
    | Jmty_strengthen { mty; mod_id } ->
       let mty = sub.module_type sub mty in
       let mod_id = map_loc sub mod_id in
       Jmty_strengthen { mty; mod_id }
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (param, body) ->
        functor_ ~loc ~attrs
          (map_functor_param sub param)
          (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_apply_unit m1 ->
        apply_unit ~loc ~attrs (sub.module_expr sub m1)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub.module_expr sub m)
                    (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

  module L = Jane_syntax.Layouts

  let map_str_layout sub : L.structure_item -> L.structure_item =
    function
    | Lstr_kind_abbrev (name, jkind) ->
        Lstr_kind_abbrev (
          map_loc sub name,
          map_loc_txt sub sub.jkind_annotation jkind
        )

  let map_structure_item_jst sub :
    Jane_syntax.Structure_item.t -> Jane_syntax.Structure_item.t =
    function
    | Jstr_layout stri ->
        Jstr_layout (map_str_layout sub stri)

  let map_structure_item sub ({pstr_loc = loc; pstr_desc = desc} as stri) =
    let open Str in
    let loc = sub.location sub loc in
    match Jane_syntax.Structure_item.of_ast stri with
    | Some jstri -> begin
        match sub.structure_item_jane_syntax sub jstri with
        | Jstr_layout stri ->
            Jane_syntax.Layouts.str_item_of ~loc stri
    end
    | None ->
    match desc with
    | Pstr_eval (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        eval ~loc ~attrs (sub.expr sub x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.type_exception sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_declaration sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        let attrs = sub.attributes sub attrs in
        extension ~loc ~attrs (sub.extension sub x)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  module C = Jane_syntax.Comprehensions
  module IA = Jane_syntax.Immutable_arrays
  module L = Jane_syntax.Layouts
  module LT = Jane_syntax.Labeled_tuples

  let map_function_param sub { pparam_loc = loc; pparam_desc = desc } =
    let loc = sub.location sub loc in
    let desc =
      match desc with
      | Pparam_val (label, def, pat) ->
          Pparam_val (label, Option.map (sub.expr sub) def, sub.pat sub pat)
      | Pparam_newtype (newtype, jkind) ->
          Pparam_newtype
            ( map_loc sub newtype
            , map_opt (map_loc_txt sub sub.jkind_annotation) jkind
            )
    in
    { pparam_loc = loc; pparam_desc = desc }

  let map_function_body sub body =
    match body with
    | Pfunction_body exp -> Pfunction_body (sub.expr sub exp)
    | Pfunction_cases (cases, loc, attrs) ->
        Pfunction_cases
          (sub.cases sub cases, sub.location sub loc, sub.attributes sub attrs)

  let map_type_constraint sub constraint_ =
    match constraint_ with
    | Pconstraint ty -> Pconstraint (sub.typ sub ty)
    | Pcoerce (ty1, ty2) ->
        Pcoerce (Option.map (sub.typ sub) ty1, sub.typ sub ty2)

  let map_function_constraint sub { mode_annotations; type_constraint } =
    { mode_annotations = sub.modes sub mode_annotations;
      type_constraint = map_type_constraint sub type_constraint;
    }

  let map_iterator sub : C.iterator -> C.iterator = function
    | Range { start; stop; direction } ->
      Range { start = sub.expr sub start;
              stop = sub.expr sub stop;
              direction }
    | In expr -> In (sub.expr sub expr)

  let map_clause_binding sub : C.clause_binding -> C.clause_binding = function
    | { pattern; iterator; attributes } ->
      { pattern = sub.pat sub pattern;
        iterator = map_iterator sub iterator;
        attributes = sub.attributes sub attributes }

  let map_clause sub : C.clause -> C.clause = function
    | For cbs -> For (List.map (map_clause_binding sub) cbs)
    | When expr -> When (sub.expr sub expr)

  let map_comp sub : C.comprehension -> C.comprehension = function
    | { body; clauses } -> { body = sub.expr sub body;
                            clauses = List.map (map_clause sub) clauses }

  let map_cexp sub : C.expression -> C.expression = function
    | Cexp_list_comprehension comp ->
      Cexp_list_comprehension (map_comp sub comp)
    | Cexp_array_comprehension (mut, comp) ->
      Cexp_array_comprehension (mut, map_comp sub comp)

  let map_iaexp sub : IA.expression -> IA.expression = function
    | Iaexp_immutable_array elts ->
      Iaexp_immutable_array (List.map (sub.expr sub) elts)

  let map_unboxed_constant_exp _sub : L.constant -> L.constant = function
    (* We can't reasonably call [sub.constant] because it might return a kind
       of constant we don't know how to unbox.
    *)
    | (Float _ | Integer _) as x -> x

  let map_layout_exp sub : L.expression -> L.expression = function
    | Lexp_constant x -> Lexp_constant (map_unboxed_constant_exp sub x)
    | Lexp_newtype (str, jkind, inner_expr) ->
      let str = map_loc sub str in
      let jkind = map_loc_txt sub sub.jkind_annotation jkind in
      let inner_expr = sub.expr sub inner_expr in
      Lexp_newtype (str, jkind, inner_expr)

  let map_ltexp sub : LT.expression -> LT.expression = function
    (* CR labeled tuples: Eventually mappers may want to see the labels. *)
    | el -> List.map (map_snd (sub.expr sub)) el

  let map_jst sub : Jane_syntax.Expression.t -> Jane_syntax.Expression.t =
    function
    | Jexp_comprehension x -> Jexp_comprehension (map_cexp sub x)
    | Jexp_immutable_array x -> Jexp_immutable_array (map_iaexp sub x)
    | Jexp_layout x -> Jexp_layout (map_layout_exp sub x)
    | Jexp_tuple ltexp -> Jexp_tuple (map_ltexp sub ltexp)

  let map sub
        ({pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} as exp) =
    let open Exp in
    let loc = sub.location sub loc in
    match Jane_syntax.Expression.of_ast exp with
    | Some (jexp, attrs) -> begin
        let attrs = sub.attributes sub attrs in
        Jane_syntax.Expression.expr_of ~loc ~attrs
          (sub.expr_jane_syntax sub jexp)
    end
    | None ->
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs (sub.constant sub x)
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.expr sub e)
    | Pexp_function (ps, c, b) ->
      function_ ~loc ~attrs
        (List.map (map_function_param sub) ps)
        (map_opt (map_function_constraint sub) c)
        (map_function_body sub b)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
        match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
          (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
        field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
          (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
          (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
          (sub.typ sub t2)
    | Pexp_constraint (e, t, m) ->
        constraint_ ~loc ~attrs (sub.expr sub e) (Option.map (sub.typ sub) t) (sub.modes sub m)
    | Pexp_send (e, s) ->
        send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
          (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
        letexception ~loc ~attrs
          (sub.extension_constructor sub cd)
          (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
    | Pexp_newtype (s, e) ->
        newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (o, e) ->
        open_ ~loc ~attrs (sub.open_declaration sub o) (sub.expr sub e)
    | Pexp_letop {let_; ands; body} ->
        letop ~loc ~attrs (sub.binding_op sub let_)
          (List.map (sub.binding_op sub) ands) (sub.expr sub body)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pexp_unreachable -> unreachable ~loc ~attrs ()
    | Pexp_stack e -> stack ~loc ~attrs (sub.expr sub e)

  let map_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    let open Exp in
    let op = map_loc sub pbop_op in
    let pat = sub.pat sub pbop_pat in
    let exp = sub.expr sub pbop_exp in
    let loc = sub.location sub pbop_loc in
    binding_op op pat exp loc

end

module P = struct
  (* Patterns *)

  module IA = Jane_syntax.Immutable_arrays
  module L = Jane_syntax.Layouts
  module LT = Jane_syntax.Labeled_tuples

  let map_iapat sub : IA.pattern -> IA.pattern = function
    | Iapat_immutable_array elts ->
      Iapat_immutable_array (List.map (sub.pat sub) elts)

  let map_unboxed_constant_pat _sub : L.constant -> L.constant = function
    (* We can't reasonably call [sub.constant] because it might return a kind
       of constant we don't know how to unbox.
    *)
    | Float _ | Integer _ as x -> x

  let map_ltpat sub : LT.pattern -> LT.pattern = function
    (* CR labeled tuples: Eventually mappers may want to see the labels. *)
    | (pl, closed) ->
      (List.map (map_snd (sub.pat sub)) pl, closed)

  let map_jst sub : Jane_syntax.Pattern.t -> Jane_syntax.Pattern.t = function
    | Jpat_immutable_array x -> Jpat_immutable_array (map_iapat sub x)
    | Jpat_layout (Lpat_constant x) ->
        Jpat_layout (Lpat_constant (map_unboxed_constant_pat sub x))
    | Jpat_tuple ltpat -> Jpat_tuple (map_ltpat sub ltpat)

  let map sub
        ({ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} as pat) =
    let open Pat in
    let loc = sub.location sub loc in
    match Jane_syntax.Pattern.of_ast pat with
    | Some (jpat, attrs) -> begin
        let attrs = sub.attributes sub attrs in
        Jane_syntax.Pattern.pat_of ~loc ~attrs (sub.pat_jane_syntax sub jpat)
    end
    | None ->
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs (sub.constant sub c)
    | Ppat_interval (c1, c2) ->
        interval ~loc ~attrs (sub.constant sub c1) (sub.constant sub c2)
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l)
          (map_opt
             (fun (vl, p) -> List.map (map_loc sub) vl, sub.pat sub p)
             p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs
               (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t, m) ->
        constraint_ ~loc ~attrs (sub.pat sub p) (Option.map (sub.typ sub) t) (sub.modes sub m)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        (fun_ ~loc ~attrs lab
          (map_opt (sub.expr sub) e)
          (sub.pat sub p)
          (sub.class_expr sub ce) [@alert "-prefer_jane_syntax"])
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub.class_expr sub ce)
          (List.map (map_snd (sub.expr sub)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.class_expr sub ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pcl_open (o, ce) ->
        open_ ~loc ~attrs (sub.open_description sub o) (sub.class_expr sub ce)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
    | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcf_inherit (o, ce, s) ->
        inherit_ ~loc ~attrs o (sub.class_expr sub ce)
          (map_opt (map_loc sub) s)
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = sub.pat sub pcstr_self;
      pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    let loc = sub.location sub pci_loc in
    let attrs = sub.attributes sub pci_attributes in
    Ci.mk ~loc ~attrs
     ~virt:pci_virt
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    constant = C.map;
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    type_exception = T.map_type_exception;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_modalities; pval_prim; pval_loc;
                 pval_attributes} ->
        Val.mk
          (map_loc this pval_name)
          (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~modalities:(this.modalities this pval_modalities)
          ~prim:pval_prim
      );

    pat = P.map;
    expr = E.map;
    binding_op = E.map_binding_op;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (this.module_type this pmd_type)
           ~attrs:(this.attributes this pmd_attributes)
           ~loc:(this.location this pmd_loc)
      );

    module_substitution =
      (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
         Ms.mk
           (map_loc this pms_name)
           (map_loc this pms_manifest)
           ~attrs:(this.attributes this pms_attributes)
           ~loc:(this.location this pms_loc)
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         Mtd.mk
           (map_loc this pmtd_name)
           ?typ:(map_opt (this.module_type this) pmtd_type)
           ~attrs:(this.attributes this pmtd_attributes)
           ~loc:(this.location this pmtd_loc)
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
           ~attrs:(this.attributes this pmb_attributes)
           ~loc:(this.location this pmb_loc)
      );


    open_declaration =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (this.module_expr this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    open_description =
      (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_expr)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );

    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc; pincl_kind} ->
         Incl.mk ~kind:pincl_kind (this.module_type this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc; pincl_kind} ->
         Incl.mk ~kind:pincl_kind (this.module_expr this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_constraint; pvb_modes; pvb_attributes; pvb_loc} ->
         let map_ct (ct:Parsetree.value_constraint) = match ct with
           | Pvc_constraint {locally_abstract_univars=vars; typ} ->
               Pvc_constraint
                 { locally_abstract_univars = List.map (map_loc this) vars;
                   typ = this.typ this typ
                 }
           | Pvc_coercion { ground; coercion } ->
               Pvc_coercion {
                 ground = Option.map (this.typ this) ground;
                 coercion = this.typ this coercion
               }
         in
         Vb.mk
           (this.pat this pvb_pat)
           (this.expr this pvb_expr)
           ?value_constraint:(Option.map map_ct pvb_constraint)
           ~loc:(this.location this pvb_loc)
           ~modes:(this.modes this pvb_modes)
           ~attrs:(this.attributes this pvb_attributes)
      );


    constructor_declaration =
      (fun this ({pcd_name; pcd_vars; pcd_args;
                  pcd_res; pcd_loc; pcd_attributes} as pcd) ->
        let name = map_loc this pcd_name in
        let args = T.map_constructor_arguments this pcd_args in
        let res = map_opt (this.typ this) pcd_res in
        let loc = this.location this pcd_loc in
        match Jane_syntax.Layouts.of_constructor_declaration pcd with
        | None ->
          let vars = List.map (map_loc this) pcd_vars in
          let attrs = this.attributes this pcd_attributes in
          Type.constructor name ~vars ~args ?res ~loc ~attrs
        | Some (vars_jkinds, attributes) ->
          let vars_jkinds = List.map (T.var_jkind this) vars_jkinds in
          let attrs = this.attributes this attributes in
          Jane_syntax.Layouts.constructor_declaration_of
            name ~vars_jkinds ~args ~res ~loc ~attrs
            ~info:Docstrings.empty_info
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_modalities; pld_attributes} ->
         Type.field
           (map_loc this pld_name)
           (this.typ this pld_type)
           ~mut:pld_mutable
           ~modalities:(this.modalities this pld_modalities)
           ~loc:(this.location this pld_loc)
           ~attrs:(this.attributes this pld_attributes)
      );

    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         {
           pc_lhs = this.pat this pc_lhs;
           pc_guard = map_opt (this.expr this) pc_guard;
           pc_rhs = this.expr this pc_rhs;
         }
      );



    location = (fun _this l -> l);

    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this a ->
      {
        attr_name = map_loc this a.attr_name;
        attr_payload = this.payload this a.attr_payload;
        attr_loc = this.location this a.attr_loc
      }
    );
    attributes = (fun this l -> List.map (this.attribute this) l);

    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PSig x -> PSig (this.signature this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );

    jkind_annotation = (fun this ->
      let open Jane_syntax in
      function
      | Default -> Default
      | Abbreviation s ->
        let {txt; loc} =
          map_loc this s
        in
        Abbreviation (Jkind.Const.mk txt loc)
      | Mod (t, mode_list) ->
        Mod (this.jkind_annotation this t, this.modes this mode_list)
      | With (t, ty) ->
        With (this.jkind_annotation this t, this.typ this ty)
      | Kind_of ty -> Kind_of (this.typ this ty));

    expr_jane_syntax = E.map_jst;
    extension_constructor_jane_syntax = T.map_extension_constructor_jst;
    module_type_jane_syntax = MT.map_jane_syntax;
    pat_jane_syntax = P.map_jst;
    signature_item_jane_syntax = MT.map_signature_item_jst;
    structure_item_jane_syntax = M.map_structure_item_jst;
    typ_jane_syntax = T.map_jst;

    modes = (fun this m ->
      List.map (map_loc this) m);

    modalities = (fun this m ->
      List.map (map_loc this) m);
  }

let extension_of_error {kind; main; sub} =
  if kind <> Location.Report_error then
    raise (Invalid_argument "extension_of_error: expected kind Report_error");
  let str_of_pp pp_msg = Format.asprintf "%t" pp_msg in
  let extension_of_sub sub =
    { loc = sub.loc; txt = "ocaml.error" },
    PStr ([Str.eval (Exp.constant
                       (Pconst_string (str_of_pp sub.txt, sub.loc, None)))])
  in
  { loc = main.loc; txt = "ocaml.error" },
  PStr (Str.eval (Exp.constant
                    (Pconst_string (str_of_pp main.txt, main.loc, None))) ::
        List.map (fun msg -> Str.extension (extension_of_sub msg)) sub)

let attribute_of_warning loc s =
  Attr.mk
    {loc; txt = "ocaml.ppwarning" }
    (PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, loc, None)))]))

let cookies = ref String.Map.empty

let get_cookie k =
  try Some (String.Map.find k !cookies)
  with Not_found -> None

let set_cookie k v =
  cookies := String.Map.add k v !cookies

let tool_name_ref = ref "_none_"

let tool_name () = !tool_name_ref


module PpxContext = struct
  open Longident
  open Asttypes
  open Ast_helper

  let lid name = { txt = Lident name; loc = Location.none }

  let make_string s = Exp.constant (Const.string s)

  let make_bool x =
    if x
    then Exp.construct (lid "true") None
    else Exp.construct (lid "false") None

  let rec make_list f lst =
    match lst with
    | x :: rest ->
      Exp.construct (lid "::") (Some (Exp.tuple [f x; make_list f rest]))
    | [] ->
      Exp.construct (lid "[]") None

  let make_pair f1 f2 (x1, x2) =
    Exp.tuple [f1 x1; f2 x2]

  let make_option f opt =
    match opt with
    | Some x -> Exp.construct (lid "Some") (Some (f x))
    | None   -> Exp.construct (lid "None") None

  let get_cookies () =
    lid "cookies",
    make_list (make_pair make_string (fun x -> x))
      (String.Map.bindings !cookies)

  let mk fields =
    {
      attr_name = { txt = "ocaml.ppx.context"; loc = Location.none };
      attr_payload = Parsetree.PStr [Str.eval (Exp.record fields None)];
      attr_loc = Location.none
    }

  let make ~tool_name () =
    let Load_path.{ visible; hidden } = Load_path.get_paths () in
    let fields =
      [
        lid "tool_name",    make_string tool_name;
        lid "include_dirs", make_list make_string (!Clflags.include_dirs);
        lid "hidden_include_dirs",
          make_list make_string (!Clflags.hidden_include_dirs);
        lid "load_path",
          make_pair (make_list make_string) (make_list make_string)
            (visible, hidden);
        lid "open_modules", make_list make_string !Clflags.open_modules;
        lid "for_package",  make_option make_string !Clflags.for_package;
        lid "debug",        make_bool !Clflags.debug;
        lid "use_threads",  make_bool !Clflags.use_threads;
        lid "use_vmthreads", make_bool false;
        lid "recursive_types", make_bool !Clflags.recursive_types;
        lid "principal", make_bool !Clflags.principal;
        lid "transparent_modules", make_bool !Clflags.transparent_modules;
        lid "unboxed_types", make_bool !Clflags.unboxed_types;
        lid "unsafe_string", make_bool false; (* kept for compatibility *)
        get_cookies ()
      ]
    in
    mk fields

  let get_fields = function
    | PStr [{pstr_desc = Pstr_eval
                 ({ pexp_desc = Pexp_record (fields, None) }, [])}] ->
        fields
    | _ ->
        raise_errorf "Internal error: invalid [@@@ocaml.ppx.context] syntax"

  let restore fields =
    let field name payload =
      let rec get_string = function
        | { pexp_desc = Pexp_constant (Pconst_string (str, _, None)) } -> str
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] string syntax" name
      and get_bool pexp =
        match pexp with
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "true"},
                                       None)} ->
            true
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "false"},
                                       None)} ->
            false
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] bool syntax" name
      and get_list elem = function
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "::"},
                             Some {pexp_desc = Pexp_tuple [exp; rest]}) } ->
            elem exp :: get_list elem rest
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "[]"}, None)} ->
            []
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] list syntax" name
      and get_pair f1 f2 = function
        | {pexp_desc = Pexp_tuple [e1; e2]} ->
            (f1 e1, f2 e2)
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] pair syntax" name
      and get_option elem = function
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "Some" }, Some exp) } ->
            Some (elem exp)
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "None" }, None) } ->
            None
        | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                             { %s }] option syntax" name
      in
      match name with
      | "tool_name" ->
          tool_name_ref := get_string payload
      | "include_dirs" ->
          Clflags.include_dirs := get_list get_string payload
      | "hidden_include_dirs" ->
          Clflags.hidden_include_dirs := get_list get_string payload
      | "load_path" ->
          (* Duplicates Compmisc.auto_include, since we can't reference Compmisc
             from this module. *)
          let auto_include find_in_dir fn =
            if !Clflags.no_auto_include_otherlibs || !Clflags.no_std_include then
              raise Not_found
            else
              let alert = Location.auto_include_alert in
              Load_path.auto_include_otherlibs alert find_in_dir fn
          in
          let visible, hidden =
            get_pair (get_list get_string) (get_list get_string) payload
          in
          Load_path.init ~auto_include ~visible ~hidden
      | "open_modules" ->
          Clflags.open_modules := get_list get_string payload
      | "for_package" ->
          Clflags.for_package := get_option get_string payload
      | "debug" ->
          Clflags.debug := get_bool payload
      | "use_threads" ->
          Clflags.use_threads := get_bool payload
      | "use_vmthreads" ->
          if get_bool payload then
            raise_errorf "Internal error: vmthreads not supported after 4.09.0"
      | "recursive_types" ->
          Clflags.recursive_types := get_bool payload
      | "principal" ->
          Clflags.principal := get_bool payload
      | "transparent_modules" ->
          Clflags.transparent_modules := get_bool payload
      | "unboxed_types" ->
          Clflags.unboxed_types := get_bool payload
      | "cookies" ->
          let l = get_list (get_pair get_string (fun x -> x)) payload in
          cookies :=
            List.fold_left
              (fun s (k, v) -> String.Map.add k v s) String.Map.empty
              l
      | _ ->
          ()
    in
    List.iter (function ({txt=Lident name}, x) -> field name x | _ -> ()) fields

  let update_cookies fields =
    let fields =
      List.filter
        (function ({txt=Lident "cookies"}, _) -> false | _ -> true)
        fields
    in
    fields @ [get_cookies ()]
end

let ppx_context = PpxContext.make

let extension_of_exn exn =
  match error_of_exn exn with
  | Some (`Ok error) -> extension_of_error error
  | Some `Already_displayed ->
      { loc = Location.none; txt = "ocaml.error" }, PStr []
  | None -> raise exn


let apply_lazy ~source ~target mapper =
  let implem ast =
    let fields, ast =
      match ast with
      | {pstr_desc = Pstr_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                      attr_payload = x})} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.structure mapper ast
      with exn ->
        [{pstr_desc = Pstr_extension (extension_of_exn exn, []);
          pstr_loc  = Location.none}]
    in
    let fields = PpxContext.update_cookies fields in
    Str.attribute (PpxContext.mk fields) :: ast
  in
  let iface ast =
    let fields, ast =
      match ast with
      | {psig_desc = Psig_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                      attr_payload = x;
                                      attr_loc = _})} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.signature mapper ast
      with exn ->
        [{psig_desc = Psig_extension (extension_of_exn exn, []);
          psig_loc  = Location.none}]
    in
    let fields = PpxContext.update_cookies fields in
    Sig.attribute (PpxContext.mk fields) :: ast
  in

  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in

  let rewrite transform =
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    let ast = transform ast in
    let oc = open_out_bin target in
    output_string oc magic;
    output_value oc !Location.input_name;
    output_value oc ast;
    close_out oc
  and fail () =
    close_in ic;
    failwith "Ast_mapper: OCaml version mismatch or malformed input";
  in

  if magic = Config.ast_impl_magic_number then
    rewrite (implem : structure -> structure)
  else if magic = Config.ast_intf_magic_number then
    rewrite (iface : signature -> signature)
  else fail ()

let drop_ppx_context_str ~restore = function
  | {pstr_desc = Pstr_attribute
                   {attr_name = {Location.txt = "ocaml.ppx.context"};
                    attr_payload = a;
                    attr_loc = _}}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let drop_ppx_context_sig ~restore = function
  | {psig_desc = Psig_attribute
                   {attr_name = {Location.txt = "ocaml.ppx.context"};
                    attr_payload = a;
                    attr_loc = _}}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let add_ppx_context_str ~tool_name ast =
  Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

let add_ppx_context_sig ~tool_name ast =
  Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast


let apply ~source ~target mapper =
  apply_lazy ~source ~target (fun () -> mapper)

let run_main mapper =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      let mapper () =
        try mapper (Array.to_list (Array.sub a 1 (n - 3)))
        with exn ->
          (* PR#6463 *)
          let f _ _ = raise exn in
          {default_mapper with structure = f; signature = f}
      in
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1) mapper
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let register_function = ref (fun _name f -> run_main f)
let register name f = !register_function name f
