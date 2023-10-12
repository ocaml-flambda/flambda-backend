open Typedtree
open Types

let mkTvar name = Tvar name
let mkTarrow (label, t1, t2, comm) = Tarrow (label, t1, t2, comm)

type texp_ident_identifier = unit

let mkTexp_ident ?id:(() = ()) (path, longident, vd) =
  Texp_ident (path, longident, vd)

type nonrec apply_arg = expression option
type texp_apply_identifier = unit

let mkTexp_apply ?id:(() = ()) (exp, args) = Texp_apply (exp, args)

type texp_tuple_identifier = unit

let mkTexp_tuple ?id:(() = ()) exps = Texp_tuple exps

type texp_construct_identifier = unit

let mkTexp_construct ?id:(() = ()) (name, desc, args) =
  Texp_construct (name, desc, args)

type texp_function = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
}

type texp_function_identifier = partial

let mkTexp_function ?id:(partial = Total)
    ({ arg_label; param; cases } : texp_function) =
  Texp_function { arg_label; param; cases; partial }

type texp_sequence_identifier = unit

let mkTexp_sequence ?id:(() = ()) (e1, e2) = Texp_sequence (e1, e2)

type texp_match_identifier = unit

let mkTexp_match ?id:(() = ()) (e, cases, partial) =
  Texp_match (e, cases, partial)

type matched_expression_desc =
  | Texp_ident of
      Path.t
      * Longident.t Location.loc
      * value_description
      * texp_ident_identifier
  | Texp_apply of
      expression * (Asttypes.arg_label * apply_arg) list * texp_apply_identifier
  | Texp_construct of
      Longident.t Location.loc
      * constructor_description
      * expression list
      * texp_construct_identifier
  | Texp_tuple of expression list * texp_tuple_identifier
  | Texp_function of texp_function * texp_function_identifier
  | Texp_sequence of expression * expression * texp_sequence_identifier
  | Texp_match of
      expression * computation case list * partial * texp_match_identifier
  | O of expression_desc

let view_texp (e : expression_desc) =
  match e with
  | Texp_ident (path, longident, vd) -> Texp_ident (path, longident, vd, ())
  | Texp_apply (exp, args) -> Texp_apply (exp, args, ())
  | Texp_construct (name, desc, args) -> Texp_construct (name, desc, args, ())
  | Texp_tuple args -> Texp_tuple (args, ())
  | Texp_function { arg_label; param; cases; partial } ->
      Texp_function ({ arg_label; param; cases }, partial)
  | Texp_sequence (e1, e2) -> Texp_sequence (e1, e2, ())
  | Texp_match (e, cases, partial) -> Texp_match (e, cases, partial, ())
  | _ -> O e

type tpat_var_identifier = unit

let mkTpat_var ?id:(() = ()) (ident, name) = Tpat_var (ident, name)

type tpat_alias_identifier = unit

let mkTpat_alias ?id:(() = ()) (p, ident, name) = Tpat_alias (p, ident, name)

type tpat_array_identifier = unit

let mkTpat_array ?id:(() = ()) l = Tpat_array l

type tpat_tuple_identifier = unit

let mkTpat_tuple ?id:(() = ()) l = Tpat_tuple l

type 'a matched_pattern_desc =
  | Tpat_var :
      Ident.t * string Location.loc * tpat_var_identifier
      -> value matched_pattern_desc
  | Tpat_alias :
      value general_pattern
      * Ident.t
      * string Location.loc
      * tpat_alias_identifier
      -> value matched_pattern_desc
  | Tpat_array :
      value general_pattern list * tpat_array_identifier
      -> value matched_pattern_desc
  | Tpat_tuple :
      value general_pattern list * tpat_tuple_identifier
      -> value matched_pattern_desc
  | O : 'a pattern_desc -> 'a matched_pattern_desc

let view_tpat (type a) (p : a pattern_desc) : a matched_pattern_desc =
  match p with
  | Tpat_var (ident, name) -> Tpat_var (ident, name, ())
  | Tpat_alias (p, ident, name) -> Tpat_alias (p, ident, name, ())
  | Tpat_array l -> Tpat_array (l, ())
  | Tpat_tuple l -> Tpat_tuple (l, ())
  | _ -> O p

type tstr_eval_identifier = unit

let mkTstr_eval ?id:(() = ()) (e, attrs) = Tstr_eval (e, attrs)

type matched_structure_item_desc =
  | Tstr_eval of expression * attributes * tstr_eval_identifier
  | O of structure_item_desc

let view_tstr (si : structure_item_desc) =
  match si with Tstr_eval (e, attrs) -> Tstr_eval (e, attrs, ()) | _ -> O si

type arg_identifier = unit

let mkArg ?id:(() = ()) e = Some e

let map_arg_or_omitted f arg =
  match arg with Some e -> Some (f e) | None -> None

let fold_arg_or_omitted f init arg =
  match arg with Some e -> f init (e, ()) | None -> init

let option_of_arg_or_omitted arg =
  match arg with Some e -> Some (e, ()) | None -> None

let mk_constructor_description cstr_name =
  {
    cstr_name;
    cstr_res = newty2 ~level:0 (mkTvar (Some "a"));
    cstr_existentials = [];
    cstr_args = [];
    cstr_arity = 0;
    cstr_tag = Cstr_constant 0;
    cstr_consts = 0;
    cstr_nonconsts = 0;
    cstr_generalized = false;
    cstr_private = Public;
    cstr_loc = Location.none;
    cstr_attributes = [];
    cstr_inlined = None;
    cstr_uid = Uid.internal_not_actually_unique;
  }

let mk_value_binding ~vb_pat ~vb_expr ~vb_attributes =
  { vb_pat; vb_expr; vb_attributes; vb_loc = Location.none }

let mkTtyp_any = Ttyp_any
let mkTtyp_var s = Ttyp_var s

let is_type_name_used desc typ_name =
  match desc with
  | Ttyp_alias (_, s) -> s = typ_name
  | Ttyp_constr (_, li, _) -> Longident.last li.txt = typ_name
  | _ -> false
