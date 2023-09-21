open Typedtree
open Types
open Mode

let dummy_layout = Layouts.Layout.value ~why:Type_argument
let dummy_value_mode = Value.legacy
let mkTvar name = Tvar { name; layout = dummy_layout }

let mkTarrow (label, t1, t2, comm) =
  Tarrow ((label, Alloc.legacy, Alloc.legacy), t1, t2, comm)

type texp_ident_identifier = ident_kind * unique_use

let mkTexp_ident ?id:(ident_kind, uu = (Id_value, shared_many_use))
    (path, longident, vd) =
  Texp_ident (path, longident, vd, ident_kind, uu)

type nonrec apply_arg = apply_arg
type texp_apply_identifier = apply_position * Locality.t

let mkTexp_apply ?id:(pos, mode = (Default, Locality.legacy)) (exp, args) =
  Texp_apply (exp, args, pos, mode)

type texp_tuple_identifier = Alloc.t

let mkTexp_tuple ?id:(mode = Alloc.legacy) exps = Texp_tuple (exps, mode)

type texp_construct_identifier = Alloc.t option

let mkTexp_construct ?id:(mode = Some Alloc.legacy) (name, desc, args) =
  Texp_construct (name, desc, args, mode)

type texp_function = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
}

type texp_function_identifier = {
  partial : partial;
  arg_mode : Alloc.t;
  alloc_mode : Alloc.t;
  region : bool;
  curry : fun_curry_state;
  warnings : Warnings.state;
  arg_sort : Layouts.sort;
  ret_sort : Layouts.sort;
}

let texp_function_defaults =
  {
    partial = Total;
    arg_mode = Alloc.legacy;
    alloc_mode = Alloc.legacy;
    region = false;
    curry = Final_arg { partial_mode = Alloc.legacy };
    warnings = Warnings.backup ();
    arg_sort = Layouts.Sort.value;
    ret_sort = Layouts.Sort.value;
  }

let mkTexp_function ?(id = texp_function_defaults)
    ({ arg_label; param; cases } : texp_function) =
  Texp_function
    {
      arg_label;
      param;
      cases;
      partial = id.partial;
      arg_mode = id.arg_mode;
      alloc_mode = id.alloc_mode;
      region = id.region;
      curry = id.curry;
      warnings = id.warnings;
      arg_sort = id.arg_sort;
      ret_sort = id.ret_sort;
    }

type texp_sequence_identifier = Layouts.sort

let mkTexp_sequence ?id:(sort = Layouts.Sort.value) (e1, e2) =
  Texp_sequence (e1, sort, e2)

type texp_match_identifier = Layouts.sort

let mkTexp_match ?id:(sort = Layouts.Sort.value) (e, cases, partial) =
  Texp_match (e, sort, cases, partial)

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
  | Texp_ident (path, longident, vd, ident_kind, uu) ->
      Texp_ident (path, longident, vd, (ident_kind, uu))
  | Texp_apply (exp, args, pos, mode) -> Texp_apply (exp, args, (pos, mode))
  | Texp_construct (name, desc, args, mode) ->
      Texp_construct (name, desc, args, mode)
  | Texp_tuple (args, mode) -> Texp_tuple (args, mode)
  | Texp_function
      {
        arg_label;
        param;
        cases;
        partial;
        arg_mode;
        alloc_mode;
        region;
        curry;
        warnings;
        arg_sort;
        ret_sort;
      } ->
      Texp_function
        ( { arg_label; param; cases },
          {
            partial;
            arg_mode;
            alloc_mode;
            region;
            curry;
            warnings;
            arg_sort;
            ret_sort;
          } )
  | Texp_sequence (e1, sort, e2) -> Texp_sequence (e1, e2, sort)
  | Texp_match (e, sort, cases, partial) -> Texp_match (e, cases, partial, sort)
  | _ -> O e

type tpat_var_identifier = Value.t

let mkTpat_var ?id:(mode = dummy_value_mode) (ident, name) =
  Tpat_var (ident, name, Uid.internal_not_actually_unique, mode)

type tpat_alias_identifier = Value.t

let mkTpat_alias ?id:(mode = dummy_value_mode) (p, ident, name) =
  Tpat_alias (p, ident, name, Uid.internal_not_actually_unique, mode)

type tpat_array_identifier = Asttypes.mutable_flag

let mkTpat_array ?id:(mut = Asttypes.Mutable) l = Tpat_array (mut, l)

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
  | O : 'a pattern_desc -> 'a matched_pattern_desc

let view_tpat (type a) (p : a pattern_desc) : a matched_pattern_desc =
  match p with
  | Tpat_var (ident, name, _uid, mode) -> Tpat_var (ident, name, mode)
  | Tpat_alias (p, ident, name, _uid, mode) -> Tpat_alias (p, ident, name, mode)
  | Tpat_array (mut, l) -> Tpat_array (l, mut)
  | _ -> O p

type tstr_eval_identifier = Layouts.sort

let mkTstr_eval ?id:(sort = Layouts.Sort.value) (e, attrs) =
  Tstr_eval (e, sort, attrs)

type matched_structure_item_desc =
  | Tstr_eval of expression * attributes * tstr_eval_identifier
  | O of structure_item_desc

let view_tstr (si : structure_item_desc) =
  match si with
  | Tstr_eval (e, sort, attrs) -> Tstr_eval (e, attrs, sort)
  | _ -> O si

type arg_identifier = Layouts.sort

let mkArg ?id:(sort = Layouts.Sort.value) e = Arg (e, sort)

let map_arg_or_omitted f arg =
  match arg with Arg (e, sort) -> Arg (f e, sort) | Omitted o -> Omitted o

let fold_arg_or_omitted f init arg =
  match arg with Arg (e, sort) -> f init (e, sort) | Omitted _ -> init

let option_of_arg_or_omitted arg =
  match arg with Arg (e, sort) -> Some (e, sort) | Omitted _ -> None

let mk_constructor_description cstr_name =
  {
    cstr_name;
    cstr_res = newty2 ~level:0 (mkTvar (Some "a"));
    cstr_existentials = [];
    cstr_args = [];
    cstr_arity = 0;
    cstr_tag = Ordinary { src_index = 0; runtime_tag = 0 };
    cstr_consts = 0;
    cstr_nonconsts = 0;
    cstr_generalized = false;
    cstr_private = Public;
    cstr_loc = Location.none;
    cstr_attributes = [];
    cstr_inlined = None;
    cstr_uid = Uid.internal_not_actually_unique;
    cstr_arg_layouts = [||];
    cstr_repr = Variant_boxed [||];
    cstr_constant = true;
  }

let mk_value_binding ~vb_pat ~vb_expr ~vb_attributes =
  {
    vb_pat;
    vb_expr;
    vb_attributes;
    vb_loc = Location.none;
    vb_sort = Layouts.Sort.value;
  }

let mkTtyp_any = Ttyp_var (None, None)
let mkTtyp_var s = Ttyp_var (Some s, None)

let is_type_name_used desc typ_name =
  match desc with
  | Ttyp_alias (_, Some s, _) -> s = typ_name
  | Ttyp_constr (_, li, _) -> Longident.last li.txt = typ_name
  | _ -> false
