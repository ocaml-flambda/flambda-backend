open Typedtree
open Types

val mkTvar : string option -> type_desc

val mkTarrow :
  Asttypes.arg_label * type_expr * type_expr * commutable -> type_desc

type apply_arg

type texp_function = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
}

val mkTexp_ident :
  Path.t * Longident.t Location.loc * value_description -> expression_desc

val mkTexp_apply :
  expression * (Asttypes.arg_label * apply_arg) list -> expression_desc

val mkTexp_tuple : expression list -> expression_desc

val mkTexp_construct :
  Longident.t Location.loc * constructor_description * expression list ->
  expression_desc

val mkTexp_function : texp_function -> expression_desc
val mkTexp_sequence : expression * expression -> expression_desc

val mkTexp_match :
  expression * computation case list * partial -> expression_desc

type matched_expression_desc =
  | Texp_ident of Path.t * Longident.t Location.loc * value_description
  | Texp_apply of expression * (Asttypes.arg_label * apply_arg) list
  | Texp_construct of
      Longident.t Location.loc * constructor_description * expression list
  | Texp_tuple of expression list
  | Texp_function of texp_function
  | Texp_sequence of expression * expression
  | Texp_match of expression * computation case list * partial
  | O of expression_desc

val view_texp : expression_desc -> matched_expression_desc
val mkTpat_var : Ident.t * string Location.loc -> value pattern_desc

val mkTpat_alias :
  value general_pattern * Ident.t * string Location.loc -> value pattern_desc

val mkTpat_array : value general_pattern list -> value pattern_desc

type 'a matched_pattern_desc =
  | Tpat_var : Ident.t * string Location.loc -> value matched_pattern_desc
  | Tpat_alias :
      value general_pattern * Ident.t * string Location.loc
      -> value matched_pattern_desc
  | Tpat_array : value general_pattern list -> value matched_pattern_desc
  | O : 'a pattern_desc -> 'a matched_pattern_desc

val view_tpat : 'a pattern_desc -> 'a matched_pattern_desc
val mkTstr_eval : expression * attributes -> structure_item_desc

type matched_structure_item_desc =
  | Tstr_eval of expression * attributes
  | O of structure_item_desc

val view_tstr : structure_item_desc -> matched_structure_item_desc
val mkArg : expression -> apply_arg
val map_arg_or_omitted : (expression -> expression) -> apply_arg -> apply_arg
val fold_arg_or_omitted : ('a -> expression -> 'a) -> 'a -> apply_arg -> 'a
val option_of_arg_or_omitted : apply_arg -> expression option
val mk_constructor_description : string -> constructor_description

val mk_value_binding :
  vb_pat:value general_pattern ->
  vb_expr:expression ->
  vb_attributes:attributes ->
  value_binding
