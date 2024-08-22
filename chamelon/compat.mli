open Typedtree
open Types

val mkTvar : string option -> type_desc

val mkTarrow :
  Asttypes.arg_label * type_expr * type_expr * commutable -> type_desc

type apply_arg
type texp_function_param_identifier
type texp_function_cases_identifier

val texp_function_cases_identifier_defaults : texp_function_cases_identifier
val texp_function_param_identifier_defaults : texp_function_param_identifier

type texp_function_param = {
  arg_label : Asttypes.arg_label;
  pattern : pattern;
  param : Ident.t;
  partial : partial;
  optional_default : expression option;
      (** The optional argument's default value. If [optional_default] is present,
      [arg_label] must be [Optional], and [pattern] matches values of type [t]
      if the parameter type is [t option]. *)
  param_identifier : texp_function_param_identifier;
}

type texp_function_body =
  | Function_body of expression
  | Function_cases of {
      cases : value case list;
      param : Ident.t;
      partial : partial;
      function_cases_identifier : texp_function_cases_identifier;
    }

type texp_function = {
  params : texp_function_param list;
  body : texp_function_body;
}

type texp_ident_identifier
type texp_apply_identifier
type texp_tuple_identifier
type texp_construct_identifier
type texp_function_identifier
type texp_sequence_identifier
type texp_match_identifier

val mkTexp_ident :
  ?id:texp_ident_identifier ->
  Path.t * Longident.t Location.loc * value_description ->
  expression_desc

val mkTexp_apply :
  ?id:texp_apply_identifier ->
  expression * (Asttypes.arg_label * apply_arg) list ->
  expression_desc

val mkTexp_tuple :
  ?id:texp_tuple_identifier -> expression list -> expression_desc

val mkTexp_construct :
  ?id:texp_construct_identifier ->
  Longident.t Location.loc * constructor_description * expression list ->
  expression_desc

val mkTexp_function :
  ?id:texp_function_identifier -> texp_function -> expression_desc

val mkTexp_sequence :
  ?id:texp_sequence_identifier -> expression * expression -> expression_desc

val mkTexp_match :
  ?id:texp_match_identifier ->
  expression * computation case list * partial ->
  expression_desc

val mkTexp_assert : expression -> Location.t -> expression_desc
val mkTtyp_any : core_type_desc
val mkTtyp_var : string -> core_type_desc
val is_type_name_used : core_type_desc -> string -> bool

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

val view_texp : expression_desc -> matched_expression_desc

type tpat_var_identifier
type tpat_alias_identifier
type tpat_array_identifier
type tpat_tuple_identifier

val mkTpat_var :
  ?id:tpat_var_identifier -> Ident.t * string Location.loc -> value pattern_desc

val mkTpat_alias :
  ?id:tpat_alias_identifier ->
  value general_pattern * Ident.t * string Location.loc ->
  value pattern_desc

val mkTpat_array :
  ?id:tpat_array_identifier -> value general_pattern list -> value pattern_desc

val mkTpat_tuple :
  ?id:tpat_tuple_identifier -> value general_pattern list -> value pattern_desc

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

val view_tpat : 'a pattern_desc -> 'a matched_pattern_desc

type tstr_eval_identifier

val mkTstr_eval :
  ?id:tstr_eval_identifier -> expression * attributes -> structure_item_desc

type matched_structure_item_desc =
  | Tstr_eval of expression * attributes * tstr_eval_identifier
  | O of structure_item_desc

val view_tstr : structure_item_desc -> matched_structure_item_desc

type arg_identifier

val mkArg : ?id:arg_identifier -> expression -> apply_arg
val map_arg_or_omitted : (expression -> expression) -> apply_arg -> apply_arg

val fold_arg_or_omitted :
  ('a -> expression * arg_identifier -> 'a) -> 'a -> apply_arg -> 'a

val option_of_arg_or_omitted : apply_arg -> (expression * arg_identifier) option
val mk_constructor_description : string -> constructor_description

val mk_value_binding :
  vb_pat:value general_pattern ->
  vb_expr:expression ->
  vb_attributes:attributes ->
  value_binding

val mk_value_description :
  val_type:type_expr ->
  val_kind:value_kind ->
  val_attributes:attributes ->
  value_description

val print_path : Path.t -> string
val replace_id_in_path : Path.t -> Ident.t -> Path.t
val unwrap_path_if_unapplied_constr : type_desc -> Path.t option
