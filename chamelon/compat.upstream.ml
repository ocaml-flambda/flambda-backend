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

type texp_function_param_identifier = unit
type texp_function_cases_identifier = unit

let texp_function_param_identifier_defaults = ()
let texp_function_cases_identifier_defaults = ()

type texp_function_param = {
  arg_label : Asttypes.arg_label;
  pattern : pattern;
  param : Ident.t;
  partial : partial;
  optional_default : expression option;
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

type texp_function_identifier = unit

let dummy_type_expr = newty2 ~level:0 (mkTvar (Some "a"))

let mk_exp ed =
  {
    exp_desc = ed;
    exp_loc = Location.none;
    exp_extra = [];
    exp_type = dummy_type_expr;
    exp_env = Env.empty;
    exp_attributes = [];
  }

(* This code can be simplified when we upgrade the upstream OCaml version past
   PR #12236, which makes Texp_function n-ary (i.e., closer to the
   [texp_function] record) instead of unary.
*)
let mkTexp_function ?id:(() = ()) ({ params; body } : texp_function) =
  let exp =
    List.fold_right
      (fun {
             arg_label;
             pattern;
             param;
             partial;
             optional_default;
             param_identifier = ();
           } acc ->
        assert (Option.is_none optional_default);
        mk_exp
          (Texp_function
             {
               arg_label;
               param;
               cases = [ { c_lhs = pattern; c_guard = None; c_rhs = acc } ];
               partial;
             }))
      params
      (match body with
      | Function_body expr -> expr
      | Function_cases { cases; param; partial; function_cases_identifier = () }
        ->
          mk_exp (Texp_function { arg_label = Nolabel; param; cases; partial }))
  in
  exp.exp_desc

type texp_sequence_identifier = unit

let mkTexp_sequence ?id:(() = ()) (e1, e2) = Texp_sequence (e1, e2)

type texp_match_identifier = unit

let mkTexp_match ?id:(() = ()) (e, cases, partial) =
  Texp_match (e, cases, partial)

let mkTexp_assert e _loc = Texp_assert e

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

let rec view_texp (e : expression_desc) =
  match e with
  | Texp_ident (path, longident, vd) -> Texp_ident (path, longident, vd, ())
  | Texp_apply (exp, args) -> Texp_apply (exp, args, ())
  | Texp_construct (name, desc, args) -> Texp_construct (name, desc, args, ())
  | Texp_tuple args -> Texp_tuple (args, ())
  | Texp_function { arg_label; param; cases; partial } ->
      let params, body =
        match cases with
        | [ { c_lhs; c_guard = None; c_rhs } ] -> (
            let param =
              {
                arg_label;
                partial;
                param;
                pattern = c_lhs;
                optional_default = None;
                param_identifier = ();
              }
            in
            match view_texp c_rhs.exp_desc with
            | Texp_function ({ params = inner_params; body = inner_body }, ())
              ->
                (param :: inner_params, inner_body)
            | _ -> ([ param ], Function_body c_rhs))
        | cases ->
            ( [],
              Function_cases
                { param; partial; cases; function_cases_identifier = () } )
      in
      Texp_function ({ params; body }, ())
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

let mk_value_description ~val_type ~val_kind ~val_attributes =
  {
    val_type;
    val_kind;
    val_loc = Location.none;
    val_attributes;
    val_uid = Uid.internal_not_actually_unique;
  }

let mkTtyp_any = Ttyp_any
let mkTtyp_var s = Ttyp_var s

let is_type_name_used desc typ_name =
  match desc with
  | Ttyp_alias (_, s) -> s = typ_name
  | Ttyp_constr (_, li, _) -> Longident.last li.txt = typ_name
  | _ -> false

let rec print_path p =
  match (p : Path.t) with
  | Pident id -> Ident.name id
  | Pdot (p, s) -> print_path p ^ "." ^ s
  | Papply (t1, t2) -> "app " ^ print_path t1 ^ " " ^ print_path t2

let rec replace_id_in_path path to_rep : Path.t =
  match (path : Path.t) with
  | Pident _ -> Pident to_rep
  | Papply (p1, p2) ->
      Papply (replace_id_in_path p1 to_rep, replace_id_in_path p2 to_rep)
  | Pdot (p, str) -> Pdot (replace_id_in_path p to_rep, str)

let unwrap_path_if_unapplied_constr desc =
  match desc with Tconstr (p, [], _) -> Some p | _ -> None
