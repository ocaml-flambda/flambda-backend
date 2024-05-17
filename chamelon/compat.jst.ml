open Typedtree
open Types
open Mode

let dummy_jkind = Jkind.Primitive.value ~why:(Unknown "dummy_layout")
let dummy_value_mode = Value.disallow_right Value.legacy
let mkTvar name = Tvar { name; jkind = dummy_jkind }

let mkTarrow (label, t1, t2, comm) =
  let label = Typetexp.transl_label label None in
  Tarrow ((label, Alloc.legacy, Alloc.legacy), t1, t2, comm)

type texp_ident_identifier = ident_kind * unique_use

let mkTexp_ident ?id:(ident_kind, uu = (Id_value, shared_many_use))
    (path, longident, vd) =
  Texp_ident (path, longident, vd, ident_kind, uu)

type nonrec apply_arg = apply_arg

type texp_apply_identifier =
  apply_position * Locality.l * Zero_alloc_utils.Assume_info.t

let mkTexp_apply
    ?id:(pos, mode, za =
        ( Default,
          Locality.disallow_right Locality.legacy,
          Zero_alloc_utils.Assume_info.none )) (exp, args) =
  let args =
    List.map (fun (label, x) -> (Typetexp.transl_label label None, x)) args
  in
  Texp_apply (exp, args, pos, mode, za)

type texp_tuple_identifier = string option list * Alloc.r

let mkTexp_tuple ?id exps =
  let labels, alloc =
    match id with
    | None -> (List.map (fun _ -> None) exps, Alloc.disallow_left Alloc.legacy)
    | Some id -> id
  in
  let exps = List.combine labels exps in
  Texp_tuple (exps, alloc)

type texp_construct_identifier = Alloc.r option

let mkTexp_construct ?id:(mode = Some (Alloc.disallow_left Alloc.legacy))
    (name, desc, args) =
  Texp_construct (name, desc, args, mode)

type texp_function_param_identifier = {
  param_sort : Jkind.Sort.t;
  param_mode : Alloc.l;
  param_curry : function_curry;
  param_newtypes : (string Location.loc * Jkind.annotation option) list;
}

type texp_function_param = {
  arg_label : Asttypes.arg_label;
  pattern : pattern;
  param : Ident.t;
  partial : partial;
  optional_default : expression option;
  param_identifier : texp_function_param_identifier;
}

type texp_function_cases_identifier = {
  last_arg_mode : Alloc.l;
  last_arg_sort : Jkind.Sort.t;
  last_arg_exp_extra : exp_extra option;
  last_arg_attributes : attributes;
  env : Env.t;
  ret_type : Types.type_expr;
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

type texp_function_identifier = {
  alloc_mode : Alloc.r;
  ret_sort : Jkind.sort;
  region : bool;
  ret_mode : Alloc.l;
  zero_alloc : Builtin_attributes.zero_alloc_attribute;
}

let texp_function_cases_identifier_defaults =
  {
    last_arg_mode = Alloc.disallow_right Alloc.legacy;
    last_arg_sort = Jkind.Sort.value;
    last_arg_exp_extra = None;
    last_arg_attributes = [];
    env = Env.empty;
    ret_type = Ctype.newvar (Jkind.Primitive.any ~why:Dummy_jkind);
  }

let texp_function_param_identifier_defaults =
  {
    param_sort = Jkind.Sort.value;
    param_mode = Alloc.disallow_right Alloc.legacy;
    param_curry = More_args { partial_mode = Alloc.disallow_right Alloc.legacy };
    param_newtypes = [];
  }

let texp_function_defaults =
  {
    alloc_mode = Alloc.disallow_left Alloc.legacy;
    ret_sort = Jkind.Sort.value;
    ret_mode = Alloc.disallow_right Alloc.legacy;
    region = false;
    zero_alloc = Builtin_attributes.Default_zero_alloc;
  }

let mkTexp_function ?(id = texp_function_defaults)
    ({ params; body } : texp_function) =
  Texp_function
    {
      params =
        List.map
          (fun {
                 arg_label;
                 pattern;
                 param;
                 partial;
                 param_identifier = id;
                 optional_default;
               } ->
            let arg_label = Typetexp.transl_label arg_label None in
            {
              fp_arg_label = arg_label;
              fp_kind =
                (match optional_default with
                | None -> Tparam_pat pattern
                | Some default ->
                    Tparam_optional_default (pattern, default, id.param_sort));
              fp_param = param;
              fp_partial = partial;
              fp_sort = id.param_sort;
              fp_mode = id.param_mode;
              fp_curry = id.param_curry;
              fp_newtypes = id.param_newtypes;
              fp_loc = Location.none;
            })
          params;
      body =
        (match body with
        | Function_body expr -> Tfunction_body expr
        | Function_cases
            { cases; param; partial; function_cases_identifier = id } ->
            Tfunction_cases
              {
                fc_cases = cases;
                fc_param = param;
                fc_partial = partial;
                fc_env = id.env;
                fc_ret_type = id.ret_type;
                fc_arg_mode = id.last_arg_mode;
                fc_arg_sort = id.last_arg_sort;
                fc_exp_extra = id.last_arg_exp_extra;
                fc_attributes = id.last_arg_attributes;
                fc_loc = Location.none;
              });
      alloc_mode = id.alloc_mode;
      region = id.region;
      ret_sort = id.ret_sort;
      ret_mode = id.ret_mode;
      zero_alloc = id.zero_alloc;
    }

type texp_sequence_identifier = Jkind.sort

let mkTexp_sequence ?id:(sort = Jkind.Sort.value) (e1, e2) =
  Texp_sequence (e1, sort, e2)

type texp_match_identifier = Jkind.sort

let mkTexp_match ?id:(sort = Jkind.Sort.value) (e, cases, partial) =
  Texp_match (e, sort, cases, partial)

let mkTexp_assert e loc = Texp_assert (e, loc)

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

let untype_label = function
  | Typedtree.Position l | Labelled l -> Asttypes.Labelled l
  | Optional l -> Optional l
  | Nolabel -> Nolabel

let view_texp (e : expression_desc) =
  match e with
  | Texp_ident (path, longident, vd, ident_kind, uu) ->
      Texp_ident (path, longident, vd, (ident_kind, uu))
  | Texp_apply (exp, args, pos, mode, za) ->
      let args = List.map (fun (label, x) -> (untype_label label, x)) args in
      Texp_apply (exp, args, (pos, mode, za))
  | Texp_construct (name, desc, args, mode) ->
      Texp_construct (name, desc, args, mode)
  | Texp_tuple (args, mode) ->
      let labels, args = List.split args in
      Texp_tuple (args, (labels, mode))
  | Texp_function
      { params; body; alloc_mode; region; ret_sort; ret_mode; zero_alloc } ->
      let params =
        List.map
          (fun param ->
            let pattern, optional_default =
              match param.fp_kind with
              | Tparam_optional_default (pattern, optional_default, _) ->
                  (pattern, Some optional_default)
              | Tparam_pat pattern -> (pattern, None)
            in
            {
              arg_label = untype_label param.fp_arg_label;
              param = param.fp_param;
              partial = param.fp_partial;
              pattern;
              optional_default;
              param_identifier =
                {
                  param_sort = param.fp_sort;
                  param_mode = param.fp_mode;
                  param_curry = param.fp_curry;
                  param_newtypes = param.fp_newtypes;
                };
            })
          params
      in
      let body =
        match body with
        | Tfunction_body body -> Function_body body
        | Tfunction_cases cases ->
            Function_cases
              {
                cases = cases.fc_cases;
                param = cases.fc_param;
                partial = cases.fc_partial;
                function_cases_identifier =
                  {
                    last_arg_mode = cases.fc_arg_mode;
                    last_arg_sort = cases.fc_arg_sort;
                    last_arg_exp_extra = cases.fc_exp_extra;
                    last_arg_attributes = cases.fc_attributes;
                    env = cases.fc_env;
                    ret_type = cases.fc_ret_type;
                  };
              }
      in
      Texp_function
        ( { params; body },
          { alloc_mode; region; ret_sort; ret_mode; zero_alloc } )
  | Texp_sequence (e1, sort, e2) -> Texp_sequence (e1, e2, sort)
  | Texp_match (e, sort, cases, partial) -> Texp_match (e, cases, partial, sort)
  | _ -> O e

type tpat_var_identifier = Value.l

let mkTpat_var ?id:(mode = dummy_value_mode) (ident, name) =
  Tpat_var (ident, name, Uid.internal_not_actually_unique, mode)

type tpat_alias_identifier = Value.l

let mkTpat_alias ?id:(mode = dummy_value_mode) (p, ident, name) =
  Tpat_alias (p, ident, name, Uid.internal_not_actually_unique, mode)

type tpat_array_identifier = mutability * Jkind.sort

let mkTpat_array
    ?id:(mut, arg_sort =
        (Mutable Alloc.Comonadic.Const.legacy, Jkind.Sort.value)) l =
  Tpat_array (mut, arg_sort, l)

type tpat_tuple_identifier = string option list

let mkTpat_tuple ?id pats =
  let labels =
    match id with
    | None -> List.map (fun _ -> None) pats
    | Some labels -> labels
  in
  Tpat_tuple (List.combine labels pats)

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
  | Tpat_var (ident, name, _uid, mode) -> Tpat_var (ident, name, mode)
  | Tpat_alias (p, ident, name, _uid, mode) -> Tpat_alias (p, ident, name, mode)
  | Tpat_array (mut, arg_sort, l) -> Tpat_array (l, (mut, arg_sort))
  | Tpat_tuple pats ->
      let labels, pats = List.split pats in
      Tpat_tuple (pats, labels)
  | _ -> O p

type tstr_eval_identifier = Jkind.sort

let mkTstr_eval ?id:(sort = Jkind.Sort.value) (e, attrs) =
  Tstr_eval (e, sort, attrs)

type matched_structure_item_desc =
  | Tstr_eval of expression * attributes * tstr_eval_identifier
  | O of structure_item_desc

let view_tstr (si : structure_item_desc) =
  match si with
  | Tstr_eval (e, sort, attrs) -> Tstr_eval (e, attrs, sort)
  | _ -> O si

type arg_identifier = Jkind.sort

let mkArg ?id:(sort = Jkind.Sort.value) e = Arg (e, sort)

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
    cstr_shape = Constructor_uniform_value;
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
    cstr_arg_jkinds = [||];
    cstr_repr = Variant_boxed [||];
    cstr_constant = true;
  }

let mk_value_binding ~vb_pat ~vb_expr ~vb_attributes =
  {
    vb_pat;
    vb_expr;
    vb_attributes;
    vb_rec_kind = Dynamic;
    vb_loc = Location.none;
    vb_sort = Jkind.Sort.value;
  }

let mk_value_description ~val_type ~val_kind ~val_attributes =
  {
    val_type;
    val_kind;
    val_loc = Location.none;
    val_attributes;
    val_uid = Uid.internal_not_actually_unique;
    val_zero_alloc = Default_zero_alloc;
  }

let mkTtyp_any = Ttyp_var (None, None)
let mkTtyp_var s = Ttyp_var (Some s, None)

let is_type_name_used desc typ_name =
  match desc with
  | Ttyp_alias (_, Some s, _) -> s = typ_name
  | Ttyp_constr (_, li, _) -> Longident.last li.txt = typ_name
  | _ -> false

let rec print_path p =
  match (p : Path.t) with
  | Pident id -> Ident.name id
  | Pdot (p, s) -> print_path p ^ "." ^ s
  | Papply (t1, t2) -> "app " ^ print_path t1 ^ " " ^ print_path t2
  | Pextra_ty _ -> Format.asprintf "%a" Path.print p

let rec replace_id_in_path path to_rep : Path.t =
  match (path : Path.t) with
  | Pident _ -> Pident to_rep
  | Papply (p1, p2) ->
      Papply (replace_id_in_path p1 to_rep, replace_id_in_path p2 to_rep)
  | Pdot (p, str) -> Pdot (replace_id_in_path p to_rep, str)
  | Pextra_ty (p, extra_ty) -> Pextra_ty (replace_id_in_path p to_rep, extra_ty)
