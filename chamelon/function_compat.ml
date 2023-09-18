open Typedtree
open Types
open Compat

type cases_view_identifier =
  | Cases of texp_function_cases_identifier
  | Param of texp_function_param_identifier

type cases_view = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
  partial : partial;
  optional_default : expression option;
  cases_view_identifier : cases_view_identifier;
}

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

let cases_view_to_function
    {
      arg_label;
      param;
      cases;
      partial;
      optional_default;
      cases_view_identifier;
    } =
  match cases_view_identifier with
  | Param param_identifier -> (
      match cases with
      | [] -> Misc.fatal_error "unexpected empty cases with Param identifier"
      | _ :: _ :: _ ->
          Misc.fatal_error "unexpected multiple cases with Param identifier"
      | [ { c_guard = Some _; _ } ] ->
          Misc.fatal_error "unexpected when-guard with Param identifier"
      | [ { c_lhs; c_guard = None; c_rhs } ] -> (
          let param : texp_function_param =
            {
              arg_label;
              pattern = c_lhs;
              param;
              partial;
              optional_default;
              param_identifier;
            }
          in
          match view_texp c_rhs.exp_desc with
          | Texp_function (inner_function, _) ->
              { inner_function with params = param :: inner_function.params }
          | _ -> { params = [ param ]; body = Function_body c_rhs }))
  | Cases function_cases_identifier ->
      {
        params = [];
        body =
          Function_cases { cases; param; partial; function_cases_identifier };
      }

let function_to_cases_view { params; body } =
  match (params, body) with
  | ( { arg_label; param; partial; pattern; optional_default; param_identifier }
      :: params,
      body ) ->
      let c_rhs =
        match (params, body) with
        | [], Function_body body -> body
        | _, _ -> mk_exp (mkTexp_function { params; body })
      in
      {
        arg_label;
        param;
        cases = [ { c_lhs = pattern; c_guard = None; c_rhs } ];
        partial;
        optional_default;
        cases_view_identifier = Param param_identifier;
      }
  | [], Function_cases { cases; param; partial; function_cases_identifier } ->
      {
        arg_label = Nolabel;
        param;
        cases;
        partial;
        optional_default = None;
        cases_view_identifier = Cases function_cases_identifier;
      }
  | [], Function_body _ -> Misc.fatal_error "function without parameters"
