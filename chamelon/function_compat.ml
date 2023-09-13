open Typedtree
open Types
open Compat

type cases_view = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
  partial : partial;
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

let cases_view_to_function { arg_label; param; cases; partial } =
  match cases with
  | [ { c_lhs; c_guard = None; c_rhs } ] -> (
      let param : texp_function_param =
        {
          arg_label;
          pattern = c_lhs;
          param;
          partial;
          param_identifier = texp_function_param_identifier_defaults;
        }
      in
      match view_texp c_rhs.exp_desc with
      | Texp_function (inner_function, _) ->
          { inner_function with params = param :: inner_function.params }
      | _ -> { params = [ param ]; body = Function_body c_rhs })
  | cases ->
      {
        params = [];
        body =
          Function_cases
            {
              cases;
              param;
              partial;
              function_cases_identifier =
                texp_function_cases_identifier_defaults;
            };
      }

let function_to_cases_view { params; body } =
  match (params, body) with
  | param :: params, body ->
      let c_rhs =
        match (params, body) with
        | [], Function_body body -> body
        | _, _ -> mk_exp (mkTexp_function { params; body })
      in
      {
        arg_label = param.arg_label;
        param = param.param;
        cases = [ { c_lhs = param.pattern; c_guard = None; c_rhs } ];
        partial = param.partial;
      }
  | [], Function_cases fc ->
      {
        arg_label = Nolabel;
        param = fc.param;
        cases = fc.cases;
        partial = fc.partial;
      }
  | [], Function_body _ -> Misc.fatal_error "function without parameters"
