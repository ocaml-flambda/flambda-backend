(* simp-app : simplify partial applications *)

open Utils
open Typedtree
open Tast_mapper
open Compat

let simplify_app_mapper should_remove =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        Tast_mapper.default.expr mapper
          (match view_texp e.exp_desc with
          | Texp_apply (app, ea_l, id) -> (
              match view_texp app.exp_desc with
              | Texp_function (f, _f_id) -> (
                  let _, arg = List.hd ea_l in
                  let f_as_cases = Function_compat.function_to_cases_view f in
                  match option_of_arg_or_omitted arg with
                  | Some (Targ_expr (arg, _)) when should_remove () ->
                      {
                        e with
                        exp_desc =
                          (let e_match =
                             mkTexp_match
                               ( arg,
                                 List.map
                                   (fun v ->
                                     {
                                       v with
                                       c_lhs = as_computation_pattern v.c_lhs;
                                     })
                                   f_as_cases.cases,
                                 f_as_cases.partial )
                           in
                           if List.length ea_l = 1 then e_match
                           else
                             mkTexp_apply ~id
                               ({ app with exp_desc = e_match }, List.tl ea_l));
                      }
                  | _ -> e)
              | _ -> e)
          | _ -> e));
  }

let minimize should_remove map cur_name =
  let mapper = simplify_app_mapper should_remove in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "simplify-application"; minimizer_func = minimize }
