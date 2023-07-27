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
          | Texp_apply (app, ea_l) -> (
              match app.exp_desc with
              | Texp_function f ->
                  if should_remove () then
                    let _, arg = List.hd ea_l in
                    match option_of_arg_or_omitted arg with
                    | Some arg ->
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
                                     f.cases,
                                   f.partial )
                             in
                             if List.length ea_l = 1 then e_match
                             else
                               mkTexp_apply
                                 ({ app with exp_desc = e_match }, List.tl ea_l));
                        }
                    | _ -> e
                  else e
              | _ -> e)
          | _ -> e));
  }

let minimize should_remove map cur_name =
  let mapper = simplify_app_mapper should_remove in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "simplify-application"; minimizer_func = minimize }
