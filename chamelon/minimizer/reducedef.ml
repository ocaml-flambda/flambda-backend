(* Minimizer red-def: reduce definitions of expressions *)

open Utils
open Typedtree
open Tast_mapper
open Dummy
open Compat

let is_dummy e =
  match view_texp e.exp_desc with
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__dummy2__"
      | _ -> false)
  | _ -> false

let minimize should_remove map cur_name =
  let reduce_def_mapper =
    {
      Tast_mapper.default with
      value_binding =
        (fun mapper vb ->
          if (not (is_dummy vb.vb_expr)) && should_remove () then
            { vb with vb_expr = apply_dummy2 }
          else Tast_mapper.default.value_binding mapper vb);
    }
  in
  let nstr =
    reduce_def_mapper.structure reduce_def_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "reduce-def"; minimizer_func = minimize }
