(* Minimizer red-expr: reduce any sub-expression *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Dummy
open Compat

exception Not_implemented

let is_simplified e =
  match view_texp e.exp_desc with
  | Texp_tuple ([], _)
  | O (Texp_constant (Const_int 0))
  | O (Texp_constant (Const_char '0'))
  | O (Texp_constant (Const_string ("", _, None))) ->
      true
  | Texp_ident (_, name, _, _) ->
      Longident.last name.txt = "__dummy2__"
      || Longident.last name.txt = "__dummy1__"
      || Longident.last name.txt = "__ignore__"
  | Texp_construct (name, _, _, _) -> Longident.last name.txt = ""
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) ->
          Longident.last name.txt = "__dummy2__"
          || Longident.last name.txt = "__dummy1__"
          || Longident.last name.txt = "__ignore__"
      | _ -> false)
  | _ -> false

let simplify e =
  match get_desc e.exp_type |> unwrap_path_if_unapplied_constr with
  | Some path -> (
      match path with
      | Path.Pident id -> (
          match name id with
          | "int" -> { e with exp_desc = Texp_constant (Const_int 0) }
          | "char" -> { e with exp_desc = Texp_constant (Const_char '0') }
          | "string" ->
              {
                e with
                exp_desc =
                  Texp_constant (Const_string ("", Location.none, None));
              }
          | "unit" -> { e with exp_desc = mkTexp_tuple [] }
          | _ -> apply_dummy2)
      | _ -> apply_dummy2)
  | None -> apply_dummy2

let minimize should_remove map cur_name =
  let reduce_def_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          if (not (is_simplified e)) && should_remove () then simplify e
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    reduce_def_mapper.structure reduce_def_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "reduce-expr"; minimizer_func = minimize }
