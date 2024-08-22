(* Minimizer rem-unit: remove unit-typed expressions *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Compat

exception Not_implemented

let eunit = mkTexp_tuple []

let is_unit e =
  match view_texp e.exp_desc with
  | Texp_construct ({ txt = Lident "()"; _ }, _, _, _) | Texp_tuple ([], _) ->
      true
  | _ -> false

let is_unit_typ (typ : type_expr) =
  match get_desc typ with
  | Ttuple [] -> true
  | desc -> (
      match unwrap_path_if_unapplied_constr desc with
      | Some path -> (
          match path with Path.Pident id -> name id = "unit" | _ -> false)
      | None -> false)

let minimize should_remove map cur_name =
  let remove_unit_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          if is_unit_typ e.exp_type && (not (is_unit e)) && should_remove ()
          then { e with exp_desc = eunit }
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    remove_unit_mapper.structure remove_unit_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "remove-unit"; minimizer_func = minimize }
