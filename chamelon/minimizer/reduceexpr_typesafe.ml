(* Minimizer red-expr: reduce any sub-expression without using %opaque *)

open Utils
open Typedtree
open Tast_mapper
open Types
open Ident
open Dummy_expr
open Env

let rec is_simplified env e =
  match e.exp_desc with
  | Texp_construct (_, cons, el) -> (
      match get_desc cons.cstr_res with
      | Tconstr (path, _, _) -> (
          let td = find_type path env in
          match td.type_kind with
          | Type_variant (cstr_list, _) ->
              List.for_all (is_simplified env) el
              && cons.cstr_name
                 = Ident.name (find_simpler_cons path cstr_list).cd_id
          | _ -> false)
      | _ -> false)
  | Texp_tuple exp_list | Texp_array exp_list ->
      List.for_all (is_simplified env) exp_list
  | Texp_record { fields; _ } ->
      Array.for_all
        (fun (_, rl_def) ->
          match rl_def with
          | Overridden (_, expr) -> is_simplified env expr
          | Kept _ -> failwith "kept in record")
        fields
  | Texp_ident _ -> (
      match get_desc e.exp_type with Tvar _ -> true | _ -> false)
  | Texp_function { cases; _ } ->
      List.for_all (fun vc -> is_simplified env vc.c_rhs) cases
  | Texp_constant (Const_int 0)
  | Texp_constant (Const_char '0')
  | Texp_constant (Const_string ("", _, None)) ->
      true
  | _ -> false

let minimize to_remove map cur_name =
  let nth_expr = ref (-1) in
  let is_removed = ref false in
  let rec reduce_def_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          incr nth_expr;
          if to_remove = !nth_expr && not (is_simplified e.exp_env e) then (
            is_removed := true;
            generate_dummy_expr e.exp_env e.exp_type)
          else Tast_mapper.default.expr mapper e);
    }
  in
  let nstr =
    reduce_def_mapper.structure reduce_def_mapper (Smap.find cur_name map)
  in
  (Smap.add cur_name nstr map, !is_removed)

(*let minimizer = {
  minimizer_name = "reduce-expr-typesafe" ;
  minimizer_func = minimize
  }*)
