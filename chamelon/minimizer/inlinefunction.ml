(* Minimizer inl-fun : inline functions*)

open Utils
open Tast_mapper
open Inlinenever
open Typedtree
open Types
open Dummy
open Compat

let find_function id1 str =
  let fun_body = ref dummy1 in
  let non_rec = ref false in
  let mapper =
    {
      Tast_mapper.default with
      structure_item =
        (fun _ str_it ->
          match str_it.str_desc with
          | Tstr_value (Nonrecursive, vb_l) ->
              let vb1 =
                List.filter
                  (fun vb ->
                    match view_tpat vb.vb_pat.pat_desc with
                    | Tpat_var (id2, _, _) -> Ident.same id1 id2
                    | _ -> false)
                  vb_l
              in
              if vb1 <> [] then (
                fun_body := (List.hd vb1).vb_expr;
                non_rec := true);
              str_it
          | _ -> str_it);
      expr =
        (fun mapper e ->
          match e.exp_desc with
          | Texp_let (_, vb_l, e) ->
              let vb1 =
                List.filter
                  (fun vb ->
                    match view_tpat vb.vb_pat.pat_desc with
                    | Tpat_var (id2, _, _) -> Ident.same id1 id2
                    | _ -> false)
                  vb_l
              in
              fun_body := (List.hd vb1).vb_expr;
              if vb1 <> [] then fun_body := (List.hd vb1).vb_expr;
              Tast_mapper.default.expr mapper e
          | _ -> Tast_mapper.default.expr mapper e);
    }
  in
  let _ = mapper.structure mapper str in
  (!fun_body.exp_desc, !non_rec)

let minimize should_remove map cur_name =
  let str = Smap.find cur_name map in
  let inline_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match view_texp e.exp_desc with
            | Texp_ident (Pident id, _, vd, _) ->
                if is_function vd.val_type then
                  let body, non_rec = find_function id str in
                  if non_rec && should_remove () then { e with exp_desc = body }
                  else e
                else e
            | _ -> e));
    }
  in
  let nstr = inline_mapper.structure inline_mapper str in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "inline-function"; minimizer_func = minimize }
