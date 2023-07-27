(* simp-match : simplify pattern matchings with a unique pattern *)

open Utils
open Typedtree
open Tast_mapper
open Compat

(** [replace_mapper id to_replace] is a mapper replacing every occurence of [id]
  by the expression [to_replace]*)
let replace_mapper id to_replace =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then
              { e with exp_desc = to_replace }
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
  }

let minimize should_remove map cur_name =
  let simplify_match_mapper =
    (* match e1 with x -> e2 => e2[x->e1]*)
    (* We could add the transf. match e1 with p -> e2 => let p = e1 in e2 *)
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match view_texp e.exp_desc with
            | Texp_match (e_match, cc_l, _, _) ->
                Format.print_flush ();
                if List.length cc_l = 1 then
                  let cc = List.hd cc_l in
                  match (cc.c_lhs.pat_desc, cc.c_guard) with
                  | Tpat_value tva, None -> (
                      match
                        view_tpat (tva :> value general_pattern).pat_desc
                      with
                      | Tpat_var (id, _, _) ->
                          if should_remove () then
                            let rep_map = replace_mapper id e_match.exp_desc in
                            rep_map.expr rep_map cc.c_rhs
                          else e
                      | _ -> e)
                  | _ -> e
                else e
            | _ -> e));
    }
  in
  let nstr =
    simplify_match_mapper.structure simplify_match_mapper
      (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer = { minimizer_name = "simplify-match"; minimizer_func = minimize }
