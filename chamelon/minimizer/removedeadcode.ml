(* Minimizer inl-never: remove unit-typed expressions *)

open Utils
open Types
open Stdlib
open Typedtree
open Tast_mapper
open List
open Compat

exception Not_implemented

(* 1) Remove unused variables
   2) Remove unused exceptions
   3) Remove unused types
   4) Remove unused modules *)

let search_in_str is_used id =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        (match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then is_used := true
        | _ -> ());
        if not !is_used then Tast_mapper.default.expr mapper e else e);
  }

let is_used_var str id =
  let is_used = ref false in
  let mapper = search_in_str is_used id in
  ignore (mapper.structure mapper str);
  !is_used

let is_used_typ str typ_name =
  let is_used = ref false in
  let search_in_str =
    {
      Tast_mapper.default with
      typ =
        (fun mapper ct ->
          if is_type_name_used ct.ctyp_desc typ_name then is_used := true;
          if not !is_used then Tast_mapper.default.typ mapper ct else ct);
      expr =
        (fun mapper e ->
          (match get_desc e.exp_type with
          | Tconstr (p, _, _) -> if Path.last p = typ_name then is_used := true
          | _ -> ());
          if not !is_used then Tast_mapper.default.expr mapper e else e);
    }
  in
  ignore (search_in_str.structure search_in_str str);
  !is_used

let is_used_exc str exc_name =
  let is_used = ref false in
  let search_in_str =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          (match view_texp e.exp_desc with
          | Texp_construct (name, _, _, _) ->
              if Longident.last name.txt = exc_name then is_used := true
          | _ -> ());
          if not !is_used then Tast_mapper.default.expr mapper e else e);
    }
  in
  ignore (search_in_str.structure search_in_str str);
  !is_used

(* extract the list of var in the pattern, so we can remove them*)
let rec var_from_pat pat_desc acc =
  match view_tpat pat_desc with
  | Tpat_var (id, _, _) -> id :: acc
  | Tpat_alias (pat, id, _, _) -> var_from_pat pat.pat_desc (id :: acc)
  | Tpat_tuple (vl, _) | Tpat_array (vl, _) | O (Tpat_construct (_, _, vl, _))
    ->
      List.fold_left (fun l pat -> var_from_pat pat.pat_desc l) acc vl
  | O (Tpat_record (r, _)) ->
      List.fold_left (fun l (_, _, pat) -> var_from_pat pat.pat_desc l) acc r
  | O (Tpat_or (p1, p2, _)) ->
      var_from_pat p1.pat_desc (var_from_pat p2.pat_desc acc)
  | O (Tpat_lazy pat) -> var_from_pat pat.pat_desc acc
  | O (Tpat_any | Tpat_constant _ | Tpat_variant _) -> []
  | O (Tpat_var _ | Tpat_alias _ | Tpat_array _ | Tpat_tuple _) -> assert false

let rec rem_in_pat str pat should_remove =
  match view_tpat pat.pat_desc with
  | Tpat_var (id, _, _) ->
      let is_used = is_used_var str id in
      if (not is_used) && should_remove () then { pat with pat_desc = Tpat_any }
      else pat
  | Tpat_alias (pat1, id, a, p_id) ->
      let is_used = is_used_var str id in
      if (not is_used) && should_remove () then
        rem_in_pat str pat1 should_remove
      else
        {
          pat with
          pat_desc =
            mkTpat_alias ~id:p_id (rem_in_pat str pat1 should_remove, id, a);
        }
  | Tpat_tuple (vl, id) ->
      {
        pat with
        pat_desc =
          mkTpat_tuple ~id
            (List.map (fun pat -> rem_in_pat str pat should_remove) vl);
      }
  | Tpat_array (vl, id) ->
      {
        pat with
        pat_desc =
          mkTpat_array ~id
            (List.map (fun pat -> rem_in_pat str pat should_remove) vl);
      }
  | O (Tpat_construct (a1, a2, vl, a3)) ->
      {
        pat with
        pat_desc =
          Tpat_construct
            ( a1,
              a2,
              List.map (fun pat -> rem_in_pat str pat should_remove) vl,
              a3 );
      }
  | O (Tpat_record (r, a1)) ->
      {
        pat with
        pat_desc =
          Tpat_record
            ( List.map
                (fun (e1, e2, pat) ->
                  (e1, e2, rem_in_pat str pat should_remove))
                r,
              a1 );
      }
  | O (Tpat_or (p1, p2, a1)) ->
      let p1 = rem_in_pat str p1 should_remove in
      let p2 = rem_in_pat str p2 should_remove in
      { pat with pat_desc = Tpat_or (p1, p2, a1) }
  | O (Tpat_lazy pat) ->
      { pat with pat_desc = Tpat_lazy (rem_in_pat str pat should_remove) }
  | O (Tpat_any | Tpat_constant _ | Tpat_variant _) -> pat
  | O (Tpat_var _ | Tpat_alias _ | Tpat_array _ | Tpat_tuple _) -> assert false

let minimize should_remove map cur_name =
  let cur_str = Smap.find cur_name map in
  let remove_dead_mapper =
    {
      Tast_mapper.default with
      structure =
        (fun mapper str ->
          Tast_mapper.default.structure mapper
            {
              str with
              str_items =
                List.rev
                  (List.fold_left
                     (fun l str_it ->
                       match str_it.str_desc with
                       | Tstr_value (r_f, vb_l) ->
                           let vb_l =
                             List.rev
                             @@ List.fold_left
                                  (fun (l : value_binding list) vb ->
                                    let npat =
                                      rem_in_pat str vb.vb_pat should_remove
                                    in
                                    if var_from_pat npat.pat_desc [] = [] then l
                                    else { vb with vb_pat = npat } :: l)
                                  [] vb_l
                           in
                           if vb_l = [] then l
                           else
                             { str_it with str_desc = Tstr_value (r_f, vb_l) }
                             :: l
                       | Tstr_primitive vd ->
                           let is_used = is_used_var str vd.val_id in
                           if (not is_used) && should_remove () then l
                           else str_it :: l
                       | Tstr_exception te ->
                           let is_used =
                             is_used_exc str te.tyexn_constructor.ext_name.txt
                           in
                           if (not is_used) && should_remove () then l
                           else str_it :: l
                       | Tstr_type (rf, tdl) ->
                           let nl =
                             List.rev
                               (List.fold_left
                                  (fun l td ->
                                    let is_used =
                                      is_used_typ str td.typ_name.txt
                                    in
                                    if (not is_used) && should_remove () then l
                                    else td :: l)
                                  [] tdl)
                           in
                           if nl = [] then l
                           else
                             { str_it with str_desc = Tstr_type (rf, nl) } :: l
                       | _ -> str_it :: l)
                     [] str.str_items);
            });
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match e.exp_desc with
            | Texp_let (rf, vb_l, e1) ->
                let nvb_l =
                  List.rev
                    (List.fold_left
                       (fun (l : value_binding list) vb ->
                         let npat =
                           rem_in_pat cur_str vb.vb_pat should_remove
                         in
                         if var_from_pat npat.pat_desc [] = [] then l
                         else { vb with vb_pat = npat } :: l)
                       [] vb_l)
                in
                if nvb_l = [] then e1
                else { e with exp_desc = Texp_let (rf, nvb_l, e1) }
            | _ -> e));
    }
  in
  let nstr = remove_dead_mapper.structure remove_dead_mapper cur_str in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "remove-dead-code"; minimizer_func = minimize }
