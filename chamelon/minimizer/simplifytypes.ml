(* Minimizer simp-typ : simplify types by removing constructors or labels *)

open Utils
open Dummy
open Stdlib
open Typedtree
open Tast_mapper
open List
open Types
open Path
open Compat

exception Not_implemented

let is_same_typ t tname =
  match get_desc t with
  | Tconstr (path, _, _) -> Path.same path tname
  | _ -> false

let remove_cons_mapper (cons_to_rem, cons_typ) =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        if not (is_same_typ e.exp_type cons_typ) then
          Tast_mapper.default.expr mapper e
        else
          match view_texp e.exp_desc with
          | Texp_construct (_, cd, _, _) ->
              if cons_to_rem = cd.cstr_name then
                { e with exp_desc = apply_dummy2.exp_desc }
              else Tast_mapper.default.expr mapper e
          | O (Texp_record record) ->
              let lab_list = Array.to_seq record.fields in
              let nlab_list =
                Seq.filter (fun (ld, _) -> ld.lbl_name = cons_to_rem) lab_list
              in
              {
                e with
                exp_desc =
                  (if Seq.is_empty nlab_list then apply_dummy2.exp_desc
                  else
                    Texp_record { record with fields = Array.of_seq nlab_list });
              }
          | Texp_function (f, id) ->
              let f_as_cases = Function_compat.function_to_cases_view f in
              {
                e with
                exp_desc =
                  mkTexp_function ~id
                    (Function_compat.cases_view_to_function
                       {
                         f_as_cases with
                         cases =
                           (let l =
                              List.fold_left
                                (fun l val_case ->
                                  match val_case.c_lhs.pat_desc with
                                  | Tpat_construct (_, cd, _, _) ->
                                      if cons_to_rem = cd.cstr_name then l
                                      else
                                        Tast_mapper.default.case mapper val_case
                                        :: l
                                  | Tpat_record (lab_list, flag) ->
                                      let nlab_list =
                                        List.filter
                                          (fun (_, ld, _) ->
                                            ld.lbl_name = cons_to_rem)
                                          lab_list
                                      in
                                      if nlab_list = [] then l
                                      else
                                        {
                                          val_case with
                                          c_lhs =
                                            {
                                              val_case.c_lhs with
                                              pat_desc =
                                                Tpat_record (nlab_list, flag);
                                            };
                                        }
                                        :: l
                                  | _ ->
                                      Tast_mapper.default.case mapper val_case
                                      :: l)
                                [] f_as_cases.cases
                            in
                            if l = [] then [ empty_value_case ] else l);
                       });
              }
          | Texp_match (e, comp_case_l, p, id) ->
              {
                e with
                exp_desc =
                  mkTexp_match ~id
                    ( mapper.expr mapper e,
                      (let l =
                         List.fold_left
                           (fun l comp_case ->
                             match comp_case.c_lhs.pat_desc with
                             | Tpat_value tva -> (
                                 match
                                   (tva :> value general_pattern).pat_desc
                                 with
                                 | Tpat_construct (_, cd, _, _) ->
                                     if cons_to_rem = cd.cstr_name then l
                                     else
                                       Tast_mapper.default.case mapper comp_case
                                       :: l
                                 | Tpat_record (lab_list, flag) ->
                                     let nlab_list =
                                       List.filter
                                         (fun (_, ld, _) ->
                                           ld.lbl_name = cons_to_rem)
                                         lab_list
                                     in
                                     if nlab_list = [] then l
                                     else
                                       {
                                         comp_case with
                                         c_lhs =
                                           as_computation_pattern
                                             {
                                               comp_case.c_lhs with
                                               pat_desc =
                                                 Tpat_record (nlab_list, flag);
                                             };
                                       }
                                       :: l
                                 | _ ->
                                     Tast_mapper.default.case mapper comp_case
                                     :: l)
                             | _ ->
                                 Tast_mapper.default.case mapper comp_case :: l)
                           [] comp_case_l
                       in
                       if l = [] then [ empty_computation_case ] else l),
                      p );
              }
          | _ -> Tast_mapper.default.expr mapper e);
  }

let minimize should_remove map cur_name =
  let name_to_remove =
    ref ("not_a_cons", Pident (Ident.create_local "not_a_cons"))
  in
  let search_cons_mapper =
    {
      Tast_mapper.default with
      structure_item =
        (fun _ str_it ->
          {
            str_it with
            str_desc =
              (match str_it.str_desc with
              | Tstr_type (rf, list_decls) ->
                  let nlist_decls =
                    List.fold_left
                      (fun l_td type_decl ->
                        {
                          type_decl with
                          typ_kind =
                            (match type_decl.typ_kind with
                            | Ttype_variant cons_decl ->
                                let ncons_decl =
                                  List.fold_left
                                    (fun l_cd cd ->
                                      if should_remove () then (
                                        name_to_remove :=
                                          ( cd.cd_name.txt,
                                            Pident type_decl.typ_id );
                                        l_cd)
                                      else cd :: l_cd)
                                    [] cons_decl
                                in
                                Ttype_variant ncons_decl
                            | Ttype_record ld_list ->
                                let nld_list =
                                  List.fold_left
                                    (fun l_ld ld ->
                                      if should_remove () then (
                                        name_to_remove :=
                                          ( ld.ld_name.txt,
                                            Pident type_decl.typ_id );
                                        l_ld)
                                      else ld :: l_ld)
                                    [] ld_list
                                in
                                Ttype_record nld_list
                            | _ -> type_decl.typ_kind);
                        }
                        :: l_td)
                      [] list_decls
                  in
                  Tstr_type (rf, nlist_decls)
              | _ -> str_it.str_desc);
          });
    }
  in
  (* FIXME ncourant: there should be several names to remove *)
  let nstr =
    search_cons_mapper.structure search_cons_mapper (Smap.find cur_name map)
  in
  let nstr =
    (remove_cons_mapper !name_to_remove).structure
      (remove_cons_mapper !name_to_remove)
      nstr
  in
  (* Replacing in multifiles *)
  let cons, typ = !name_to_remove in
  let name_clr = String.sub cur_name 0 (String.length cur_name - 1) in
  let typ_mf =
    Path.Pdot (Pident (Ident.create_local name_clr), Path.name typ)
  in
  let mapper_mf = remove_cons_mapper (cons, typ_mf) in
  let nmap =
    Smap.map (mapper_mf.structure mapper_mf) (Smap.add cur_name nstr map)
  in
  (* Final result*)
  nmap

let minimizer = { minimizer_name = "simplify-types"; minimizer_func = minimize }
