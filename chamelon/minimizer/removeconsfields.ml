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

let remove_cons_mapper (i, cons_to_rem, _) =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        Tast_mapper.default.expr mapper
          (match view_texp e.exp_desc with
          | Texp_construct (li, cd, exp_l, id) ->
              if cons_to_rem = cd.cstr_name then
                {
                  e with
                  exp_desc =
                    mkTexp_construct ~id
                      (li, cd, List.filteri (fun j _ -> i = j) exp_l);
                }
              else e
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
                                  | Tpat_construct (li, cd, pat_list, typs) ->
                                      if cons_to_rem = cd.cstr_name then
                                        {
                                          val_case with
                                          c_lhs =
                                            {
                                              val_case.c_lhs with
                                              pat_desc =
                                                Tpat_construct
                                                  ( li,
                                                    cd,
                                                    List.filteri
                                                      (fun j _ -> i = j)
                                                      pat_list,
                                                    typs );
                                            };
                                        }
                                        :: l
                                      else val_case :: l
                                  | _ -> val_case :: l)
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
                    ( e,
                      List.rev
                        (List.fold_left
                           (fun l comp_case ->
                             match comp_case.c_lhs.pat_desc with
                             | Tpat_value tva -> (
                                 match
                                   (tva :> value general_pattern).pat_desc
                                 with
                                 | Tpat_construct (li, cd, pat_list, typs) ->
                                     if cons_to_rem = cd.cstr_name then
                                       {
                                         comp_case with
                                         c_lhs =
                                           as_computation_pattern
                                             {
                                               comp_case.c_lhs with
                                               pat_desc =
                                                 Tpat_construct
                                                   ( li,
                                                     cd,
                                                     List.filteri
                                                       (fun j _ -> i = j)
                                                       pat_list,
                                                     typs );
                                             };
                                       }
                                       :: l
                                     else comp_case :: l
                                 | _ -> comp_case :: l)
                             | _ -> comp_case :: l)
                           [] comp_case_l),
                      p );
              }
          | _ -> Tast_mapper.default.expr mapper e));
  }

let minimize should_remove map cur_name =
  let fields_to_remove = ref [] in
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
                    List.fold_left (* iterate on type declarations *)
                      (fun l_td type_decl ->
                        {
                          type_decl with
                          typ_kind =
                            (match type_decl.typ_kind with
                            | Ttype_variant cons_decl ->
                                let ncons_decl =
                                  List.fold_left
                                    (* iterate on constructor declarations *)
                                      (fun l_cd
                                           (cd :
                                             Typedtree.constructor_declaration) ->
                                      {
                                        cd with
                                        cd_args =
                                          (match cd.cd_args with
                                          | Cstr_tuple arg_list ->
                                              Cstr_tuple
                                                (List.filteri
                                                   (fun i _ ->
                                                     if should_remove () then (
                                                       fields_to_remove :=
                                                         ( i,
                                                           cd.cd_name.txt,
                                                           Pident
                                                             type_decl.typ_id )
                                                         :: !fields_to_remove;
                                                       false)
                                                     else true)
                                                   arg_list)
                                          | _ -> cd.cd_args);
                                      }
                                      :: l_cd)
                                    [] cons_decl
                                in
                                Ttype_variant (List.rev ncons_decl)
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
  let nstr =
    search_cons_mapper.structure search_cons_mapper (Smap.find cur_name map)
  in
  let nstr =
    List.fold_left
      (fun nstr to_rem ->
        (remove_cons_mapper to_rem).structure (remove_cons_mapper to_rem) nstr)
      nstr !fields_to_remove
  in
  (* Replacing in multifiles *)
  let name_clr = String.sub cur_name 0 (String.length cur_name - 3) in
  let mapper str =
    List.fold_left
      (fun nstr (i, cons, typ) ->
        let typ_mf =
          Path.Pdot (Pident (Ident.create_local name_clr), Path.name typ)
        in
        let mapper_mf = remove_cons_mapper (i, cons, typ_mf) in
        mapper_mf.structure mapper_mf nstr)
      str !fields_to_remove
  in
  let nmap = Smap.map mapper (Smap.add cur_name nstr map) in
  (* Final result*)
  nmap

let minimizer =
  { minimizer_name = "remove-cons-fields"; minimizer_func = minimize }
