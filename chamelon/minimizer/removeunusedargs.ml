(* Minimizer rem-arg : remove unused arguments in functions *)

open Utils
open Typedtree
open Tast_mapper
open Ident
open Dummy
open Stdlib
open Asttypes
open Compat

exception Not_implemented

let rec fun_wrapper arg_list acc_id depth path_fun n =
  if depth = 0 then
    let n = ref 0 in
    mkTexp_apply
      ( exp_desc_to_exp
          (mkTexp_ident
             ( path_fun,
               { txt = Lident (Path.name path_fun); loc = Location.none },
               dummy_value_description )),
        List.map2
          (fun arg_lab id ->
            incr n;
            ( arg_lab,
              mkArg
                (exp_desc_to_exp
                   (mkTexp_ident
                      ( Pident id,
                        {
                          txt = Lident ("arg" ^ string_of_int !n);
                          loc = Location.none;
                        },
                        dummy_value_description ))) ))
          arg_list acc_id )
  else
    let id_arg = create_local ("arg" ^ string_of_int n) in
    mkTexp_function
      (Function_compat.cases_view_to_function
         {
           arg_label = Nolabel;
           param = id_arg;
           partial = Total;
           optional_default = None;
           cases_view_identifier = Param texp_function_param_identifier_defaults;
           cases =
             [
               {
                 c_lhs =
                   {
                     pat_desc =
                       mkTpat_var
                         ( id_arg,
                           {
                             txt = "arg" ^ string_of_int n;
                             loc = Location.none;
                           } );
                     pat_loc = Location.none;
                     pat_extra = [];
                     pat_env = Env.empty;
                     pat_attributes = [];
                     pat_type = dummy_type_expr;
                     pat_unique_barrier = fresh_unique_barrier ();
                   };
                 c_guard = None;
                 c_rhs =
                   exp_desc_to_exp
                     (fun_wrapper arg_list
                        (if n = 1 then acc_id else id_arg :: acc_id)
                        (depth - 1) path_fun (n + 1));
               };
             ];
         })

let string_label = function
  | Nolabel -> "Nolabel"
  | Optional s -> "Optional " ^ s
  | Labelled s -> "Labelled " ^ s

let replace_mapper id to_replace label =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_ident (path, _, _, _) ->
            if Ident.same (Path.head path) id then
              { e with exp_desc = to_replace }
            else Tast_mapper.default.expr mapper e
        | Texp_apply (e, ae_l, id) ->
            {
              e with
              exp_desc =
                mkTexp_apply ~id
                  ( mapper.expr mapper e,
                    List.rev
                      (List.fold_left
                         (fun l (a, e) ->
                           match (a, label) with
                           | Optional s1, Optional s2 | Labelled s1, Labelled s2
                             ->
                               if s1 = s2 then
                                 ( Nolabel,
                                   map_arg_or_omitted (mapper.expr mapper) e )
                                 :: l
                               else
                                 (a, map_arg_or_omitted (mapper.expr mapper) e)
                                 :: l
                           | _ ->
                               (a, map_arg_or_omitted (mapper.expr mapper) e)
                               :: l)
                         [] ae_l) );
            }
        | _ -> Tast_mapper.default.expr mapper e);
  }

let find_unused_arg_mapper e =
  let depth = ref 0 in
  let arg = ref [] in
  let arg_label = ref Nolabel in
  let is_modified = ref false in
  let mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          match view_texp e.exp_desc with
          | Texp_function (f, f_id) ->
              let ({ cases = vc_l; _ } as f_as_cases
                    : Function_compat.cases_view) =
                Function_compat.function_to_cases_view f
              in
              incr depth;
              if List.length vc_l = 1 && not !is_modified then
                let vc = List.hd vc_l in
                match view_tpat vc.c_lhs.pat_desc with
                | Tpat_var (id, _, _) ->
                    let is_used = ref false in
                    let mapper_used = Removedeadcode.search_in_str is_used id in
                    ignore (mapper_used.expr mapper_used vc.c_rhs);
                    if not !is_used then (
                      is_modified := true;
                      arg_label := f_as_cases.arg_label;
                      vc.c_rhs)
                    else (
                      arg := f_as_cases.arg_label :: !arg;
                      {
                        e with
                        exp_desc =
                          mkTexp_function ~id:f_id
                            (Function_compat.cases_view_to_function
                               {
                                 f_as_cases with
                                 cases =
                                   [
                                     {
                                       vc with
                                       c_rhs = mapper.expr mapper vc.c_rhs;
                                     };
                                   ];
                               });
                      })
                | _ -> e
              else e
          | _ -> e);
    }
  in
  let nfun = mapper.expr mapper e in
  (!is_modified, nfun, !depth, !arg, !arg_label)

let locate_unused_var should_remove to_suppress =
  {
    Tast_mapper.default with
    structure_item =
      (fun mapper str_it ->
        {
          str_it with
          str_desc =
            (match str_it.str_desc with
            | Tstr_value (r, vb_l) ->
                Tstr_value
                  ( r,
                    List.fold_left
                      (fun l vb ->
                        if Inlinenever.is_function vb.vb_expr.exp_type then
                          let is_rep, nfun, depth, arg_list, arg_label =
                            find_unused_arg_mapper vb.vb_expr
                          in
                          if is_rep then
                            match view_tpat vb.vb_pat.pat_desc with
                            | Tpat_var (id, _, _) when should_remove () ->
                                to_suppress :=
                                  (id, depth, arg_list, arg_label)
                                  :: !to_suppress;
                                { vb with vb_expr = nfun } :: l
                            | _ -> vb :: l
                          else vb :: l
                        else vb :: l)
                      [] vb_l )
            | _ -> (Tast_mapper.default.structure_item mapper str_it).str_desc);
        });
    expr =
      (fun mapper e ->
        match e.exp_desc with
        | Texp_let (rf, vb_l, e_in) ->
            {
              e with
              exp_desc =
                Texp_let
                  ( rf,
                    List.fold_left
                      (fun l vb ->
                        if Inlinenever.is_function vb.vb_expr.exp_type then
                          let is_rep, nfun, depth, arg_list, arg_label =
                            find_unused_arg_mapper vb.vb_expr
                          in
                          if is_rep then
                            match view_tpat vb.vb_pat.pat_desc with
                            | Tpat_var (id, _, _) when should_remove () ->
                                to_suppress :=
                                  (id, depth, arg_list, arg_label)
                                  :: !to_suppress;
                                { vb with vb_expr = nfun } :: l
                            | _ -> vb :: l
                          else vb :: l
                        else vb :: l)
                      [] vb_l,
                    e_in );
            }
        | _ -> Tast_mapper.default.expr mapper e);
  }

let minimize should_remove map cur_name =
  let to_suppress = ref [] in
  let mapper = locate_unused_var should_remove to_suppress in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  (* Remove unused arguments*)
  let nstr =
    List.fold_left
      (fun nstr (id_fun, depth, arg_list, arg_label) ->
        let to_replace = fun_wrapper arg_list [] depth (Pident id_fun) 1 in
        let mapper = replace_mapper id_fun to_replace arg_label in
        mapper.structure mapper nstr)
      nstr !to_suppress
  in
  let nmap = Smap.add cur_name nstr map in
  (* Replacing in multifiles *)
  let name_clr = String.sub cur_name 0 (String.length cur_name - 1) in
  let global_replace str =
    List.fold_left
      (fun nstr (id_fun, depth, arg_list, arg_label) ->
        let id_fun_ext =
          Path.Pdot (Pident (Ident.create_local name_clr), Ident.name id_fun)
        in
        let to_replace_mf = fun_wrapper arg_list [] depth id_fun_ext 1 in
        let mapper_mf = replace_mapper id_fun to_replace_mf arg_label in
        mapper_mf.structure mapper_mf nstr)
      str !to_suppress
  in
  let nmap = Smap.map global_replace nmap in
  (* Final result *)
  nmap

let minimizer =
  { minimizer_name = "remove-unused-args"; minimizer_func = minimize }
