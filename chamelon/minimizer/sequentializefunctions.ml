(* Minimizer seq-fun : (__dummy__ ()) a1 a2 ... -> ignore a1 ; ignore a2 ; ... ; (__dummy__ ())  *)

open Utils
open Typedtree
open Tast_mapper
open Dummy
open Stdlib
open Asttypes
open Compat

let is_ignore e =
  match view_texp e.exp_desc with
  | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__ignore__"
  | _ -> false

let remove_double_ignore =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        match view_texp e.exp_desc with
        | Texp_apply (f, ae_l, _) ->
            if is_ignore f && List.length ae_l = 1 then
              let _, arg = List.hd ae_l in
              match option_of_arg_or_omitted arg with
              | Some (e1, _) -> (
                  match view_texp e1.exp_desc with
                  | Texp_apply (f2, ae_l2, id) ->
                      if is_ignore f2 then
                        mapper.expr mapper
                          { e with exp_desc = mkTexp_apply ~id (f, ae_l2) }
                      else Tast_mapper.default.expr mapper e
                  | _ -> Tast_mapper.default.expr mapper e)
              | _ -> Tast_mapper.default.expr mapper e
            else Tast_mapper.default.expr mapper e
        | _ -> Tast_mapper.default.expr mapper e);
  }

let is_dummy e =
  match view_texp e.exp_desc with
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__dummy2__"
      | _ -> false)
  | _ -> false

let make_sequence exp_list =
  List.fold_left
    (fun res e -> mkTexp_sequence (exp_desc_to_exp e, exp_desc_to_exp res))
    apply_dummy2.exp_desc exp_list

let seq_function_mapper should_remove =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        Tast_mapper.default.expr mapper
          (match view_texp e.exp_desc with
          | Texp_apply (f, ae_l, _) ->
              if is_dummy f && should_remove () then
                {
                  e with
                  exp_desc =
                    make_sequence
                      (List.fold_left
                         (fun l (_, eo) ->
                           fold_arg_or_omitted
                             (fun l (e, id) ->
                               mkTexp_apply
                                 (Dummy.ignore, [ (Nolabel, mkArg ~id e) ])
                               :: l)
                             l eo)
                         [] ae_l);
                }
              else e
          | _ -> e));
  }

let minimize should_remove map cur_name =
  let mapper = seq_function_mapper should_remove in
  let nstr = mapper.structure mapper (Smap.find cur_name map) in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "sequentialize-functions"; minimizer_func = minimize }
