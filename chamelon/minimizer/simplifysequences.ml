(* Minimizer simp_seq: simplify () ; _ sequences *)

open Utils
open Typedtree
open Tast_mapper
open Stdlib
open Compat

exception Not_implemented

let is_dummy e =
  match view_texp e.exp_desc with
  | Texp_apply (d, _, _) -> (
      match view_texp d.exp_desc with
      | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__dummy2__"
      | _ -> false)
  | _ -> false

let is_ignore e =
  match view_texp e.exp_desc with
  | Texp_ident (_, name, _, _) -> Longident.last name.txt = "__ignore__"
  | _ -> false

let is_sequence e = match e.exp_desc with Texp_sequence _ -> true | _ -> false

let break_sequence e =
  match view_texp e.exp_desc with
  | Texp_sequence (e1, e2, _) -> (e1, e2)
  | _ -> failwith "Argument is not a sequence"

let remove_dummy_mapper should_remove =
  {
    Tast_mapper.default with
    expr =
      (fun mapper e ->
        Tast_mapper.default.expr mapper
          (match view_texp e.exp_desc with
          | Texp_sequence (e1, e2, _) ->
              if is_dummy e2 && should_remove () then e1 else e
          | _ -> e));
  }

let minimize should_remove map cur_name =
  let simp_seq_mapper =
    {
      Tast_mapper.default with
      expr =
        (fun mapper e ->
          Tast_mapper.default.expr mapper
            (match view_texp e.exp_desc with
            | Texp_sequence (e1, e2, _) ->
                if (Removeunit.is_unit e1 || is_dummy e1) && should_remove ()
                then e2
                  (* else if is_sequence e1 then
                     let a, b = break_sequence e1 in
                       { e with
                         exp_desc =
                           Texp_sequence(a, Layouts.Layout.value,{ e2 with exp_desc = Texp_sequence(b,Layouts.Layout.value,e2)})
                       } *)
                else e
                  (*
                     {e with exp_desc = Texp_sequence(e1, Layouts.Layout.value, e2) } *)
            | O (Texp_ifthenelse (e1, e2, Some e3)) ->
                if
                  Reduceexpr.is_simplified e1
                  && (Removeunit.is_unit e2 || Removeunit.is_unit e3)
                  && should_remove ()
                then
                  if Removeunit.is_unit e2 then e3
                  else { e with exp_desc = Texp_ifthenelse (e1, e2, None) }
                else e
            | Texp_apply (f, ael, id) ->
                if is_dummy f then
                  {
                    e with
                    exp_desc =
                      mkTexp_apply ~id
                        ( f,
                          List.rev
                            (List.fold_left
                               (fun l (a, e) ->
                                 fold_arg_or_omitted
                                   (fun l -> function
                                    | Targ_expr (e1, _) as arg ->
                                      if
                                        Reduceexpr.is_simplified e1
                                        && should_remove ()
                                      then l
                                      else (a, mkArg arg) :: l
                                    | arg -> (a, mkArg arg) :: l)
                                   l e)
                               [] ael) );
                  }
                else if is_ignore f then
                  {
                    e with
                    exp_desc =
                      mkTexp_apply ~id
                        ( f,
                          List.rev
                            (List.fold_left
                               (fun l (a, e) ->
                                 fold_arg_or_omitted
                                   (fun l -> function
                                    | Targ_expr (e1, id) ->
                                      if is_sequence e1 then
                                        let rdm =
                                          remove_dummy_mapper should_remove
                                        in
                                        (a, mkExpArg ~id (rdm.expr rdm e1)) :: l
                                      else l
                                    | arg -> (a, mkArg arg) :: l)
                                   l e)
                               [] ael) );
                  }
                else e
            | _ -> e));
    }
  in
  let nstr =
    simp_seq_mapper.structure simp_seq_mapper (Smap.find cur_name map)
  in
  Smap.add cur_name nstr map

let minimizer =
  { minimizer_name = "simplify-sequences"; minimizer_func = minimize }
