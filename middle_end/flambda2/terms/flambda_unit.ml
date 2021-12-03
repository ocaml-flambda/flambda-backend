(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

type t =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    body : Flambda.Expr.t;
    module_symbol : Symbol.t;
    used_closure_vars : Var_within_closure.Set.t Or_unknown.t
  }

let create ~return_continuation ~exn_continuation ~body ~module_symbol
    ~used_closure_vars =
  { return_continuation;
    exn_continuation;
    body;
    module_symbol;
    used_closure_vars
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let body t = t.body

let module_symbol t = t.module_symbol

let used_closure_vars t = t.used_closure_vars

let [@ocamlformat "disable"] print ppf
      { return_continuation; exn_continuation; body; module_symbol;
        used_closure_vars;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(module_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(used_closure_vars@ %a)@]@ \
        @[<hov 1>%a@]\
      )@]"
    Symbol.print module_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    (Or_unknown.print Var_within_closure.Set.print) used_closure_vars
    Flambda.Expr.print body

let apply_renaming
    { return_continuation;
      exn_continuation;
      body;
      module_symbol;
      used_closure_vars
    } perm =
  let body = Expr.apply_renaming body perm in
  let module_symbol = Renaming.apply_symbol perm module_symbol in
  let return_continuation =
    Renaming.apply_continuation perm return_continuation
  in
  let exn_continuation = Renaming.apply_continuation perm exn_continuation in
  create ~return_continuation ~exn_continuation ~body ~module_symbol
    ~used_closure_vars

let permute_everything t =
  (* Only symbols (and code_ids) from the current compilation unit, and that are
     not the module symbol can be safely permuted *)
  let current_comp_unit = Compilation_unit.get_current_exn () in
  let ids = Expr.all_ids_for_export t.body in
  let perm = Renaming.empty in
  let perm =
    Symbol.Set.fold
      (fun symbol perm ->
        if Symbol.in_compilation_unit symbol current_comp_unit
           && not (Symbol.equal t.module_symbol symbol)
        then
          let guaranteed_fresh = Symbol.rename symbol in
          Renaming.add_fresh_symbol perm symbol ~guaranteed_fresh
        else perm)
      ids.symbols perm
  in
  let perm =
    Code_id.Set.fold
      (fun code_id perm ->
        if Code_id.in_compilation_unit code_id current_comp_unit
        then
          let guaranteed_fresh = Code_id.rename code_id in
          Renaming.add_fresh_code_id perm code_id ~guaranteed_fresh
        else perm)
      ids.code_ids perm
  in
  let perm =
    Variable.Set.fold
      (fun variable perm ->
        let guaranteed_fresh = Variable.rename variable in
        Renaming.add_fresh_variable perm variable ~guaranteed_fresh)
      ids.variables perm
  in
  let perm =
    Continuation.Set.fold
      (fun k perm ->
        let guaranteed_fresh = Continuation.rename k in
        Renaming.add_fresh_continuation perm k ~guaranteed_fresh)
      ids.continuations perm
  in
  apply_renaming t perm

(* Iter on all sets of closures of a given program. *)
(* CR mshinwell: These functions should be pushed directly into [Flambda] *)
module Iter = struct
  let rec expr f_c f_s e =
    match (Expr.descr e : Expr.descr) with
    | Let e' -> let_expr f_c f_s e'
    | Let_cont e' -> let_cont f_c f_s e'
    | Apply e' -> apply_expr f_c f_s e'
    | Apply_cont e' -> apply_cont f_c f_s e'
    | Switch e' -> switch f_c f_s e'
    | Invalid e' -> invalid f_c f_s e'

  and named let_expr (bound_pattern : Bound_pattern.t) f_c f_s n =
    match (n : Named.t) with
    | Simple _ | Prim _ | Rec_info _ -> ()
    | Set_of_closures s ->
      let is_phantom =
        Name_mode.is_phantom (Bound_pattern.name_mode bound_pattern)
      in
      f_s ~closure_symbols:None ~is_phantom s
    | Static_consts consts -> (
      match bound_pattern with
      | Symbols { bound_symbols; _ } ->
        static_consts f_c f_s bound_symbols consts
      | Singleton _ | Set_of_closures _ ->
        Misc.fatal_errorf "[Static_const] can only be bound to [Symbols]:@ %a"
          Let.print let_expr)

  and let_expr f_c f_s t =
    Let.pattern_match t ~f:(fun bound_pattern ~body ->
        let e = Let.defining_expr t in
        named t bound_pattern f_c f_s e;
        expr f_c f_s body)

  and let_cont f_c f_s (let_cont : Flambda.Let_cont.t) =
    match let_cont with
    | Non_recursive { handler; _ } ->
      Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
          let h = Non_recursive_let_cont_handler.handler handler in
          let_cont_aux f_c f_s k h body)
    | Recursive handlers ->
      Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
          assert (not (Continuation_handlers.contains_exn_handler conts));
          let_cont_rec f_c f_s conts body)

  and let_cont_aux f_c f_s k h body =
    continuation_handler f_c f_s k h;
    expr f_c f_s body

  and let_cont_rec f_c f_s conts body =
    let map = Continuation_handlers.to_map conts in
    Continuation.Map.iter (continuation_handler f_c f_s) map;
    expr f_c f_s body

  and continuation_handler f_c f_s _ h =
    Continuation_handler.pattern_match h ~f:(fun _ ~handler ->
        expr f_c f_s handler)

  (* Expression application, continuation application and Switches only use
     single expressions and continuations, so no sets_of_closures can
     syntatically appear inside. *)
  and apply_expr _ _ _ = ()

  and apply_cont _ _ _ = ()

  and switch _ _ _ = ()

  and invalid _ _ _ = ()

  and static_consts f_c f_s bound_symbols static_consts =
    Static_const_group.match_against_bound_symbols static_consts bound_symbols
      ~init:()
      ~code:(fun () code_id (code : Code.t) ->
        f_c ~id:code_id (Some code);
        let params_and_body = Code.params_and_body code in
        Function_params_and_body.pattern_match params_and_body
          ~f:(fun
               ~return_continuation:_
               ~exn_continuation:_
               _
               ~body
               ~my_closure:_
               ~is_my_closure_used:_
               ~my_depth:_
               ~free_names_of_body:_
             -> expr f_c f_s body))
      ~deleted_code:(fun () code_id -> f_c ~id:code_id None)
      ~set_of_closures:(fun () ~closure_symbols set_of_closures ->
        f_s ~closure_symbols:(Some closure_symbols) ~is_phantom:false
          set_of_closures)
      ~block_like:(fun () _ _ -> ())
end

let ignore_code ~id:_ _ = ()

let ignore_set_of_closures ~closure_symbols:_ ~is_phantom:_ _ = ()

let iter ?(code = ignore_code) ?(set_of_closures = ignore_set_of_closures) t =
  Iter.expr code set_of_closures t.body
