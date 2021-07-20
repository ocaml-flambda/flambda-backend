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

module K = Flambda_kind

module Result_continuation = struct
  type t =
    | Return of Continuation.t
    | Never_returns

  include Container_types.Make(struct
      type nonrec t = t

      let compare h1 h2 =
        match h1, h2 with
        | Return k1, Return k2 -> Continuation.compare k1 k2
        | Never_returns, Never_returns -> 0
        | Return _, Never_returns -> 1
        | Never_returns, Return _ -> -1

      let equal h1 h2 = compare h1 h2 = 0

      let hash = function
        | Return k -> Continuation.hash k
        | Never_returns as h -> Hashtbl.hash h

      let print fmt = function
        | Return k -> Continuation.print fmt k
        | Never_returns -> Format.fprintf fmt "∅"

      let output ch h =
        print (Format.formatter_of_out_channel ch) h
    end)

  let free_names = function
    | Return k -> Name_occurrences.singleton_continuation k
    | Never_returns -> Name_occurrences.empty

  let apply_renaming t perm =
    match t with
    | Return k -> Return (Renaming.apply_continuation perm k)
    | Never_returns -> Never_returns

  let all_ids_for_export t =
    match t with
    | Return k -> Ids_for_export.singleton_continuation k
    | Never_returns -> Ids_for_export.empty
end

type t = {
  callee : Simple.t;
  continuation : Result_continuation.t;
  exn_continuation : Exn_continuation.t;
  args : Simple.t list;
  call_kind : Call_kind.t;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
  inlining_state : Inlining_state.t;
}

let print ppf { callee; continuation; exn_continuation; args; call_kind;
      dbg; inline; inlining_state; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(%a\u{3008}%a\u{3009}\u{300a}%a\u{300b}@ (%a))@]@ \
      @[<hov 1>(call_kind@ %a)@]@ \
      @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
      @[<hov 1>(inline@ %a)@]@ \
      @[<hov 1>(inlining_state@ %a)@]\
      )@]"
    Simple.print callee
    Result_continuation.print continuation
    Exn_continuation.print exn_continuation
    Simple.List.print args
    Call_kind.print call_kind
    (Flambda_colours.debuginfo ())
    Debuginfo.print_compact dbg
    (Flambda_colours.normal ())
    Inline_attribute.print inline
    Inlining_state.print inlining_state

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env
      ({ callee;
        continuation;
        exn_continuation;
        args;
        call_kind;
        dbg;
        inline;
        inlining_state;
      } as t) =
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print t
    in
    let module E = Invariant_env in
    Call_kind.invariant env call_kind;
(*
    let stack = E.current_continuation_stack env in
*)
    E.check_simple_is_bound_and_of_kind env callee K.value;
    begin match call_kind with
    | Function (Direct { code_id = _; closure_id = _; return_arity = _; }) ->
      (* Note that [return_arity] is checked for all the cases below. *)
      E.check_simples_are_bound env args
    | Function Indirect_unknown_arity ->
      E.check_simples_are_bound_and_of_kind env args K.value
    | Function (Indirect_known_arity { param_arity; return_arity = _; }) ->
      ignore (param_arity : Flambda_arity.With_subkinds.t);
      E.check_simples_are_bound env args
    | Method { kind; obj; } ->
      ignore (kind : Call_kind.method_kind);
      E.check_simple_is_bound_and_of_kind env obj K.value;
      E.check_simples_are_bound_and_of_kind env args K.value
    | C_call { alloc = _; param_arity = _; return_arity = _; } ->
      (* CR mshinwell: Check exactly what Cmmgen can compile and then
         add further checks on [param_arity] and [return_arity] *)
      if not (Simple.is_symbol callee) then begin
        (* CR-someday mshinwell: We could expose indirect C calls at the
           source language level. *)
        Misc.fatal_errorf "For [C_call] applications the callee must be \
            directly specified as a [Symbol]:@ %a"
          print t
      end
    end;
    begin match continuation with
    | Never_returns ->
      begin match Call_kind.return_arity call_kind with
      | [] -> ()
      | a ->
        Misc.fatal_errorf "This [Apply] never returns and so expects an empty \
                           arity, but has a call kind arity of %a:@ %a"
          Flambda_arity.With_subkinds.print a print t
      end
    (* general case *)
    | Return continuation ->
      begin match E.find_continuation_opt env continuation with
      | None ->
        unbound_continuation continuation "[Apply] term"
      | Some (arity, kind (*, cont_stack *)) ->
        begin match kind with
        | Normal -> ()
        | Exn_handler ->
          Misc.fatal_errorf "Continuation %a is an exception handler \
                             but is used in this [Apply] term as a \
                             return continuation:@ %a"
            Continuation.print continuation
            print t
        end;
        let expected_arity = Call_kind.return_arity call_kind in
        if not (Flambda_arity.With_subkinds.compatible expected_arity
          ~when_used_at:arity)
        then begin
          Misc.fatal_errorf "Continuation %a called with wrong arity in \
                             this [Apply] term: expected %a but used at %a:@ %a"
            Continuation.print continuation
            Flambda_arity.With_subkinds.print expected_arity
            Flambda_arity.With_subkinds.print arity
            print t
        end (*;
              E.Continuation_stack.unify continuation stack cont_stack *)
      end;
      begin match
        E.find_continuation_opt env
          (Exn_continuation.exn_handler exn_continuation)
      with
      | None ->
        unbound_continuation continuation
          "[Apply] term exception continuation"
      | Some (arity, kind (*, cont_stack *)) ->
        begin match kind with
        | Normal ->
          Misc.fatal_errorf "Continuation %a is a normal continuation \
                             but is used in this [Apply] term as an exception handler:@ %a"
            Continuation.print continuation
            print t
        | Exn_handler -> ()
        end;
        let expected_arity = [Flambda_kind.With_subkind.any_value] in
        if not (Flambda_arity.With_subkinds.equal arity expected_arity)
        then begin
          Misc.fatal_errorf "Exception continuation %a named in this \
                             [Apply] term has the wrong arity: expected %a but have %a:@ %a"
            Continuation.print continuation
            Flambda_arity.With_subkinds.print expected_arity
            Flambda_arity.With_subkinds.print arity
            print t
        end (*;
              E.Continuation_stack.unify exn_continuation stack cont_stack *)
      end;
      ignore (dbg : Debuginfo.t);
      ignore (inline : Inline_attribute.t);
      Inlining_state.invariant inlining_state;
    end

let create ~callee ~continuation exn_continuation ~args ~call_kind dbg ~inline
      ~inlining_state =
  (* CR mshinwell: We should still be able to check some of the invariant
     properties now.  (We can't do them all as we don't have the
     environment.) *)
  { callee;
    continuation;
    exn_continuation;
    args;
    call_kind;
    dbg;
    inline;
    inlining_state;
  }

let callee t = t.callee
let continuation t = t.continuation
let exn_continuation t = t.exn_continuation
let args t = t.args
let call_kind t = t.call_kind
let dbg t = t.dbg
let inline t = t.inline
let inlining_state t = t.inlining_state

let free_names
      { callee;
        continuation;
        exn_continuation;
        args;
        call_kind;
        dbg = _;
        inline = _;
        inlining_state = _;
      } =
  Name_occurrences.union_list [
    Simple.free_names callee;
    Result_continuation.free_names continuation;
    Exn_continuation.free_names exn_continuation;
    Simple.List.free_names args;
    Call_kind.free_names call_kind;
  ]

let apply_renaming
      ({ callee;
         continuation;
         exn_continuation;
         args;
         call_kind;
         dbg;
         inline;
         inlining_state;
      } as t)
      perm =
  let continuation' =
    Result_continuation.apply_renaming continuation perm
  in
  let exn_continuation' =
    Exn_continuation.apply_renaming exn_continuation perm
  in
  let callee' = Simple.apply_renaming callee perm in
  let args' = Simple.List.apply_renaming args perm in
  let call_kind' = Call_kind.apply_renaming call_kind perm in
  if continuation == continuation'
    && exn_continuation == exn_continuation'
    && callee == callee'
    && args == args'
    && call_kind == call_kind'
  then
    t
  else
    { callee = callee';
      continuation = continuation';
      exn_continuation = exn_continuation';
      args = args';
      call_kind = call_kind';
      dbg;
      inline;
      inlining_state;
    }

let all_ids_for_export
      { callee;
        continuation;
        exn_continuation;
        args;
        call_kind;
        dbg = _;
        inline = _;
        inlining_state = _;
      } =
  let callee_ids = (Ids_for_export.from_simple callee) in
  let callee_and_args_ids =
    List.fold_left (fun ids arg ->
        Ids_for_export.add_simple ids arg)
      callee_ids
      args
  in
  let call_kind_ids = Call_kind.all_ids_for_export call_kind in
  let result_continuation_ids =
    Result_continuation.all_ids_for_export continuation
  in
  let exn_continuation_ids =
    Exn_continuation.all_ids_for_export exn_continuation
  in
  Ids_for_export.union
    (Ids_for_export.union callee_and_args_ids call_kind_ids)
    (Ids_for_export.union result_continuation_ids exn_continuation_ids)

let with_continuation t continuation =
  { t with continuation; }

let with_continuations t continuation exn_continuation =
  { t with
    continuation;
    exn_continuation;
  }

let with_exn_continuation t exn_continuation =
  { t with exn_continuation; }

let with_call_kind t call_kind =
  { t with call_kind; }

let with_args t args =
  { t with args; }

let with_continuation_callee_and_args t continuation ~callee ~args =
  { t with
    continuation;
    callee;
    args;
  }
let inlining_arguments t =
  inlining_state t |> Inlining_state.arguments
