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

module Result_continuation = struct
  type t =
    | Return of Continuation.t
    | Never_returns

  include Container_types.Make (struct
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

    let [@ocamlformat "disable"] print fmt = function
      | Return k -> Continuation.print fmt k
      | Never_returns -> Format.fprintf fmt "∅"

    let output ch h = print (Format.formatter_of_out_channel ch) h
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

type t =
  { callee : Simple.t;
    continuation : Result_continuation.t;
    exn_continuation : Exn_continuation.t;
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    inlining_state : Inlining_state.t;
    probe_name : string option
  }

let [@ocamlformat "disable"] print ppf { callee; continuation; exn_continuation; args; call_kind;
      dbg; inline; inlining_state; probe_name; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(%a\u{3008}%a\u{3009}\u{300a}%a\u{300b}@ (%a))@]@ \
      @[<hov 1>(call_kind@ %a)@]@ \
      @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
      @[<hov 1>(inline@ %a)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      @[<hov 1>(probe_name@ %a)@]\
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
    (fun ppf probe_name ->
      match probe_name with
      | None -> Format.pp_print_string ppf "()"
      | Some probe_name -> Format.pp_print_string ppf probe_name)
    probe_name

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

let invariant
    ({ callee;
       continuation;
       exn_continuation = _;
       args = _;
       call_kind;
       dbg = _;
       inline = _;
       inlining_state = _;
       probe_name = _
     } as t) =
  begin
    match call_kind with
    | Function _ | Method _ -> ()
    | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ }
      ->
      if not (Simple.is_symbol callee)
      then
        (* CR-someday mshinwell: We could expose indirect C calls at the source
           language level. *)
        Misc.fatal_errorf
          "For [C_call] applications the callee must be directly specified as \
           a [Symbol]:@ %a"
          print t
  end;
  match continuation with
  | Never_returns -> begin
    match Call_kind.return_arity call_kind with
    | [] -> ()
    | a ->
      Misc.fatal_errorf
        "This [Apply] never returns and so expects an empty arity, but has a \
         call kind arity of %a:@ %a"
        Flambda_arity.With_subkinds.print a print t
  end
  | Return _ -> ()

let create ~callee ~continuation exn_continuation ~args ~call_kind dbg ~inline
    ~inlining_state ~probe_name =
  let t =
    { callee;
      continuation;
      exn_continuation;
      args;
      call_kind;
      dbg;
      inline;
      inlining_state;
      probe_name
    }
  in
  invariant t;
  t

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
      probe_name = _
    } =
  Name_occurrences.union_list
    [ Simple.free_names callee;
      Result_continuation.free_names continuation;
      Exn_continuation.free_names exn_continuation;
      Simple.List.free_names args;
      Call_kind.free_names call_kind ]

let apply_renaming
    ({ callee;
       continuation;
       exn_continuation;
       args;
       call_kind;
       dbg;
       inline;
       inlining_state;
       probe_name
     } as t) perm =
  let continuation' = Result_continuation.apply_renaming continuation perm in
  let exn_continuation' =
    Exn_continuation.apply_renaming exn_continuation perm
  in
  let callee' = Simple.apply_renaming callee perm in
  let args' = Simple.List.apply_renaming args perm in
  let call_kind' = Call_kind.apply_renaming call_kind perm in
  if continuation == continuation'
     && exn_continuation == exn_continuation'
     && callee == callee' && args == args' && call_kind == call_kind'
  then t
  else
    { callee = callee';
      continuation = continuation';
      exn_continuation = exn_continuation';
      args = args';
      call_kind = call_kind';
      dbg;
      inline;
      inlining_state;
      probe_name
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
      probe_name = _
    } =
  let callee_ids = Ids_for_export.from_simple callee in
  let callee_and_args_ids =
    List.fold_left
      (fun ids arg -> Ids_for_export.add_simple ids arg)
      callee_ids args
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

let with_continuation t continuation = { t with continuation }

let with_continuations t continuation exn_continuation =
  { t with continuation; exn_continuation }

let with_exn_continuation t exn_continuation = { t with exn_continuation }

let with_call_kind t call_kind =
  let t = { t with call_kind } in
  invariant t;
  t

let with_args t args = { t with args }

let with_continuation_callee_and_args t continuation ~callee ~args =
  let t = { t with continuation; callee; args } in
  invariant t;
  t

let inlining_arguments t = inlining_state t |> Inlining_state.arguments

let probe_name t = t.probe_name
