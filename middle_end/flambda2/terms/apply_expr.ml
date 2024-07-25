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
  end)

  let free_names = function
    | Return k -> Name_occurrences.singleton_continuation k
    | Never_returns -> Name_occurrences.empty

  let apply_renaming t renaming =
    match t with
    | Return k -> Return (Renaming.apply_continuation renaming k)
    | Never_returns -> Never_returns

  let ids_for_export t =
    match t with
    | Return k -> Ids_for_export.singleton_continuation k
    | Never_returns -> Ids_for_export.empty
end

module Position = struct
  type t =
    | Normal
    | Nontail

  let equal t1 t2 =
    match t1, t2 with
    | Normal, Normal -> true
    | Normal, Nontail -> false
    | Nontail, Normal -> false
    | Nontail, Nontail -> true
end

type t =
  { callee : Simple.t option;
    continuation : Result_continuation.t;
    exn_continuation : Exn_continuation.t;
    args : Simple.t list;
    args_arity : [`Complex] Flambda_arity.t;
    return_arity : [`Unarized] Flambda_arity.t;
    call_kind : Call_kind.t;
    (* CR mshinwell: we could move the [alloc_mode] out of [Call_kind] into
       here *)
    dbg : Debuginfo.t;
    inlined : Inlined_attribute.t;
    inlining_state : Inlining_state.t;
    probe : Probe.t;
    tail : Tail.t;
    position : Position.t;
    relative_history : Inlining_history.Relative.t
  }

let [@ocamlformat "disable"] print_inlining_paths ppf relative_history =
  if !Flambda_backend_flags.dump_inlining_paths then
    Format.fprintf ppf "@[<hov 1>(relative_history@ %a)@]@ "
      Inlining_history.Relative.print relative_history

let [@ocamlformat "disable"] print_normal ppf
    { callee; continuation; exn_continuation; args; args_arity;
      return_arity; call_kind; dbg; inlined; inlining_state; probe;
      tail = tail; position; relative_history } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(%a\u{3008}%a\u{3009}\u{300a}%a\u{300b}\
      (%a))@]@ \
      @[<hov 1>(args_arity@ %a)@]@ \
      @[<hov 1>(return_arity@ %a)@]@ \
      @[<hov 1>(call_kind@ %a)@]@ \
      @[<hov 1>%t(dbg@ %a)%t@]@ \
      @[<hov 1>(inline@ %a)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      %a\
      @[<hov 1>(probe@ %a)@]@ \
      @[<hov 1>(tail@ %a)@]@ \
      @[<hov 1>(position@ %a)@]\
      )@]"
    (Misc.Stdlib.Option.print Simple.print) callee
    Result_continuation.print continuation
    Exn_continuation.print exn_continuation
    Simple.List.print args
    Flambda_arity.print args_arity
    Flambda_arity.print return_arity
    Call_kind.print call_kind
    Flambda_colours.debuginfo
    Debuginfo.print_compact dbg
    Flambda_colours.pop
    Inlined_attribute.print inlined
    Inlining_state.print inlining_state
    print_inlining_paths relative_history
    Probe.print probe
    Tail.print tail
    (fun ppf position ->
       match position with
       | Position.Normal -> Format.pp_print_string ppf "Normal"
       | Position.Nontail -> Format.pp_print_string ppf "Nontail")
    position

let [@ocamlformat "disable"] print_effect ppf
    { callee = _; continuation; exn_continuation; args = _; args_arity = _;
      return_arity = _; call_kind; dbg; inlined = _; inlining_state = _;
      probe = _; position; relative_history = _; tail = _ } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>%a@]@ \
      @[<hov 1>\u{3008}%a\u{3009}\u{300a}%a\u{300b}@]@ \
      @[<hov 1>%t(dbg@ %a)%t@]@ \
      @[<hov 1>(position@ %a)@]\
      )@]"
    Call_kind.print call_kind
    Result_continuation.print continuation
    Exn_continuation.print exn_continuation
    Flambda_colours.debuginfo
    Debuginfo.print_compact dbg
    Flambda_colours.pop
    (fun ppf position ->
       match position with
       | Position.Normal -> Format.pp_print_string ppf "Normal"
       | Position.Nontail -> Format.pp_print_string ppf "Nontail")
    position

let print ppf t =
  match t.call_kind with
  | Function _ | Method _ | C_call _ -> print_normal ppf t
  | Effect _ -> print_effect ppf t

let invariant
    ({ callee;
       continuation = _;
       exn_continuation = _;
       args;
       args_arity;
       return_arity;
       call_kind;
       dbg = _;
       inlined = _;
       inlining_state = _;
       probe = _;
       tail = _;
       position = _;
       relative_history = _
     } as t) =
  (match callee with
  | Some _ -> ()
  | None -> (
    match[@ocaml.warning "-fragile-match"] call_kind with
    | Function { function_call = Direct _; _ } | Effect _ -> ()
    | _ -> Misc.fatal_errorf "Missing callee:@ %a" print t));
  (match call_kind with
  | Function _ | Method _ -> ()
  | C_call _ -> (
    (match callee with
    | Some callee when Simple.is_symbol callee -> ()
    | None | Some _ ->
      (* CR-someday mshinwell: We could expose indirect C calls at the source
         language level. *)
      Misc.fatal_errorf
        "For [C_call] applications the callee must be directly specified as a \
         [Symbol]:@ %a"
        print t);
    match Flambda_arity.unarized_components return_arity with
    | [] | [_] -> ()
    | _ :: _ :: _ ->
      Misc.fatal_errorf "Illegal return arity for C call:@ %a"
        Flambda_arity.print return_arity)
  | Effect _ -> (
    match callee, args with
    | None, [] -> ()
    | Some _, [] | (None | Some _), _ :: _ ->
      Misc.fatal_errorf
        "Algebraic effect operations in [Apply_expr] must have no callee and \
         no arguments; all data are specified in the [Call_kind]:@ %a"
        print t));
  if List.compare_lengths args (Flambda_arity.unarize args_arity) <> 0
  then
    Misc.fatal_errorf
      "Length of argument and arity lists disagree in [Apply]:@ %a" print t

let create ~callee ~continuation exn_continuation ~args ~args_arity
    ~return_arity ~(call_kind : Call_kind.t) dbg ~inlined ~inlining_state ~probe
    ~tail ~position ~relative_history =
  let t =
    { callee;
      continuation;
      exn_continuation;
      args;
      args_arity;
      return_arity;
      call_kind;
      dbg;
      inlined;
      inlining_state;
      probe;
      tail;
      position;
      relative_history
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

let inlined t = t.inlined

let inlining_state t = t.inlining_state

let relative_history t = t.relative_history

let position t = t.position

let free_names_without_exn_continuation
    { callee;
      continuation;
      exn_continuation = _;
      args;
      args_arity = _;
      return_arity = _;
      call_kind;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      tail = _;
      position = _;
      relative_history = _
    } =
  Name_occurrences.union_list
    [ (match callee with
      | None -> Name_occurrences.empty
      | Some callee -> Simple.free_names callee);
      Result_continuation.free_names continuation;
      Simple.List.free_names args;
      Call_kind.free_names call_kind ]

let free_names_except_callee
    { callee = _;
      continuation;
      exn_continuation;
      args;
      args_arity = _;
      return_arity = _;
      call_kind;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      tail = _;
      position = _;
      relative_history = _
    } =
  Name_occurrences.union_list
    [ Result_continuation.free_names continuation;
      Exn_continuation.free_names exn_continuation;
      Simple.List.free_names args;
      Call_kind.free_names call_kind ]

let free_names t =
  Name_occurrences.union
    (match t.callee with
    | None -> Name_occurrences.empty
    | Some callee -> Simple.free_names callee)
    (free_names_except_callee t)

let apply_renaming
    ({ callee;
       continuation;
       exn_continuation;
       args;
       args_arity;
       return_arity;
       call_kind;
       dbg;
       inlined;
       inlining_state;
       probe;
       tail;
       position;
       relative_history
     } as t) renaming =
  let continuation' =
    Result_continuation.apply_renaming continuation renaming
  in
  let exn_continuation' =
    Exn_continuation.apply_renaming exn_continuation renaming
  in
  let callee' =
    match callee with
    | None -> None
    | Some orig_callee ->
      let new_callee = Simple.apply_renaming orig_callee renaming in
      if orig_callee == new_callee then callee else Some new_callee
  in
  let args' = Simple.List.apply_renaming args renaming in
  let call_kind' = Call_kind.apply_renaming call_kind renaming in
  if continuation == continuation'
     && exn_continuation == exn_continuation'
     && callee == callee' && args == args' && call_kind == call_kind'
  then t
  else
    { callee = callee';
      continuation = continuation';
      exn_continuation = exn_continuation';
      args = args';
      args_arity;
      return_arity;
      call_kind = call_kind';
      dbg;
      inlined;
      inlining_state;
      probe;
      tail;
      position;
      relative_history
    }

let ids_for_export
    { callee;
      continuation;
      exn_continuation;
      args;
      args_arity = _;
      return_arity = _;
      call_kind;
      dbg = _;
      inlined = _;
      inlining_state = _;
      probe = _;
      tail = _;
      position = _;
      relative_history = _
    } =
  let callee_ids =
    match callee with
    | None -> Ids_for_export.empty
    | Some callee -> Ids_for_export.from_simple callee
  in
  let callee_and_args_ids =
    List.fold_left
      (fun ids arg -> Ids_for_export.add_simple ids arg)
      callee_ids args
  in
  let call_kind_ids = Call_kind.ids_for_export call_kind in
  let result_continuation_ids =
    Result_continuation.ids_for_export continuation
  in
  let exn_continuation_ids = Exn_continuation.ids_for_export exn_continuation in
  Ids_for_export.union
    (Ids_for_export.union callee_and_args_ids call_kind_ids)
    (Ids_for_export.union result_continuation_ids exn_continuation_ids)

let erase_callee t = { t with callee = None }

let with_continuation t continuation = { t with continuation }

let with_continuations t continuation exn_continuation =
  { t with continuation; exn_continuation }

let with_exn_continuation t exn_continuation = { t with exn_continuation }

let with_call_kind t call_kind =
  let t = { t with call_kind } in
  invariant t;
  t

let with_args t args ~args_arity = { t with args; args_arity }

let inlining_arguments t = inlining_state t |> Inlining_state.arguments

let probe t = t.probe

let tail t = t.tail

let returns t =
  match continuation t with Return _ -> true | Never_returns -> false

let args_arity t = t.args_arity

let return_arity t = t.return_arity

let with_inlined_attribute t inlined = { t with inlined }
