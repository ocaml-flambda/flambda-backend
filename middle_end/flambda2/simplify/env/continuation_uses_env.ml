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

module T = Flambda2_types

type t = { continuation_uses : Continuation_uses.t Continuation.Map.t }

let [@ocamlformat "disable"] print ppf { continuation_uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation_uses@ %a)@]\
      )@]"
    (Continuation.Map.print Continuation_uses.print) continuation_uses

let empty = { continuation_uses = Continuation.Map.empty }

let add_continuation_use t cont kind ~id ~env_at_use ~arg_types =
  let use = One_continuation_use.create kind ~env_at_use id ~arg_types in
  let continuation_uses =
    Continuation.Map.update cont
      (function
        | None ->
          let arity = T.arity_of_list arg_types in
          let uses = Continuation_uses.create cont arity in
          Some (Continuation_uses.add_use uses use)
        | Some uses -> Some (Continuation_uses.add_use uses use))
      t.continuation_uses
  in
  { continuation_uses }

let record_continuation_use t cont kind ~env_at_use ~arg_types =
  let id = Apply_cont_rewrite_id.create () in
  let t = add_continuation_use t cont kind ~id ~env_at_use ~arg_types in
  t, id

let get_typing_env_no_more_than_one_use t k =
  match Continuation.Map.find k t.continuation_uses with
  | exception Not_found -> None
  | cont_uses -> Continuation_uses.get_typing_env_no_more_than_one_use cont_uses

let get_continuation_uses t cont =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found -> None
  | uses -> Some uses

let num_continuation_uses t cont =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found -> 0
  | uses -> Continuation_uses.number_of_uses uses

let all_continuations_used t = Continuation.Map.keys t.continuation_uses

let union t1 t2 =
  let continuation_uses =
    Continuation.Map.union
      (fun _ uses1 uses2 -> Some (Continuation_uses.union uses1 uses2))
      t1.continuation_uses t2.continuation_uses
  in
  { continuation_uses }

let remove t cont =
  { continuation_uses = Continuation.Map.remove cont t.continuation_uses }

let delete_continuation_uses = remove

let clear_continuation_uses t cont =
  { continuation_uses =
      Continuation.Map.update cont
        (Option.map Continuation_uses.clear_uses)
        t.continuation_uses
  }

let mark_non_inlinable { continuation_uses } =
  let continuation_uses =
    Continuation.Map.map Continuation_uses.mark_non_inlinable continuation_uses
  in
  { continuation_uses }
