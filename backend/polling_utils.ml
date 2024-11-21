(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Xavier Leroy and Damien Doligez, projet Cambium, INRIA Paris      *)
(*               Sadiq Jaffer, OCaml Labs Consultancy Ltd                 *)
(*          Stephen Dolan and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module String = Misc.Stdlib.String

let function_is_assumed_to_never_poll func =
  String.begins_with ~prefix:"caml_apply" func
  || String.begins_with ~prefix:"caml_send" func

let is_disabled fun_name =
  (not Config.poll_insertion)
  || !Flambda_backend_flags.disable_poll_insertion
  || function_is_assumed_to_never_poll fun_name

(* These are used for the poll error annotation later on*)
type polling_point =
  | Alloc
  | Poll
  | Function_call
  | External_call

type error = Poll_error of Debuginfo.t * (polling_point * Debuginfo.t) list

exception Error of error

(* "Might_not_poll" means there exists a path from the function entry to a
   Potentially Recursive Tail Call (an Itailcall_ind or Itailcall_imm to a
   forward function) that does not go through an Ialloc or Ipoll instruction.

   "Always_polls", therefore, means the function always polls (via Ialloc or
   Ipoll) before doing a PRTC. *)

type polls_before_prtc =
  | Might_not_poll
  | Always_polls

module Polls_before_prtc = struct
  type t = polls_before_prtc

  let bot = Always_polls

  let join t1 t2 =
    match t1, t2 with
    | Might_not_poll, Might_not_poll
    | Might_not_poll, Always_polls
    | Always_polls, Might_not_poll ->
      Might_not_poll
    | Always_polls, Always_polls -> Always_polls

  let lessequal t1 t2 =
    match t1, t2 with
    | Always_polls, Always_polls
    | Always_polls, Might_not_poll
    | Might_not_poll, Might_not_poll ->
      true
    | Might_not_poll, Always_polls -> false
end
