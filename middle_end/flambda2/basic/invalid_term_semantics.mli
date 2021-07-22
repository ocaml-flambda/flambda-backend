(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** What the optimizer should do when it reaches a term that is known to be
    invalid (for example because it is not type correct).  In all cases, code
    _after_ invalid code will be deleted. *)

type t =
  | Treat_as_unreachable
  (** Invalid code should be treated as unreachable and thus deleted.  The
      unreachability property may be propagated backwards through the term
      possibly causing other parts to be deleted. *)
  | Halt_and_catch_fire
  (** Invalid code should be replaced by an abort trap.  No back-propagation
      is performed. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int
