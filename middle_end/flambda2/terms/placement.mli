(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Whether an expression can be moved around, including duplication *)
type t =
  | Delay
      (** The expression should be placed as late as possible, even if it is
          duplicated *)
  | Strict
      (** The expression must not be moved around (it has non-generative
          effects, or coeffects, or doesn't benefit from being bound later *)

(** Print function. *)
val print : Format.formatter -> t -> unit

(** Comparison function. *)
val compare : t -> t -> int

(** Join *)
val join : t -> t -> t
