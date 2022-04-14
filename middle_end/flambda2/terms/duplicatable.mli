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

(** Whether a primitive should be considered for duplication *)
type t =
  | Duplicatable  (** It's acceptable to duplicate the primitive at its uses *)
  | Not_duplicatable
      (** The primitive has effects or coeffects, or is too expensive *)

(** Print function. *)
val print : Format.formatter -> t -> unit

(** Comparison function. *)
val compare : t -> t -> int

(** Join *)
val join : t -> t -> t
