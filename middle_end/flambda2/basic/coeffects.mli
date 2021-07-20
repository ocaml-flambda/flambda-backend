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

(** Things that the world does to a primitive application. *)
type t =
  | No_coeffects
  (** "No coeffects" means that the primitive does not observe the effects (in
      the sense described above) of other expressions. For example, it must not
      read from any mutable storage or call arbitrary external functions.

      It is assumed in Flambda that, subject to data dependencies,
      expressions with neither effects nor coeffects may be reordered with
      respect to other expressions. *)
  | Has_coeffects
  (** The primitive may be affected by effects from other expressions. *)

val print : Format.formatter -> t -> unit
(** Print function. *)

val compare : t -> t -> int
(** Comparison function. *)

val join : t -> t -> t
(** Join two coeffects. *)


