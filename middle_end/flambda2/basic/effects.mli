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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Things that a primitive does to the world, or expects from the world. *)

type t =
  | No_effects
  (** The primitive does not change the observable state of the world. For
      example, it must not write to any mutable storage, call arbitrary external
      functions or change control flow (e.g. by raising an exception). Note that
      allocation is not "No effects" (see below).

      It is assumed in Flambda that applications of primitives with no
      effects, whose results are not used, may be eliminated.  It is further
      assumed that applications of primitives with no effects may be
      duplicated (and thus possibly executed more than once).

      Exceptions arising from allocation points, for example "out of memory" or
      exceptions propagated from finalizers or signal handlers, are treated as
      "effects out of the ether" and thus ignored for our determination here
      of effectfulness.  The same goes for floating point operations that may
      cause hardware traps on some platforms. *)
  | Only_generative_effects of Mutability.t
  (** The primitive does not change the observable state of the world save for
      possibly affecting the state of the garbage collector by performing an
      allocation. Applications of primitives that only have generative effects
      and whose results are unused may be eliminated by the compiler. However,
      unlike "No effects" primitives, such applications will never be eligible
      for duplication.
      The argument to [Only_generative_effects] states whether the returned
      value from the primitive is mutable. *)
  | Arbitrary_effects
  (** The primitive may have effects beyond those described by [No_effects]
      and [Only_generative_effects]. *)

val print : Format.formatter -> t -> unit
(** Printing function. *)

val compare : t -> t -> int
(** Comparison function. *)

val join : t -> t -> t
(** join two effects, effectively computing the maximum of the two
    given effects. *)
