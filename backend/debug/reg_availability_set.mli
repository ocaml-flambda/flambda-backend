(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Register availability sets. *)

type t =
  | Ok of Reg_with_debug_info.Set.t
  | Unreachable

(** See comments in the .ml file about these functions. *)

val of_list : Reg_with_debug_info.t list -> t

val union : t -> t -> t

val inter : t -> t -> t

val diff : t -> t -> t

(** This returns the initial value in the [Unreachable] case *)
val fold : (Reg_with_debug_info.t -> 'a -> 'a) -> t -> 'a -> 'a

(** Return a subset of the given availability set which contains no registers
    that are not associated with debug info (and holding values of
    non-persistent identifiers); and where no two registers share the same
    location. *)
val canonicalise : t -> t

val equal : t -> t -> bool

(** For debugging purposes only. *)
val print :
  print_reg:(Format.formatter -> Reg.t -> unit) -> Format.formatter -> t -> unit
