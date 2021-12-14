(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

type binding_time = t

include Container_types.S with type t := t

val consts_and_discriminants : t

val symbols : t

val imported_variables : t

val earliest_var : t

val succ : t -> t

val strictly_earlier : t -> than:t -> bool

val equal : t -> t -> bool

module With_name_mode : sig
  type t = private int

  val consts_and_discriminants : t

  val symbols : t

  val imported_variables : t

  val create : binding_time -> Name_mode.t -> t

  val binding_time : t -> binding_time

  val name_mode : t -> Name_mode.t

  val scoped_name_mode : t -> min_binding_time:binding_time -> Name_mode.t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end
