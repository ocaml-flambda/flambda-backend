(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generic module for handling permutations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (N : Container_types.S) : sig
  type t

  val empty : t

  val print : Format.formatter -> t -> unit

  val is_empty : t -> bool

  val apply : t -> N.t -> N.t

  val compose_one : first:t -> N.t -> N.t -> t

  val compose_one_fresh : t -> N.t -> fresh:N.t -> t

  val compose : second:t -> first:t -> t
end
