(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Make_set (_ : sig
  val print : Format.formatter -> int -> unit
end) : sig
  include Container_types.Set
    with module T := Numeric_types.Int
end

module Make_map (_ : sig
  val print : Format.formatter -> int -> unit
end) (Set : Container_types.Set with module T := Numeric_types.Int) : sig
  include Container_types.Map
    with module T := Numeric_types.Int
    with module Set = Set
end
