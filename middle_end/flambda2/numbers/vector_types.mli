(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** SIMD vector numeric type layouts. *)

module Kind : sig
  type t =
    | Vec128
    | Vec256
    | Vec512
end

module Vec128 : sig
  module Bit_pattern : sig
    (** 128-bit value whose comparison and equality relations are lexicographically
      ordered by bit pattern. *)

    include Container_types.S

    val zero : t

    type bits =
      { word0 : int64; (* Least significant *)
        word1 : int64
      }

    val to_bits : t -> bits

    val of_bits : bits -> t
  end
end

module Vec256 : sig
  module Bit_pattern : sig
    (** 256-bit value whose comparison and equality relations are lexicographically
      ordered by bit pattern. *)

    include Container_types.S

    val zero : t

    type bits =
      { word0 : int64; (* Least significant *)
        word1 : int64;
        word2 : int64;
        word3 : int64
      }

    val to_bits : t -> bits

    val of_bits : bits -> t
  end
end

module Vec512 : sig
  module Bit_pattern : sig
    (** 512-bit value whose comparison and equality relations are lexicographically
      ordered by bit pattern. *)

    include Container_types.S

    val zero : t

    type bits =
      { word0 : int64; (* Least significant *)
        word1 : int64;
        word2 : int64;
        word3 : int64;
        word4 : int64;
        word5 : int64;
        word6 : int64;
        word7 : int64
      }

    val to_bits : t -> bits

    val of_bits : bits -> t
  end
end
