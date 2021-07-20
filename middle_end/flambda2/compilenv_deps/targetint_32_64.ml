(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type repr =
  | Int32 of int32
  | Int64 of int64

type num_bits =
  | Thirty_two
  | Sixty_four

module type S = sig
  type t
  type targetint = t

  val num_bits : num_bits
  val repr: t -> repr

  include Container_types.S with type t := t

  val zero : t
  val one : t
  val minus_one : t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val min: t -> t -> t
  val max: t -> t -> t

  val get_least_significant_16_bits_then_byte_swap : t -> t

  module Pair : sig
    type nonrec t = t * t
    include Container_types.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  val max_int : t
  val min_int : t

  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val swap_byte_endianness : t -> t

  val of_int_exn : int -> t

  val of_int : int -> t
  val of_int32 : int32 -> t
  val of_int64 : int64 -> t
  val of_float : float -> t
  val of_string : string -> t

  val to_int : t -> int
  val to_int32 : t -> int32
  val to_int64 : t -> int64
  val to_float : t -> float
  val to_string : t -> string

  val unsigned_div : t -> t -> t
  val unsigned_rem : t -> t -> t
  val unsigned_compare : t -> t -> int

  module Targetint_set = Set
end

let size = Sys.word_size
(* Later, this will be set by the configure script
   in order to support cross-compilation. *)

module Int32 = struct
  include Int32

  type targetint = t

  let of_int_exn =
    match Sys.word_size with (* size of [int] *)
    | 32 ->
        Int32.of_int
    | 64 ->
        fun n ->
          if n < Int32.(to_int min_int) || n > Int32.(to_int max_int) then
            Misc.fatal_errorf "Targetint_32_64.of_int_exn: 0x%x out of range" n
          else
            Int32.of_int n
    | _ ->
        assert false

  let num_bits = Thirty_two
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let repr x = Int32 x

  include Container_types.Make (struct
    type nonrec t = t
    let compare = Int32.compare
    let equal = Int32.equal
    let hash = Hashtbl.hash
    let print ppf t = Format.fprintf ppf "%ld" t
    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let min t1 t2 =
    if compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 =
    if compare t1 t2 <= 0 then t2 else t1

  module Targetint_set = Set

  module Pair = struct
    type nonrec t = t * t

    module T_pair = Container_types.Pair (T) (T)

    include Container_types.Make (T_pair)
  end

  let cross_product set1 set2 =
    Set.fold (fun elt1 result ->
        Set.fold (fun elt2 result ->
            Pair.Set.add (elt1, elt2) result)
          set2
          result)
      set1
      Pair.Set.empty

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int32.logand t 0xffl in
    let second_to_least_significant_byte =
      shift_right_logical (Int32.logand t 0xff00l) 8
    in
    Int32.logor second_to_least_significant_byte
      (shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int32"
end

module Int64 = struct
  include Int64

  type targetint = t

  let num_bits = Sixty_four
  let of_int_exn = Int64.of_int
  let of_int64 x = x
  let to_int64 x = x
  let repr x = Int64 x

  let min t1 t2 =
    if compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 =
    if compare t1 t2 <= 0 then t2 else t1

  include Container_types.Make (struct
    type nonrec t = t
    let compare = Int64.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
    let print ppf t = Format.fprintf ppf "%Ld" t
    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  module Targetint_set = Set

  module Pair = struct
    type nonrec t = t * t

    module T_pair = Container_types.Pair (T) (T)

    include Container_types.Make (T_pair)
  end

  let cross_product set1 set2 =
    Set.fold (fun elt1 result ->
        Set.fold (fun elt2 result ->
            Pair.Set.add (elt1, elt2) result)
          set2
          result)
      set1
      Pair.Set.empty

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int64.logand t 0xffL in
    let second_to_least_significant_byte =
      Int64.shift_right_logical (Int64.logand t 0xff00L) 8
    in
    Int64.logor second_to_least_significant_byte
      (Int64.shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int64"
end

include (val
          (match size with
           | 32 -> (module Int32)
           | 64 -> (module Int64)
           | _ -> assert false
          ) : S)
