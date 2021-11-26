(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let () =
  match Targetint_32_64.num_bits with
  | Sixty_four -> ()
  | Thirty_two ->
    if Flambda_features.flambda2_is_enabled ()
    then Misc.fatal_error "Flambda 2 does not yet support 32-bit compilation"

(* CR mshinwell/gbury: maybe we might want to consider adding some more checks
   in some of the conversions functions to be more safe and more consistent in
   the handling of overflows ? For instance One_bit_fewer.of_int silently
   truncates the input int to make it fit, whereas we probably want to make it
   produce an error ? *)

module Imm = struct
  module T0 = struct
    include Numeric_types.Int64

    (* CR mshinwell/gbury: Do these need specialising to int64? *)

    let compare = compare

    let equal = equal

    let hash = hash

    let print = print

    let minus_one = -1L

    let zero = 0L

    let one = 1L

    let ten = 10L

    let hex_ff = 0xffL

    let min_value = Int64.min_int

    let max_value = Int64.max_int

    let sub = Int64.sub

    let neg = Int64.neg

    let shift_left = Int64.shift_left

    let shift_right = Int64.shift_right

    let shift_right_logical = Int64.shift_right_logical

    let xor = Int64.logxor

    let or_ = Int64.logor

    let and_ = Int64.logand

    let mod_ = Int64.rem

    let div = Int64.div

    let mul = Int64.mul

    let add = Int64.add

    let bottom_byte_to_int t = Int64.to_int (Int64.logand t hex_ff)

    let of_char c = Int64.of_int (Char.code c)

    let of_int = Int64.of_int

    let to_int = Int64.to_int

    let of_int_option i = Some (of_int i)

    let of_int32 t = Int64.of_int32 t

    let to_int32 t = Int64.to_int32 t

    let of_int64 t = t

    let to_int64 t = t

    let of_float = Int64.of_float

    let to_float = Int64.to_float

    let to_targetint = Targetint_32_64.of_int64

    let of_targetint = Targetint_32_64.to_int64

    let max_array_length = Int64.sub (Int64.shift_left 1L 54) 1L

    let max_string_length = Int64.sub (Int64.mul 8L max_array_length) 1L

    let max t1 t2 = if Int64.compare t1 t2 < 0 then t2 else t1

    let min t1 t2 = if Int64.compare t1 t2 < 0 then t1 else t2

    let ( <= ) t1 t2 = Stdlib.( <= ) (compare t1 t2) 0

    let ( < ) t1 t2 = Stdlib.( < ) (compare t1 t2) 0

    let to_int_option t =
      let min_int_as_int64 = Int64.of_int Stdlib.min_int in
      let max_int_as_int64 = Int64.of_int Stdlib.max_int in
      if min_int_as_int64 <= t && t <= max_int_as_int64
      then Some (to_int t)
      else None

    let to_int_exn t =
      match to_int_option t with
      | Some i -> i
      | None ->
        Misc.fatal_errorf "Targetint_31_63.Imm.to_int_exn: %Ld out of range" t

    let get_least_significant_16_bits_then_byte_swap t =
      let least_significant_byte = Int64.logand t 0xffL in
      let second_to_least_significant_byte =
        Int64.shift_right_logical (Int64.logand t 0xff00L) 8
      in
      Int64.logor second_to_least_significant_byte
        (Int64.shift_left least_significant_byte 8)
  end

  include One_bit_fewer.Make (T0)
  include Container_types.Make (T0)

  let to_string t = Format.asprintf "%a" print t
end

type 'a or_wrong =
  | Ok of 'a
  | Wrong

type t =
  { value : Imm.t;
    print_as_char : bool
  }

type immediate = t

module T0 = struct
  type nonrec t = t

  let compare t1 t2 = Imm.compare t1.value t2.value

  let equal t1 t2 = compare t1 t2 = 0

  let hash t = Imm.hash t.value

  let [@ocamlformat "disable"] print ppf t =
    let print_as_char =
      t.print_as_char
      && Imm.compare t.value Imm.zero >= 0
      && Imm.compare t.value Imm.hex_ff <= 0
    in
    if print_as_char then
      Format.fprintf ppf "%C" (Char.chr (Imm.bottom_byte_to_int t.value))
    else Format.fprintf ppf "%a" Imm.print t.value
end

module Self = Container_types.Make (T0)
include Self
module Lmap = Lmap.Make (T0)

module Pair = struct
  include
    Container_types.Make_pair
      (struct
        type nonrec t = t

        include Self
      end)
      (struct
        type nonrec t = t

        include Self
      end)

  type nonrec t = t * t
end

let cross_product = Pair.create_from_cross_product

let join t1 t2 : t or_wrong =
  if not (Imm.equal t1.value t2.value)
  then Wrong
  else
    let print_as_char = t1.print_as_char && t2.print_as_char in
    Ok { value = t1.value; print_as_char }

let join_set t1s t2s =
  let only_in_t2s = Set.diff t2s t1s in
  let join =
    Set.fold
      (fun t1 result ->
        match Set.find t1 t2s with
        | exception Not_found -> Set.add t1 result
        | t2 -> (
          match join t1 t2 with Wrong -> result | Ok t -> Set.add t result))
      t1s Set.empty
  in
  Set.union join only_in_t2s

let bool_true = { value = Imm.one; print_as_char = false }

let bool_false = { value = Imm.zero; print_as_char = false }

let bool b = if b then bool_true else bool_false

let int value = { value; print_as_char = false }

let char value = { value = Imm.of_char value; print_as_char = true }

let to_targetint t = t.value

let to_targetint' t = Imm.to_targetint t.value

let map t ~f = { value = f t.value; print_as_char = t.print_as_char }

let is_non_negative t = Imm.compare t.value Imm.zero >= 0

let set_to_targetint_set (set : Set.t) : Imm.Set.t =
  Set.fold
    (fun t targetints -> Imm.Set.add t.value targetints)
    set Imm.Set.empty

let set_to_targetint_set' (set : Set.t) : Targetint_32_64.Set.t =
  Set.fold
    (fun t targetints ->
      Targetint_32_64.Set.add (Imm.to_targetint t.value) targetints)
    set Targetint_32_64.Set.empty

let all_bools = Set.of_list [bool_true; bool_false]

let zero_one_and_minus_one =
  Set.of_list [int Imm.zero; int Imm.one; int Imm.minus_one]

let map_value1 f t = { t with value = f t.value }

let map_value2 f t0 t1 =
  { value = f t0.value t1.value;
    print_as_char = t0.print_as_char && t1.print_as_char
  }

let map_value2' f t i = { t with value = f t.value i }

let get_least_significant_16_bits_then_byte_swap =
  map_value1 Imm.get_least_significant_16_bits_then_byte_swap

let shift_right_logical = map_value2' Imm.shift_right_logical

let shift_right = map_value2' Imm.shift_right

let shift_left = map_value2' Imm.shift_left

let xor = map_value2 Imm.xor

let or_ = map_value2 Imm.or_

let and_ = map_value2 Imm.and_

let mod_ = map_value2 Imm.mod_

let div = map_value2 Imm.div

let mul = map_value2 Imm.mul

let sub = map_value2 Imm.sub

let add = map_value2 Imm.add

let neg = map_value1 Imm.neg

let minus_one = int Imm.minus_one

let zero = int Imm.zero

let one = int Imm.one
