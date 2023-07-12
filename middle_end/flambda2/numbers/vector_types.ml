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

module type Vector_width = sig
  val size_in_int64s : int
end

module Vector_by_bit_pattern (Width : Vector_width) = struct
  module T0 = struct
    type t = Int64.t Array.t

    let rec compare l r i =
      if i = Width.size_in_int64s
      then 0
      else
        let cmp = Int64.compare l.(i) r.(i) in
        if cmp = 0 then compare l r (i + 1) else cmp

    let compare l r = compare l r 0

    let equal = Array.for_all2 Int64.equal

    let hash v = Hashtbl.hash v

    let print ppf t =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ':')
        (fun ppf i64 -> Format.fprintf ppf "%016Lx" i64)
        ppf (Array.to_list t)
  end

  include T0
  module Self = Container_types.Make (T0)
  include Self

  let zero = Array.init Width.size_in_int64s (fun _ -> 0L)

  let to_int64_array t = t

  let of_int64_array t =
    if not (Array.length t = Width.size_in_int64s)
    then
      Misc.fatal_error
        "Vector_by_bit_pattern.of_int64_array: wrong length array";
    t
end

module Vec128 = struct
  type t =
    | Int8x16
    | Int16x8
    | Int32x4
    | Int64x2
    | Float32x4
    | Float64x2

  let name = function
    | Int8x16 -> "Int8x16"
    | Int16x8 -> "Int16x8"
    | Int32x4 -> "Int32x4"
    | Int64x2 -> "Int64x2"
    | Float32x4 -> "Float32x4"
    | Float64x2 -> "Float64x2"

  let name_lowercase = function
    | Int8x16 -> "int8x16"
    | Int16x8 -> "int16x8"
    | Int32x4 -> "int32x4"
    | Int64x2 -> "int64x2"
    | Float32x4 -> "float32x4"
    | Float64x2 -> "float64x2"

  let equal l r =
    match l, r with
    | Int8x16, Int8x16
    | Int16x8, Int16x8
    | Int32x4, Int32x4
    | Int64x2, Int64x2
    | Float32x4, Float32x4
    | Float64x2, Float64x2 ->
      true
    | (Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2), _ ->
      false

  let to_lambda : t -> Lambda.vec128_type = function
    | Int8x16 -> Int8x16
    | Int16x8 -> Int16x8
    | Int32x4 -> Int32x4
    | Int64x2 -> Int64x2
    | Float32x4 -> Float32x4
    | Float64x2 -> Float64x2

  let of_lambda : Lambda.vec128_type -> t = function
    | Int8x16 -> Int8x16
    | Int16x8 -> Int16x8
    | Int32x4 -> Int32x4
    | Int64x2 -> Int64x2
    | Float32x4 -> Float32x4
    | Float64x2 -> Float64x2

  module Bit_pattern = struct
    include Vector_by_bit_pattern (struct
      let size_in_int64s = 2
    end)

    type bits =
      { high : int64;
        low : int64
      }

    let to_bits t =
      match to_int64_array t with
      | [| high; low |] -> { high; low }
      | _ -> Misc.fatal_error "Vec128.to_bits: wrong size vector"

    let of_bits { high; low } = of_int64_array [| high; low |]
  end
end

type t = Vec128 of Vec128.t

let is_vec128 = function Vec128 _ -> true

let name = function Vec128 l -> Vec128.name l

let name_lowercase = function Vec128 l -> Vec128.name_lowercase l

let equal l r = match l, r with Vec128 l, Vec128 r -> Vec128.equal l r

let to_lambda : t -> Lambda.boxed_vector = function
  | Vec128 v -> Pvec128 (Vec128.to_lambda v)

let of_lambda : Lambda.boxed_vector -> t = function
  | Pvec128 v -> Vec128 (Vec128.of_lambda v)
