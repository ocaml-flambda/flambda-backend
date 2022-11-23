(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open struct
  open Int64

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let ( land ) = logand

  let ( lor ) = logor

  let ( lxor ) = logxor

  let ( lsr ) = shift_right_logical
end

(* Taken from Base. Implementation adapted from:
   https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation *)

let int64_popcount =
  let m1 = 0x5555555555555555L in
  (* 0b01010101... *)
  let m2 = 0x3333333333333333L in
  (* 0b00110011... *)
  let m4 = 0x0f0f0f0f0f0f0f0fL in
  (* 0b00001111... *)
  let h01 = 0x0101010101010101L in
  (* 1 bit set per byte *)
  fun [@inline] x ->
    (* gather the bit count for every pair of bits *)
    let x = x - ((x lsr 1) land m1) in
    (* gather the bit count for every 4 bits *)
    let x = (x land m2) + ((x lsr 2) land m2) in
    (* gather the bit count for every byte *)
    let x = (x + (x lsr 4)) land m4 in
    (* sum the bit counts in the top byte and shift it down *)
    Int64.to_int ((x * h01) lsr 56)

type t =
  { mutable seed : int64;
    odd_gamma : int64
  }

let golden_gamma = 0x9e37_79b9_7f4a_7c15L

let of_int seed = { seed = Int64.of_int seed; odd_gamma = golden_gamma }

let copy { seed; odd_gamma } = { seed; odd_gamma }

let mix_bits z n = z lxor (z lsr n)

let mix64 z =
  let z = mix_bits z 33 * 0xff51_afd7_ed55_8ccdL in
  let z = mix_bits z 33 * 0xc4ce_b9fe_1a85_ec53L in
  mix_bits z 33

let mix64_variant13 z =
  let z = mix_bits z 30 * 0xbf58_476d_1ce4_e5b9L in
  let z = mix_bits z 27 * 0x94d0_49bb_1331_11ebL in
  mix_bits z 31

let mix_odd_gamma z =
  let z = mix64_variant13 z lor 1L in
  let n = int64_popcount (z lxor (z lsr 1)) in
  (* The original paper uses [>=] in the conditional immediately below; however
     this is a typo, and we correct it by using [<]. This was fixed in response
     to [1] and [2].

     [1] https://github.com/janestreet/splittable_random/issues/1

     [2] http://www.pcg-random.org/posts/bugs-in-splitmix.html *)
  if n < 24 then z lxor 0xaaaa_aaaa_aaaa_aaaaL else z

let next_seed t =
  let next = t.seed + t.odd_gamma in
  t.seed <- next;
  next

let of_seed_and_gamma ~seed ~gamma =
  let seed = mix64 seed in
  let odd_gamma = mix_odd_gamma gamma in
  { seed; odd_gamma }

let split t =
  let seed = next_seed t in
  let gamma = next_seed t in
  of_seed_and_gamma ~seed ~gamma

let next_int64 t = mix64 (next_seed t)

let perturb t salt =
  let next = t.seed + mix64 (Int64.of_int salt) in
  t.seed <- next

let int t = next_int64 t |> Int64.to_int
