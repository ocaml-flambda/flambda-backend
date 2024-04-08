(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2013--2024 OCamlPro SAS                                    *)
(*   Copyright 2014--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = int32

let to_bits x = x

let of_bits x = x

external neg : t -> t = "flambda2_float32_neg_boxed" "flambda2_float32_neg"
  [@@unboxed]

external abs : t -> t = "flambda2_float32_abs_boxed" "flambda2_float32_abs"
  [@@unboxed]

external add : t -> t -> t = "flambda2_float32_add_boxed" "flambda2_float32_add"
  [@@unboxed]

external sub : t -> t -> t = "flambda2_float32_sub_boxed" "flambda2_float32_sub"
  [@@unboxed]

external mul : t -> t -> t = "flambda2_float32_mul_boxed" "flambda2_float32_mul"
  [@@unboxed]

external div : t -> t -> t = "flambda2_float32_div_boxed" "flambda2_float32_div"
  [@@unboxed]

external mod_ : t -> t -> t
  = "flambda2_float32_mod_boxed" "flambda2_float32_mod"
  [@@unboxed]

external compare : (t[@unboxed]) -> (t[@unboxed]) -> int
  = "flambda2_float32_compare_boxed" "flambda2_float32_compare"

external equal : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "flambda2_float32_equal_boxed" "flambda2_float32_equal"

external of_float : float -> t
  = "flambda2_float32_of_float_boxed" "flambda2_float32_of_float"
  [@@unboxed]

external to_float : t -> float
  = "flambda2_float32_to_float_boxed" "flambda2_float32_to_float"
  [@@unboxed]

external of_string : string -> t = "flambda2_float32_of_string"
