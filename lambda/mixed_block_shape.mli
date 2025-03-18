(******************************************************************************
 *                             flambda-backend                                *
 *                        Xavier Clerc, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** A mixed block shape is essentially the runtime representation of a block
    ({i i.e.}) value prefix and flat suffix), and the permutation from the
    type definition in the surface language to that representation. *)
type 'a t

type path

module Singleton_mixed_block_element : sig
  type 'a t = private
    | Value of Lambda.value_kind
    | Float_boxed of 'a
    | Float64
    | Float32
    | Bits32
    | Bits64
    | Vec128
    | Word

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

val print : Format.formatter -> _ t -> unit

val of_mixed_block_elements :
  print_locality:(Format.formatter -> 'a -> unit) ->
  'a Lambda.mixed_block_element array ->
  'a t

val value_prefix : 'a t -> 'a Lambda.mixed_block_element array

val flat_suffix : 'a t -> 'a Lambda.mixed_block_element array

val value_prefix_len : 'a t -> int

val flat_suffix_len : 'a t -> int

(* XXX add "reordered" into the name *)

(** Access to the shape, as flattened and following the runtime restriction. *)
val flattened_shape : 'a t -> 'a Lambda.mixed_block_element array

(** (Same as [flattened_shape]). *)
val flattened_shape_unit : 'a t -> unit Lambda.mixed_block_element array

val lookup_path_producing_new_indexes : 'a t -> int list -> int list

val new_indexes_to_old_indexes : 'a t -> int array
