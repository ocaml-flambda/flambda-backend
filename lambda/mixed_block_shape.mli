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
    ({i i.e.}) value prefix and flat suffix), after flattening of products
    and reordering of the type definition in the surface language.. *)
type 'a t

(* CR-soon xclerc for xclerc: make abstract, and move to `Lambda`,
   where it should be used in `Pmixedfield` and `Psetmixedfield`. *)
type path = int list

val of_mixed_block_elements : 'a Lambda.mixed_block_element array -> 'a t

val value_prefix : 'a t -> 'a Lambda.mixed_block_element array

val flat_suffix : 'a t -> 'a Lambda.mixed_block_element array

val value_prefix_len : 'a t -> int

val flat_suffix_len : 'a t -> int

(** Access to the shape, as flattened and reordered to follow the runtime restriction. *)
val flattened_and_reordered_shape : 'a t -> 'a Lambda.mixed_block_element array

(** (Same as [flattened_shape]). *)
val flattened_and_reordered_shape_unit :
  'a t -> unit Lambda.mixed_block_element array

(* CR-soon xclerc for xclerc: make abstract? *)
type new_indexes = int list

(** Return the list of indices in the runtime representation for to access the values
    corresponding to the passed path. A path does not have to lead to a single value
    when products are involved.*)
val lookup_path : 'a t -> path -> new_indexes

(** Return an array corresponding to a map from old indices to new indices. Can only
    be used if the block values are already been flattened. *)
val new_indexes_to_old_indexes : 'a t -> int array
