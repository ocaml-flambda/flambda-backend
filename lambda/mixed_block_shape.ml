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

type subst = int array

type 'a shape = 'a Lambda.mixed_block_element array

type 'a t =
  { (* CR-soon xclerc for xclerc: once the nesting/flatenning work is done,
       revisit this record to only keep the fields actually needed. *)
    original_shape : 'a shape;
    prefix : 'a shape;
    suffix : 'a shape;
    reordered_shape : 'a shape;
    new_index_to_old_index : subst;
    old_index_to_new_index : subst
  }

let of_mixed_block_elements (original_shape : 'a shape) : 'a t =
  let prefix = ref [] in
  let suffix = ref [] in
  for idx = Array.length original_shape - 1 downto 0 do
    let elem = original_shape.(idx) in
    let is_value =
      match elem with
      | Value _ -> true
      | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Vec128 | Word ->
        false
    in
    if is_value
    then prefix := (elem, idx) :: !prefix
    else suffix := (elem, idx) :: !suffix
  done;
  let prefix = Array.of_list !prefix in
  let suffix = Array.of_list !suffix in
  let reordered_shape_with_indices = Array.append prefix suffix in
  let reordered_shape, new_index_to_old_index =
    Array.split reordered_shape_with_indices
  in
  let old_index_to_new_index =
    Array.make (Array.length new_index_to_old_index) (-1)
  in
  Array.iteri
    (fun new_index old_index -> old_index_to_new_index.(old_index) <- new_index)
    new_index_to_old_index;
  { original_shape;
    prefix = Array.map fst prefix;
    suffix = Array.map fst suffix;
    reordered_shape;
    new_index_to_old_index;
    old_index_to_new_index
  }

let reorder_array t src =
  match Array.length src with
  | 0 -> [||]
  | len ->
    let dst = Array.make len src.(0) in
    for old_index = 0 to pred len do
      let new_index = t.old_index_to_new_index.(old_index) in
      dst.(new_index) <- src.(old_index)
    done;
    dst

let get_reordered t i = t.reordered_shape.(i)

let value_prefix t = t.prefix

let flat_suffix t = t.suffix

let value_prefix_len t = Array.length t.prefix

let flat_suffix_len t = Array.length t.suffix

let original_shape t = t.original_shape

let reordered_shape t = t.reordered_shape

let reordered_shape_unit t =
  Array.map
    (fun (elt : _ Lambda.mixed_block_element) : _ Lambda.mixed_block_element ->
      match elt with
      | Float_boxed _ -> Float_boxed ()
      | (Value _ | Float64 | Float32 | Bits32 | Bits64 | Vec128 | Word) as elem
        ->
        elem)
    t.reordered_shape

let old_index_to_new_index t i = t.old_index_to_new_index.(i)

let new_index_to_old_index t i = t.new_index_to_old_index.(i)
