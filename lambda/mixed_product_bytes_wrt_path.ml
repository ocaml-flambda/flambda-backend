(******************************************************************************
 *                             flambda-backend                                *
 *                          Ryan Tjoa, Jane Street                            *
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

module MPB = Mixed_product_bytes

type t =
  { here : MPB.t;
    left : MPB.t;
    right : MPB.t
  }

let zero = { here = MPB.zero; left = MPB.zero; right = MPB.zero }

let add { here; left; right } { here = here'; left = left'; right = right' } =
  { here = MPB.add here here';
    left = MPB.add left left';
    right = MPB.add right right'
  }

let rec count (el : _ Lambda.mixed_block_element) path =
  match path with
  | [] -> { zero with here = MPB.count el }
  | Lambda.In_singleton :: path_rest -> count el path_rest
  | Lambda.In_product i :: path_rest -> (
    match el with
    | Product els ->
      let _, totals =
        Array.fold_left
          (fun (j, totals) el ->
            ( j + 1,
              add totals
                (if j = i
                then count el path_rest
                else if j < i
                then { zero with left = MPB.count el }
                else { zero with right = MPB.count el }) ))
          (0, zero) els
      in
      totals
    | Value _ | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Word
    | Vec128 ->
      Misc.fatal_error "Mixed_product_bytes_wrt_path: bad mixed block path")

let all { here; left; right } =
  let value = MPB.Byte_count.(add (add here.value left.value) right.value) in
  let flat = MPB.Byte_count.(add (add here.flat left.flat) right.flat) in
  MPB.{ value; flat }

let lowest_invalid_gap_on_64_bit_arch = 1 lsl 16

type offset_and_gap_bytes =
  { offset_bytes : MPB.Byte_count.t;
    gap_bytes : MPB.Byte_count.t
  }

let offset_and_gap { here; left; right } =
  let offset_bytes =
    if MPB.Byte_count.is_zero here.value
    then
      (* If the element is all flats, then the offset points to the first flat
         element rather than the first value element *)
      MPB.Byte_count.(add (add left.value left.flat) right.value)
    else left.value
  in
  if MPB.has_value_and_flat here
  then
    let gap_bytes = MPB.Byte_count.add left.flat right.value in
    (* Conservatively assumes that *all* values and flats in [here] can become
        part of the gap upon deepening (but really, if the deepened pointer
        still has a gap, it must have at least one value and one flat.) *)
    let deepened_gap_upper_bound =
      MPB.Byte_count.(add (add gap_bytes here.value) here.flat)
    in
    if MPB.Byte_count.on_64_bit_arch deepened_gap_upper_bound
       >= lowest_invalid_gap_on_64_bit_arch
    then None
    else Some { offset_bytes; gap_bytes }
  else Some { offset_bytes; gap_bytes = MPB.Byte_count.zero }
