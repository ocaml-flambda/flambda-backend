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

module Byte_count = struct
  type t = int

  let on_64_bit_arch t = t

  let zero = 0

  let is_zero = Int.equal 0

  let add = Int.add
end

type t =
  { value : int;
    flat : int
  }

let zero = { value = 0; flat = 0 }

let add { value; flat } { value = value'; flat = flat' } =
  { value = value + value'; flat = flat + flat' }

let rec count (el : _ Lambda.mixed_block_element) : t =
  match el with
  | Value _ -> { value = 8; flat = 0 }
  | Float_boxed _ | Float64 | Float32 | Bits32 | Bits64 | Word ->
    (* In a record, [float32]s and [bits32]s aren't packed tightly *)
    { value = 0; flat = 8 }
  | Vec128 -> { value = 0; flat = 16 }
  | Product layouts ->
    Array.fold_left (fun cts l -> add cts (count l)) zero layouts

let has_value_and_flat { value; flat } = value > 0 && flat > 0
