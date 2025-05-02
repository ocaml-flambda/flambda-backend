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

(** A path into a mixed block element (identifying some subelement) can be
    considered to partition the mixed block element into three parts: to the
    left, at subelement, and to the right.

    This module counts the number of value and flat bytes in each part, for the
    purpose of computing block indices to the subelement. Also see Note
    [Representation of block indices] in [lambda/translcore.ml].
*)

type t =
  { here : Mixed_product_bytes.t;
    left : Mixed_product_bytes.t;
    right : Mixed_product_bytes.t
  }

val zero : t

val add : t -> t -> t

(** Given an outer mixed block element, and a path into that as a list of
    positions, count value/flat bytes to the left, at, and to the right that
    subelement *)
val count : _ Lambda.mixed_block_element -> Lambda.pos list -> t

val all : t -> Mixed_product_bytes.t

type offset_and_gap_bytes =
  { offset_bytes : Mixed_product_bytes.Byte_count.t;
    gap_bytes : Mixed_product_bytes.Byte_count.t
  }

(** Compute the offset and gap in bytes for an index to [here]. Returns [None]
    if the index could lead an "illegal" gap of 2^16 or greater bytes, which
    is the case if either:
    - An index to [here] would have an illegal gap.
    - Conservatively, whether an index to [here] could be deepened to have an
      illegal gap. *)
val offset_and_gap : t -> offset_and_gap_bytes option
