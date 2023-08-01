(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

(** Information required to rewrite [Debuginfo.t] values in the body of an
    inlined function.  These rewrites do three things:

    1. augment the inlined frame information directly in the [Debuginfo.t]
    values (c.f. [Debuginfo.inline]);

    2. allow the backend to distinguish between different instances of
    inlining, by assigning each such instance a unique identifier;

    3. allow the backend to determine the function symbol for each inlined
    frame specified by a [Debuginfo.t] value.

    Points 2 and 3 are required for the generation of DWARF inlined frame
    information. *)

(** A value of type [t] represents one instance of inlining out a
    particular function application. *)
type t

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val none : t

val is_none : t -> bool

(** [create ~called_code_id ~apply_dbg] should be used when a function
    application (i.e. [Apply_expr] term) with debuginfo [apply_dbg] is
    to be inlined out.  [create] must be called for each such instance of
    inlining. *)
val create : called_code_id:Code_id.t -> apply_dbg:Debuginfo.t -> t

(** For an instance of inlining [t] and a debuginfo value [dbg] occurring in
    the body of the corresponding function to be inlined, return a new
    debuginfo value to replace the old one.

    The new debuginfo value will consistently have the same, fresh, uid on every
    item corresponding to the inlined body (i.e. every item except those at the
    head of the list, which are those---typically just one item in fact---that
    came from the [apply_dbg] passed to [create]).

    In addition, the first (outermost, less deep inlining) debuginfo item
    arising from the inlined body is annotated with the function symbol from the
    application term being inlined out.  Inductively, in association with the
    general propagation of [Debuginfo.t] values by Flambda 2, the consequence is
    that [Debuginfo.t] values returned from this function end up annotated with
    function symbols at all points except the outermost frame. (The symbol for
    that frame will be known by the backend, since it is the function into which
    everything is ultimately being inlined.)
*)
val rewrite : t -> Debuginfo.t -> Debuginfo.t
