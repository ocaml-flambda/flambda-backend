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

(** Merge an existing value of type [t] with [from_apply_expr], the latter
    corresponding to an [Apply_expr] term that is being inlined out. *)
val merge : t -> from_apply_expr:t -> t

(** For an instance of inlining [t] and a debuginfo value [dbg] occurring in
    the body of the corresponding function to be inlined, return a new
    debuginfo value to replace the old one.

    The new debuginfo value will consistently have the same, fresh, uid on every
    item corresponding to the inlined body (i.e. every item except those at the
    head of the list, which are those---typically just one item in fact---that
    came from the [apply_dbg] passed to [create]).  It might seem that
    every debuginfo value ends up with the same uid throughout---but this will
    not be the case in the situation where [apply_dbg] contains inlined frames
    (i.e. the inlined-out call site was only exposed by inlining) in
    addition to the inlined frame(s) arising from the inlined-out body.
    See the last example below.

    In addition, the first (outermost, less deep inlining) debuginfo item
    arising from the inlined body is annotated with the function symbol from the
    application term being inlined out.  Inductively, in association with the
    general propagation of [Debuginfo.t] values by Flambda 2, the consequence is
    that [Debuginfo.t] values returned from this function end up annotated with
    function symbols at all points except the outermost frame. (The symbol for
    that frame will be known by the backend, since it is the function into which
    everything is ultimately being inlined.)

    For example in:

    let[@inline] f x = (Sys.opaque_identity x) + 1
    let[@inline] g x = 2 * f x
    let foo x = g x - f x

    we have the following in [f]:

    Paddint/44N = ((+ Popaque/43N 1) example.ml:1,19--46)

    and the following in [g]:

    Paddint/61N =
     ((+ Popaque/60N 1)
      example.ml:2,23--26;
      example.ml:1,19--46[871832529][FS=camlExample.f_0_3_code])
    Pmulint/62N = (( * 2 Paddint/61N) example.ml:2,19--26)

    and the following in [h]:

    Paddint/80N =
     ((+ Popaque/79N 1)
      example.ml:3,18--21;
      example.ml:1,19--46[19414450][FS=camlExample.f_0_3_code])
    Paddint/90N =            <- call this addition A
     ((+ Popaque/89N 1)
      example.ml:3,12--15;
      example.ml:2,23--26[83388650][FS=camlExample.g_1_4_code];
      example.ml:1,19--46[83388650][FS=camlExample.f_0_3_code])
    Pmulint/91N =
     (( * 2 Paddint/90N)
      example.ml:3,12--15;
      example.ml:2,19--26[83388650][FS=camlExample.g_1_4_code])

    Note that in [h] the two instances of the addition have different uids.
    Moreover, even though in [g] the addition and multiplication have
    different uids (indeed no uid on the multiplication, as it hasn't come
    from an inline frame), after inlining of [g] they both have the same
    uid.

    To show why different uids may occur within a single [Debuginfo.t],
    consider the following extension of the above example, where a call
    site is revealed by inlining:

    let[@inline] bar f x = f x
    let baz x = bar foo x

    The term marked as "addition A" above now looks like this:

    Paddint/144N =
     ((+ Popaque/143N 1)
      example.ml:6,12--21;
      example.ml:5,23--26[88799133][FS=camlExample.bar_3_8_code];
      example.ml:3,12--15[777401542][FS=camlExample.foo_2_7_code];
      example.ml:2,23--26[777401542][FS=camlExample.g_1_6_code];
      example.ml:1,19--46[777401542][FS=camlExample.f_0_5_code])

    The uid 88799133 corresponds to the instance of inlining that revealed
    the call to [foo] inside [bar]; and the uid 777401542 corresponds to
    the instance of inlining that populated the body of [bar] itself.
*)
val rewrite : t -> Debuginfo.t -> Debuginfo.t
