(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type !'a t

external make : 'a -> 'a t @@ portable = "%makemutable"
external make_contended : 'a -> 'a t @@ portable = "caml_atomic_make_contended"
external get : 'a t -> 'a @@ portable = "%atomic_load"
external exchange : 'a t -> 'a -> 'a @@ portable = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool @@ portable = "%atomic_cas"
external fetch_and_add : int t -> int -> int @@ portable = "%atomic_fetch_add"

external ignore : 'a -> unit @@ portable = "%ignore"

let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
