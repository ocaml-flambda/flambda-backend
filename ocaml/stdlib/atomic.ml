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

(* BACKPORT BEGIN switch to primtiives
external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
*)
external make : 'a -> 'a t = "caml_atomic_make"
external get : 'a t -> 'a = "caml_atomic_load"
external exchange : 'a t -> 'a -> 'a = "caml_atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "caml_atomic_cas"
external fetch_and_add : int t -> int -> int = "caml_atomic_fetch_add"
(* BACKPORT END *)

external ignore : 'a -> unit = "%ignore"

let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
