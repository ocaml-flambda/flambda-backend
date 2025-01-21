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

external make : 'a -> 'a t = "%makemutable"
external make_contended : 'a -> 'a t = "caml_atomic_make_contended"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_compare_set"
external compare_exchange : 'a t -> 'a -> 'a -> 'a = "%atomic_compare_exchange"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
external add : int t -> int -> unit = "%atomic_add"
external sub : int t -> int -> unit = "%atomic_sub"
external logand : int t -> int -> unit = "%atomic_land"
external logor : int t -> int -> unit = "%atomic_lor"
external logxor : int t -> int -> unit = "%atomic_lxor"

external ignore : 'a -> unit = "%ignore"

let set r x = ignore (exchange r x)
let incr r = add r 1
let decr r = sub r 1
