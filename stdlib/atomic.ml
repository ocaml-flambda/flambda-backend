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

type !'a t : value mod portable uncontended

module Unsafe = struct
  external make : 'a -> 'a t = "%makemutable"
  external make_contended : 'a -> 'a t = "caml_atomic_make_contended"
  external get : 'a t -> 'a = "%atomic_load"
  external set : 'a t -> 'a -> unit = "%atomic_set"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
  external compare_exchange : 'a t -> 'a -> 'a -> 'a = "%atomic_compare_exchange"
end

external fetch_and_add : int t -> int -> int @@ portable = "%atomic_fetch_add"
external add : int t -> int -> unit @@ portable = "%atomic_add"
external sub : int t -> int -> unit @@ portable = "%atomic_sub"
external logand : int t -> int -> unit @@ portable = "%atomic_land"
external logor : int t -> int -> unit @@ portable = "%atomic_lor"
external logxor : int t -> int -> unit @@ portable = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

module Safe = struct
  external make : 'a @ portable contended -> 'a t @@ portable = "%makemutable"
  external make_contended : 'a @ portable contended -> 'a t @@ portable = "caml_atomic_make_contended"
  external get : 'a t -> 'a @ portable contended @@ portable = "%atomic_load"
  external set : 'a t -> 'a @ portable contended -> unit @@ portable = "%atomic_set"
  external exchange : 'a t -> 'a @ portable contended -> 'a @ portable contended @@ portable = "%atomic_exchange"
  external compare_and_set : 'a t -> 'a @ portable contended -> 'a @ portable contended -> bool @@ portable = "%atomic_cas"
  external compare_exchange : 'a t -> 'a @ portable contended -> 'a @ portable contended -> 'a @ portable contended @@ portable = "%atomic_compare_exchange"
end

include Unsafe
