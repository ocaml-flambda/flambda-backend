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

module Unsafe = struct
  type !'a t : value mod portable uncontended

  external make : 'a -> 'a t = "%makemutable"
  external make_contended : 'a -> 'a t = "caml_atomic_make_contended"
  external get : 'a t -> 'a = "%atomic_load"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
  external compare_exchange : 'a t -> 'a -> 'a -> 'a = "%atomic_compare_exchange"
  external fetch_and_add : int t -> int -> int @@ portable = "%atomic_fetch_add"

  external ignore : 'a -> unit @@ portable = "%ignore"

  let set r x = ignore (exchange r x)
  let incr r = ignore (fetch_and_add r 1)
  let decr r = ignore (fetch_and_add r (-1))
end

module Safe = struct
  type 'a t = 'a Unsafe.t

  external make : 'a @ portable contended -> 'a t @@ portable = "%makemutable"
  external make_contended : 'a @ portable contended -> 'a t @@ portable = "caml_atomic_make_contended"
  external get : 'a t -> 'a @ portable contended @@ portable = "%atomic_load"
  external exchange : 'a t -> 'a @ portable contended -> 'a @ portable contended @@ portable = "%atomic_exchange"
  external compare_and_set : 'a t -> 'a @ portable contended -> 'a @ portable contended -> bool @@ portable = "%atomic_cas"
  external compare_exchange : 'a t -> 'a @ portable contended -> 'a @ portable contended -> 'a @ portable contended @@ portable = "%atomic_compare_exchange"

  external ignore : 'a @ contended -> unit @@ portable = "%ignore"

  let set r x = ignore (exchange r x)
end

include Unsafe
