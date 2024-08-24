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

(* CR aspsmith: Reimplement in terms of atomic record fields *)

module Loc = struct
  type ('a : value_or_null) t = 'a atomic_loc

  external get
    : ('a : value_or_null).
        'a t @ local -> 'a @@ portable
    = "%atomic_load_loc"

  (* CR aspsmith: [set] *)

  external exchange
    : ('a : value_or_null).
        'a t @ local -> 'a -> 'a @@ portable
    = "%atomic_exchange_loc"

  (* CR aspsmith: compare_exchange *)

  external compare_and_set
    : ('a : value_or_null).
        'a t @ local -> 'a -> 'a -> bool @@ portable
    = "%atomic_cas_loc"

  external fetch_and_add
    : ('a : value_or_null).
        int t @ contended local -> int -> int @@ portable
    = "%atomic_fetch_add_loc"

  let set t v =
    ignore (exchange t v)
  let incr t =
    ignore (fetch_and_add t 1)
  let decr t =
    ignore (fetch_and_add t (-1))

  (* CR aspsmith: add, sub, logand, logor, logxor *)

  module Contended = struct
    external get
      : ('a : value_or_null mod contended).
          'a t @ contended local -> 'a @@ portable
      = "%atomic_load_loc"

    external exchange
      : ('a : value_or_null mod contended portable).
          'a t @ contended local -> 'a -> 'a @@ portable
      = "%atomic_exchange_loc"

    external compare_and_set
      : ('a : value_or_null mod portable).
          'a t @ contended local -> 'a -> 'a -> bool @@ portable
      = "%atomic_cas_loc"

    (* CR aspsmith: ...etc *)
  end
end


type (!'a : value_or_null) t = { mutable contents : 'a [@atomic] }

(* CR aspsmith: Define in terms of loc ops *)

external make : ('a : value_or_null).
  'a -> ('a t[@local_opt]) @@ portable = "%makemutable"

external make_contended : ('a : value_or_null).
  'a -> ('a t[@local_opt]) @@ portable = "caml_atomic_make_contended"

let get t = t.contents
let set t v = t.contents <- v

external exchange : ('a : value_or_null).
  'a t @ local -> 'a -> 'a @@ portable =
  "%atomic_exchange"
external compare_and_set : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> bool @@ portable =
  "%atomic_cas"
external compare_exchange : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> 'a @@ portable =
  "%atomic_compare_exchange"
external fetch_and_add : int t @ contended local -> int -> int @@ portable = "%atomic_fetch_add"
external add : int t @ contended local -> int -> unit @@ portable = "%atomic_add"
external sub : int t @ contended local -> int -> unit @@ portable = "%atomic_sub"
external logand : int t @ contended local -> int -> unit @@ portable = "%atomic_land"
external logor : int t @ contended local -> int -> unit @@ portable = "%atomic_lor"
external logxor : int t @ contended local -> int -> unit @@ portable = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

module Contended = struct
  external get : ('a : value_or_null mod contended).
    'a t @ contended local -> 'a @@ portable =
    "%atomic_load"
  external set : ('a : value_or_null mod portable).
    'a t @ contended local -> 'a -> unit @@ portable =
    "%atomic_set"
  external exchange : ('a : value_or_null mod contended portable).
    'a t @ contended local -> 'a -> 'a @@ portable =
    "%atomic_exchange"
  external compare_and_set : ('a : value_or_null mod portable).
    'a t @ contended local -> 'a -> 'a -> bool @@ portable =
    "%atomic_cas"
  external compare_exchange : ('a : value_or_null mod contended portable).
    'a t @ contended local -> 'a -> 'a -> 'a @@ portable =
    "%atomic_compare_exchange"
end
