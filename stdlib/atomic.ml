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

type (!'a : value_or_null) t : mutable_data with 'a

external make
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "%makemutable"

external make_contended
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "caml_atomic_make_contended"

external get
  : ('a : value_or_null).
  'a t @ local -> 'a
  @@ portable
  = "%atomic_load"

external set
  : ('a : value_or_null).
  'a t @ local -> 'a -> unit
  @@ portable
  = "%atomic_set"

external exchange
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a
  @@ portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> bool
  @@ portable
  = "%atomic_cas"

external compare_exchange
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> 'a
  @@ portable
  = "%atomic_compare_exchange"

external fetch_and_add
  :  int t @ contended local
  -> int
  -> int
  @@ portable
  = "%atomic_fetch_add"

external add
  :  int t @ contended local
  -> int
  -> unit
  @@ portable
  = "%atomic_add"

external sub
  :  int t @ contended local
  -> int
  -> unit
  @@ portable
  = "%atomic_sub"

external logand
  :  int t @ contended local
  -> int
  -> unit
  @@ portable
  = "%atomic_land"

external logor
  :  int t @ contended local
  -> int
  -> unit
  @@ portable
  = "%atomic_lor"

external logxor
  :  int t @ contended local
  -> int
  -> unit
  @@ portable
  = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

module Contended = struct
  external get
    : ('a : value_or_null).
    'a t @ contended local -> 'a @ contended
    @@ portable
    = "%atomic_load"

  external set
    : ('a : value_or_null mod contended).
    'a t @ contended local -> 'a @ portable -> unit
    @@ portable
    = "%atomic_set"

  external exchange
    : ('a : value_or_null mod contended).
    'a t @ contended local -> 'a @ portable -> 'a
    @@ portable
    = "%atomic_exchange"

  external compare_and_set
    : ('a : value_or_null mod contended).
    'a t @ contended local -> 'a -> 'a @ portable -> bool
    @@ portable
    = "%atomic_cas"

  external compare_exchange
    : ('a : value_or_null mod contended).
    'a t @ contended local -> 'a -> 'a @ portable -> 'a @ contended
    @@ portable
    = "%atomic_compare_exchange"
end
