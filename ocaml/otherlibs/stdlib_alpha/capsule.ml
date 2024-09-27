(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Diana Kalinichenko, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Password : sig
  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t @@ portable
end = struct
  type 'k t = unit

  let unsafe_mk () = ()
end

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t
  external create: unit -> t @@ portable = "caml_ml_mutex_new"
  external lock: t -> unit @@ portable = "caml_ml_mutex_lock"
  external unlock: t -> unit @@ portable = "caml_ml_mutex_unlock"
end

(* Like [Stdlib.raise], but [portable], and the value
   it never returns is also [portable] *)
external reraise : exn -> 'a @ portable @@ portable = "%reraise"

module Mutex = struct

  type 'k t = { mutex : M.t; mutable poisoned : bool }

  type packed = P : 'k t -> packed

  exception Poisoned

  let with_lock :
    'k t
    -> ('k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      M.lock t.mutex;
      match t.poisoned with
      | true -> M.unlock t.mutex; reraise Poisoned
      | false ->
        match f (Password.unsafe_mk ()) with
        | x -> M.unlock t.mutex; x
        | exception exn ->
          t.poisoned <- true;
          (* NOTE: [unlock] does not poll for asynchronous exceptions *)
          M.unlock t.mutex;
          reraise exn

  let destroy t =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      t.poisoned <- true;
      M.unlock t.mutex;
      Password.unsafe_mk ()
end

let create_with_mutex () =
  Mutex.P { mutex = M.create (); poisoned = false }

module Data = struct
  type ('a, 'k) t : value mod portable uncontended

  exception Contended of exn @@ contended

  external unsafe_mk : 'a -> ('a, 'k) t @@ portable = "%identity"

  external unsafe_get : ('a, 'k) t -> 'a @@ portable = "%identity"

  let create f = unsafe_mk (f ())

  let map _ f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise (Contended exn)

  let both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let extract _ f t =
    let v = unsafe_get t in
    try f v with
    |  exn -> reraise (Contended exn)

  let inject = unsafe_mk

  let project = unsafe_get

  let bind _ f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise (Contended exn)

  let iter _ f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise (Contended exn)

  let expose _ t = unsafe_get t
end
