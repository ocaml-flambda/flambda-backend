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


module Access : sig
  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod global portable many unique

  type packed = P : 'k t -> packed

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t @@ portable

  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
end = struct
  type dummy

  type 'k t = T : dummy t

  type packed = P : 'k t -> packed

  external unsafe_rebrand : 'k t -> 'j t @@ portable = "%identity"

  let unsafe_mk (type k) () : k t = unsafe_rebrand T

  let equality_witness (type k j) (T : k t) (T : j t) : (k, j) Type.eq =
    Type.Equal

end

let current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.unsafe_mk ()

module Password : sig
  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod portable many unique uncontended

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t @@ portable

  module Shared : sig
    (* CR layouts v5: this should have layout [void], but
       [void] can't be used for function argument and return types yet. *)
    type 'k t

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t @@ portable
  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable
end = struct
  type 'k t = unit

  let unsafe_mk () = ()

  module Shared = struct
    type 'k t = unit

    let unsafe_mk () = ()
  end

  let shared () = ()

end

(* Like [Stdlib.raise], but [portable], and the value
   it never returns is also [portable] *)
external reraise : exn -> 'a @ portable @@ portable = "%reraise"

exception Contended of exn @@ contended

let access (type k) (_ : k Password.t) f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> reraise (Contended exn)

let access_shared (type k) (_ : k Password.Shared.t) f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> reraise (Contended exn)

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t : value mod portable uncontended
  external create: unit -> t @@ portable = "caml_ml_mutex_new"
  external lock: t -> unit @@ portable = "caml_ml_mutex_lock"
  external unlock: t -> unit @@ portable = "caml_ml_mutex_unlock"
end

(* Reader writer lock *)
module Rw = struct
  type t : value mod portable uncontended
  external create: unit -> t @@ portable = "caml_ml_rwlock_new"
  external lock_read: t -> unit @@ portable = "caml_ml_rwlock_rdlock"
  external lock_write: t -> unit @@ portable = "caml_ml_rwlock_wrlock"
  external unlock: t -> unit @@ portable = "caml_ml_rwlock_unlock"
end

module Mutex = struct

  (* Illegal mode crossing: ['k t] has a mutable field [poisoned]. It's safe,
     since [poisoned] protected by [mutex] and not exposed in the API, but
     is not allowed by the type system. *)
  type 'k t : value mod portable uncontended = { mutex : M.t; mutable poisoned : bool }

  (* CR: illegal mode crossing on the current version of the compiler,
     but should be legal. *)
  type packed : value mod portable uncontended = P : 'k t -> packed

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
      Access.unsafe_mk ()
end

module Rwlock = struct

  type 'k t : value mod portable uncontended = { rwlock : Rw.t; mutable poisoned : bool }

  type packed : value mod portable uncontended = P : 'k t -> packed

  exception Poisoned

  let with_write_lock :
    'k t
    -> ('k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Rw.lock_write t.rwlock;
      match t.poisoned with
      | true -> Rw.unlock t.rwlock; reraise Poisoned
      | false ->
        match f (Password.unsafe_mk ()) with
        | x -> Rw.unlock t.rwlock; x
        | exception exn ->
          t.poisoned <- true;
          Rw.unlock t.rwlock;
          reraise exn

  let with_read_lock :
    'k t
    -> ('k Password.Shared.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Rw.lock_read t.rwlock;
      match t.poisoned with
      | true -> Rw.unlock t.rwlock; reraise Poisoned
      | false ->
        match f (Password.Shared.unsafe_mk()) with
        | x -> Rw.unlock t.rwlock; x
        | exception exn ->
          (* Here we are not poisoning the RwLock, see [capsule.mli] for explanation *)
          Rw.unlock t.rwlock;
          reraise exn

  let destroy t =
    Rw.lock_write t.rwlock;
    match t.poisoned with
    | true ->
      Rw.unlock t.rwlock;
      reraise Poisoned
    | false ->
      t.poisoned <- true;
      Rw.unlock t.rwlock;
      Access.unsafe_mk ()

end

let create_with_mutex () =
  Mutex.P { mutex = M.create (); poisoned = false }

let create_with_rwlock () =
  Rwlock.P { rwlock = Rw.create (); poisoned = false }

module Data = struct
  type ('a, 'k) t : value mod portable uncontended

  external unsafe_mk : 'a -> ('a, 'k) t @@ portable = "%identity"

  external unsafe_get : ('a, 'k) t -> 'a @@ portable = "%identity"

  let wrap _ t = unsafe_mk t

  let unwrap _ t = unsafe_get t

  let unwrap_shared _ t = unsafe_get t

  let create f = unsafe_mk (f ())

  let map _ f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise (Contended exn)

  let both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let split t =
    let t1, t2 = unsafe_get t in
    unsafe_mk t1, unsafe_mk t2

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

  let map_shared _ f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise (Contended exn)

  let extract_shared _ f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise (Contended exn)

end
