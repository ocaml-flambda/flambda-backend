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

(* Like [int Stdlib.Atomic.t], but [portable]. *)
module A = struct
  type t : value mod portable uncontended

  external make : int -> t @@ portable = "%makemutable"
  external fetch_and_add : t -> int -> int @@ portable = "%atomic_fetch_add"
end

(* Like [Stdlib.Magic], but [portable]. *)
module O = struct
  external magic : 'a -> 'b @@ portable = "%obj_magic"
end

(* Like [Stdlib.( = ), but [portable]. *)
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%equal"

module Name : sig
  type 'k t : value mod global portable many uncontended unique
  type packed = P : 'k t -> packed

  val make : unit -> packed @@ portable
  val equality_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) Type.eq option @@ portable
end = struct
  type 'k t = int
  type packed = P : 'k t -> packed

  let ctr = A.make 0
  let make () = P (A.fetch_and_add ctr 1)

  let equality_witness t1 t2 =
    if t1 = t2
    then Some (O.magic Type.Equal)
    else None
end

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
  val unsafe_mk : 'k Name.t -> 'k t @@ portable
  val name : 'k t @ local -> 'k Name.t @@ portable

  module Shared : sig
    (* CR layouts v5: this should have layout [void], but
       [void] can't be used for function argument and return types yet. *)
    type 'k t

    (* Can break the soundness of the API. *)
    val unsafe_mk : 'k Name.t -> 'k t @@ portable
    val name : 'k t @ local -> 'k Name.t @@ portable
  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable
end = struct
  type 'k t = 'k Name.t

  let unsafe_mk name = name
  let name t = t

  module Shared = struct
    type 'k t = 'k Name.t

    let unsafe_mk name = name
    let name t = t
  end

  let shared t = t

end

(* Like [Stdlib.raise], but [portable], and the value
   it never returns is also [portable] *)
external reraise : exn -> 'a @ portable @@ portable = "%reraise"

external raise_with_backtrace :
  exn -> Printexc.raw_backtrace -> 'a @ portable @@ portable = "%raise_with_backtrace"

module Data = struct
  type ('a, 'k) t : value mod portable uncontended

  exception Encapsulated : 'k Name.t * (exn, 'k) t -> exn

  external unsafe_mk : 'a -> ('a, 'k) t @@ portable = "%identity"

  external unsafe_get : ('a, 'k) t -> 'a @@ portable = "%identity"

  let wrap _ t = unsafe_mk t

  let unwrap _ t = unsafe_get t

  let unwrap_shared _ t = unsafe_get t

  let create f = unsafe_mk (f ())

  let reraise_encapsulated password exn =
    reraise (Encapsulated (Password.name password, unsafe_mk exn))

  let reraise_encapsulated_shared password exn =
    reraise (Encapsulated (Password.Shared.name password, unsafe_mk exn))

  let map pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated pw exn

  let fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1

  let snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2

  let both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let extract pw f t =
    let v = unsafe_get t in
    try f v with
    |  exn -> reraise_encapsulated pw exn

  let inject = unsafe_mk

  let project = unsafe_get

  let bind pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let iter pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let map_shared pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated_shared pw exn

  let extract_shared pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated_shared pw exn

end

exception Encapsulated = Data.Encapsulated

let access (type k) (pw : k Password.t) f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated pw exn

let access_shared (type k) (pw : k Password.Shared.t) f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated_shared pw exn

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t : value mod portable uncontended
  external create: unit -> t @@ portable = "caml_capsule_mutex_new"
  external lock: t -> unit @@ portable = "caml_capsule_mutex_lock"
  external unlock: t -> unit @@ portable = "caml_capsule_mutex_unlock"
end

(* Reader writer lock *)
module Rw = struct
  type t : value mod portable uncontended
  external create: unit -> t @@ portable = "caml_capsule_rwlock_new"
  external lock_read: t -> unit @@ portable = "caml_capsule_rwlock_rdlock"
  external lock_write: t -> unit @@ portable = "caml_capsule_rwlock_wrlock"
  external unlock: t -> unit @@ portable = "caml_capsule_rwlock_unlock"
end

module Mutex = struct

  (* Illegal mode crossing: ['k t] has a mutable field [poisoned]. It's safe,
     since [poisoned] protected by [mutex] and not exposed in the API, but
     is not allowed by the type system. *)
  type 'k t : value mod portable uncontended =
    { name : 'k Name.t
    ; mutex : M.t
    ; mutable poisoned : bool
    }

  (* CR: illegal mode crossing on the current version of the compiler,
     but should be legal. *)
  type packed : value mod portable uncontended = P : 'k t -> packed

  let name t = t.name

  exception Poisoned

  let with_lock :
    type k.
    k t
    -> (k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      M.lock t.mutex;
      match t.poisoned with
      | true -> M.unlock t.mutex; reraise Poisoned
      | false ->
        match f (Password.unsafe_mk t.name) with
        | x -> M.unlock t.mutex; x
        | exception exn ->
          t.poisoned <- true;
          (* NOTE: [unlock] does not poll for asynchronous exceptions *)
          M.unlock t.mutex;
          let exn =
            match exn with
            | Encapsulated (name, data) ->
              (match Name.equality_witness name t.name with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
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

  type 'k t : value mod portable uncontended =
    { name : 'k Name.t
    ; rwlock : Rw.t
    ; mutable poisoned : bool
    }

  type packed : value mod portable uncontended = P : 'k t -> packed

  exception Poisoned

  let with_write_lock :
    type k.
    k t
    -> (k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Rw.lock_write t.rwlock;
      match t.poisoned with
      | true -> Rw.unlock t.rwlock; reraise Poisoned
      | false ->
        match f (Password.unsafe_mk t.name) with
        | x -> Rw.unlock t.rwlock; x
        | exception exn ->
          t.poisoned <- true;
          Rw.unlock t.rwlock;
          let exn =
            match exn with
            | Encapsulated (name, data) ->
              (match Name.equality_witness name t.name with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
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
        match f (Password.Shared.unsafe_mk t.name) with
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

module Condition = struct

  type 'k t : value mod portable uncontended

  external create : unit -> 'k t @@ portable = "caml_capsule_condition_new"
  external wait : 'k t -> M.t -> unit @@ portable = "caml_capsule_condition_wait"
  external signal : 'k t -> unit @@ portable = "caml_capsule_condition_signal"
  external broadcast : 'k t -> unit @@ portable = "caml_capsule_condition_broadcast"

  let wait t (mut : 'k Mutex.t) _password =
    (* [mut] is locked, so we know it is not poisoned. *)
    wait t mut.mutex

end

let create_with_mutex () =
  let (P name) = Name.make () in
  Mutex.P { name; mutex = M.create (); poisoned = false }

let create_with_rwlock () =
  let (P name) = Name.make () in
  Rwlock.P { name; rwlock = Rw.create (); poisoned = false }

exception Protected : 'k Mutex.t * (exn, 'k) Data.t -> exn

(* CR-soon mslater: replace with portable stdlib *)
let get_raw_backtrace : unit -> Printexc.raw_backtrace @@ portable =
  O.magic O.magic Printexc.get_raw_backtrace

let protect f =
  try f () with
  | exn ->
    let (P mut) = create_with_mutex () in
    raise_with_backtrace (Protected (mut, Data.unsafe_mk exn)) (get_raw_backtrace ())
  ;;

