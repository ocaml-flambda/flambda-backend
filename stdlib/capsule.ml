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

module Name : sig
  type 'k t : value mod portable uncontended global
  type packed = P : 'k t -> packed

  val make : unit -> packed @@ portable

  type (_, _) eq = Eq : ('k, 'k) eq

  val equal : _ t -> _ t -> bool @@ portable
  val equal_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) eq option @@ portable
end = struct
  type 'k t = int
  type packed = P : 'k t -> packed

  let ctr = Atomic.make_safe 0
  let make () = P (Atomic.fetch_and_add ctr 1)

  type (_, _) eq = Eq : ('k, 'k) eq

  let equal t1 t2 = t1 = t2

  let equal_witness t1 t2 =
    if equal t1 t2
    then Some (Obj.magic Eq)
    else None
end

module Password : sig
  type 'k t
  type packed = P : 'k t -> packed

  val name : 'k t @ local -> 'k Name.t @@ portable
  val make : unit -> packed @@ portable

  (* Can break the soundness of the API. *)
  val unsafe_mk : 'k Name.t -> 'k t @@ portable
end = struct
  type 'k t = 'k Name.t
  type packed = P : 'k t -> packed

  let name t = t
  let unsafe_mk name = name

  let make () =
    let (P name) = Name.make () in
    P name
end

module Data_ = struct
  type ('a, 'k) t : value mod portable uncontended

  exception Encapsulated : 'k Name.t * (exn, 'k) t -> exn

  external unsafe_mk : 'a -> ('a, 'k) t @@ portable = "%identity"

  external unsafe_get : ('a, 'k) t -> 'a @@ portable = "%identity"

  let create f = unsafe_mk (f ())

  let reraise password exn =
    raise (Encapsulated (Password.name password, unsafe_mk exn))

  let map password f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise password exn

  let both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let extract password f t =
    let v = unsafe_get t in
    try f v with
    |  exn -> reraise password exn

  let inject = unsafe_mk

  let project = unsafe_get

  let bind password f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise password exn

  let iter password f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise password exn

  let expose _ t = unsafe_get t
end

module Mutex_ = Mutex

module Mutex = struct

  (* Illegal mode crossing: ['k t] has a mutable field [poisoned]. It's safe,
     since [poisoned] protected by [mutex] and not exposed in the API, but
     is not allowed by the type system. *)
  type 'k t : value mod portable uncontended =
    { name : 'k Name.t
    ; mutex : Mutex_.t
    ; mutable poisoned : bool }

  (* CR: illegal mode crossing on the current version of the compiler,
     but should be legal. *)
  type packed : value mod portable uncontended = P : 'k t -> packed

  let name t = t.name

  exception Poisoned

  let with_lock :
    'k t
    -> ('k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Mutex_.lock t.mutex;
      match t.poisoned with
      | true -> Mutex_.unlock t.mutex; raise Poisoned
      | false ->
        match f (Password.unsafe_mk t.name) with
        | x -> Mutex_.unlock t.mutex; x
        | exception exn ->
          t.poisoned <- true;
          (* NOTE: [unlock] does not poll for asynchronous exceptions *)
          Mutex_.unlock t.mutex;
          (match exn with
           | Data_.Encapsulated (exn_name, exn_cap) when Name.equal t.name exn_name ->
             raise (Data_.unsafe_get exn_cap)
           | _ -> raise exn)

  let destroy t =
    Mutex_.lock t.mutex;
    match t.poisoned with
    | true ->
      Mutex_.unlock t.mutex;
      raise Poisoned
    | false ->
      t.poisoned <- true;
      Mutex_.unlock t.mutex;
      Password.unsafe_mk t.name
end

let create_with_mutex () =
  let (P name) = Name.make () in
  Mutex.P { name; mutex = Mutex_.create (); poisoned = false }

module Data = struct
  include Data_

  external raise_with_backtrace:
    exn -> Printexc.raw_backtrace -> 'a @ portable @@ portable = "%raise_with_backtrace"

  exception Protected : 'k Mutex.t * (exn, 'k) t -> exn

  let protect f =
  try f () with
  | exn ->
    let (P mut) = create_with_mutex () in
    raise_with_backtrace (Protected (mut, unsafe_mk exn)) (Printexc.get_raw_backtrace ())
  ;;
end

module Condition = struct
  type 'k t : value mod portable uncontended

  external create : unit -> 'k t @@ portable = "caml_ml_condition_new"
  external wait : 'k t -> Mutex_.t -> unit @@ portable = "caml_ml_condition_wait"
  external signal : 'k t -> unit @@ portable = "caml_ml_condition_signal"
  external broadcast : 'k t -> unit @@ portable = "caml_ml_condition_broadcast"

  let wait t (mut : 'k Mutex.t) _password =
    (* mut is locked, so it must not be poisoned *)
    wait t mut.mutex

end
