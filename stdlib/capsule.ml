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

module Global = struct
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

open Global

(* Like [int Stdlib.Atomic.t], but [portable]. *)
module A = struct
  type t : value mod portable uncontended

  external make : int -> t @@ portable = "%makemutable"

  external get : (t[@local_opt]) -> int @@ portable = "%atomic_load"
  external compare_exchange : (t[@local_opt]) -> int -> int -> int @@ portable = "%atomic_compare_exchange"
  external fetch_and_add : (t[@local_opt]) -> int -> int @@ portable = "%atomic_fetch_add"

  (* Unsafe unless [A.t] is only accessible by 1 thread. *)
  external get_unsync : (t[@local_opt]) -> int @@ portable = "%field0"
  external set_unsync : (t[@local_opt]) -> int -> unit @@ portable = "%setfield0"
end

(* Like [Stdlib.Magic], but [portable]. *)
module O = struct
  external magic : 'a -> 'b @@ portable = "%obj_magic"
end

(* Like [Stdlib.( = ), but [portable]. *)
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%equal"

module Access : sig
  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod external_ global portable many unique

  type packed = P : 'k t -> packed [@@unboxed]

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t @@ portable

  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
end = struct
  type dummy

  type 'k t = T : dummy t

  type packed = P : 'k t -> packed [@@unboxed]

  external unsafe_rebrand : 'k t -> 'j t @@ portable = "%identity"

  let unsafe_mk (type k) () : k t = unsafe_rebrand T

  let equality_witness (type k j) (T : k t) (T : j t) : (k, j) Type.eq =
    Type.Equal

end

let current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.unsafe_mk ()

module Password : sig
  type 'k t : value mod portable uncontended

  module Id : sig
    type 'k t : value mod external_ global portable many uncontended unique

    val equality_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) Type.eq option @@ portable
  end

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t @@ portable
  val id : 'k t @ local -> 'k Id.t @@ portable

  module Shared : sig
    type 'k t : value mod portable uncontended

    val id : 'k t @ local -> 'k Id.t @@ portable
  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable
end = struct
  module Id = struct
    type 'k t = int

    let uninitialized = 0
    let ctr = A.make (uninitialized + 1)
    let unsafe_mk () = A.fetch_and_add ctr 1

    let equality_witness t1 t2 =
      if t1 = t2
      then Some (O.magic Type.Equal)
      else None
  end

  type 'k t = A.t

  let unsafe_mk () = A.make Id.uninitialized
  let id t =
    (* Safe since [Password.t] is only ever accessible by 1 fiber. *)
    match A.get_unsync t with
    | id when id = Id.uninitialized ->
      let set_id = Id.unsafe_mk () in
      A.set_unsync t set_id;
      set_id
    | set_id -> set_id

  module Shared = struct
    type 'k t = A.t

    (* Multiple fibers can access the same [Password.Shared.t] concurrently.
       Therefore, we use atomic operations. *)
    let id t =
      match A.get t with
      | id when id = Id.uninitialized ->
        let new_id = Id.unsafe_mk () in
        (match A.compare_exchange t Id.uninitialized new_id with
        | id when id = Id.uninitialized -> new_id
        | already_set_id -> already_set_id)
      | set_id -> set_id
  end

  let shared t = t
end

(* Like [Stdlib.raise], but [portable], and the value
   it never returns is also [portable unique] *)
external raise : exn -> 'a @ portable unique @@ portable = "%raise"
external raise_with_backtrace: exn -> Printexc.raw_backtrace -> 'a @ portable @@ portable = "%raise_with_backtrace"
external get_raw_backtrace: unit -> Printexc.raw_backtrace @@ portable = "caml_get_exception_raw_backtrace"

module Data = struct
  type ('a, 'k) t : value mod portable uncontended

  exception Encapsulated : 'k Password.Id.t * (exn, 'k) t -> exn

  external unsafe_mk : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) @@ portable = "%identity"

  external unsafe_get : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"

  let[@inline] wrap _ t = unsafe_mk t
  let[@inline] wrap_local _ t = exclave_ unsafe_mk t

  let[@inline] unwrap _ t = unsafe_get t
  let[@inline] unwrap_local _ t = exclave_ unsafe_get t

  let[@inline] unwrap_shared _ t = unsafe_get t
  let[@inline] unwrap_shared_local _ t = exclave_ unsafe_get t

  let[@inline] create f = unsafe_mk (f ())
  let[@inline] create_local f = exclave_ unsafe_mk (f ())

  (* CR-soon mslater/tdelvecchio: copying the backtrace at each reraise can cause quadratic
     behavior when propagating the exception through nested handlers. This should use a
     new reraise-with-current-backtrace primitive that doesn't do the copy. *)
  let[@inline] reraise_encapsulated password exn =
    raise_with_backtrace (Encapsulated (Password.id password, unsafe_mk exn)) (get_raw_backtrace ())

  let[@inline] reraise_encapsulated_shared password exn =
    raise_with_backtrace (Encapsulated (Password.Shared.id password, unsafe_mk exn)) (get_raw_backtrace ())

  let[@inline] map pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated pw exn

  let[@inline] map_local pw f t = exclave_
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated pw exn

  let[@inline] fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1

  let[@inline] fst_local t = exclave_
    let t1, _ = unsafe_get t in
    unsafe_mk t1

  let[@inline] snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2

  let[@inline] snd_local t = exclave_
    let _, t2 = unsafe_get t in
    unsafe_mk t2

  let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)
  let[@inline] both_local t1 t2 = exclave_ unsafe_mk (unsafe_get t1, unsafe_get t2)

  let[@inline] extract pw f t =
    let v = unsafe_get t in
    try f v with
    |  exn -> reraise_encapsulated pw exn

  let[@inline] extract_local pw f t = exclave_
    let v = unsafe_get t in
    try f v with
    |  exn -> reraise_encapsulated pw exn

  let inject = unsafe_mk
  let inject_local = unsafe_mk

  let project = unsafe_get
  let project_local = unsafe_get

  let[@inline] bind pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let[@inline] bind_local pw f t = exclave_
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let[@inline] iter pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let[@inline] iter_local pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let[@inline] map_shared pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated_shared pw exn

  let[@inline] map_shared_local pw f t = exclave_
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated_shared pw exn


  let[@inline] extract_shared pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated_shared pw exn

  let[@inline] extract_shared_local pw f t = exclave_
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated_shared pw exn

end

exception Encapsulated = Data.Encapsulated

module Key : sig
  type 'k t : value mod external_ portable uncontended

  type packed = P : 'k t -> packed [@@unboxed]

  val unsafe_mk : unit -> 'k t @ unique @@ portable

  val with_password :
    'k t @ unique
    -> ('k Password.t @ local -> 'a @ unique) @ local
    -> 'a * 'k t @ unique @@ portable
  val with_password_local :
    'k t @ unique
    -> ('k Password.t @ local -> 'a @ local) @ local
    -> 'a @ local @@ portable

  val access :
    'k t @ unique
    -> ('k Access.t -> 'a @ unique portable contended) @ local portable
    -> 'a * 'k t @ unique portable contended @@ portable

    val access_local :
    'k t @ unique
    -> ('k Access.t -> 'a @ local unique portable contended) @ local portable
    -> 'a * 'k t Modes.Global.t @ local unique portable contended
    @@ portable


  val destroy : 'k t @ unique -> 'k Access.t @@ portable
end = struct
  type 'k t : value mod external_ portable uncontended = unit

  type packed = P : 'k t -> packed [@@unboxed]

  let unsafe_mk () = ()

  let[@inline] with_password (type k) (_ : k t @@ unique) (f : _ @ local -> _ @ unique) =
    let pw : k Password.t = Password.unsafe_mk () in
    try (f pw, () : _ @@ unique) with
    | Encapsulated (id, data) as exn ->
      let exn =
        match Password.Id.equality_witness (Password.id pw) id with
        | Some Equal -> Data.unsafe_get data
        | None -> exn
      in
      raise exn
  let[@inline] with_password_local (type k) (_ : k t @@ unique) (f : _ @ local -> _ @ local) =
    let pw : k Password.t = Password.unsafe_mk () in
    exclave_ (try (f pw : _ @@ local) with
    | Encapsulated (id, data) as exn ->
      let exn =
        match Password.Id.equality_witness (Password.id pw) id with
        | Some Equal -> Data.unsafe_get data
        | None -> exn
      in
      raise exn)

  let[@inline] access (_ : _ t @@ unique) (f : _ @ unique -> _ @ unique portable contended) = (f (Access.unsafe_mk ()), () : _ @@ unique portable contended)
  let[@inline] access_local (type k) (_ : k t @@ unique) (f : _ @ unique -> _ @ local unique portable contended) = exclave_ (f (Access.unsafe_mk ()), (Modes.Global.{global=()} : k t Modes.Global.t @@ unique portable contended) : _ @@ local unique portable contended)

  let destroy _ = Access.unsafe_mk ()
end

let[@inline] create () = Key.P (Key.unsafe_mk ())

let[@inline] access_local (type k) (pw : k Password.t) f = exclave_
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated pw exn

let[@inline] access pw f =
  (access_local pw (fun access -> { global = f access })).global

let[@inline] access_shared_local (type k) (pw : k Password.Shared.t) f = exclave_
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated_shared pw exn

let[@inline] access_shared pw f =
  (access_shared_local pw (fun access -> { global = f access })).global

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t : value mod portable uncontended
  external create: unit -> t @@ portable = "caml_ml_mutex_new"
  external lock: t @ local -> unit @@ portable = "caml_ml_mutex_lock"
  external unlock: t @ local -> unit @@ portable = "caml_ml_mutex_unlock"
end



module Mutex = struct

  (* Illegal mode crossing: ['k t] has a mutable field [poisoned]. It's safe,
     since [poisoned] protected by [mutex] and not exposed in the API, but
     is not allowed by the type system. *)
  type 'k t : value mod portable uncontended =
    { pw : 'k Password.t
    ; mutex : M.t
    ; mutable poisoned : bool
    }

  (* CR: illegal mode crossing on the current version of the compiler,
     but should be legal. *)
  type packed : value mod portable uncontended = P : 'k t -> packed

  let create _ = { pw = Password.unsafe_mk (); mutex = M.create (); poisoned = false }

  exception Poisoned

  let id t =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      raise Poisoned
    | false ->
      let id = Password.id t.pw in
      M.unlock t.mutex;
      id

  let[@inline] with_lock :
    type k.
    k t
    -> (k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      M.lock t.mutex;
      match t.poisoned with
      | true -> M.unlock t.mutex; raise Poisoned
      | false ->
        match f t.pw with
        | x -> M.unlock t.mutex; x
        | exception exn ->
          t.poisoned <- true;
          (* NOTE: [unlock] does not poll for asynchronous exceptions *)
          M.unlock t.mutex;
          let exn =
            match exn with
            | Encapsulated (id, data) ->
              (match Password.Id.equality_witness (Password.id t.pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
          raise exn

  let destroy t =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      raise Poisoned
    | false ->
      t.poisoned <- true;
      M.unlock t.mutex;
      Key.unsafe_mk ()
end


module Condition = struct

  type 'k t : value mod portable uncontended

  external create : unit -> 'k t @@ portable = "caml_ml_condition_new"
  external wait : 'k t -> M.t -> unit @@ portable = "caml_ml_condition_wait"
  external signal : 'k t -> unit @@ portable = "caml_ml_condition_signal"
  external broadcast : 'k t -> unit @@ portable = "caml_ml_condition_broadcast"

  let wait t (mut : 'k Mutex.t) _password =
    (* [mut] is locked, so we know it is not poisoned. *)
    wait t mut.mutex

end
