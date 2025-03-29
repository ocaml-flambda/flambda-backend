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
  (* CR dkalinichenko: [global] should imply [aliased]. *)
  type 'a t = { global : 'a @@ global aliased } [@@unboxed]
end

open Global

(* Like [int Stdlib.Atomic.t], but supports [local]. *)
module A = struct
  type t : value mod many portable contended

  external make : int -> (t[@local_opt]) @@ portable = "%makemutable"

  external get : (t[@local_opt]) -> int @@ portable = "%atomic_load"
  external compare_exchange : (t[@local_opt]) -> int -> int -> int @@ portable = "%atomic_compare_exchange"
  external fetch_and_add : (t[@local_opt]) -> int -> int @@ portable = "%atomic_fetch_add"

  (* Unsafe unless [A.t] is only accessible by 1 thread. *)
  external get_unsync : (t[@local_opt]) -> int @@ portable = "%field0"
  external set_unsync : (t[@local_opt]) -> int -> unit @@ portable = "%setfield0"
end

(* Like [Stdlib.Magic], but [portable]. *)

module Access : sig
  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod external_ global portable many aliased

  type packed = P : 'k t -> packed [@@unboxed]

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t @@ portable

  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
end = struct
  type dummy

  type 'k t = T : dummy t

  type packed = P : 'k t -> packed [@@unboxed]

  external unsafe_rebrand : 'k t -> 'j t @@ portable = "%identity"

  let[@inline] unsafe_mk (type k) () : k t = unsafe_rebrand T

  let[@inline] equality_witness (type k j) (T : k t) (T : j t) : (k, j) Type.eq =
    Type.Equal

end

let[@inline] current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.unsafe_mk ()

module Password : sig
  type 'k t : value mod many portable contended

  module Id : sig
    type 'k t : immediate

    val equality_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) Type.eq option @@ portable
  end

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t @ local @@ portable
  val id : 'k t @ local -> 'k Id.t @@ portable

  module Shared : sig
    type 'k t : value mod many portable contended

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t @ local unyielding @@ portable
    val id : 'k t @ local -> 'k Id.t @@ portable
  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable
end = struct
  module Id = struct
    type 'k t = int

    let uninitialized = 0
    let ctr = A.make (uninitialized + 1)
    let[@inline] unsafe_mk () = A.fetch_and_add ctr 1

    let[@inline] equality_witness t1 t2 =
      if Int.equal t1 t2
      then Some (Obj.magic Type.Equal)
      else None
  end

  type 'k t = A.t

  let[@inline] unsafe_mk () : _ @@ local = A.make Id.uninitialized
  let[@inline] id t =
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
    let[@inline] id t =
      match A.get t with
      | id when id = Id.uninitialized ->
        let new_id = Id.unsafe_mk () in
        (match A.compare_exchange t Id.uninitialized new_id with
         | id when id = Id.uninitialized -> new_id
         | already_set_id -> already_set_id)
      | set_id -> set_id

    let unsafe_mk () : _ @@ local unyielding = A.make Id.uninitialized
  end

  let[@inline] shared t = t
end

(* Like [Stdlib.raise], but [portable], and the value
   it never returns is also [portable unique] *)
external raise : exn -> 'a @ portable unique @@ portable = "%raise"
external raise_with_backtrace: exn -> Printexc.raw_backtrace -> 'a @ portable @@ portable = "%raise_with_backtrace"
external get_raw_backtrace: unit -> Printexc.raw_backtrace @@ portable = "caml_get_exception_raw_backtrace"

module Data = struct
  type ('a, 'k) t : value mod portable contended

  exception Encapsulated : 'k Password.Id.t * (exn, 'k) t -> exn

  external unsafe_mk : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) @@ portable = "%identity"
  external unsafe_get : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"

  let[@inline] wrap _ t = unsafe_mk t
  let[@inline] unwrap _ t = unsafe_get t
  let[@inline] unwrap_shared _ t = unsafe_get t

  let[@inline] create f = unsafe_mk (f ())

  (* CR-soon mslater/tdelvecchio: copying the backtrace at each reraise can cause quadratic
     behavior when propagating the exception through nested handlers. This should use a
     new reraise-with-current-backtrace primitive that doesn't do the copy. *)
  let reraise_encapsulated password exn =
    raise_with_backtrace (Encapsulated (Password.id password, unsafe_mk exn)) (get_raw_backtrace ())

  let[@inline] map pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated pw exn

  let[@inline] fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1

  let[@inline] snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2

  let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let[@inline] extract pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let inject = unsafe_mk
  let project = unsafe_get

  let[@inline] bind pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  let[@inline] iter pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated pw exn

  module Shared = struct
    type ('a, 'k) data = ('a, 'k) t

    type ('a, 'k) t = ('a, 'k) data

    exception Encapsulated_shared : 'k Password.Id.t * (exn, 'k) t -> exn

    (* CR-soon mslater/tdelvecchio: copying the backtrace at each reraise can cause quadratic
       behavior when propagating the exception through nested handlers. This should use a
       new reraise-with-current-backtrace primitive that doesn't do the copy. *)
    let reraise_encapsulated_shared p e =
      raise_with_backtrace
        (Encapsulated_shared (Password.Shared.id p, unsafe_mk e))
        (get_raw_backtrace ())

    let[@inline] wrap _ v = unsafe_mk v
    let[@inline] unwrap _ t = unsafe_get t
    let[@inline] expose _ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())

    let[@inline] map p f t =
      let v = unsafe_get t in
      match f v with
      | r -> unsafe_mk r
      | exception e -> reraise_encapsulated_shared p e

    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] fst t =
      let x, _ = unsafe_get t in
      unsafe_mk x

    let[@inline] snd t =
      let _, y = unsafe_get t in
      unsafe_mk y

    let[@inline] extract p f t =
      let v = unsafe_get t in
      try f v with e -> reraise_encapsulated_shared p e

    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t

    let[@inline] bind p f t =
      let v = unsafe_get t in
      try f v with e -> reraise_encapsulated_shared p e

    let[@inline] iter p f t =
      let v = unsafe_get t in
      try f v with e -> reraise_encapsulated_shared p e

    let[@inline] map_into pw f t =
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated_shared pw exn

    module Local = struct
      let[@inline] wrap _ v = exclave_ (unsafe_mk v)
      let[@inline] unwrap _ t = exclave_ (unsafe_get t)
      let[@inline] create f = exclave_ (unsafe_mk (f ()))

      let[@inline] map p f t =
        exclave_ (
          let v = unsafe_get t in
          match f v with
          | r -> unsafe_mk r
          | exception e -> reraise_encapsulated_shared p e
        )

      let[@inline] both t1 t2 =
        exclave_ (unsafe_mk (unsafe_get t1, unsafe_get t2))

      let[@inline] fst t =
        exclave_ (
          let x, _ = unsafe_get t in
          unsafe_mk x
        )

      let[@inline] snd t =
        exclave_ (
          let _, y = unsafe_get t in
          unsafe_mk y
        )

      let[@inline] extract p f t =
        exclave_ (
          let v = unsafe_get t in
          try f v with e -> reraise_encapsulated_shared p e
        )

      let[@inline] inject v = exclave_ (unsafe_mk v)
      let[@inline] project t = exclave_ (unsafe_get t)

      let[@inline] bind p f t =
        exclave_ (
          let v = unsafe_get t in
          try f v with e -> reraise_encapsulated_shared p e
        )

      let[@inline] iter p f t =
        let v = unsafe_get t in
        try f v with e -> reraise_encapsulated_shared p e

      let[@inline] map_into pw f t = exclave_ (
        let v = unsafe_get t in
        match f v with
        | res -> unsafe_mk res
        | exception exn -> reraise_encapsulated_shared pw exn
      )
    end
  end

  let reraise_encapsulated_shared = Shared.reraise_encapsulated_shared

  let[@inline] map_shared pw f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated_shared pw exn

  let[@inline] extract_shared pw f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated_shared pw exn

  module Local = struct
    let[@inline] wrap _ t = exclave_ unsafe_mk t
    let[@inline] unwrap _ t = exclave_ unsafe_get t
    let[@inline] unwrap_shared _ t = exclave_ unsafe_get t
    let[@inline] create f = exclave_ unsafe_mk (f ())

    let[@inline] map pw f t = exclave_ (
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated pw exn
    )

    let[@inline] fst t = exclave_ (
      let t1, _ = unsafe_get t in
      unsafe_mk t1
    )

    let[@inline] snd t = exclave_ (
      let _, t2 = unsafe_get t in
      unsafe_mk t2
    )

    let[@inline] both t1 t2 = exclave_ unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] extract pw f t = exclave_ (
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated pw exn
    )

    let[@inline] inject v = exclave_ unsafe_mk v
    let[@inline] project t = exclave_ unsafe_get t

    let[@inline] bind pw f t = exclave_ (
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated pw exn
    )

    let[@inline] iter pw f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated pw exn

    let[@inline] map_shared pw f t = exclave_ (
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated_shared pw exn
    )

    let[@inline] extract_shared pw f t = exclave_ (
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated_shared pw exn
    )
  end
end

exception Encapsulated = Data.Encapsulated
exception Encapsulated_shared = Data.Shared.Encapsulated_shared

module Key : sig
  type 'k t : value mod external_ portable contended

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

  val with_password_shared :
    'k t
    -> ('k Password.Shared.t @ local unyielding -> 'a) @ local
    -> 'a
    @@ portable

  val with_password_shared_local :
    'k t
    -> ('k Password.Shared.t @ local unyielding -> 'a @ local) @ local
    -> 'a @ local
    @@ portable

  val access :
    'k t @ unique
    -> ('k Access.t -> 'a @ unique portable contended) @ local portable
    -> 'a * 'k t @ unique portable contended @@ portable

  val access_local :
    'k t @ unique
    -> ('k Access.t -> 'a @ local unique portable contended) @ local portable
    -> 'a * 'k t @ local unique portable contended @@ portable

  val access_shared :
    'k t
    -> ('k Access.t @ shared -> 'a @ portable contended) @ local portable
    -> 'a @ portable contended
    @@ portable

  val access_shared_local :
    'k t
    -> ('k Access.t @ shared -> 'a @ local portable contended) @ local portable
    -> 'a @ local portable contended
    @@ portable

  val globalize_unique : 'k t @ local unique -> 'k t @ unique @@ portable

  val destroy : 'k t @ unique -> 'k Access.t @@ portable
end = struct
  type 'k t = unit

  type packed = P : 'k t -> packed [@@unboxed]

  let[@inline] unsafe_mk () = ()

  let[@inline] with_password_shared (type k) _ f =
    let pw : k Password.Shared.t = Password.Shared.unsafe_mk () in
    try f pw with
    | Encapsulated_shared (id, data) as exn ->
      let exn =
        match Password.Id.equality_witness (Password.Shared.id pw) id with
        | Some Equal -> Data.unsafe_get data
        | None -> exn
      in
      raise exn

  let[@inline] with_password_shared_local (type k) _ f =
    exclave_ (
      let pw : k Password.Shared.t = Password.Shared.unsafe_mk () in
      try f pw with
      | Encapsulated_shared (id, data) as exn ->
        let exn =
          match Password.Id.equality_witness (Password.Shared.id pw) id with
          | Some Equal -> Data.unsafe_get data
          | None -> exn
        in
        raise exn
    )

  let[@inline] with_password (type k) k f =
    let pw : k Password.t = Password.unsafe_mk () in
    try f pw, k with
    | exn ->
      let exn =
        match exn with
        | Encapsulated (id, data) ->
          (match Password.Id.equality_witness (Password.id pw) id with
           | Some Equal -> Data.unsafe_get data
           | None -> exn)
        | Encapsulated_shared (id, data) ->
          (match Password.Id.equality_witness (Password.id pw) id with
           | Some Equal -> Data.unsafe_get data
           | None -> exn)
        | _ -> exn
      in
      raise exn

  let[@inline] with_password_local (type k) _ f =
    exclave_ (
      let pw : k Password.t = Password.unsafe_mk () in
      try f pw with
      | exn ->
        let exn =
          match exn with
          | Encapsulated (id, data) ->
            (match Password.Id.equality_witness (Password.id pw) id with
             | Some Equal -> Data.unsafe_get data
             | None -> exn)
          | Encapsulated_shared (id, data) ->
            (match Password.Id.equality_witness (Password.id pw) id with
             | Some Equal -> Data.unsafe_get data
             | None -> exn)
          | _ -> exn
        in
        raise exn
    )

  let[@inline] access k f = f (Access.unsafe_mk ()), k
  let[@inline] access_local k f = exclave_ (f (Access.unsafe_mk ()), k)

  let[@inline] access_shared _ f =
    let c : 'k Access.t = Access.unsafe_mk () in
    f c

  let[@inline] access_shared_local _ f =
    let c : 'k Access.t = Access.unsafe_mk () in
    exclave_ (f c)

  let[@inline] globalize_unique k = k

  let[@inline] destroy _ = Access.unsafe_mk ()
end

let[@inline] create () = Key.P (Key.unsafe_mk ())

let[@inline] access_local (type k) (pw : k Password.t) f =
  exclave_ (
    let c : k Access.t = Access.unsafe_mk () in
    match f c with
    | res -> res
    | exception exn -> Data.reraise_encapsulated pw exn
  )

let[@inline] access pw f =
  (access_local pw (fun access -> { global = f access })).global

let[@inline] access_shared_local (type k) (pw : k Password.Shared.t) f =
  exclave_ (
    let c : k Access.t = Access.unsafe_mk () in
    match f c with
    | res -> res
    | exception exn -> Data.reraise_encapsulated_shared pw exn
  )

let[@inline] access_shared pw f =
  (access_shared_local pw (fun access -> { global = f access })).global

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t : value mod portable contended
  external create: unit -> t @@ portable = "caml_capsule_mutex_new"
  external lock: t @ local -> unit @@ portable = "caml_capsule_mutex_lock"
  external unlock: t @ local -> unit @@ portable = "caml_capsule_mutex_unlock"
end

(* Reader writer lock *)
module Rw = struct
  type t : value mod portable contended
  external create: unit -> t @@ portable = "caml_capsule_rwlock_new"
  external lock_read: t @ local -> unit @@ portable = "caml_capsule_rwlock_rdlock"
  external lock_write: t @ local -> unit @@ portable = "caml_capsule_rwlock_wrlock"
  external unlock: t @ local -> unit @@ portable = "caml_capsule_rwlock_unlock"
end

module Mutex = struct

  type 'k t : value mod portable contended =
    { mutex : M.t
    ; mutable poisoned : bool
    }
  [@@unsafe_allow_any_mode_crossing]

  type packed : value mod portable contended = P : 'k t -> packed
  [@@unsafe_allow_any_mode_crossing
    "CR layouts v2.8: illegal mode crossing on the current version of the compiler, but \
     should be legal."]

  let[@inline] create _ = { mutex = M.create (); poisoned = false }

  exception Poisoned

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
        let pw : k Password.t = Password.unsafe_mk () in
        match f pw with
        | x -> M.unlock t.mutex; x
        | exception exn ->
          let exn =
            match exn with
            | Encapsulated (id, data) ->
              (match Password.Id.equality_witness (Password.id pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | Encapsulated_shared (id, data) ->
              (match Password.Id.equality_witness (Password.id pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
          t.poisoned <- true;
          M.unlock t.mutex;
          raise exn

  let[@inline] destroy t =
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

module Rwlock = struct
  type state = Open | Frozen | Poisoned

  type 'k t : value mod portable contended =
    { rwlock : Rw.t
    ; mutable state : state
    }
  [@@unsafe_allow_any_mode_crossing]

  type packed : value mod portable contended = P : 'k t -> packed
  [@@unsafe_allow_any_mode_crossing
    "CR layouts v2.8: This can go away once we have proper mode crossing \
     inference for GADT constructors "]

  let[@inline] create _ = { rwlock = Rw.create (); state = Open }

  exception Frozen
  exception Poisoned

  let[@inline] with_write_lock :
    type k.
    k t
    -> (k Password.t @ local -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Rw.lock_write t.rwlock;
      match t.state with
      | Poisoned -> Rw.unlock t.rwlock; raise Poisoned
      | Frozen -> Rw.unlock t.rwlock; raise Frozen
      | Open ->
        let pw : k Password.t = Password.unsafe_mk () in
        match f pw with
        | x -> Rw.unlock t.rwlock; x
        | exception exn ->
          let exn =
            match exn with
            | Encapsulated (id, data) ->
              (match Password.Id.equality_witness (Password.id pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | Encapsulated_shared (id, data) ->
              (match Password.Id.equality_witness (Password.id pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
          Rw.unlock t.rwlock;
          t.state <- Poisoned;
          raise exn

  let[@inline] with_read_lock :
    type k.
    k t
    -> (k Password.Shared.t @ local unyielding -> 'a) @ local
    -> 'a
    @@ portable
    = fun t f ->
      Rw.lock_read t.rwlock;
      match t.state with
      | Poisoned -> Rw.unlock t.rwlock; raise Poisoned
      | Open | Frozen ->
        let pw : k Password.Shared.t = Password.Shared.unsafe_mk () in
        match f pw with
        | x -> Rw.unlock t.rwlock; x
        | exception exn ->
          let exn =
            match exn with
            | Encapsulated_shared (id, data) ->
              (match Password.Id.equality_witness (Password.Shared.id pw) id with
               | Some Equal -> Data.unsafe_get data
               | None -> exn)
            | _ -> exn
          in
          (* This racy write is ok according to the memory model, because:
             1. All threads which race to write here are writing [Frozen],
                and the only other write to this field in the program was
                the initial write setting it to [Open].
             2. All threads which race to read here do not distinguish between
                [Open] and [Frozen].
             All operations that distinguish between [Open] and [Frozen]
             are protected by the write lock. *)
          t.state <- Frozen;
          Rw.unlock t.rwlock;
          raise exn

  let[@inline] freeze t =
    Rw.lock_write t.rwlock;
    match t.state with
    | Poisoned -> raise Poisoned
    | Open | Frozen ->
      t.state <- Frozen;
      Rw.unlock t.rwlock;
      Key.unsafe_mk ()

  let[@inline] destroy t =
    Rw.lock_write t.rwlock;
    match t.state with
    | Poisoned ->
      Rw.unlock t.rwlock;
      raise Poisoned
    | Frozen ->
      Rw.unlock t.rwlock;
      raise Frozen
    | Open ->
      t.state <- Poisoned;
      Rw.unlock t.rwlock;
      Key.unsafe_mk ()
end

module Condition = struct
  type 'k t : value mod portable contended

  external create : unit -> 'k t @@ portable = "caml_capsule_condition_new"
  external wait : 'k t -> M.t -> unit @@ portable = "caml_capsule_condition_wait"
  external signal : 'k t -> unit @@ portable = "caml_capsule_condition_signal"
  external broadcast : 'k t -> unit @@ portable = "caml_capsule_condition_broadcast"

  let[@inline] wait t (mut : 'k Mutex.t) _password =
    (* [mut] is locked, so we know it is not poisoned. *)
    wait t mut.mutex
end
