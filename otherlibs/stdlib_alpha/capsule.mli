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

(** Capsules are a mechanism for safely having [uncontended]
    access to mutable data from multiple domains. The interface
    in this module ensures that only one domain can have [uncontended]
    access to that data at a time.

    We consider every piece of mutable data in the program to live
    inside of some capsule. This might be an explicit capsule created
    by the user using this interface, or an implicit capsule created
    when a new domain is started. Whenever some thread is executing
    a function it has uncontended access to a single capsule and any
    new mutable data it creates is created within that capsule. We say
    that the function is "running within" the capsule.

    Capsules are only ever associated with threads from one domain at
    a time, which ensures there are no data races in the program. The
    implicit capsules created when a new domain is started are only ever
    associated with threads in that domain. Explicit capsules can change
    which domains have uncontended access to them using synchronization
    primitives. Currently each explicit capsule has an associated mutex
    that controls access to it. In the future there will also be other
    synchronization mechanisms available to control access to explicit capsules.

    Each explicit capsule is associated with a type "brand" -- written ['k]
    throughout this interface -- which allows us to statically reason about
    access to the capsule within the type system.  In the documentation of
    this interface we will often use ['k] to refer to the capsule associated
    with that brand. *)

(** A [Name.t] is used to enable runtime identification of capsules. *)
module Name : sig

  type 'k t : value mod global portable many uncontended unique
  (** A ['k Name.t] represents the identity of a capsule. *)

  val equality_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) Type.eq option @@ portable
  (** [equality_witness a b] performs a runtime check that the two given names
      identify the same capsule, returning a witness of the equality if so. *)

end

(** An [Access.t] allows wraping and unwraping [Data.t] values from the current
    capsule. *)
module Access : sig

  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod external_ global portable many unique
  (** ['k t] represents access to the current capsule, allowing wraping
      and unwraping [Data.t] values. An [uncontended] ['k t] indicates
      that ['k] is the current capsule. A [shared] ['k t] indicates that
      ['k] is the current capsule but that it may be shared with other
      domains. *)

  type packed = P : 'k t -> packed [@@unboxed]
  (** [packed] is the type of access to some unknown capsule.
      Unpacking one provides a ['k t] together with a fresh existential
      type brand for ['k]. *)

  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
  (** [equality_witness a b] returns a witness that the brands of [a]
      and [b] are equal. This must be true since they are both present
      uncontended within the same capsule. *)

end

(** Obtain as [Access.t] for the current capsule. Since we do not
    know the brand for the current capsule, we receive a fresh
    one. *)
val current : unit -> Access.packed @@ portable

(** The brand for the initial capsule. *)
type initial

(** An [Access.t] for the initial capsule *)
val initial : initial Access.t

(** Passwords represent permission to get access to a capsule. *)
module Password : sig

  (* CR layouts v5: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t : value mod external_ portable many unique uncontended
  (** ['k t] is the type of "passwords" representing permission for the
     current fiber to have [uncontended] access to the capsule
     ['k]. They are only ever avilable locally, so that they cannot move
     between fibers.

      Obtaining a ['k t] requires acquiring the mutex associated with
     ['k], and modes prevent retaining the [t] after releasing the
     mutex. This guarantees that uncontended access to the capsule is
     only granted to a single domain at once. *)

  type packed = P : 'k t -> packed [@@unboxed]
  (** [packed] is the type of a password for some unknown capsule.
      Unpacking one provides a ['k t] together with a fresh existential
      type brand for ['k]. *)

  val name : 'k t @ local -> 'k Name.t @@ portable
  (** [name t] identifies the capsule that [t] is associated with. *)

  (** Shared passwords represent permission to get shared access to a capsule *)
  module Shared : sig

    type 'k t : value mod external_ portable many unique uncontended
    (** ['k t] is the type of "shared passwords" representing permission
        for the current fiber to have [shared] access to the capsule
        ['k]. They are only ever avilable locally, so that they cannot
        move between fibers.

        Obtaining a ['k t] requires read acquire the reader-writer lock
        associate with ['k]. *)

    val name : 'k t @ local -> 'k Name.t @@ portable
    (** [name t] identifies the capsule that [t] is associated with. *)

  end

  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable
  (** [shared t] is a shared password for the same capsule as [t]. *)

end

val access :
  'k Password.t @ local
  -> ('k Access.t -> 'a @ portable contended) @ local portable
  -> 'a @ portable contended
  @@ portable
(** [access p f] runs [f] within the capsule ['k], providing it with
    an {!Access.t} for ['k]. The result is within ['k] so it must be
    [portable] and it is marked [contended]. *)

val access_local :
  'k Password.t @ local
  -> ('k Access.t -> 'a @ local portable contended) @ local portable
  -> 'a @ local portable contended
  @@ portable
(** [access_local p f] runs [f] within the capsule ['k], providing it with
    an {!Access.t} for ['k]. The result is within ['k] so it must be
    [portable] and it is marked [contended]. *)

val access_shared :
  'k Password.Shared.t @ local
  -> ('k Access.t @ shared -> 'a @ portable contended) @ local portable
  -> 'a @ portable contended
  @@ portable
(** [shared_access p f] runs [f] within the capsule ['k], providing it
    with a shared {!Access.t} for ['k]. The result is within ['k] so it
    must be [portable] and it is marked [contended]. *)

val access_shared_local :
  'k Password.Shared.t @ local
  -> ('k Access.t @ shared -> 'a @ local portable contended) @ local portable
  -> 'a @ local portable contended
  @@ portable
(** [shared_access_local p f] runs [f] within the capsule ['k], providing it
    with a shared {!Access.t} for ['k]. The result is within ['k] so it
    must be [portable] and it is marked [contended]. *)

(** Does *not* require runtime5. In runtime4, implemented as a no-op, hence
    does not provide mutual exclusion between systhreads.  *)
module Mutex : sig

    type 'k t : value mod portable uncontended
    (** ['k t] is the type of the mutex that controls access to
        the capsule ['k]. This mutex is created when creating
        the capsule ['k] using {!create_with_mutex}. *)

    type packed : value mod portable uncontended = P : 'k t -> packed
    (** [packed] is the type of a mutex for some unknown capsule.
        Unpacking one provides a ['k Mutex.t] together with a fresh
        existential type brand for ['k]. *)

    val name : 'k t @ local -> 'k Name.t @@ portable
    (** [name m] identifies the capsule that [m] is associated with. *)

    exception Poisoned
    (** Mutexes can get marked as poisoned. Any operations on a poisoned mutex
        raise the [Poisoned] exception. *)

    val with_lock :
        'k t
        -> ('k Password.t @ local -> 'a) @ local
        -> 'a
        @@ portable
    (** [with_lock m f] tries to acquire the mutex [m]. If [m] is already
        locked, blocks the current thread until it's unlocked. If successful,
        provides [f] a password for the capsule ['k] associated with [m].

        If [f] raises an exception, the mutex is marked as poisoned.

        If [m] is already locked by the current thread, raises [Sys_error]. *)

    val destroy : 'k t -> 'k Access.t @@ portable
    (** [destroy m] acquires the mutex [m] and merges the capsule ['k]
        with the current capsule by leaking access to it. It marks the
        lock as poisoned. *)
end

(** Requires runtime5. *)
module Rwlock : sig

    type 'k t : value mod portable uncontended
    (** ['k t] is the type of the reader-writer lock that controls reader and writer
        access to the capsule ['k]. This reader-writer lock can be created when creating
        the capsule ['k] using {!create_with_rwlock} *)

    type packed : value mod portable uncontended = P : 'k t -> packed
    (** [packed] is the type of a reader-writer lock for some unknown capsule.
        Unpacking one provides a ['k Rwlock.t] together with a fresh existential type
        brand for ['k]. *)

    exception Poisoned
    (** Reader-writer locks can get marked as poisoned. Any operations on a poisoned
        reader-writer lock raise the [Poisoned] exception. *)

    val with_write_lock :
        'k t
        -> ('k Password.t @ local -> 'a) @ local
        -> 'a
        @@ portable
    (** [with_write_lock rw f] tries to write acquire the rwlock [rw]. If [rw] is already
        write or read locked, blocks the current thread until it's unlocked. If successful,
        provides [f] a password for the capsule ['k] associated with [rw].

        If [f] raises an exception, the mutex is marked as poisoned. *)

    val with_read_lock :
        'k t
        -> ('k Password.Shared.t @ local -> 'a) @ local
        -> 'a
        @@ portable
    (** [with_read_lock rw f] tries to read acquire the rwlock [rw]. If [rw] is already
        write locked, increases the reader count by one until it's unlocked. If successful,
        provides [f] a password for the capsule ['k] associated with [rw].

        If [f] raises an exception, the reader-writer lock decreases the reader count,
        and reraises the exception. Note that unlike [with_write_lock], [rw] is not
        poisoned: doing so would create a data-race with other readers trying to read
        from the [poison] bit. Since readers can't write into a capsule, they should not
        be able to leave protected data in a bad state  *)

    val destroy : 'k t -> 'k Access.t @@ portable
    (** [destroy rw] write acquires the rwlock [rw] and merges the capsule ['k]
        with the current capsule by leaking access to it. It marks the
        lock as poisoned. *)
end

(** Requires runtime5. *)
module Condition : sig

  type 'k t : value mod portable uncontended
  (** ['k t] is the type of a condition variable associated with the capsule ['k].
      This condition may only be used with the matching ['k Mutex.t]. *)

  val create : unit -> 'k t @@ portable
  (** [create ()] creates and returns a new condition variable.
      This condition variable is associated with the matching ['k Mutex.t]
      and with a certain property {i P} that is protected by the mutex. *)

  val wait : 'k t -> 'k Mutex.t -> 'k Password.t @ local -> unit @@ portable
  (** [wait c m] atomically unlocks the mutex [m] and suspends the
      current thread on the condition variable [c]. This thread can
      later be woken up after the condition variable [c] has been signaled
      via {!signal} or {!broadcast}; however, it can also be woken up for
      no reason. The mutex [m] is locked again before [wait] returns. One
      cannot assume that the property {i P} associated with the condition
      variable [c] holds when [wait] returns; one must explicitly test
      whether {i P} holds after calling [wait]. *)

  val signal : 'k t -> unit @@ portable
  (** [signal c] wakes up one of the threads waiting on the condition
      variable [c], if there is one. If there is none, this call has no effect.
      It is recommended to call [signal c] inside a critical section, that is,
      while the mutex [m] associated with [c] is locked. *)

  val broadcast : 'k t -> unit @@ portable
  (** [broadcast c] wakes up all threads waiting on the condition
      variable [c]. If there are none, this call has no effect.
      It is recommended to call [broadcast c] inside a critical section,
      that is, while the mutex [m] associated with [c] is locked. *)
end

val create_with_mutex : unit -> Mutex.packed @@ portable
(** [create_with_mutex ()] creates a new capsule with an associated mutex. *)

val create_with_rwlock : unit -> Rwlock.packed @@ portable
(** [create_with_rwlock ()] creates a new capsule with an associated reader-writer lock. *)

(** Pointers to data within a capsule. *)
module Data : sig

    type ('a, 'k) t : value mod portable uncontended
    (** [('a, 'k) t] is the type of ['a]s within the capsule ['k]. It
        can be passed between domains.  Operations on [('a, 'k) t]
        require a ['k Password.t], created from the ['k Mutex.t]. *)

    val wrap :
      'k Access.t @ local shared
      -> 'a
      -> ('a, 'k) t
      @@ portable
    (** [wrap c v] is a pointer to a value [v] from the
        current capsule. *)

    val wrap_local :
      'k Access.t @ local shared
      -> 'a @ local
      -> ('a, 'k) t @ local
      @@ portable
    (** [wrap_local c v] is a pointer to a value [v] from the
        current capsule. *)

    val unwrap :
      'k Access.t @ local
      -> ('a, 'k) t
      -> 'a
      @@ portable
    (** [unwrap c t] returns the value of [t] which is from the
        current capsule. *)

    val unwrap_local :
      'k Access.t @ local
      -> ('a, 'k) t @ local
      -> 'a @ local
      @@ portable
    (** [unwrap_local c t] returns the value of [t] which is from the
        current capsule. *)

    val unwrap_shared :
      ('a : value mod portable) 'k.
      'k Access.t @ local shared
      -> ('a, 'k) t
      -> 'a @ shared
      @@ portable
    (** [unwrap_shared c t] returns the shared value of [t] which is from
        the current capsule. *)

    val unwrap_shared_local :
      ('a : value mod portable) 'k.
      'k Access.t @ local shared
      -> ('a, 'k) t @ local
      -> 'a @ local shared
      @@ portable
    (** [unwrap_shared_local c t] returns the shared value of [t] which is from
        the current capsule. *)

    val create :
      (unit -> 'a) @ local portable
      -> ('a, 'k) t
      @@ portable
    (** [create f] runs [f] within the capsule ['k] and creates a pointer to
        the result of [f]. *)

    val create_local :
      (unit -> 'a @ local) @ local portable
      -> ('a, 'k) t @ local
      @@ portable
    (** [create_local f] runs [f] within the capsule ['k] and creates a pointer to
        the result of [f]. *)

    val map :
      'k Password.t @ local
      -> ('a -> 'b) @ local portable
      -> ('a, 'k) t
      -> ('b, 'k) t
      @@ portable
    (** [map p f t] applies [f] to the value of [p] within the capsule ['k],
        creating a pointer to the result. *)

    val map_local :
      'k Password.t @ local
      -> ('a @ local -> 'b @ local) @ local portable
      -> ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      @@ portable
    (** [map_local p f t] applies [f] to the value of [p] within the capsule ['k],
        creating a pointer to the result. *)

    val both :
      ('a, 'k) t
      -> ('b, 'k) t
      -> ('a * 'b, 'k) t
      @@ portable
    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)

    val both_local :
      ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      -> ('a * 'b, 'k) t @ local
      @@ portable
    (** [both_local t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)

    val fst :
      ('a * 'b, 'k) t
      -> ('a, 'k) t
      @@ portable
    (** [fst t] gives a pointer to the first value inside [t] *)

    val fst_local :
      ('a * 'b, 'k) t @ local
      -> ('a, 'k) t @ local
      @@ portable
    (** [fst_local t] gives a pointer to the first value inside [t] *)

    val snd :
      ('a * 'b, 'k) t
      -> ('b, 'k) t
      @@ portable
    (** [snd t] gives a pointer to the second value inside [t] *)

    val snd_local :
      ('a * 'b, 'k) t @ local
      -> ('b, 'k) t @ local
      @@ portable
    (** [snd_local t] gives a pointer to the second value inside [t] *)

    val extract :
      'k Password.t @ local
      -> ('a -> 'b @ portable contended) @ local portable
      -> ('a, 'k) t
      -> 'b @ portable contended
      @@ portable
    (** [extract p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. *)

    val extract_local :
      'k Password.t @ local
      -> ('a -> 'b @ local portable contended) @ local portable
      -> ('a, 'k) t
      -> 'b @ local portable contended
      @@ portable
    (** [extract_local p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. *)

    val inject :
      ('a : value mod uncontended) 'k.
      'a @ portable -> ('a, 'k) t
      @@ portable
    (** [inject v] is a pointer to an immutable value [v] injected
        into the capsule ['k]. *)

    val inject_local :
      ('a : value mod uncontended) 'k.
      'a @ local portable -> ('a, 'k) t @ local
      @@ portable
    (** [inject_local v] is a pointer to an immutable value [v] injected
        into the capsule ['k]. *)

    val project :
      ('a : value mod portable) 'k.
      ('a, 'k) t -> 'a @ contended
      @@ portable
    (** [project t] returns the value of [t]. The result is within
        ['k] so it must be [portable] and it is marked [contended]. *)

    val project_local :
      ('a : value mod portable) 'k.
      ('a, 'k) t @ local -> 'a @ local contended
      @@ portable
    (** [project_local t] returns the value of [t]. The result is within
        ['k] so it must be [portable] and it is marked [contended]. *)

    val bind :
      'k Password.t @ local
      -> ('a -> ('b, 'j) t) @ local portable
      -> ('a, 'k) t
      -> ('b, 'j) t
      @@ portable
    (** [bind f t] is [project (map f t)]. *)

    val bind_local :
      'k Password.t @ local
      -> ('a @ local -> ('b, 'j) t @ local) @ local portable
      -> ('a, 'k) t @ local
      -> ('b, 'j) t @ local
      @@ portable
    (** [bind_local f t] is [project_local (map_local f t)]. *)

    val iter :
      'k Password.t @ local
      -> ('a -> unit) @ local portable
      -> ('a, 'k) t
      -> unit
      @@ portable
    (** [iter] is [extract] with result type specialized to [unit]. *)

    val iter_local :
      'k Password.t @ local
      -> ('a @ local -> unit) @ local portable
      -> ('a, 'k) t @ local
      -> unit
      @@ portable
    (** [iter_local] is [extract_local] with result type specialized to [unit]. *)

    val map_shared :
      ('a : value mod portable) 'b 'k.
      'k Password.Shared.t @ local
      -> ('a @ shared -> 'b) @ local portable
      -> ('a, 'k) t
      -> ('b, 'k) t
      @@ portable
    (** [map_shared p f t] applies [f] to the shared parts of [p] within the capsule ['k],
        creating a pointer to the result. Since [nonportable] functions may enclose
        [uncontended] (and thus write) access to data, ['a] must cross [portability] *)

    val map_shared_local :
      ('a : value mod portable) 'b 'k.
      'k Password.Shared.t @ local
      -> ('a @ local shared -> 'b @ local) @ local portable
      -> ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      @@ portable
    (** [map_shared_local p f t] applies [f] to the shared parts of [p] within the capsule ['k],
        creating a pointer to the result. Since [nonportable] functions may enclose
        [uncontended] (and thus write) access to data, ['a] must cross [portability] *)

    val extract_shared :
      ('a : value mod portable) 'b 'k.
      'k Password.Shared.t @ local
      -> ('a @ shared -> 'b @ portable contended) @ local portable
      -> ('a, 'k) t
      -> 'b @ portable contended
      @@ portable
    (** [extract_shared p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. Since [nonportable]
        functions may enclose [uncontended] (and thus write) access to data,
        ['a] must cross [portability] *)

    val extract_shared_local :
      ('a : value mod portable) 'b 'k.
      'k Password.Shared.t @ local
      -> ('a @ local shared -> 'b @ local portable contended) @ local portable
      -> ('a, 'k) t @ local
      -> 'b @ local portable contended
      @@ portable
    (** [extract_shared_local p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. Since [nonportable]
        functions may enclose [uncontended] (and thus write) access to data,
        ['a] must cross [portability] *)
end

exception Encapsulated : 'k Name.t * (exn, 'k) Data.t -> exn
(** If a function accessing the contents of the capsule raises an
    exception, it is wrapped in [Encapsulated] to avoid leaking access to
    the data. The [Name.t] can be used to associate the [Data.t] with a
    particular [Password.t] or [Mutex.t]. *)

(* CR-soon mslater: ['k Key.t] instead of ['k Mutex.t]. *)
exception Protected : 'k Mutex.t * (exn, 'k) Data.t -> exn
(** If a function passed to [protect] raises an exception, it is wrapped
    in [Protected] to avoid leaking access to the data. The [Mutex.t] can
    be used to access the [Data.t]. *)

val protect : (Password.packed @ local -> 'a) @ local -> 'a @@ portable
(** [protect f] runs [f password] in a fresh capsule represented by [password].
    If [f] returns normally, [protect] merges the capsule into the caller's capsule.
    If [f] raises an [Encapsulated] exception in the capsule represented by [password],
    [protect] unwraps the exception and re-raises it as [Protected].
    If [f] raises any other exception, [protect] re-raises it as [Protected]. *)

val with_password : (Password.packed @ local -> 'a) @ local -> 'a @@ portable
(** [with_password f] runs [f password] in a fresh capsule represented by [password].
    If [f] returns normally, [with_password] merges the capsule into the caller's capsule.
    If [f] raises an [Encapsulated] exception in the capsule represented by [password],
    [with_password] unwraps the exception and re-raises it directly. *)

val protect_local : (Password.packed @ local -> 'a @ local) @ local -> 'a @ local @@ portable
(** See [protect]. *)

val with_password_local : (Password.packed @ local -> 'a @ local) @ local -> 'a @ local @@ portable
(** See [with_password]. *)
