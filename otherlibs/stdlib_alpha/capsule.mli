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

(** Passwords represent access to a capsule. *)
module Password : sig

    (* CR layouts v5: this should have layout [void], but
       [void] can't be used for function argument and return types yet. *)
    type 'k t
    (** ['k t] is the type of "passwords" representing
        the ability of the current domain to have [uncontended]
        access to the capsule ['k]. They don't have a runtime representation,
        and can't be passed between domains. Just as we don't share passwords
        between people, we don't share [Password.t]s between domains.

        Obtaining a ['k t] requires acquiring the mutex associated with ['k],
        and modes prevent retaining the [t] after releasing the mutex. This
        guarantees that uncontended access to the capsule is only granted
        to a single domain at once. *)

end

(** ReaderPasswords represent read access to a capsule. *)
module ReaderPassword : sig

    type 'k t
    (** ['k t] is the type of "reader passwords" representing the ability of the
        current domain to have [shared] access to the capsule ['k]. As with [Password.t],
        they don't have a runtime representation and they can't be passed between
        domains.

        Obtaining a ['k t] requires read acquire the reader-writer lock associate
        with ['k]. *)
end

val weaken_password : 'k Password.t @ local -> 'k ReaderPassword.t @ local @@ portable

  (** Mutual exclusion primtives for controlling uncontended access to a capsule.

      Requires OCaml 5 runtime. *)
module Mutex : sig

    type 'k t : value mod portable uncontended
    (** ['k t] is the type of the mutex that controls access to
        the capsule ['k]. This mutex is created when creating
        the capsule ['k] using {!create_with_mutex}. *)

    type packed : value mod portable uncontended = P : 'k t -> packed
    (** [packed] is the type of a mutex for some unknown capsule.
        Unpacking one provides a ['k Mutex.t] together with a fresh
        existential type brand for ['k]. *)

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

    val destroy : 'k t -> 'k Password.t @@ portable
    (** [destroy m] acquires the mutex [m] and leaks its password.
        It marks the lock as poisoned. *)
end

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
        -> ('k ReaderPassword.t @ local -> 'a) @ local
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

    val destroy : 'k t -> 'k Password.t @@ portable
end

val create_with_mutex : unit -> Mutex.packed @@ portable
(** [create_with_mutex ()] creates a new capsule with an associated mutex. *)

val create_with_rwlock : unit -> Rwlock.packed @@ portable
(** [create_with_rwlock ()] creates a new capsule with an associated reader-writer lock. *)

(** Pointers to data within a capsule. *)
module Data : sig

    type ('a, 'k) t : value mod portable uncontended
    (** [('a, 'k) t] is the type of pointers to ['a]s in
        the capsule ['k]. It can be passed between domains.
        Operations on [('a, 'k) t] require a ['k Password.t],
        created from the ['k Mutex.t]. *)

    exception Contended of exn @@ contended
    (** If a function accessing the contents of the capsule raises an exception,
        it is wrapped in [Contended] to avoid leaking access to the data. *)

    val create :
      (unit -> 'a) @ local portable
      -> ('a, 'k) t
      @@ portable
    (** [create f] runs [f] within the capsule ['k] and creates
        a pointer to the result of [f]. *)


    val map :
      'k Password.t @ local
      -> ('a -> 'b) @ local portable
      -> ('a, 'k) t
      -> ('b, 'k) t
      @@ portable
    (** [map p f t] applies [f] to the value of [p] within the capsule ['k],
        creating a pointer to the result. *)

    val both :
      ('a, 'k) t
      -> ('b, 'k) t
      -> ('a * 'b, 'k) t
      @@ portable
    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)

    val extract :
      'k Password.t @ local
      -> ('a -> 'b @ portable contended) @ local portable
      -> ('a, 'k) t
      -> 'b @ portable contended
      @@ portable
    (** [extract p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. *)

    val inject :
      ('a : value mod uncontended) 'k.
      'a @ portable -> ('a, 'k) t
      @@ portable
    (** [inject v] is a pointer to an immutable value [v] injected
        into the capsule ['k]. *)

    val project :
      ('a : value mod portable) 'k.
      ('a, 'k) t -> 'a @ contended
      @@ portable
    (** [project t] returns the value of [t]. The result is within
        ['k] so it must be [portable] and it is marked [contended]. *)

    val bind :
      'k Password.t @ local
      -> ('a -> ('b, 'j) t) @ local portable
      -> ('a, 'k) t
      -> ('b, 'j) t
      @@ portable
    (** [bind f t] is [project (map f t)]. *)

    val iter :
      'k Password.t @ local
      -> ('a -> unit) @ local portable
      -> ('a, 'k) t
      -> unit
      @@ portable
    (** [iter] is [extract] with result type specialized to [unit]. *)

    val expose :
      'k Password.t
      -> ('a, 'k) t
      -> 'a
      @@ portable
    (** [expose p t] retrieves the value stored by [Ptr.t]. It requires
        a [global] [Password.t] leaked by [Mutex.destroy]. *)

    val map_shared :
      ('a : value mod portable) 'b 'k.
      'k ReaderPassword.t @ local
      -> ('a @ shared -> 'b) @ local portable
      -> ('a, 'k) t
      -> ('b, 'k) t
      @@ portable
    (** [map_shared p f t] applies [f] to the shared parts of [p] within the capsule ['k],
        creating a pointer to the result. Since [nonportable] functions may enclose
        [uncontended] (and thus write) access to data, ['a] must cross [portability] *)

    val extract_shared :
      ('a : value mod portable) 'b 'k.
      'k ReaderPassword.t @ local
      -> ('a @ shared -> 'b @ portable contended) @ local portable
      -> ('a, 'k) t
      -> 'b @ portable contended
      @@ portable
    (** [extract p f t] applies [f] to the value of [t] within
        the capsule ['k] and returns the result. The result is within ['k]
        so it must be [portable] and it is marked [contended]. Since [nonportable]
        functions may enclose [uncontended] (and thus write) access to data,
        ['a] must cross [portability] *)
  end
