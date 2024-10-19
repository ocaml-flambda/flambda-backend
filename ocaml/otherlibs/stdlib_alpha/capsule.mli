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

(** Converters can wrap and unwrap [Data.t] values from the current
    capsule. *)
module Converter : sig

    (* CR layouts v5: this should have layout [void], but
       [void] can't be used for function argument and return types yet. *)
    type 'k t : value mod global portable many unique
    (** ['k t] is the type of "converters" that can wrap and unwrap
        [Data.t] values. An [uncontended] ['k t] indicates that ['k] is the
        current capsule.  *)

    type packed = P : 'k t -> packed
    (** [packed] is the type of a converter for some unknown capsule.
        Unpacking one provides a ['k t] together with a fresh
        existential type brand for ['k]. *)

end

(** Obtain a converter for the current capsule. Since we do not
    know the brand for the current capsule, we receive a fresh
    one. *)
val current : unit -> Converter.packed @@ portable

(** The brand for the initial capsule. *)
type initial

(** A converter for the initial capsule *)
val initial : initial Converter.t

exception Contended of exn @@ contended
(** If a function accessing the contents of the capsule raises an
   exception, it is wrapped in [Contended] to avoid leaking access to
   the data. *)

(** Passwords represent permission to access to a capsule. *)
module Password : sig

    (* CR layouts v5: this should have layout [void], but
       [void] can't be used for function argument and return types yet. *)
    type 'k t : value mod portable many unique uncontended
  (** ['k t] is the type of "passwords" representing permission for the
     current fiber to have [uncontended] access to the capsule
     ['k]. They don't have a runtime representation. They are only ever
     avilable at locally, so that they cannot move between fibers.
     Just as we don't share passwords between people, we don't share
     [Password.t]s between fibers.

     Obtaining a ['k t] requires acquiring the mutex associated with
     ['k], and modes prevent retaining the [t] after releasing the
     mutex. This guarantees that uncontended access to the capsule is
     only granted to a single domain at once. *)

end

val access :
  'k Password.t @ local
  -> ('k Converter.t -> 'a @ portable) @ local portable
  -> 'a @ contended
(** [access p f] runs [f] within the capsule ['k], providing it with a
   converter for ['k]. The result is within ['k] so it must be
   [portable] and it is marked [contended]. *)

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

    val destroy : 'k t -> 'k Converter.t @@ portable
    (** [destroy m] acquires the mutex [m] and merges the capsule ['k]
        with the current capsule by leaking a converter. It marks the
        lock as poisoned. *)
end

val create_with_mutex : unit -> Mutex.packed @@ portable
(** [create_with_mutex ()] creates a new capsule with an associated mutex. *)

(** Pointers to data within a capsule. *)
module Data : sig

    type ('a, 'k) t : value mod portable uncontended
    (** [('a, 'k) t] is the type of pointers to ['a]s in
        the capsule ['k]. It can be passed between domains.
        Operations on [('a, 'k) t] require a ['k Password.t],
        created from the ['k Mutex.t]. *)

    val wrap :
      'k Converter.t @ local
      -> 'a
      -> ('a, 'k) t
      @@ portable
    (** [wrap c v] is a pointer to a value [v] from the current
       capsule. *)

    val unwrap :
      'k Converter.t @ local
      -> ('a, 'k) t
      -> 'a
      @@ portable
    (** [unwrap c t] returns the value of [t] which is from the current
       capsule. *)

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

end

