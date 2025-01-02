# 2 "domain.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@alert unstable
    "The Domain interface may change in incompatible ways in the future."
]

(** Domains.

    See 'Parallel programming' chapter in the manual.

    @since 5.0 *)

type !'a t
(** A domain of type ['a t] runs independently, eventually producing a
    result of type 'a, or an exception *)

val spawn : (unit -> 'a) -> 'a t @@ nonportable
[@@alert unsafe_parallelism
           "This function is unsafe and should not be used in production \
            code.\nA safe interface for parallelism is forthcoming."]
[@@alert unsafe_multidomain "Use [Domain.Safe.spawn]."]
(** [spawn f] creates a new domain that runs in parallel with the
    current domain.

    @raise Failure if the program has insufficient resources to create another
    domain. *)

val join : 'a t -> 'a @@ portable
(** [join d] blocks until domain [d] runs to completion. If [d] results in a
    value, then that is returned by [join d]. If [d] raises an uncaught
    exception, then that is re-raised by [join d]. *)

type id = private int
(** Domains have unique integer identifiers *)

val get_id : 'a t -> id @@ portable
(** [get_id d] returns the identifier of the domain [d] *)

val self : unit -> id @@ portable
(** [self ()] is the identifier of the currently running domain *)

val cpu_relax : unit -> unit @@ portable
(** If busy-waiting, calling cpu_relax () between iterations
    will improve performance on some CPU architectures *)

val is_main_domain : unit -> bool @@ portable
(** [is_main_domain ()] returns true if called from the initial domain. *)

val recommended_domain_count : unit -> int @@ portable
(** The recommended maximum number of domains which should be running
    simultaneously (including domains already running).

    The value returned is at least [1]. *)

val before_first_spawn : (unit -> unit) -> unit @@ nonportable
(** [before_first_spawn f] registers [f] to be called before the first domain
    is spawned by the program. The functions registered with
    [before_first_spawn] are called on the main (initial) domain. The functions
    registered with [before_first_spawn] are called in 'first in, first out'
    order: the oldest function added with [before_first_spawn] is called first.

    @raise Invalid_argument if the program has already spawned a domain. *)

val at_exit : (unit -> unit) -> unit @@ nonportable
(** [at_exit f] registers [f] to be called when the current domain exits. Note
    that [at_exit] callbacks are domain-local and only apply to the calling
    domain. The registered functions are called in 'last in, first out' order:
    the function most recently added with [at_exit] is called first. An example:

    {[
let temp_file_key = Domain.DLS.new_key (fun _ ->
  let tmp = snd (Filename.open_temp_file "" "") in
  Domain.at_exit (fun () -> close_out_noerr tmp);
  tmp)
    ]}

    The snippet above creates a key that when retrieved for the first
    time will open a temporary file and register an [at_exit] callback
    to close it, thus guaranteeing the descriptor is not leaked in
    case the current domain exits. *)

module DLS : sig
(** Domain-local Storage *)

    type 'a key : value mod portable uncontended
    (** Type of a DLS key *)

    val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key @@ nonportable
    (** [new_key f] returns a new key bound to initialiser [f] for accessing
,        domain-local variables.

        If [split_from_parent] is not provided, the value for a new
        domain will be computed on-demand by the new domain: the first
        [get] call will call the initializer [f] and store that value.

        {b Warning.} [f] may be called several times if another call
        to [get] occurs during initialization on the same domain. Only
        the 'first' value computed will be used, the other now-useless
        values will be discarded. Your initialization function should
        support this situation, or contain logic to detect this case
        and fail.

        If [split_from_parent] is provided, spawning a domain will
        derive the child value (for this key) from the parent
        value. This computation happens in the parent domain and it
        always happens, regardless of whether the child domain will
        use it.
        If the splitting function is expensive or requires
        child-side computation, consider using ['a Lazy.t key]:

        {[
        let init () = ...

        let split_from_parent parent_value =
          ... parent-side computation ...;
          lazy (
            ... child-side computation ...
          )

        let key = Domain.DLS.new_key ~split_from_parent init

        let get () = Lazy.force (Domain.DLS.get key)
        ]}

        In this case a part of the computation happens on the child
        domain; in particular, it can access [parent_value]
        concurrently with the parent domain, which may require
        explicit synchronization to avoid data races.
    *)

    val get : 'a key -> 'a @@ nonportable
    (** [get k] returns [v] if a value [v] is associated to the key [k] on
        the calling domain's domain-local state. Sets [k]'s value with its
        initialiser and returns it otherwise. *)

    val set : 'a key -> 'a -> unit @@ nonportable
    (** [set k v] updates the calling domain's domain-local state to associate
        the key [k] with value [v]. It overwrites any previous values associated
        to [k], which cannot be restored later. *)

end
[@@alert unsafe_multidomain "Use [Domain.Safe.DLS]."]

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig

  (** Like {!DLS}, but uses modes to enforce properties necessary for data-race freedom.

      The data in the DLS may only be accessed when the user has an ([uncontended])
      {!Access.t}. This value acts as a witness that the currently executing function is
      running in the (conceptual) capsule of the current domain, and so will not
      transfer data unsafely between capsule boundaries. A user can get a temporary
      [Access.t] using {!access} below. *)
  module DLS : sig

    (** An {!Access.t} acts as a witness that the currently executing function is
        running in the (conceptual) capsule of the current domain. *)
    module Access : sig

      type t : value mod global portable many unique
      (** [t] represents access to the current domain's capsule, allowing interaction with
          data in its DLS. *)

      val for_initial_domain : t @@ nonportable
      (** [for_initial_domain] is a permanently available [t] that can be used by any
          top-level [nonportable] function safely, as such functions can only ever be
          executed on the primary domain. *)
    end

    type 'a key : value mod portable uncontended = 'a DLS.key [@alert "-unsafe_multidomain"]
    (** See {!DLS.key}. *)

    val access
      :  (Access.t -> 'a @ portable contended) @ local portable
      -> 'a @ portable contended
      @@ portable
    (** [access f] scopes the computation [f] to (conceptually) run it in the current
        domain's capsule, even if called from an explicit capsule. During its execution,
        [f] may access the current domain's DLS. *)

    val new_key
      :  ?split_from_parent:('a -> (Access.t -> 'a) @ portable) @ portable
      -> (Access.t -> 'a) @ portable
      -> 'a key
      @@ portable
    (** Like {!DLS.new_key}, but is safe to use in the presence of multiple domains.

        When a new domain is spawned, if [split_from_parent] is provided, then each entry
        in the DLS is forced in the parent domain, and [split_from_parent] is called on
        each of those values; then, in the child domain, the resulting closure is called
        (with {!Access.t} to the new domain) to generate an initial value for that DLS
        entry in the child domain. This inner closure must be [portable] as it may not
        unsafely close over any data from the parent domain.

        If {!get} is called on an entry that is not populated for the current domain, then
        the provided closure is called with {!Access.t} to the rest of the DLS.

        Both provided arguments must be [portable] as they may be called from any domain,
        not just the current one. *)

    val get : Access.t -> 'a key -> 'a @@ portable
    (** Like {!DLS.get}, but is safe to use in the presence of multiple domains.

        An additional {!Access.t} argument is taken as a witness that the returned value
        does not escape the current domain's capsule. *)

    val set : Access.t -> 'a key -> 'a -> unit @@ portable
    (** Like {!DLS.set}, but is safe to use in the presence of multiple domains.

        An additional {!Access.t} argument is taken as a witness that the provided value
        does not unsafely close over data from the current capsule. *)
  end

  val spawn : (unit -> 'a) @ portable -> 'a t @@ portable
  [@@alert unsafe_parallelism
             "This function is unsafe and should not be used in production \
              code.\nA safe interface for parallelism is forthcoming."]
  (** Like {!spawn}, but enforces thread-safety via modes. In particular, the provided
      computation must be [portable], and so cannot close over and interact with any
      unsynchronized mutable data in the current domain. *)

  val spawn' : (DLS.Access.t -> 'a) @ portable -> 'a t @@ portable
  [@@alert unsafe_parallelism
             "This function is unsafe and should not be used in production \
              code.\nA safe interface for parallelism is forthcoming."]
  (** Like {!spawn}, but additionally provides the user with a witness that the provided
      computation is running in the new domain's capsule, and so may {!Access.t} the DLS.
  *)

  val at_exit : (unit -> unit) @ portable -> unit @@ portable
  (** Like {!at_exit}, but can be called from any domain.

      The provided closure must be [portable] to enforce that it does not unsafely close
      over any data in the current capsule, which the current domain may not have
      uncontended access to at exit. *)

  val at_exit' : DLS.Access.t -> (unit -> unit) -> unit @@ portable
  (** Like {!at_exit}, but can be called from any domain.

      An additional {!DLS.Access.t} is taken as a witness that the provided closure only
      closes over mutable data from the current domain. *)
end
