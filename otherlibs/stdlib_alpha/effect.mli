(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Leo White, Jane Street, London                      *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

@@ portable

(** Effects.

    @since 5.0 *)

exception Continuation_already_resumed
(** Exception raised when a continuation is continued or discontinued more
    than once. *)

module Handler : sig

  type 'e t
  (** ['e t] is an effect handler for effect ['e] on the current stack. *)

  module List : sig

    type 'e handler := 'e t

    type 'es t =
      | [] : unit t
      | (::) : 'e handler * 'es t -> ('e * 'es) t
    (** ['es t] is a typed list of [Handler.t]s *)

    module Length : sig

      type x = X

      type 'es t =
        | [] : unit t
        | (::) : x * 'es t -> ('e * 'es) t
      (** ['es t] represents the length of a typed list of [Handler.t]s. It has
          slightly unusual constructors so that lengths can be written as
          [[X; X; X]] rather than e.g. [(S (S (S Z)))]. *)

    end

    val length : 'es t @ local -> 'es Length.t
    (** [length hs] is the length of [hs] *)

  end

end

module Continuation : sig

  type (-'a, +'b, 'es) t : value mod contended
  (** [('a, 'b, 'es) continuation] is a delimited continuation that expects an ['a]
      value and returns a ['b] value. It requires handlers for the effects ['es].

      Continuations cross contention and and are [portable] if created from
      a [portable] closure. *)

  val get_callstack : ('a, 'b, 'es) t -> int -> Printexc.raw_backtrace
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)

end

val continue :
  ('a, 'b, 'es) Continuation.t
  -> 'a
  -> 'es Handler.List.t @ local
  -> 'b
(** [continue k v hs] resumes the continuation [k] with value [v]. [hs] are
    used to handle [k]'s additional effects.

    @raise Continuation_already_resumed if the continuation has already been
    resumed. *)

val discontinue :
  ('a, 'b, 'es) Continuation.t
  -> exn
  -> 'es Handler.List.t @ local
  -> 'b
(** [discontinue k e hs] resumes the continuation [k] by raising the
    exception [e]. [hs] are used to handle [k]'s additional effects.

    @raise Continuation_already_resumed if the continuation has already been
    resumed. *)

val discontinue_with_backtrace :
  ('a, 'b, 'es) Continuation.t
  -> exn
  -> Printexc.raw_backtrace
  -> 'es Handler.List.t @ local
  -> 'b
(** [discontinue_with_backtrace k e bt hs] resumes the continuation [k] by raising the
    exception [e] using the raw backtrace [bt] as the origin of the exception.
    [hs] are used to handle [k]'s additional effects.

    @raise Continuation_already_resumed if the continuation has already been
    resumed. *)

(** The signature for effects with no type parameters *)
module type S = sig
  @@ portable

  type t
  (** [t] represents the effect. It only appears as the argument to
      [Handler.t]. *)

  type ('a, 'e) ops
  (** [('a, 'e) ops] is the type of operations of effect [t]. ['a] is
      the return type of the given operation. ['e] will be filled in
      with [t] to tie the knot on recursive operations. *)

  module Result : sig
    @@ portable

    type eff := t

    type ('a, 'es) t =
      | Value : 'a @@ global -> ('a, 'es) t
      | Exception : exn @@ global -> ('a, 'es) t
      | Operation :
          ('o, eff) ops @@ global
          * ('o, ('a, 'es) t, 'es) Continuation.t
          -> ('a, 'es) t
    (** [('a, 'es) t] is the result of running a continuation until it
        either finishes and returns an ['a] value, raises an exception, or
        performs an operation. *)

    type ('a, 'es) handler =
      { handle :
          'o. ('o, eff) ops
          -> ('o, ('a, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'es) t -> ('a, 'es) handler -> 'a
    (** [handle r f] uses [f] to handle the [Operation] case of [r]. The [Value]
        and [Exception] cases are handled by returning and raising respectively. *)
  end

  type ('a, 'es) result = ('a, 'es) Result.t =
    | Value : 'a @@ global -> ('a, 'es) result
    | Exception : exn @@ global -> ('a, 'es) result
    | Operation :
        ('o, t) ops @@ global
        * ('o, ('a, 'es) result, 'es) Continuation.t
        -> ('a, 'es) result

  val fiber :
    (t Handler.t @ local -> 'a -> 'b)
    -> ('a, ('b, unit) Result.t, unit) Continuation.t
  (** [fiber f] constructs a continuation that runs the computation [f]. [f]
      is passed a [t Handler.t] so that it can perform operations from effect
      [t]. *)

  val fiber_with :
    'es Handler.List.Length.t @ local
    -> ((t * 'es) Handler.List.t @ local -> 'a -> 'b)
    -> ('a, ('b, 'es) Result.t, 'es) Continuation.t
  (** [fiber_with l f] constructs a continuation that runs the computation [f],
      which requires handlers for [l] additional effects. [f] is passed a typed
      list of handlers so that it can perform operations from effect [t] as
      well as from the additional effects ['es]. *)

  val run : (t Handler.t @ local -> 'a) -> ('a, unit) Result.t
  (** [run f] constructs a continuation that runs the computation [f], and
      immediately continues it. [f] is passed a [t Handler.t] so that it can
      perform operations from effect [t]. *)

  val run_with :
    'es Handler.List.t @ local
    -> ((t * 'es) Handler.List.t @ local -> 'a)
    -> ('a, 'es) Result.t
  (** [run_with hs f] constructs a continuation that runs the computation [f],
      and immediately continues it with handlers [hs]. [f] is passed a typed list
      of handlers so that it can perform operations from effect [t] as well as
      from the additional effects ['es]. *)

  val perform : t Handler.t @ local -> ('a, t) ops -> 'a
  (** [perform h e] performs an effect [e] at the handler [h] *)

  module Contended : sig
    module Result : sig
      type eff := t

      type ('a, 'es) t =
        | Value : 'a @@ global -> ('a, 'es) t
        | Exception : exn @@ global -> ('a, 'es) t
        | Operation :
            ('o, eff) ops @@ global contended
            * ('o Modes.Portable.t, ('a, 'es) t, 'es) Continuation.t
            -> ('a, 'es) t
    end

    val fiber :
      ('a : value mod portable) 'b.
      (t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, unit) Result.t, unit) Continuation.t
    (** [fiber f] works as the non-contended version, but provides
        a [portable] handler at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val fiber_with :
      ('a : value mod portable) 'b 'es.
      'es Handler.List.Length.t @ local
      -> ((t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, 'es) Result.t, 'es) Continuation.t
    (** [fiber_with l f] works as the non-contended version, but provides
        [portable] handlers at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val run :
      (t Handler.t @ local portable -> 'a)
      -> ('a, unit) Result.t
    (** [run f] works as the non-contended version, but provides
        a [portable] handler and returns a [Contended.Result.t]. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> ((t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'es) Result.t
    (** [run_with hs f] works as the non-contended version, but provides
        [portable] handlers and returns a [Contended.Result.t]. *)

    val perform :
      t Handler.t @ local contended ->
      ('a, t) ops @ portable ->
      'a @ contended
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec t = t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'es) t =
      ('a, ('b, 'es) Result.t, 'es) Continuation.t
    (** [('a, 'b, 'es) t] is the type of continuations for [t] handlers that expect an
        ['a] and return a ['b]. It requires additional handlers for the effects ['es].

        This is not to be confused with the other [Continuation.t] type available in this
        file; external uses will be of the form [This_effect.Continuation.t] and
        [That_effect.Continuation.t], and thus more visually distinct. *)
  end
end

(** The signature for effects with one type parameter *)
module type S1 = sig
  @@ portable

  type 'p t
  (** ['p t] represents the effect. It only appears as the argument to
      [Handler.t]. *)

  type ('a, 'p, 'e) ops
  (** [('a, 'p, 'e) ops] is the type of operations of effect ['p t]. ['a] is
      the return type of the given operation. ['e] will be filled in
      with ['p t] to tie the knot on recursive operations. *)

  module Result : sig
    @@ portable

    type 'p eff := 'p t

    type ('a, 'p, 'es) t =
      | Value : 'a @@ global -> ('a, 'p, 'es) t
      | Exception : exn @@ global -> ('a, 'p, 'es) t
      | Operation :
          ('o, 'p, 'p eff) ops @@ global
          * ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'es) t
    (** [('a, 'p, 'es) t] is the result of running a continuation until it
        either finishes and returns an ['a] value, raises an exception, or
        performs an operation. *)

    type ('a, 'p, 'es) handler =
      { handle :
          'o. ('o, 'p, 'p eff) ops
          -> ('o, ('a, 'p, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'es) t -> ('a, 'p, 'es) handler -> 'a
    (** [handle r f] uses [f] to handle the [Operation] case of [r]. The [Value]
        and [Exception] cases are handled by returning and raising respectively. *)
  end

  type ('a, 'p, 'es) result = ('a, 'p, 'es) Result.t =
    | Value : 'a @@ global -> ('a, 'p, 'es) result
    | Exception : exn @@ global -> ('a, 'p, 'es) result
    | Operation :
        ('o, 'p, 'p t) ops @@ global
        * ('o, ('a, 'p, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'es) result

  val fiber :
    ('p t Handler.t @ local -> 'a -> 'b)
    -> ('a, ('b, 'p, unit) Result.t, unit) Continuation.t
  (** [fiber f] constructs a continuation that runs the computation [f]. [f]
      is passed a [t Handler.t] so that it can perform operations from effect
      [t]. *)

  val fiber_with :
    'es Handler.List.Length.t @ local
    -> (('p t * 'es) Handler.List.t @ local -> 'a -> 'b)
    -> ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t
  (** [fiber_with l f] constructs a continuation that runs the computation [f],
      which requires handlers for [l] additional effects. [f] is passed a typed
      list of handlers so that it can perform operations from effect [t] as
      well as from the additional effects ['es]. *)

  val run : ('p t Handler.t @ local -> 'a) -> ('a, 'p, unit) Result.t
  (** [run f] constructs a continuation that runs the computation [f], and
      immediately continues it. *)

  val run_with :
    'es Handler.List.t @ local
    -> (('p t * 'es) Handler.List.t @ local -> 'a)
    -> ('a, 'p, 'es) Result.t
  (** [run_with hs f] constructs a continuation that runs the computation [f],
      and immediately continues it with handlers [hs]. [f] is passed a typed list
      of handlers so that it can perform operations from effect [t] as well as
      from the additional effects ['es]. *)

  val perform : 'p t Handler.t @ local -> ('a, 'p, 'p t) ops -> 'a
  (** [perform h e] performs an effect [e] at the handler [h] *)

  module Contended : sig
    module Result : sig
      type 'p eff := 'p t

      type ('a, 'p, 'es) t =
        | Value : 'a @@ global -> ('a, 'p, 'es) t
        | Exception : exn @@ global -> ('a, 'p, 'es) t
        | Operation :
            ('o, 'p, 'p eff) ops @@ global contended
            * ('o Modes.Portable.t, ('a, 'p, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'es) t
    end

    val fiber :
      ('a : value mod portable) 'b 'p.
      ('p t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, 'p, unit) Result.t, unit) Continuation.t
    (** [fiber f] works as the non-contended version, but provides
        a [portable] handler at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val fiber_with :
      ('a : value mod portable) 'b 'p 'es.
      'es Handler.List.Length.t @ local
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t
    (** [fiber_with l f] works as the non-contended version, but provides
        [portable] handlers at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val run :
      ('p t Handler.t @ local portable -> 'a)
      -> ('a, 'p, unit) Result.t
    (** [run f] works as the non-contended version, but provides
        a [portable] handler and returns a [Contended.Result.t]. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> (('p t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'p, 'es) Result.t
    (** [run_with hs f] works as the non-contended version, but provides
        [portable] handlers and returns a [Contended.Result.t]. *)

    val perform :
      'p t Handler.t @ local contended ->
      ('a, 'p, 'p t) ops @ portable ->
      'a @ contended
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec 'p t = 'p t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'es) t =
      ('a, ('b, 'p, 'es) Result.t, 'es) Continuation.t
  end
end

(** The signature for effects with two type parameters *)
module type S2 = sig
  @@ portable

  type ('p, 'q) t
  (** [('p, 'q) t] represents the effect. It only appears as the argument to
      [Handler.t]. *)

  type ('a, 'p, 'q, 'e) ops
  (** [('a, 'p, 'q, 'e) ops] is the type of operations of effect [('p, 'q) t].
      ['a] is the return type of the given operation. ['e] will be filled in
      with [('p, 'q) t] to tie the knot on recursive operations. *)

  module Result : sig
    @@ portable

    type ('p, 'q) eff := ('p, 'q) t

    type ('a, 'p, 'q, 'es) t =
      | Value : 'a @@ global -> ('a, 'p, 'q, 'es) t
      | Exception : exn @@ global -> ('a, 'p, 'q, 'es) t
      | Operation :
          ('o, 'p, 'q, ('p, 'q) eff) ops @@ global
          * ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> ('a, 'p, 'q, 'es) t
    (** [('a, 'p, 'q, 'es) t] is the result of running a continuation until
        it either finishes and returns an ['a] value, raises an exception, or
        performs an operation. *)

    type ('a, 'p, 'q, 'es) handler =
      { handle :
          'o. ('o, 'p, 'q, ('p, 'q) eff) ops
          -> ('o, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
          -> 'a }
      [@@unboxed]

    val handle : ('a, 'p, 'q, 'es) t -> ('a, 'p, 'q, 'es) handler -> 'a
    (** [handle r f] uses [f] to handle the [Operation] case of [r]. The [Value]
        and [Exception] cases are handled by returning and raising respectively. *)
  end

  type ('a, 'p, 'q, 'es) result = ('a, 'p, 'q, 'es) Result.t =
    | Value : 'a @@ global -> ('a, 'p, 'q, 'es) result
    | Exception : exn @@ global -> ('a, 'p, 'q, 'es) result
    | Operation :
        ('o, 'p, 'q, ('p, 'q) t) ops @@ global
        * ('o, ('a, 'p, 'q, 'es) result, 'es) Continuation.t
        -> ('a, 'p, 'q, 'es) result

  val fiber :
    (('p, 'q) t Handler.t @ local -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, unit) Result.t, unit) Continuation.t
  (** [fiber f] constructs a continuation that runs the computation [f]. [f]
      is passed a [t Handler.t] so that it can perform operations from effect
      [t]. *)

  val fiber_with :
    'es Handler.List.Length.t @ local
    -> ((('p, 'q) t * 'es) Handler.List.t @ local -> 'a -> 'b)
    -> ('a, ('b, 'p, 'q, 'es) Result.t, 'es) Continuation.t
  (** [fiber_with l f] constructs a continuation that runs the computation [f],
      which requires handlers for [l] additional effects. [f] is passed a typed
      list of handlers so that it can perform operations from effect [t] as
      well as from the additional effects ['es]. *)

  val run :
    (('p, 'q) t Handler.t @ local -> 'a)
    -> ('a, 'p, 'q, unit) Result.t
  (** [run f] constructs a continuation that runs the computation [f], and
      immediately continues it. *)

  val run_with :
    'es Handler.List.t @ local
    -> ((('p, 'q) t * 'es) Handler.List.t @ local -> 'a)
    -> ('a, 'p, 'q, 'es) Result.t
  (** [run_with hs f] constructs a continuation that runs the computation [f],
      and immediately continues it with handlers [hs]. [f] is passed a typed list
      of handlers so that it can perform operations from effect [t] as well as
      from the additional effects ['es]. *)

  val perform :
    ('p, 'q) t Handler.t @ local
    -> ('a, 'p, 'q, ('p, 'q) t) ops
    -> 'a
  (** [perform h e] performs an effect [e] at the handler [h] *)

  module Contended : sig
    module Result : sig
      type ('p, 'q) eff := ('p, 'q) t

      type ('a, 'p, 'q, 'es) t =
        | Value : 'a @@ global -> ('a, 'p, 'q, 'es) t
        | Exception : exn @@ global -> ('a, 'p, 'q, 'es) t
        | Operation :
            ('o, 'p, 'q, ('p, 'q) eff) ops @@ global contended
            * ('o Modes.Portable.t, ('a, 'p, 'q, 'es) t, 'es) Continuation.t
            -> ('a, 'p, 'q, 'es) t
    end

    val fiber :
      ('a : value mod portable) 'b 'p 'q.
      (('p, 'q) t Handler.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, 'p, 'q, unit) Result.t, unit) Continuation.t
    (** [fiber f] works as the non-contended version, but provides
        a [portable] handler at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val fiber_with :
      ('a : value mod portable) 'b 'p 'q 'es.
      'es Handler.List.Length.t @ local
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a @ contended -> 'b)
      -> ('a, ('b, 'p, 'q, 'es) Result.t, 'es) Continuation.t
    (** [fiber_with l f] works as the non-contended version, but provides
        [portable] handlers at the cost of requiring a [value mod portable]
        argument and returning a [Contended.Result.t]. *)

    val run :
      (('p, 'q) t Handler.t @ local portable -> 'a)
      -> ('a, 'p, 'q, unit) Result.t
    (** [run f] works as the non-contended version, but provides
        a [portable] handler and returns a [Contended.Result.t]. *)

    val run_with :
      'es Handler.List.t @ local portable
      -> ((('p, 'q) t * 'es) Handler.List.t @ local portable -> 'a)
      -> ('a, 'p, 'q, 'es) Result.t
    (** [run_with hs f] works as the non-contended version, but provides
        [portable] handlers and returns a [Contended.Result.t]. *)

    val perform :
      ('p, 'q) t Handler.t @ local contended ->
      ('a, 'p, 'q, ('p, 'q) t) ops @ portable ->
      'a @ contended
    (** [perform h e] performs an effect [e] at the [contended] handler [h] *)
  end

  module Handler : sig

    type nonrec ('p, 'q) t = ('p, 'q) t Handler.t

  end

  module Continuation : sig

    type ('a, 'b, 'p, 'q, 'es) t =
      ('a, ('b, 'p, 'q, 'es) Result.t, 'es) Continuation.t
  end
end

module type Operations = sig

  type 'a t

end

module type Operations_rec = sig

  type ('a, 'e) t

end

module type Operations1 = sig

  type ('a, 'p) t

end

module type Operations1_rec = sig

  type ('a, 'p, 'e) t

end

module type Operations2 = sig

  type ('a, 'p, 'q) t

end

module type Operations2_rec = sig

  type ('a, 'p, 'q, 'e) t

end

module Make (Ops : Operations) : S with type ('a, 'e) ops := 'a Ops.t

module Make_rec (Ops : Operations_rec)
  : S with type ('a, 'e) ops := ('a, 'e) Ops.t

module Make1 (Ops : Operations1)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p) Ops.t

module Make1_rec (Ops : Operations1_rec)
  : S1 with type ('a, 'p, 'e) ops := ('a, 'p, 'e) Ops.t

module Make2 (Ops : Operations2)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q) Ops.t

module Make2_rec (Ops : Operations2_rec)
  : S2 with type ('a, 'p, 'q, 'e) ops := ('a, 'p, 'q, 'e) Ops.t
