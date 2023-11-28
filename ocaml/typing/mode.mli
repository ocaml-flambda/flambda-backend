(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type changes

val undo_changes : changes -> unit

val change_log : (changes -> unit) ref

module Locality : sig
  module Const : sig
    type t =
      | Global
      | Local

    val legacy : t

    val min : t

    val max : t

    val le : t -> t -> bool

    val join : t -> t -> t

    val meet : t -> t -> t

    val print : Format.formatter -> t -> unit
  end

  type t

  val legacy : t

  val of_const : Const.t -> t

  val global : t

  val local : t

  val submode : t -> t -> (unit, unit) result

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, unit) result

  val join : t list -> t

  val constrain_upper : t -> Const.t

  val constrain_lower : t -> Const.t

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  val newvar_above : t -> t * bool

  val check_const : t -> Const.t option

  val print' : ?verbose:bool -> ?label:string -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end

module Regionality : sig
  module Const : sig
    type t =
      | Global
      | Regional
      | Local
  end

  type t

  type error =
    [ `Regionality
    | `Locality ]

  val global : t

  val regional : t

  val local : t

  val submode : t -> t -> (unit, error) result

  val of_locality : Locality.t -> t

  val regional_to_local : t -> t

  val global_to_regional : t -> t

  val local_to_regional : t -> t

  val regional_to_global : t -> t

  val regional_to_global_locality : t -> Locality.t

  val print : Format.formatter -> t -> unit
end

module Uniqueness : sig
  module Const : sig
    type t =
      | Unique
      | Shared

    val legacy : t

    val min : t

    val max : t

    val le : t -> t -> bool

    val join : t -> t -> t

    val meet : t -> t -> t

    val print : Format.formatter -> t -> unit
  end

  type t

  val legacy : t

  val of_const : Const.t -> t

  val unique : t

  val shared : t

  val submode : t -> t -> (unit, unit) result

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, unit) result

  val join : t list -> t

  val meet : t list -> t

  val constrain_upper : t -> Const.t

  val constrain_lower : t -> Const.t

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  val newvar_above : t -> t * bool

  val check_const : t -> Const.t option

  val print' : ?verbose:bool -> ?label:string -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end

module Linearity : sig
  module Const : sig
    type t =
      | Many
      | Once

    val legacy : t

    val min : t

    val max : t

    val le : t -> t -> bool

    val join : t -> t -> t

    val meet : t -> t -> t

    val print : Format.formatter -> t -> unit

    val to_dual : t -> Uniqueness.Const.t

    val of_dual : Uniqueness.Const.t -> t
  end

  type t

  val legacy : t

  val of_const : Const.t -> t

  val to_dual : t -> Uniqueness.t

  val of_dual : Uniqueness.t -> t

  val once : t

  val many : t

  val submode : t -> t -> (unit, unit) result

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, unit) result

  val join : t list -> t

  val constrain_upper : t -> Const.t

  val constrain_lower : t -> Const.t

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  val newvar_above : t -> t * bool

  val check_const : t -> Const.t option

  val print' : ?verbose:bool -> ?label:string -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end

type ('a, 'b, 'c) modes =
  { locality : 'a;
    uniqueness : 'b;
    linearity : 'c
  }

module Alloc : sig
  module Const : sig
    type t = (Locality.Const.t, Uniqueness.Const.t, Linearity.Const.t) modes

    val legacy : t

    val join : t -> t -> t

    val close_over : t -> t

    (** [partial_apply] is a special case of [close_over], where some function
        is closed over by some closure, but only used in a limited capacity and
        thus imposes less constraints on the closure. Concretely speaking,
        [close_over] an unique variable gives an once closure; but in the
        special case of [partial_apply], the unique variable (the function) does
        not exercise its uniqueness capability, and does not make the closure
        once. *)
    val partial_apply : t -> t

    val min_with_uniqueness : Uniqueness.Const.t -> t
  end

  type t

  val legacy : t

  val local : t

  val unique : t

  val local_unique : t

  val prod : Locality.t -> Uniqueness.t -> Linearity.t -> t

  val of_const : Const.t -> t

  val is_const : t -> bool

  val min_mode : t

  val max_mode : t

  (** Projections to Locality, Uniqueness and Linearity *)

  val locality : t -> Locality.t

  val uniqueness : t -> Uniqueness.t

  val linearity : t -> Linearity.t

  type error =
    [ `Locality
    | `Uniqueness
    | `Linearity ]

  val submode : t -> t -> (unit, error) result

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, error) result

  val join : t list -> t

  (* Force a mode variable to its upper bound *)
  val constrain_upper : t -> Const.t

  (* Force a mode variable to its lower bound *)
  val constrain_lower : t -> Const.t

  (* Force a mode variable to legacys *)
  val constrain_legacy : t -> Const.t

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  (* Same as [newvar_below] but only on the comonadic axes *)
  val newvar_below_comonadic : t -> t * bool

  val newvar_above : t -> t * bool

  val with_locality : Locality.t -> t -> t

  val with_uniqueness : Uniqueness.t -> t -> t

  val with_linearity : Linearity.t -> t -> t

  val of_uniqueness : Uniqueness.t -> t

  val of_locality : Locality.t -> t

  val of_linearity : Linearity.t -> t

  val check_const :
    t ->
    ( Locality.Const.t option,
      Uniqueness.Const.t option,
      Linearity.Const.t option )
    modes

  val print' : ?verbose:bool -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit

  val close_over : t -> t

  val partial_apply : t -> t
end

module Value : sig
  module Const : sig
    type t = (Regionality.Const.t, Uniqueness.Const.t, Linearity.Const.t) modes
  end

  type t

  val legacy : t

  val regional : t

  val local : t

  val unique : t

  val regional_unique : t

  val local_unique : t

  val of_const : Const.t -> t

  val max_mode : t

  val min_mode : t

  (** Injections from Locality and Uniqueness into [Value_mode.t] *)

  (* The 'min_with_*' functions extend the min_mode,
     the 'max_with_' functions extend the max_mode,
     the 'with_*' functions extend given mode.
  *)
  val min_with_uniqueness : Uniqueness.t -> t

  val max_with_uniqueness : Uniqueness.t -> t

  val min_with_locality : Regionality.t -> t

  val max_with_locality : Regionality.t -> t

  val min_with_linearity : Linearity.t -> t

  val with_locality : Regionality.t -> t -> t

  val with_uniqueness : Uniqueness.t -> t -> t

  val with_linearity : Linearity.t -> t -> t

  (** Projections to Locality, Uniqueness and Linearity *)

  val locality : t -> Regionality.t

  val uniqueness : t -> Uniqueness.t

  val linearity : t -> Linearity.t

  (** Injections from [Alloc.t] into [Value_mode.t] *)

  (** [of_alloc] maps [Global] to [Global] and [Local] to [Local] *)
  val of_alloc : Alloc.t -> t

  (** Kernel operators *)

  (** The kernel operator [local_to_regional] maps [Local] to
      [Regional] and leaves the others unchanged. *)
  val local_to_regional : t -> t

  (** The kernel operator [regional_to_global] maps [Regional]
      to [Global] and leaves the others unchanged. *)
  val regional_to_global : t -> t

  val to_global : t -> t

  val to_unique : t -> t

  val to_many : t -> t

  (** Closure operators *)

  (** The closure operator [regional_to_local] maps [Regional]
      to [Local] and leaves the others unchanged. *)
  val regional_to_local : t -> t

  (** The closure operator [global_to_regional] maps [Global] to
      [Regional] and leaves the others unchanged. *)
  val global_to_regional : t -> t

  val to_local : t -> t

  val to_shared : t -> t

  val to_once : t -> t

  (** Note that the kernal and closure operators are in the following
      adjunction relationship:
      {v
        local_to_regional
        -| regional_to_local
        -| regional_to_global
        -| global_to_regional
      v}

      Equivalently,
      {v
        local_to_regional a <= b  iff  a <= regional_to_local b
        regional_to_local a <= b  iff  a <= regional_to_global b
        regional_to_global a <= b  iff  a <= global_to_regional b
      v}

      As well as:
      {v
        to_global -| to_local
        to_unique -| to_shared
      v}
   *)

  (** Versions of the operators that return [Alloc.t] *)

  (** Maps [Regional] to [Global] and leaves the others unchanged. *)
  val regional_to_global_alloc : t -> Alloc.t

  (** Maps [Regional] to [Local] and leaves the others unchanged. *)
  val regional_to_local_alloc : t -> Alloc.t

  (** Maps [Regional] to [Global] *)
  val regional_to_global_locality : t -> Locality.t

  (** Maps [Regional] to [Local] *)
  val regional_to_local_locality : t -> Locality.t

  type error =
    [ `Regionality
    | `Locality
    | `Uniqueness
    | `Linearity ]

  val submode : t -> t -> (unit, error) result

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, error) result

  val submode_meet : t -> t list -> (unit, error) result

  val join : t list -> t

  val constrain_upper : t -> Const.t

  val constrain_lower : t -> Const.t

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  val newvar_above : t -> t * bool

  val check_const :
    t ->
    ( Regionality.Const.t option,
      Uniqueness.Const.t option,
      Linearity.Const.t option )
    modes

  val print' : ?verbose:bool -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end
