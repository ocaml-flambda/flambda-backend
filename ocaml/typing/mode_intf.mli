(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Solver_intf

(* While all our lattices are bi-Heyting algebras (see [mode.ml]), the extra
   structure is not directly useful to the user, so we only expose the basic
   lattice structure. *)
module type Lattice = sig
  type t

  val min : t

  val max : t

  val legacy : t

  val le : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end

type equate_step =
  | Left_le_right
  | Right_le_left

module type Common = sig
  module Const : Lattice

  type error

  type equate_error = equate_step * error

  type 'd t constraint 'd = 'l * 'r

  (** Left-only mode *)
  type l = (allowed * disallowed) t

  (** Right-only mode *)
  type r = (disallowed * allowed) t

  (** Left-right mode *)
  type lr = (allowed * allowed) t

  include Allow_disallow with type (_, _, 'd) sided = 'd t

  val min : lr

  val max : lr

  val legacy : lr

  val newvar : unit -> ('l * 'r) t

  val submode : (allowed * 'r) t -> ('l * allowed) t -> (unit, error) result

  val equate : lr -> lr -> (unit, equate_error) result

  val submode_exn : (allowed * 'r) t -> ('l * allowed) t -> unit

  val equate_exn : lr -> lr -> unit

  val join : (allowed * 'r) t list -> left_only t

  val meet : ('l * allowed) t list -> right_only t

  val newvar_above : (allowed * 'r) t -> ('l * 'r_) t * bool

  val newvar_below : ('l * allowed) t -> ('l_ * 'r) t * bool

  val print : ?verbose:bool -> unit -> Format.formatter -> ('l * 'r) t -> unit

  val of_const : Const.t -> ('l * 'r) t
end

module type S = sig
  module Global_flag : sig
    type t =
      | Global
      | Unrestricted

    val compare : t -> t -> int
  end

  type changes

  val undo_changes : changes -> unit

  val set_append_changes : (changes ref -> unit) -> unit

  type nonrec allowed = allowed

  type nonrec disallowed = disallowed

  type nonrec equate_step = equate_step

  type ('a, 'd) mode_monadic constraint 'd = 'l * 'r

  type ('a, 'd) mode_comonadic constraint 'd = 'l * 'r

  type ('a, 'b) monadic_comonadic =
    { monadic : 'a;
      comonadic : 'b
    }

  module Locality : sig
    module Const : sig
      type t =
        | Global
        | Local

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_comonadic

    val global : lr

    val local : lr

    val zap_to_floor : (allowed * 'r) t -> Const.t

    val zap_to_ceil : ('l * allowed) t -> Const.t

    module Guts : sig
      (** This module exposes some functions that allow callers to inspect modes
      directly, which could be useful for error printing and dev tools (such as
      merlin). Any usage of this in type checking should be pondered. *)

      (** Returns [Some c] if the given mode has been constrained to constant
          [c]. see notes on [get_floor] in [solver_intf.mli] for cautions. *)
      val check_const : (allowed * allowed) t -> Const.t option

      (** Similar to [check_const] but doesn't run the further constraining
          needed for precise bounds. As a result, it is inexpensive and returns
          a conservative result. I.e., it might return [None] for
          fully-constrained modes. *)
      val check_const_conservative : (l * 'r) t -> Const.t option
    end
  end

  module Regionality : sig
    module Const : sig
      type t =
        | Global
        | Regional
        | Local

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_comonadic

    val global : lr

    val regional : lr

    val local : lr
  end

  module Linearity : sig
    module Const : sig
      type t =
        | Many
        | Once

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_comonadic

    val many : lr

    val once : lr
  end

  module Uniqueness : sig
    module Const : sig
      type t =
        | Unique
        | Shared

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_monadic

    val shared : lr

    val unique : lr
  end

  type 'a comonadic_with = private 'a * Linearity.Const.t

  type monadic = private Uniqueness.Const.t * unit

  module Axis : sig
    (** ('p, 'r) t represents a projection from a product of type ['p] to an
    element of type ['r]. *)
    type ('p, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Linearity : ('areality comonadic_with, Linearity.Const.t) t
      | Uniqueness : (monadic, Uniqueness.Const.t) t

    val print : Format.formatter -> ('p, 'r) t -> unit
  end

  (** The most general mode. Used in most type checking,
      including in value bindings in [Env] *)
  module Value : sig
    module Monadic : sig
      module Const : Lattice with type t = monadic

      include Common with module Const := Const
    end

    module Comonadic : sig
      module Const : Lattice with type t = Regionality.Const.t comonadic_with

      type error = Error : (Const.t, 'a) Axis.t * 'a Solver.error -> error

      include Common with type error := error and module Const := Const
    end

    (** Represents a mode axis in this product whose constant is ['a], and
        whose variable is ['m] given the allowness ['d]. *)
    type ('m, 'a, 'd) axis =
      | Monadic :
          (Monadic.Const.t, 'a) Axis.t
          -> (('a, 'd) mode_monadic, 'a, 'd) axis
      | Comonadic :
          (Comonadic.Const.t, 'a) Axis.t
          -> (('a, 'd) mode_comonadic, 'a, 'd) axis

    type ('a, 'b, 'c) modes =
      { regionality : 'a;
        linearity : 'b;
        uniqueness : 'c
      }

    module Const : sig
      include
        Lattice
          with type t =
            (Regionality.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes

      (** Prints a constant on any axis. *)
      val print_axis : ('m, 'a, 'd) axis -> Format.formatter -> 'a -> unit
    end

    type error = Error : ('m, 'a, 'd) axis * 'a Solver.error -> error

    type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t := 'd t

    module List : sig
      (* No new types exposed to avoid too many type names *)
      include Allow_disallow with type (_, _, 'd) sided = 'd t list
    end

    val proj : ('m, 'a, 'l * 'r) axis -> ('l * 'r) t -> 'm

    val max_with : ('m, 'a, 'l * 'r) axis -> 'm -> (disallowed * 'r) t

    val min_with : ('m, 'a, 'l * 'r) axis -> 'm -> ('l * disallowed) t

    val meet_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> ('l * disallowed) t

    val join_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> (disallowed * 'r) t

    val comonadic_to_monadic : ('l * 'r) Comonadic.t -> ('r * 'l) Monadic.t

    val meet_const : Const.t -> ('l * 'r) t -> ('l * disallowed) t

    val imply : Const.t -> ('l * 'r) t -> (disallowed * 'r) t
  end

  (** The mode on arrow types. Compared to [Value], it contains the [Locality]
      axis instead of [Regionality] axis, as arrow types are exposed to users
      and would be hard to understand if it involves [Regionality]. *)
  module Alloc : sig
    module Monadic : sig
      module Const : Lattice with type t = monadic

      include Common with module Const := Const

      val imply : Const.t -> ('l * 'r) t -> (disallowed * 'r) t
    end

    module Comonadic : sig
      module Const : sig
        include Lattice with type t = Locality.Const.t comonadic_with

        val eq : t -> t -> bool
      end

      include Common with module Const := Const

      val meet_const : Const.t -> ('l * 'r) t -> ('l * disallowed) t
    end

    (** Represents a mode axis in this product whose constant is ['a], and
        whose variable is ['m] given the allowness ['d]. *)
    type ('m, 'a, 'd) axis =
      | Monadic :
          (Monadic.Const.t, 'a) Axis.t
          -> (('a, 'd) mode_monadic, 'a, 'd) axis
      | Comonadic :
          (Comonadic.Const.t, 'a) Axis.t
          -> (('a, 'd) mode_comonadic, 'a, 'd) axis

    type ('loc, 'lin, 'uni) modes =
      { locality : 'loc;
        linearity : 'lin;
        uniqueness : 'uni
      }

    module Const : sig
      include
        Lattice
          with type t =
            (Locality.Const.t, Linearity.Const.t, Uniqueness.Const.t) modes

      val split : t -> (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic

      val merge : (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic -> t

      module Option : sig
        type some = t

        type t =
          ( Locality.Const.t option,
            Linearity.Const.t option,
            Uniqueness.Const.t option )
          modes

        val none : t

        val value : t -> default:some -> some
      end

      (** [diff a b] returns [None] for axes where [a] and [b] match, and [Some
      a0] for axes where [a] is [a0] and [b] isn't. *)
      val diff : t -> t -> Option.t

      (** Similar to [Alloc.close_over] but for constants *)
      val close_over : t -> t

      (** Similar to [Alloc.partial_apply] but for constants *)
      val partial_apply : t -> t

      (** Prints a constant on any axis. *)
      val print_axis : ('m, 'a, 'd) axis -> Format.formatter -> 'a -> unit
    end

    type error = Error : ('m, 'a, 'd) axis * 'a Solver.error -> error

    type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t := 'd t

    val proj : ('m, 'a, 'l * 'r) axis -> ('l * 'r) t -> 'm

    val max_with : ('m, 'a, 'l * 'r) axis -> 'm -> (disallowed * 'r) t

    val min_with : ('m, 'a, 'l * 'r) axis -> 'm -> ('l * disallowed) t

    val meet_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> ('l * disallowed) t

    val join_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> (disallowed * 'r) t

    val zap_to_legacy : lr -> Const.t

    val zap_to_ceil : ('l * allowed) t -> Const.t

    val meet_const : Const.t -> ('l * 'r) t -> ('l * disallowed) t

    val imply : Const.t -> ('l * 'r) t -> (disallowed * 'r) t

    (* The following two are about the scenario where we partially apply a
       function [A -> B -> C] to [A] and get back [B -> C]. The mode of the
       three are constrained. *)

    (** Returns the lower bound needed for [B -> C] in relation to [A] *)
    val close_over :
      (('l * allowed) Monadic.t, (allowed * 'r) Comonadic.t) monadic_comonadic ->
      l

    (** Returns the lower bound needed for [B -> C] in relation to [A -> B -> C] *)
    val partial_apply : (allowed * 'r) t -> l
  end

  module Const : sig
    val alloc_as_value : Alloc.Const.t -> Value.Const.t
  end

  (** Converts regional to local, identity otherwise *)
  val regional_to_local : ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Inject locality into regionality *)
  val locality_as_regionality : ('l * 'r) Locality.t -> ('l * 'r) Regionality.t

  (** Converts regional to global, identity otherwise *)
  val regional_to_global : ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Similar to [locality_as_regionality], behaves as identity on other axes *)
  val alloc_as_value : ('l * 'r) Alloc.t -> ('l * 'r) Value.t

  (** Similar to [local_to_regional], behaves as identity in other axes *)
  val alloc_to_value_l2r : ('l * 'r) Alloc.t -> ('l * disallowed) Value.t

  (** Similar to [regional_to_local], behaves as identity on other axes *)
  val value_to_alloc_r2l : ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  (** Similar to [regional_to_global], behaves as identity on other axes *)
  val value_to_alloc_r2g : ('l * 'r) Value.t -> ('l * 'r) Alloc.t
end
