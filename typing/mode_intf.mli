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

open Allowance

(* While all our lattices are bi-Heyting algebras (see [mode.ml]), the extra
   structure is not directly useful to the user, so we only expose the basic
   lattice structure. *)
module type Lattice = sig
  type t

  val min : t

  val max : t

  val legacy : t

  val le : t -> t -> bool

  (** [equal a b] is equivalent to [le a b && le b a], but defined separately for
      performance reasons *)
  val equal : t -> t -> bool

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

  (* [allowed] and [disallowed] is from [Allowance], see Note [Allowance]
     in allowance.mli. *)

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
      val check_const_conservative : ('l * 'r) t -> Const.t option
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

  module Portability : sig
    module Const : sig
      type t =
        | Portable
        | Nonportable

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_comonadic
  end

  module Uniqueness : sig
    module Const : sig
      type t =
        | Unique
        | Aliased

      include Lattice with type t := t
    end

    module Const_op : Lattice with type t = Const.t

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_monadic

    val aliased : lr

    val unique : lr

    val zap_to_ceil : ('l * allowed) t -> Const.t
  end

  module Contention : sig
    module Const : sig
      type t =
        | Contended
        | Shared
        | Uncontended

      include Lattice with type t := t
    end

    module Const_op : Lattice with type t = Const.t

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_monadic
  end

  module Yielding : sig
    module Const : sig
      type t =
        | Yielding
        | Unyielding

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t = (Const.t, 'd) mode_comonadic
  end

  type 'a comonadic_with =
    { areality : 'a;
      linearity : Linearity.Const.t;
      portability : Portability.Const.t;
      yielding : Yielding.Const.t
    }

  type monadic =
    { uniqueness : Uniqueness.Const.t;
      contention : Contention.Const.t
    }

  module Axis : sig
    (** ('p, 'r) t represents a projection from a product of type ['p] to an
    element of type ['r]. *)
    type ('p, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Linearity : ('areality comonadic_with, Linearity.Const.t) t
      | Portability : ('areality comonadic_with, Portability.Const.t) t
      | Yielding : ('areality comonadic_with, Yielding.Const.t) t
      | Uniqueness : (monadic, Uniqueness.Const.t) t
      | Contention : (monadic, Contention.Const.t) t

    val print : Format.formatter -> ('p, 'r) t -> unit

    val eq : ('p, 'r0) t -> ('p, 'r1) t -> ('r0, 'r1) Misc.eq option
  end

  module type Mode := sig
    module Areality : Common

    module Monadic : sig
      module Const : sig
        include Lattice with type t = monadic

        val max_axis : (t, 'a) Axis.t -> 'a

        val min_axis : (t, 'a) Axis.t -> 'a
      end

      module Const_op : Lattice with type t = monadic

      include Common with module Const := Const

      val join_const : Const.t -> ('l * 'r) t -> ('l * 'r) t
    end

    module Comonadic : sig
      module Const : sig
        include Lattice with type t = Areality.Const.t comonadic_with

        val eq : t -> t -> bool

        val print_axis : (t, 'a) Axis.t -> Format.formatter -> 'a -> unit

        val max_axis : (t, 'a) Axis.t -> 'a

        val min_axis : (t, 'a) Axis.t -> 'a
      end

      type error = Error : (Const.t, 'a) Axis.t * 'a Solver.error -> error

      include Common with type error := error and module Const := Const

      val meet_const : Const.t -> ('l * 'r) t -> ('l * 'r) t
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

    type 'd axis_packed = P : ('m, 'a, 'd) axis -> 'd axis_packed

    val print_axis : Format.formatter -> ('m, 'a, 'd) axis -> unit

    (** Gets the normal lattice for comonadic axes and the "op"ped lattice for
        monadic ones. *)
    val lattice_of_axis : ('m, 'a, 'd) axis -> (module Lattice with type t = 'a)

    val all_axes : ('l * 'r) axis_packed list

    type ('a, 'b, 'c, 'd, 'e, 'f) modes =
      { areality : 'a;
        linearity : 'b;
        uniqueness : 'c;
        portability : 'd;
        contention : 'e;
        yielding : 'f
      }

    module Const : sig
      include
        Lattice
          with type t =
            ( Areality.Const.t,
              Linearity.Const.t,
              Uniqueness.Const.t,
              Portability.Const.t,
              Contention.Const.t,
              Yielding.Const.t )
            modes

      module Option : sig
        type some = t

        type t =
          ( Areality.Const.t option,
            Linearity.Const.t option,
            Uniqueness.Const.t option,
            Portability.Const.t option,
            Contention.Const.t option,
            Yielding.Const.t option )
          modes

        val none : t

        val value : t -> default:some -> some

        val print : Format.formatter -> t -> unit
      end

      val is_max : ('m, 'a, 'd) axis -> 'a -> bool

      val is_min : ('m, 'a, 'd) axis -> 'a -> bool

      val split : t -> (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic

      val merge : (Monadic.Const.t, Comonadic.Const.t) monadic_comonadic -> t

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

    module List : sig
      (* No new types exposed to avoid too many type names *)
      include Allow_disallow with type (_, _, 'd) sided = 'd t list
    end

    val proj : ('m, 'a, 'l * 'r) axis -> ('l * 'r) t -> 'm

    val max_with : ('m, 'a, 'l * 'r) axis -> 'm -> (disallowed * 'r) t

    val min_with : ('m, 'a, 'l * 'r) axis -> 'm -> ('l * disallowed) t

    val meet_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> ('l * 'r) t

    val join_with : (_, 'a, _) axis -> 'a -> ('l * 'r) t -> ('l * 'r) t

    val zap_to_legacy : lr -> Const.t

    val zap_to_ceil : ('l * allowed) t -> Const.t

    val comonadic_to_monadic : ('l * 'r) Comonadic.t -> ('r * 'l) Monadic.t

    val meet_const : Const.t -> ('l * 'r) t -> ('l * 'r) t

    val imply : Const.t -> ('l * 'r) t -> (disallowed * 'r) t

    val join_const : Const.t -> ('l * 'r) t -> ('l * 'r) t

    val subtract : Const.t -> ('l * 'r) t -> ('l * disallowed) t

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

  (** The most general mode. Used in most type checking,
      including in value bindings in [Env] *)
  module Value : Mode with module Areality := Regionality

  (** The mode on arrow types. Compared to [Value], it contains the [Locality]
      axis instead of [Regionality] axis, as arrow types are exposed to users
      and would be hard to understand if it involves [Regionality]. *)
  module Alloc : Mode with module Areality := Locality

  module Const : sig
    val alloc_as_value : Alloc.Const.t -> Value.Const.t

    module Axis : sig
      val alloc_as_value : 'd Alloc.axis_packed -> 'd Value.axis_packed
    end

    val locality_as_regionality : Locality.Const.t -> Regionality.Const.t
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

  module Modality : sig
    type ('m, 'a) raw =
      | Meet_with : 'a -> (('a, 'd) mode_comonadic, 'a) raw
          (** [Meet_with c] takes [x] and returns [meet c x]. [c] can be [max]
          in which case it's the identity modality. *)
      | Join_with : 'a -> (('a, 'd) mode_monadic, 'a) raw
          (** [Join_with c] takes [x] and returns [join c x]. [c] can be [min]
          in which case it's the identity modality. *)

    (** An atom modality is a [raw] accompanied by the axis it acts on. *)
    type t = Atom : ('m, 'a, _) Value.axis * ('m, 'a) raw -> t

    (** Test if the given modality is the identity modality. *)
    val is_id : t -> bool

    (** Test if the given modality is a constant modality. *)
    val is_constant : t -> bool

    (** Printing for debugging *)
    val print : Format.formatter -> t -> unit

    module Value : sig
      type atom := t

      type error =
        | Error : ('m, 'a, _) Value.axis * ('m, 'a) raw Solver.error -> error

      type nonrec equate_error = equate_step * error

      (* In the following we have both [Const.t] and [t]. The former is parameterized by
         constant modes and thus its behavior fully determined. It is what users read and
         write on constructor arguments, record fields and value descriptions in signatures.

         The latter is parameterized by variable modes and thus its behavior changes as the
         variable modes change. It is used in module type inference: structures are inferred
         to have a signature containing a list of value descriptions, each of which carries a
         modality. This modality depends on the mode of the value, which is a variable.
         Therefore, we parameterize the modality over the variable mode.

         Utilities are provided to convert between [Const.t] and [t], such as [of_const],
         [zap_to_id], [zap_to_floor], etc.. *)

      module Const : sig
        (** A modality that acts on [Value] modes. Conceptually it is a sequnce
            of [atom] that acts on individual axes. *)
        type t

        (** The identity modality. *)
        val id : t

        (** Test if the given modality is the identity modality. *)
        val is_id : t -> bool

        (** Apply a modality on mode. *)
        val apply : t -> ('l * 'r) Value.t -> ('l * 'r) Value.t

        (** [compose ~then_ t] returns the modality that is [then_] after [t]. *)
        val compose : then_:atom -> t -> t

        (** [concat ~then t] returns the modality that is [then_] after [t]. *)
        val concat : then_:t -> t -> t

        (** [singleton m] returns the modality containing only [m]. *)
        val singleton : atom -> t

        (** Returns the list of [atom] in the given modality. The list is
            commutative. Post-condition: each axis is represented in the
            output list exactly once. *)
        val to_list : t -> atom list

        (** Builds up a modality from a list of [atom], by composing each atom with
            identity. The modalities are applied left to right. *)
        val of_list : atom list -> t

        (** Project out the [atom] for the given axis in the given modality. *)
        val proj : ('m, 'a, 'd) Value.axis -> t -> atom

        (** [equate t0 t1] checks that [t0 = t1].
            Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
        val equate : t -> t -> (unit, equate_error) Result.t

        (** Printing for debugging. *)
        val print : Format.formatter -> t -> unit
      end

      (** A modality that acts on [Value] modes. Conceptually it is a sequnce of
          [atom] that acts on individual axes. *)
      type t

      (** The identity modality. *)
      val id : t

      (** The undefined modality. *)
      val undefined : t

      (** Apply a modality on a left mode. *)
      val apply : t -> (allowed * 'r) Value.t -> Value.l

      (** [sub t0 t1] checks that [t0 <= t1].
          Definition: [t0 <= t1] iff [forall a. t0(a) <= t1(a)].

          In case of failure, [Error (ax, {left; right})] is returned, where
          [ax] is the axis on which the modalities disagree. [left] is the
          projection of [t0] on [ax], and [right] is the projection of [t1] on
          [ax]. *)
      val sub : t -> t -> (unit, error) Result.t

      (** [equate t0 t1] checks that [t0 = t1].
          Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
      val equate : t -> t -> (unit, equate_error) Result.t

      (** Printing for debugging. *)
      val print : Format.formatter -> t -> unit

      (** Given [md_mode] the mode of a module, and [mode] the mode of a value
      to be put in that module, return the inferred modality to be put on the
      value description in the inferred module type.

      The caller should ensure that for comonadic axes, [md_mode >= mode]. *)
      val infer : md_mode:Value.lr -> mode:Value.lr -> t

      (* The following zapping functions possibly mutate a potentially inferred
         modality [m] to a constant modality [c]. The constant modality is
         returned. The following coherence conditions hold:
         - [m <= c] always holds, even after further mutations to [m].
         - [c0 <= c1] always holds, where [c0] and [c1] are results of two
            abitrary zappings of some [m], even after further mutations to [m].
            Essentially that means [c0 = c1].

         NB: zapping an inferred modality will zap both [md_mode] and [mode] that
         it contains. The caller is reponsible for correct zapping order.
      *)

      (** Zap an inferred modality towards identity modality. *)
      val zap_to_id : t -> Const.t

      (** Zap an inferred modality towards the lowest (strongest) modality. *)
      val zap_to_floor : t -> Const.t

      (** Asserts the given modality is a const modality, and returns it. *)
      val to_const_exn : t -> Const.t

      (** Checks if the given modality is a const modality *)
      val to_const_opt : t -> Const.t option

      (** Inject a constant modality. *)
      val of_const : Const.t -> t

      (** The top modality; [sub x max] succeeds for any [x]. *)
      val max : t
    end
  end

  module Crossing : sig
    (** The mode crossing capability pertaining to a type.

    Some modes might be indistinguishable for values of some type, in which case
    the actual/expected mode of values can be adjusted accordingly to make more
    programs mode-check. The adjustment is called mode crossing. *)
    type t

    (* CR zqian: Complete the lattice structure of mode crossing. *)

    (* CR zqian: jkind modal bounds should just be our [t]. In particular, jkind
       should infer the modal bounds of a type in the form of [Value] instead of
       [Alloc]. For example, a type could have [regional] modality, in which case
       it can cross to [regional] but not [global]. *)

    (** Convert from jkind modal bounds. *)
    val of_bounds :
      (Alloc.Monadic.Const.t, Alloc.Comonadic.Const.t) monadic_comonadic -> t

    (** [modality m t] gives the mode crossing of type [T] wrapped in modality
    [m] where [T] has mode crossing [t]. *)
    val modality : Modality.Value.Const.t -> t -> t

    (** Apply mode crossing on a left mode, making it stronger. *)
    val apply_left : t -> Value.l -> Value.l

    (** Apply mode crossing on a right mode, making it more permissive. *)
    val apply_right : t -> Value.r -> Value.r

    (* We extend mode crossing on [Value] to [Alloc] via [alloc_as_value].
       Concretely, two [Alloc] modes are indistinguishable if their image under
       [alloc_as_value] are indistinguishable. Currently types cross locality
       either fully or fully not, and therefore [alloc_as_value] seems sufficient. *)

    (** Similar to [apply_left] but for [Alloc] via [alloc_as_value] *)
    val apply_left_alloc : t -> Alloc.l -> Alloc.l

    (** Similar to [apply_right] but for [Alloc] via [alloc_as_value] *)
    val apply_right_alloc : t -> Alloc.r -> Alloc.r

    (** Apply mode crossong on the left comonadic fragment, and the right
        monadic fragment. *)
    val apply_left_right_alloc :
      t ->
      (Alloc.Monadic.r, Alloc.Comonadic.l) monadic_comonadic ->
      (Alloc.Monadic.r, Alloc.Comonadic.l) monadic_comonadic

    (** [le t0 t1] returns [true] if [t0] allows more mode crossing than [t1]. *)
    val le : t -> t -> bool

    (** Print the mode crossing by axis. Omit axes that do not cross. *)
    val print : Format.formatter -> t -> unit
  end
end
