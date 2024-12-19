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

  (* [allowed] and [disallowed] is from [Solver_intf], see Note [Allowance]
     in that file. *)

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

  val generic_level : int

  val newvar : int -> ('l * 'r) t

  val submode : (allowed * 'r) t -> ('l * allowed) t -> (unit, error) result

  val update_level : int -> ('l * 'r) t -> unit

  val generalize :
    current_level:int ->
    ('l * 'r) t ->
    unit

  val generalize_structure :
    current_level:int ->
    ('l * 'r) t ->
    unit

  val equate : lr -> lr -> (unit, equate_error) result

  val submode_exn : (allowed * 'r) t -> ('l * allowed) t -> unit

  val equate_exn : lr -> lr -> unit

  val join : (allowed * 'r) t list -> left_only t

  val meet : ('l * allowed) t list -> right_only t

  val newvar_above : int -> (allowed * 'r) t -> ('l * 'r_) t * bool

  val newvar_below : int -> ('l * allowed) t -> ('l_ * 'r) t * bool

  val newvar_above_if_nonzero : (allowed * 'r) t -> (allowed * 'r) t

  val print : ?verbose:bool -> unit -> Format.formatter -> ('l * 'r) t -> unit

  val of_const : Const.t -> ('l * 'r) t

  val check_level : ('l * 'r) t -> int -> bool

  val check_level_var : ('l * 'r) t -> int -> bool
end

module type S = sig
  type changes

  val undo_changes : changes -> unit

  val set_append_changes : (changes ref -> unit) -> unit

  type copy_scope

  val with_copy_scope : (copy_scope -> 'a) -> 'a

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

    (** zaps non-generic variables to floor, returns [None] if variable is generic. *)
    val zap_to_floor : (allowed * 'r) t -> Const.t option

    (** zaps non-generic variables to ceil, returns [None] if variable is generic. *)
    val zap_to_ceil : ('l * allowed) t -> Const.t option

    (** zaps all variables to floor (including generic variables): use with caution *)
    val zap_to_floor_force : (allowed * 'r) t -> Const.t

    (** zaps all variables to ceil (including generic variables): use with caution *)
    val zap_to_ceil_force : ('l * allowed) t -> Const.t

    (** zaps non-generic variables to floor, raises a [Cannot_zap_generic] excetion if
        variable is generic *)
    val zap_to_floor_exn : (allowed * 'r) t -> Const.t

    (** zaps non-generic variables to ceil, raises a [Cannot_zap_generic] excetion if
        variable is generic *)
    val zap_to_ceil_exn : ('l * allowed) t -> Const.t

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

  type 'a comonadic_with =
    { areality : 'a;
      linearity : Linearity.Const.t;
      portability :  Portability.Const.t; }

  type monadic =
    { uniqueness : Uniqueness.Const.t;
      contention : Contention.Const.t }

  module Axis : sig
    (** ('p, 'r) t represents a projection from a product of type ['p] to an
    element of type ['r]. *)
    type ('p, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Linearity : ('areality comonadic_with, Linearity.Const.t) t
      | Portability : ('areality comonadic_with, Portability.Const.t) t
      | Uniqueness : (monadic, Uniqueness.Const.t) t
      | Contention : (monadic, Contention.Const.t) t

    val print : Format.formatter -> ('p, 'r) t -> unit
  end

  module type Mode := sig
    module Areality : Common

    module Monadic : sig
      module Const : Lattice with type t = monadic

      module Const_op : Lattice with type t = monadic

      include Common with module Const := Const

      val join_const : Const.t -> (disallowed * 'r) t -> (disallowed * 'r) t
    end

    module Comonadic : sig
      module Const : sig
        include Lattice with type t = Areality.Const.t comonadic_with

        val eq : t -> t -> bool

        val print_axis : (t, 'a) Axis.t -> Format.formatter -> 'a -> unit
      end

      type error = Error : (Const.t, 'a) Axis.t * 'a Solver.error -> error

      include Common with type error := error and module Const := Const

      val meet_const : Const.t -> ('l * disallowed) t -> ('l * disallowed) t
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

    type ('a, 'b, 'c, 'd, 'e) modes =
      { areality : 'a;
        linearity : 'b;
        uniqueness : 'c;
        portability : 'd;
        contention : 'e
      }

    module Const : sig
      include
        Lattice
          with type t =
            ( Areality.Const.t,
              Linearity.Const.t,
              Uniqueness.Const.t,
              Portability.Const.t,
              Contention.Const.t )
            modes

      module Option : sig
        type some = t

        type t =
          ( Areality.Const.t option,
            Linearity.Const.t option,
            Uniqueness.Const.t option,
            Portability.Const.t option,
            Contention.Const.t option )
          modes

        val none : t

        val value : t -> default:some -> some

        val print : Format.formatter -> t -> unit

        val partial_print : Format.formatter -> t -> unit
      end

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

    (** Scope containing pending zap jobs *)
    type zap_scope

    val with_zap_scope : (zap_scope:zap_scope -> 'a) -> 'a

    (** Exposed subset of the monotone Lattices interface *)
    module C : sig
      type ('a, 'b, 'd) morph
      type 'a obj

      val le : 'a obj -> 'a -> 'a -> bool

      val eq_obj : 'a obj -> 'b obj -> ('a, 'b) Misc.eq option

      val src : 'b obj -> ('a, 'b, 'd) morph -> 'a obj

      val id : ('a, 'a, 'd) morph

      val compose :
        'c obj -> ('b, 'c, 'd) morph -> ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph

      val eq_morph :
        'b obj ->
        ('a0, 'b, 'l0 * 'r0) morph ->
        ('a1, 'b, 'l1 * 'r1) morph ->
        ('a0, 'a1) Misc.eq option

      val left_adjoint :
        'b obj -> ('a, 'b, 'l * allowed) morph -> ('b, 'a, left_only) morph

      val disallow_right : ('a, 'b, 'l * 'r) morph -> ('a, 'b, 'l * disallowed) morph

      val apply : 'b obj -> ('a, 'b, 'd) morph -> 'a -> 'b

      val print_morph : 'b obj -> Format.formatter -> ('a, 'b, 'd) morph -> unit

    end

    (** The exposed description of modes *)
    module Desc : sig

      module Var : sig
        type 'a t

        type ('b, 'd) t_with_morph =
        | Amorphvar : 'a t * ('a, 'b, 'd) C.morph -> ('b, 'd) t_with_morph

        module Head : sig

          type 'a t = {
            desc_id : int;
            desc_upper : 'a;
            desc_lower : 'a;
            desc_vlower : (('a,left_only) t_with_morph) list;
            desc_level : int;
          }

          val equal : 'a t -> 'b t -> bool

          val hash : 'a t -> int
        end

        val force : 'a C.obj -> 'a t -> 'a Head.t
      end

      type ('b, 'd) morphvar =
      | Amorphvar : 'a Var.Head.t * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar

      type ('a, 'd) t =
      | Amode : 'a -> ('a, 'l * 'r) t
      | Amodevar : ('a, 'd) morphvar -> ('a, 'd) t
      | Amodejoin :
          'a * ('a, 'l * disallowed) morphvar list
          -> ('a, 'l * disallowed) t
      | Amodemeet :
         'a * ('a, disallowed * 'r) morphvar list
         -> ('a, disallowed * 'r) t

      val print : 'a C.obj -> Format.formatter -> ('a, ('l * 'r)) t -> unit
    end

    val obj_monadic : Monadic.Const.t C.obj

    val obj_comonadic : Comonadic.Const.t C.obj

    val get_comonadic_desc : 'd Comonadic.t -> (Comonadic.Const.t, 'd) Desc.t

    val get_monadic_desc : ('l * 'r) Monadic.t -> (Monadic.Const.t, ('r * 'l)) Desc.t

    val meet_const_morph : 'a -> ('a, 'a, allowed * disallowed) C.morph

    val pretty_print_monadic_morph :
      (Format.formatter -> 'a -> unit) -> 'a
      -> Format.formatter -> ('d, Monadic.Const.t, 'f) C.morph -> unit

    val pretty_print_comonadic_morph :
      (Format.formatter -> 'a -> unit) -> 'a
      -> Format.formatter -> ('d, Comonadic.Const.t, 'f) C.morph -> unit

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t := 'd t

    module List : sig
      (* No new types exposed to avoid too many type names *)
      include Allow_disallow with type (_, _, 'd) sided = 'd t list
    end

    val meet_const :
      Comonadic.Const.t -> ('l * 'r) t -> ('l * disallowed) t

    val join_const :
      Monadic.Const.t -> ('l * 'r) t -> (disallowed * 'r) t

    val imply :
      Comonadic.Const.t -> ('l * 'r) t -> (disallowed * 'r) t

    val subtract :
      Monadic.Const.t -> ('l * 'r) t -> ('l * disallowed) t

    val proj : ('m, 'a, 'l * 'r) axis -> ('l * 'r) t -> 'm

    val max_with : ('m, 'a, 'l * 'r) axis -> 'm -> (disallowed * 'r) t

    val min_with : ('m, 'a, 'l * 'r) axis -> 'm -> ('l * disallowed) t

    val meet_with :
      ('m, 'a, 'l2 * allowed) axis -> 'm -> ('l1 * allowed) t -> right_only t

    val join_with :
      ('m, 'a, allowed * 'r2) axis -> 'm -> (allowed * 'r1) t -> left_only t

    val meet_const_with :
      (Comonadic.Const.t, 'a) Axis.t
      -> 'a
      -> ('l * 'r) t
      -> ('l * disallowed) t

    val join_const_with :
      (Monadic.Const.t, 'a) Axis.t
      -> 'a
      -> ('l * 'r) t
      -> (disallowed * 'r) t

    val imply_with :
      (Comonadic.Const.t, 'a) Axis.t
      -> 'a
      -> ('l * 'r) t
      -> (disallowed * 'r) t

    val subtract_with :
      (Monadic.Const.t, 'a) Axis.t
      -> 'a
      -> ('l * 'r) t
      -> ('l * disallowed) t

    val add_mode_to_zap_scope :
      (allowed * allowed) t
      -> zap_scope
      -> unit

    val zap_to_legacy_exn : lr -> Const.t

    val zap_to_legacy : lr -> Const.t option

    val zap_to_legacy_force : lr -> Const.t

    val zap_to_ceil_exn : ('l * allowed) t -> Const.t

    val zap_to_floor_exn : (allowed * 'r) t -> Const.t

    val zap_to_ceil_force : ('l * allowed) t -> Const.t

    val zap_to_floor_force : (allowed * 'r) t -> Const.t

    val comonadic_to_monadic : ('l * 'r) Comonadic.t -> ('r * 'l) Monadic.t

    (* The following two are about the scenario where we partially apply a
       function [A -> B -> C] to [A] and get back [B -> C]. The mode of the
       three are constrained. *)

    (** Returns the lower bound needed for [B -> C] in relation to [A] *)
    val close_over :
      (('l * allowed) Monadic.t, (allowed * 'r) Comonadic.t) monadic_comonadic ->
      l

    (** Returns the lower bound needed for [B -> C] in relation to [A -> B -> C] *)
    val partial_apply : (allowed * 'r) t -> l

    val instantiate :
      copy_scope:copy_scope ->
      current_level:int ->
      ('l * 'r) t ->
      ('l * 'r) t

    val copy_generic :
      copy_scope:copy_scope ->
      ('l * 'r) t ->
      ('l * 'r) t

    val duplicate :
      copy_scope:copy_scope ->
      ('l * 'r) t ->
      ('l * 'r) t

    module Guts : sig
      (** Returns the precise bounds of a mode, as close to legacy as possible.
          see notes on [get_floor] in [solver_intf.mli] for cautions. *)
      val get_legacy : (allowed * allowed) t -> Const.t

      (** Returns the precise ceiling of a mode. see notes on [get_ceil] in
          [solver_intf.mli] for cautions. *)
      val get_ceil : ('l * allowed) t -> Const.t

      (** Returns [Some c] if the given mode has been constrained to constant
          [c]. see notes on [get_floor] in [solver_intf.mli] for cautions. *)
      val check_const : (allowed * allowed) t -> Const.t option

      (** Checks that a constant is within the precise bounds of a mode. see notes on
          [get_floor] in [solver_intf.mli] for cautions. *)
      val in_bounds : Const.t -> (allowed * allowed) t -> bool
    end
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

    val alloc_as_value_comonadic :
      Alloc.Comonadic.Const.t -> Value.Comonadic.Const.t

    val locality_as_regionality : Locality.Const.t -> Regionality.Const.t
  end

  (** Inject locality into regionality *)
  val locality_as_regionality : Locality.l -> Regionality.l

  (** Similar to [locality_as_regionality], behaves as identity on other axes *)
  val alloc_as_value : ('l * 'r) Alloc.t -> ('l * 'r) Value.t

  (** Similar to [local_to_regional], behaves as identity in other axes *)
  val alloc_to_value_l2r : ('l * 'r) Alloc.t -> ('l * disallowed) Value.t

  (** Similar to [regional_to_local], behaves as identity on other axes *)
  val value_to_alloc_r2l : ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  (** Similar to [regional_to_global], behaves as identity on other axes *)
  val value_to_alloc_r2g :
    ('l * 'r) Value.t -> (disallowed * 'r) Alloc.t

  module Modality : sig
    type ('m, 'a) raw =
      | Meet_const : 'a -> (('a, 'd) mode_comonadic, 'a) raw
          (** [Meet_const c] takes [x] and returns [meet c x]. [c] can be [max]
          in which case it's the identity modality. *)
      | Join_const : 'a -> (('a, 'd) mode_monadic, 'a) raw
          (** [Join_const c] takes [x] and returns [join c x]. [c] can be [min]
          in which case it's the identity modality. *)

    (** An atom modality is a [raw] accompanied by the axis it acts on. *)
    type t = Atom : ('m, 'a, _) Value.axis * ('m, 'a) raw -> t

    (** Test if the given modality is the identity modality. *)
    val is_id : t -> bool

    (** Printing for debugging *)
    val print : Format.formatter -> t -> unit

    module Value : sig
      type atom := t

      type error =
        | Error : ('m, 'a, _) Value.axis * ('m, 'a) raw Solver.error -> error

      type nonrec equate_error = equate_step * error

      module Const : sig
        (** A modality that acts on [Value] modes. Conceptually it is a sequnce
            of [atom] that acts on individual axes. *)
        type t

        (** The identity modality. *)
        val id : t

        (** Apply a modality on left mode. *)
        val apply_left : t -> (allowed * 'r) Value.t -> Value.l

        (** Apply a modality on right mode. *)
        val apply_right : t -> ('l * allowed) Value.t -> Value.r

        (** [compose ~then_ t] returns the modality that is [then_] after [t]. *)
        val compose : then_:atom -> t -> t

        (** [concat ~then t] returns the modality that is [then_] after [t]. *)
        val concat : then_:t -> t -> t

        (** [singleton m] returns the modality containing only [m]. *)
        val singleton : atom -> t

        (** Returns the list of [atom] in the given modality. The list is
            commutative. *)
        val to_list : t -> atom list

        (** [equate t0 t1] checks that [t0 = t1].
            Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
        val equate : t -> t -> (unit, equate_error) Result.t
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

      (** [update_level n t] updates the level of mode variables in [t] to [n] *)
      val update_level : int -> t -> unit

      (** [equate t0 t1] checks that [t0 = t1].
          Definition: [t0 = t1] iff [t0 <= t1] and [t1 <= t0]. *)
      val equate : t -> t -> (unit, equate_error) Result.t

      (** Printing for debugging. *)
      val print : Format.formatter -> t -> unit

      (** Given [md_mode] the mode of a module, and [mode] the mode of a value
      to be put in that module, return the inferred modality to be put on the
      value description in the inferred module type. *)
      val infer : md_mode:Value.lr -> mode:Value.l -> t

      (* The following zapping functions possibly mutate a potentially inferred
         modality [m] to a constant modality [c]. The constant modality is
         returned. [m <= c] holds, even after further mutations to [m]. *)

      (** Returns a const modality weaker than the given modality. *)
      val zap_to_id : t -> Const.t

      (** Returns a const modality lowest (strongest) possible. *)
      val zap_to_floor : t -> Const.t

      (** Asserts the given modality is a const modality, and returns it. *)
      val to_const_exn : t -> Const.t

      (** Inject a constant modality. *)
      val of_const : Const.t -> t

      (** The top modality; [sub x max] succeeds for any [x]. *)
      val max : t
    end
  end
end
