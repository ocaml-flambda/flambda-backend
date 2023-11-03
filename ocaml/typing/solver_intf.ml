type allowed = private Allowed

type disallowed = private Disallowed

type positive = private Positive

type negative = private Negative

type left_only = allowed * disallowed

type right_only = disallowed * allowed

type both = allowed * allowed

type ('a, 'b) eq = Refl : ('a, 'a) eq

type 'a pos = 'b * 'c constraint 'a = 'b * 'c

type 'a neg = 'c * 'b constraint 'a = 'b * 'c

(** A collection of lattices, indexed by [obj] *)
module type Lattices = sig
  (** Lattice identifers, indexed by ['a] the carrier type of that lattice *)
  type 'a obj

  val min : 'a obj -> 'a

  val max : 'a obj -> 'a

  val le : 'a obj -> 'a -> 'a -> bool

  val join : 'a obj -> 'a -> 'a -> 'a

  val meet : 'a obj -> 'a -> 'a -> 'a

  val print : 'a obj -> Format.formatter -> 'a -> unit

  val eq_obj : 'a obj -> 'b obj -> ('a, 'b) eq option
end

(** Extend [Lattices] with monotone functions (including identity) to form a
   category. Among those monotone functions some will have left and right
   adjoints. *)
module type Lattices_mono = sig
  include Lattices

  (** Morphism from object of base type ['a] to object of base type ['b].
      ['d] is ['l] * ['r], where ['l] can be:
      - [allowed], meaning the morphism can be on the left/having right adjoint)
      - [disallowed], meaning the morphism cannot be on the right/not having right adjoint)
      Similar for ['r] *)
  type ('a, 'b, 'd) morph

  (* Due to the implementation in [solver.ml], a mode doesn't have sufficient
     information to infer the object it lives in,  whether at compile-time or
     runtime. There is info at compile-time to distinguish between different
     carrier types, but one can imagine multiple objects with the same carrier
     type. Therefore, we can treat modes as object-blind.

     As a result, user of the solver needs to provide the object the modes live
     in, every time it invokes the solver on some modes.

     Roughly, ['a mode] is represented in the solver as constant of ['a], or [f
     v] where [f] is a morphism from ['b] to ['a] and [v] is some variable in
     ['a]. The ['a] needs additional ['a obj] to decide its position in the
     lattice structure (because again, multiple lattices can share the same
     carrier type). One might think the morphism [f] should know its own source
     and target objects. But since its target object is already given by the
     user for each invocation anyway, we decide to exploit this, and say that "a
     morphism is determined by some [('a, 'b, 'd) morph] together with some ['b
     obj]". That helps reduce the information each [morph] needs to store.

     As a result, in the interaction between the solver and the lattices,
     [morph] always comes with its target object. *)

  (** Give the source object of a morphism  *)
  val src : 'b obj -> ('a, 'b, 'd) morph -> 'a obj

  (** Give the identity morphism on an object *)
  val id : ('a, 'a, 'd) morph

  (** Compose two morphisms *)
  val compose :
    'c obj -> ('b, 'c, 'd) morph -> ('a, 'b, 'd) morph -> ('a, 'c, 'd) morph

  (* The following returns weaker than what we want, which is "\exists r.
     allowed * r". But ocaml doesn't like existentials, and this weaker version
     is good enough for us *)

  (** Give left adjoint of a morphism  *)
  val left_adjoint :
    'b obj -> ('a, 'b, 'l * allowed) morph -> ('b, 'a, left_only) morph

  (** Give the right adjoint of a morphism *)
  val right_adjoint :
    'b obj -> ('a, 'b, allowed * 'r) morph -> ('b, 'a, right_only) morph

  (** Forget whether a function can be on the right. *)
  val disallow_right :
    ('a, 'b, 'l * 'r) morph -> ('a, 'b, 'l * disallowed) morph

  (** Forget whether a morphism can be on the left. *)
  val disallow_left : ('a, 'b, 'l * 'r) morph -> ('a, 'b, disallowed * 'r) morph

  (** Generalize a morphism that can be on the right   *)
  val allow_right : ('a, 'b, 'l * allowed) morph -> ('a, 'b, 'l * 'r) morph

  (** Generalize a morphism that can be on the left  *)
  val allow_left : ('a, 'b, allowed * 'r) morph -> ('a, 'b, 'l * 'r) morph

  (** Apply morphism on constant *)
  val apply : 'b obj -> ('a, 'b, 'd) morph -> 'a -> 'b

  val print_obj : Format.formatter -> 'a obj -> unit

  (** Print morphism *)
  val print_morph : 'b obj -> Format.formatter -> ('a, 'b, 'd) morph -> unit
end

module type S = sig
  type 'a error =
    { left : 'a;
      right : 'a
    }

  (** Solver that supports polarized lattices; needed because some morphisms
      are antitone  *)
  module Solver_polarized (C : Lattices_mono) : sig
    type changes

    val undo_changes : changes -> unit

    val append_changes : (changes ref -> unit) ref

    (* First construct a new category based on the original category C. The
       objects are those from the C, and those from C but flipped lattice
       structure. The morphisms are the obvious four copies of the original
       morhpisms. *)

    type 'a obj =
      | Positive : 'a C.obj -> ('a * positive) obj
          (** The original lattice of obj *)
      | Negative : 'a C.obj -> ('a * negative) obj
          (** the dual lattice of obj *)

    (* ['a] and ['b] are source and destination objects;
       ['d] and ['e] are source and desitnation adjoint status *)
    type ('a, 'd, 'b, 'e) morph =
      | Pos_Pos :
          ('a, 'b, 'd) C.morph
          -> ('a * positive, 'd pos, 'b * positive, 'd pos) morph
          (** The monotone morphism from a positive lattice to a positive lattice  *)
      | Pos_Neg :
          ('a, 'b, 'd) C.morph
          -> ('a * positive, 'd pos, 'b * negative, 'd neg) morph
          (** The antitone morphism from a positive lattice to a negative lattice  *)
      | Neg_Pos :
          ('a, 'b, 'd) C.morph
          -> ('a * negative, 'd neg, 'b * positive, 'd pos) morph
          (** The antitone morphism from a negative lattice to a positive lattice  *)
      | Neg_Neg :
          ('a, 'b, 'd) C.morph
          -> ('a * negative, 'd neg, 'b * negative, 'd neg) morph
          (** The monotone morphism from a negative lattice to a negative lattice *)

    (* [id] and [compose] not used; just for fun *)
    val id : 'a obj -> ('a, 'l * 'r, 'a, 'l * 'r) morph

    val compose :
      'c obj ->
      ('b, 'bl * 'br, 'c, 'cl * 'cr) morph ->
      ('a, 'al * 'ar, 'b, 'bl * 'br) morph ->
      ('a, 'al * 'ar, 'c, 'cl * 'cr) morph

    type ('a, 'd) mode

    (** Disallows a mode of being on the RHS of [submode].  *)
    val disallow_right : ('a, 'l * 'r) mode -> ('a, 'l * disallowed) mode

    (** Disallows a mode of being on the LHS of [submode].  *)
    val disallow_left : ('a, 'l * 'r) mode -> ('a, disallowed * 'r) mode

    (** Generalizes a mode's ability to be on the RHS of [submode].  *)
    val allow_right : ('a, 'l * allowed) mode -> ('a, 'l * 'r) mode

    (** Generalizes a mode's ability to be on the LHS of [submode].  *)
    val allow_left : ('a, allowed * 'r) mode -> ('a, 'l * 'r) mode

    (** Returns the result of applying the morphism to the mode. *)
    val apply :
      'b obj ->
      ('a, 'd0 * 'd1, 'b, 'e0 * 'e1) morph ->
      ('a, 'd0 * 'd1) mode ->
      ('b, 'e0 * 'e1) mode

    (** Returns the mode representing the given constant. *)
    val of_const : ('a * 'p) obj -> 'a -> ('a * 'p, 'l * 'r) mode

    (** The minimum mode in the lattice *)
    val min : 'a obj -> ('a, 'l * 'r) mode

    (** The maximum mode in the lattice *)
    val max : 'a obj -> ('a, 'l * 'r) mode

    (** Pushes the mode variable to the lowest constant possible. *)
    val constrain_lower : ('a * 'p) obj -> ('a * 'p, allowed * 'r) mode -> 'a

    (** Pushes the mode variable to the highest constant possible. *)
    val constrain_upper : ('a * 'p) obj -> ('a * 'p, 'l * allowed) mode -> 'a

    (** Create a new mode variable of the full range. *)
    val newvar : 'a obj -> ('a, 'l * 'r) mode

    (** Try to constrain the first mode below the second mode. *)
    val submode :
      ('a * 'p) obj ->
      ('a * 'p, allowed * 'r) mode ->
      ('a * 'p, 'l * allowed) mode ->
      (unit, 'a error) result

    (** Creates a new mode variable above the given mode and returns [true]. In
        the speical case where the given mode is top, returns a constant mode
        and [false]. *)
    val newvar_above :
      'a obj -> ('a, allowed * 'r_) mode -> ('a, 'l * 'r) mode * bool

    (** Creates a new mode variable below the given mode and returns [true]. In
        the speical case where the given mode is bottom, returns a constant mode
        and [false]. *)
    val newvar_below :
      'a obj -> ('a, 'l_ * allowed) mode -> ('a, 'l * 'r) mode * bool

    (** Returns the join of the list of modes. *)
    val join : 'a obj -> ('a, allowed * 'r) mode list -> ('a, left_only) mode

    (** Return the meet of the list of modes. *)
    val meet : 'a obj -> ('a, 'l * allowed) mode list -> ('a, right_only) mode

    (** Checks if a mode has been constrained sufficiently to a constant.
        Expensive. *)
    val check_const : ('a * 'p) obj -> ('a * 'p, 'l * 'r) mode -> 'a option

    (** Print a mode. Calls [check_const] for cleaner printing and thus
    expensive.  *)
    val print :
      ?verbose:bool -> 'a obj -> Format.formatter -> ('a, 'l * 'r) mode -> unit

    (** Print a mode without calling [check_const]. *)
    val print_raw :
      ?verbose:bool -> 'a obj -> Format.formatter -> ('a, 'l * 'r) mode -> unit
  end
end
