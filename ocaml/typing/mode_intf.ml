open Solver_intf

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

module type Lattice_indexed_bound = sig
  module Index : sig
    type t
  end

  type t

  val min : t

  val max : Index.t -> t

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

  type 'd t

  (** Left-only mode *)
  type l = (allowed * disallowed) t

  (** Right-only mode *)
  type r = (disallowed * allowed) t

  (** Left-right mode *)
  type lr = (allowed * allowed) t

  val min : lr

  val max : lr

  val legacy : lr

  val disallow_right : ('l * 'r) t -> ('l * disallowed) t

  val disallow_left : ('l * 'r) t -> (disallowed * 'r) t

  val allow_right : ('l * allowed) t -> ('l * 'r) t

  val allow_left : (allowed * 'r) t -> ('l * 'r) t

  val newvar : unit -> ('l * 'r) t

  val submode : (allowed * 'r) t -> ('l * allowed) t -> (unit, error) result

  val equate : lr -> lr -> (unit, equate_error) result

  val submode_exn : (allowed * 'r) t -> ('l * allowed) t -> unit

  val equate_exn : lr -> lr -> unit

  val join : (allowed * 'r) t list -> left_only t

  val meet : ('l * allowed) t list -> right_only t

  val newvar_above : (allowed * 'r) t -> ('l * 'r_) t * bool

  val newvar_below : ('l * allowed) t -> ('l_ * 'r) t * bool

  val print :
    ?raw:bool ->
    ?verbose:bool ->
    unit ->
    Format.formatter ->
    ('l * 'r) t ->
    unit

  val zap_to_floor : (allowed * 'r) t -> Const.t

  val zap_to_ceil : ('l * allowed) t -> Const.t

  val check_const : ('l * 'r) t -> Const.t option

  val of_const : Const.t -> ('l * 'r) t
end

module type Common_indexed_bound = sig
  module Index : sig
    type t
  end

  module Const : Lattice_indexed_bound with module Index := Index

  type error

  type equate_error = equate_step * error

  type 'd t

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  val min : Index.t -> ('l * 'r) t

  val max : Index.t -> ('l * 'r) t

  val disallow_right : ('l * 'r) t -> ('l * disallowed) t

  val disallow_left : ('l * 'r) t -> (disallowed * 'r) t

  val allow_right : ('l * allowed) t -> ('l * 'r) t

  val allow_left : (allowed * 'r) t -> ('l * 'r) t

  val newvar : Index.t -> ('l * 'r) t

  val submode :
    Index.t -> (allowed * 'r) t -> ('l * allowed) t -> (unit, error) result

  val equate : Index.t -> lr -> lr -> (unit, equate_error) result

  val submode_exn : Index.t -> (allowed * 'r) t -> ('l * allowed) t -> unit

  val equate_exn : Index.t -> lr -> lr -> unit

  val join : Index.t -> (allowed * 'r) t list -> left_only t

  val meet : Index.t -> ('l * allowed) t list -> right_only t

  val newvar_above : Index.t -> (allowed * 'r) t -> ('l * 'r_) t * bool

  val newvar_below : Index.t -> ('l * allowed) t -> ('l_ * 'r) t * bool

  val print :
    ?raw:bool ->
    ?verbose:bool ->
    Index.t ->
    Format.formatter ->
    ('l * 'r) t ->
    unit

  val zap_to_floor : Index.t -> (allowed * 'r) t -> Const.t

  val zap_to_ceil : Index.t -> ('l * allowed) t -> Const.t

  val check_const : Index.t -> ('l * 'r) t -> Const.t option

  val of_const : Const.t -> ('l * 'r) t
end

module type S = sig
  type changes

  val undo_changes : changes -> unit

  val append_changes : (changes ref -> unit) ref

  type nonrec allowed = allowed

  type nonrec disallowed = disallowed

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

      val legacy : t
    end

    type error = Const.t Solver.error

    include Common with module Const := Const and type error := error

    val global : lr

    val local : lr

    val zap_to_legacy : (allowed * 'r) t -> Const.t
  end

  module Regionality : sig
    module Index : sig
      type t

      val min : t

      val max : t

      val le : t -> t -> bool

      val lt : t -> t -> bool

      val succ : t -> t

      (* [pred] is not provided on purpose as it would be unsafe. *)

      val print : Format.formatter -> t -> unit
    end

    module Const : sig
      type t

      include Lattice_indexed_bound with type t := t and module Index = Index

      val global : t

      val local : Index.t -> t

      (** returns the mode needed to escape certain region *)
      val escape : Index.t -> t
    end

    type error = Const.t Solver.error

    include
      Common_indexed_bound
        with module Index := Index
         and module Const := Const
         and type error := error

    val global : lr

    val local : Index.t -> ('l * 'r) t

    val escape : Index.t -> ('l * 'r) t

    val zap_to_legacy : Index.t -> (allowed * 'r) t -> Const.t

    val inj : src:Index.t -> dst:Index.t -> ('l * 'r) t -> ('l * 'r) t

    val inj_l : src:Index.t -> dst:Index.t -> ('l * 'r) t -> ('l * disallowed) t
  end

  module Linearity : sig
    module Const : sig
      type t =
        | Many
        | Once

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include Common with module Const := Const and type error := error

    val many : lr

    val once : lr

    val zap_to_legacy : (allowed * 'r) t -> Const.t
  end

  module Uniqueness : sig
    module Const : sig
      type t =
        | Unique
        | Shared

      include Lattice with type t := t
    end

    type error = Const.t Solver.error

    include Common with module Const := Const and type error := error

    val shared : lr

    val unique : lr

    val zap_to_legacy : ('l * allowed) t -> Const.t
  end

  module Value : sig
    module Monadic : sig
      include Common with type error = [`Uniqueness of Uniqueness.error]

      val check_const : ('l * 'r) t -> Uniqueness.Const.t option
    end

    module Comonadic : sig
      include
        Common_indexed_bound
          with type error =
            [ `Regionality of Regionality.error
            | `Linearity of Linearity.error ]
          with module Index := Regionality.Index

      val check_const :
        Regionality.Index.t ->
        ('l * 'r) t ->
        Regionality.Const.t option * Linearity.Const.t option

      val linearity :
        Regionality.Index.t -> ('l * 'r) t -> ('l * 'r) Linearity.t
    end

    module Const : sig
      include
        Lattice_indexed_bound
          with type t =
            Regionality.Const.t * Linearity.Const.t * Uniqueness.Const.t
           and module Index := Regionality.Index

      val legacy : t
    end

    type error =
      [ `Regionality of Regionality.error
      | `Uniqueness of Uniqueness.error
      | `Linearity of Linearity.error ]

    type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

    include
      Common_indexed_bound
        with module Index := Regionality.Index
         and module Const := Const
         and type error := error
         and type 'd t := 'd t

    val legacy : lr

    (* some overriding *)
    val print :
      ?raw:bool ->
      ?verbose:bool ->
      Regionality.Index.t ->
      Format.formatter ->
      ('l * 'r) t ->
      unit

    val check_const :
      Regionality.Index.t ->
      ('l * 'r) t ->
      Regionality.Const.t option
      * Linearity.Const.t option
      * Uniqueness.Const.t option

    val regionality :
      Regionality.Index.t -> ('l * 'r) t -> ('l * 'r) Regionality.t

    val inj :
      src:Regionality.Index.t ->
      dst:Regionality.Index.t ->
      ('l * 'r) t ->
      ('l * 'r) t

    val inj_l :
      src:Regionality.Index.t ->
      dst:Regionality.Index.t ->
      ('l * 'r) t ->
      ('l * disallowed) t

    val cap_r :
      src:Regionality.Index.t ->
      dst:Regionality.Index.t ->
      ('l * 'r) t ->
      (disallowed * 'r) t

    val cap :
      src:Regionality.Index.t ->
      dst:Regionality.Index.t ->
      ('l * 'r) t ->
      ('l * 'r) t

    val uniqueness : ('l * 'r) t -> ('l * 'r) Uniqueness.t

    val linearity : Regionality.Index.t -> ('l * 'r) t -> ('l * 'r) Linearity.t

    val max_with_uniqueness :
      Regionality.Index.t -> ('l * 'r) Uniqueness.t -> (disallowed * 'r) t

    val min_with_uniqueness :
      Regionality.Index.t -> ('l * 'r) Uniqueness.t -> ('l * disallowed) t

    val min_with_regionality :
      Regionality.Index.t -> ('l * 'r) Regionality.t -> ('l * disallowed) t

    val max_with_regionality :
      Regionality.Index.t -> ('l * 'r) Regionality.t -> (disallowed * 'r) t

    val min_with_linearity :
      Regionality.Index.t -> ('l * 'r) Linearity.t -> ('l * disallowed) t

    val max_with_linearity :
      Regionality.Index.t -> ('l * 'r) Linearity.t -> (disallowed * 'r) t

    val set_regionality_min :
      Regionality.Index.t -> ('l * 'r) t -> ('l * disallowed) t

    val set_regionality_max :
      Regionality.Index.t -> ('l * 'r) t -> (disallowed * 'r) t

    val set_linearity_min :
      Regionality.Index.t -> ('l * 'r) t -> ('l * disallowed) t

    val set_linearity_max :
      Regionality.Index.t -> ('l * 'r) t -> (disallowed * 'r) t

    val set_uniqueness_min : ('l * 'r) t -> ('l * disallowed) t

    val set_uniqueness_max : ('l * 'r) t -> (disallowed * 'r) t

    val zap_to_legacy : Regionality.Index.t -> lr -> Const.t
  end

  module Alloc : sig
    module Monadic : sig
      include Common with type error = [`Uniqueness of Uniqueness.error]

      val check_const : ('l * 'r) t -> Uniqueness.Const.t option
    end

    module Comonadic : sig
      include
        Common
          with type error =
            [ `Locality of Locality.error
            | `Linearity of Linearity.error ]

      val check_const :
        ('l * 'r) t -> Locality.Const.t option * Linearity.Const.t option
    end

    module Const : sig
      include
        Lattice
          with type t =
            Locality.Const.t * Linearity.Const.t * Uniqueness.Const.t

      (** Returns the lower bound needed for a closure in relation to argument *)
      val close_over : t -> t

      (** Returns the lower bound needed for a closure in relation to the outer function *)
      val partial_apply : t -> t
    end

    type error =
      [ `Locality of Locality.error
      | `Uniqueness of Uniqueness.error
      | `Linearity of Linearity.error ]

    type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

    include
      Common
        with module Const := Const
         and type error := error
         and type 'd t := 'd t

    (* some overriding *)
    val print :
      ?raw:bool ->
      ?verbose:bool ->
      unit ->
      Format.formatter ->
      ('l * 'r) t ->
      unit

    val check_const :
      ('l * 'r) t ->
      Locality.Const.t option
      * Linearity.Const.t option
      * Uniqueness.Const.t option

    val locality : ('l * 'r) t -> ('l * 'r) Locality.t

    val uniqueness : ('l * 'r) t -> ('l * 'r) Uniqueness.t

    val linearity : ('l * 'r) t -> ('l * 'r) Linearity.t

    val max_with_uniqueness : ('l * 'r) Uniqueness.t -> (disallowed * 'r) t

    val min_with_uniqueness : ('l * 'r) Uniqueness.t -> ('l * disallowed) t

    val min_with_locality : ('l * 'r) Locality.t -> ('l * disallowed) t

    val max_with_locality : ('l * 'r) Locality.t -> (disallowed * 'r) t

    val min_with_linearity : ('l * 'r) Linearity.t -> ('l * disallowed) t

    val max_with_linearity : ('l * 'r) Linearity.t -> (disallowed * 'r) t

    val set_locality_min : ('l * 'r) t -> ('l * disallowed) t

    val set_locality_max : ('l * 'r) t -> (disallowed * 'r) t

    val set_linearity_min : ('l * 'r) t -> ('l * disallowed) t

    val set_linearity_max : ('l * 'r) t -> (disallowed * 'r) t

    val set_uniqueness_min : ('l * 'r) t -> ('l * disallowed) t

    val set_uniqueness_max : ('l * 'r) t -> (disallowed * 'r) t

    val zap_to_legacy : lr -> Const.t

    (** Returns the lower bound needed for a closure in relation to argument *)
    val close_over : (allowed * 'r) Comonadic.t -> ('l * allowed) Monadic.t -> l

    (** Returns the lower bound needed for a closure in relation to the outer function *)
    val partial_apply : (allowed * 'r) t -> l
  end

  (** Returns the linearity dual to the given uniqueness *)
  val unique_to_linear : ('l * 'r) Uniqueness.t -> ('r * 'l) Linearity.t

  (** Returns the uniqueness dual to the given linearity *)
  val linear_to_unique : ('l * 'r) Linearity.t -> ('r * 'l) Uniqueness.t

  (** Converts regional to local, identity otherwise *)
  val regional_to_local :
    Regionality.Index.t -> ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Inject locality into regionality *)
  val locality_as_regionality :
    Regionality.Index.t -> ('l * 'r) Locality.t -> ('l * 'r) Regionality.t

  (** Converts regional to global, identity otherwise *)
  val regional_to_global :
    Regionality.Index.t -> ('l * 'r) Regionality.t -> ('l * 'r) Locality.t

  (** Similar to [locality_as_regionality], behaves as identity on other axes *)
  val alloc_as_value :
    Regionality.Index.t -> ('l * 'r) Alloc.t -> ('l * 'r) Value.t

  (** Similar to [local_to_regional], behaves as identity in other axes *)
  val alloc_to_value_l2r :
    Regionality.Index.t -> ('l * 'r) Alloc.t -> ('l * disallowed) Value.t

  (** Similar to [regional_to_local], behaves as identity on other axes *)
  val value_to_alloc_r2l :
    Regionality.Index.t -> ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  (** Similar to [regional_to_global], behaves as identity on other axes *)
  val value_to_alloc_r2g :
    Regionality.Index.t -> ('l * 'r) Value.t -> ('l * 'r) Alloc.t

  module Const : sig
    (** Returns the linearity dual to the given uniqueness *)
    val unique_to_linear : Uniqueness.Const.t -> Linearity.Const.t
  end
end
