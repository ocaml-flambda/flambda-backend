type any_locality_mode = Any_locality_mode

module Integer_comparison : sig
  type t =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge

  val to_string : t -> string

  val swap : t -> t

  val negate : t -> t
end

module Float_comparison : sig
  type t =
    | CFeq
    | CFneq
    | CFlt
    | CFnlt
    | CFgt
    | CFngt
    | CFle
    | CFnle
    | CFge
    | CFnge

  val to_string : t -> string

  val swap : t -> t

  val negate : t -> t
end

module Maybe_naked : sig
  type ('a, 'b) t =
    | Value of 'a
    | Naked of 'b
end

module type S := sig
  type 'a width

  type nonrec 'a t = ('a width, any_locality_mode width) Maybe_naked.t

  val all : any_locality_mode t list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val ignore_locality : 'a t -> any_locality_mode t

  val width : any_locality_mode t -> any_locality_mode width

  val to_string : any_locality_mode t -> string

  val sort : any_locality_mode t -> Jkind_types.Sort.Const.t
end

module type Integral_width_constants := sig
  type 'a t

  val int8 : _ t

  val int16 : _ t

  val int32 : any_locality_mode t

  val int64 : any_locality_mode t

  val int : _ t

  val nativeint : any_locality_mode t
end

module type Integral_constants := sig
  type 'a t

  include Integral_width_constants with type 'a t := 'a t

  val naked_int8 : _ t

  val naked_int16 : _ t

  val naked_int32 : any_locality_mode t

  val naked_int64 : any_locality_mode t

  val naked_int : _ t

  val naked_nativeint : any_locality_mode t
end

module type Float_width_constants := sig
  type 'a t

  val float32 : any_locality_mode t

  val float : any_locality_mode t
end

module type Float_constants := sig
  type 'a t

  include Float_width_constants with type 'a t := 'a t

  val naked_float32 : any_locality_mode t

  val naked_float : any_locality_mode t
end

module Integral : sig
  module Taggable : sig
    module Width : sig
      type t =
        | Int8
        | Int16
        | Int

      val to_string : t -> string
    end

    include S with type 'a width := Width.t
  end

  module Boxable : sig
    module Width : sig
      type 'mode t =
        | Int32 of 'mode
        | Nativeint of 'mode
        | Int64 of 'mode

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    include S with type 'a width := 'a Width.t
  end

  module Width : sig
    type 'mode t =
      | Taggable of Taggable.Width.t
      | Boxable of 'mode Boxable.Width.t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    include Integral_width_constants with type 'a t := 'a t
  end

  include S with type 'a width := 'a Width.t

  include Integral_constants with type 'a t := 'a t
end

module Floating : sig
  module Width : sig
    type 'mode t =
      | Float32 of 'mode
      | Float64 of 'mode

    val map : 'a t -> f:('a -> 'b) -> 'b t

    include Float_width_constants with type 'a t := 'a t
  end

  include S with type 'a width := 'a Width.t

  include Float_constants with type 'a t := 'a t
end

module Width : sig
  type 'mode t =
    | Floating of 'mode Floating.Width.t
    | Integral of 'mode Integral.Width.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val ignore_locality : _ t -> any_locality_mode t

  include Integral_width_constants with type 'a t := 'a t

  include Float_width_constants with type 'a t := 'a t
end

include S with type 'a width := 'a Width.t

include Integral_constants with type 'a t := 'a t

include Float_constants with type 'a t := 'a t

val integral : 'a Integral.t -> 'a t

val floating : 'a Floating.t -> 'a t

module Intrinsic : sig
  type 'mode info =
    { can_raise : bool;
      result : 'mode t
    }

  module Unary : sig
    module Int_op : sig
      type t =
        | Neg
        | Succ  (** add 1 *)
        | Pred  (** subtract 1 *)
        | Bswap

      val to_string : t -> string
    end

    module Float_op : sig
      type t =
        | Neg
        | Abs

      val to_string : t -> string
    end

    type nonrec 'mode t =
      | Integral of 'mode Integral.t * Int_op.t
      | Floating of 'mode Floating.t * Float_op.t
      | Static_cast of
          { src : any_locality_mode t;
            dst : 'mode t
          }

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val info : 'a t -> 'a info

    val sort :
      any_locality_mode t -> Jkind_types.Sort.Const.t * Jkind_types.Sort.Const.t
  end

  module Binary : sig
    module Int_op : sig
      type division_is_safe =
        | Safe
        | Unsafe

      type t =
        | Add
        | Sub
        | Mul
        | Div of division_is_safe
        | Mod of division_is_safe
        | And
        | Or
        | Xor

      val to_string : t -> string
    end

    module Shift_op : sig
      module Rhs : sig
        type t = Int
      end

      type t =
        | Lsl
        | Asr
        | Lsr

      val to_string : t -> string
    end

    module Float_op : sig
      type t =
        | Add
        | Sub
        | Mul
        | Div

      val to_string : t -> string
    end

    (* CR jvanburen: add comparisons that return naked values *)

    (** comparisons return a tagged immediate *)
    type nonrec 'mode t =
      | Integral of 'mode Integral.t * Int_op.t
      | Shift of 'mode Integral.t * Shift_op.t * Shift_op.Rhs.t
      | Floating of 'mode Floating.t * Float_op.t
      | Icmp of any_locality_mode Integral.t * Integer_comparison.t
      | Fcmp of any_locality_mode Floating.t * Float_comparison.t
      | Three_way_compare of any_locality_mode t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val info : 'a t -> 'a info

    val sort :
      any_locality_mode t ->
      Jkind_types.Sort.Const.t
      * Jkind_types.Sort.Const.t
      * Jkind_types.Sort.Const.t
  end

  type 'mode t =
    | Unary of 'mode Unary.t
    | Binary of 'mode Binary.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val sort : any_locality_mode t -> Jkind_types.Sort.Const.t list

  val arity : _ t -> int

  val info : 'a t -> 'a info

  val to_string : any_locality_mode t -> string

  module With_percent_prefix : sig
    type nonrec t = any_locality_mode t

    val to_string : t -> string

    val of_string : string -> t
  end
end
