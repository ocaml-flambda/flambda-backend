
module Integer : sig
  type t = Lambda.integer_comparison =
    | Ceq | Cne | Clt | Cgt | Cle | Cge

  val negate : t -> t
  val swap : t -> t

  module With_signedness : sig
    type nonrec t =
        Isigned of t
      | Iunsigned of t

    val invert : t -> t
  end
end

module Float : sig
  (* With floats [not (x < y)] is not the same as [x >= y] due to NaNs,
     so we provide additional comparisons to represent the negations.*)
  type t = Lambda.float_comparison =
    | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

  val negate : t -> t
  val swap : t -> t
end

module Test : sig
  type t =
      Itruetest
    | Ifalsetest
    | Iinttest of Integer.With_signedness.t
    | Iinttest_imm of Integer.With_signedness.t * int
    | Ifloattest of Float.t
    | Ioddtest
    | Ieventest

  val invert : t -> t
end
