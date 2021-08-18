
module Integer = struct
  type t = Lambda.integer_comparison =
    | Ceq | Cne | Clt | Cgt | Cle | Cge

  let negate = Lambda.negate_integer_comparison

  let swap = Lambda.swap_integer_comparison

  module With_signedness = struct
    type nonrec t =
        Isigned of t
      | Iunsigned of t

    let invert = function
        Isigned cmp -> Isigned(negate cmp)
      | Iunsigned cmp -> Iunsigned(negate cmp)

  end
end

module Float = struct
  type t = Lambda.float_comparison =
    | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

  let negate = Lambda.negate_float_comparison
  let swap = Lambda.swap_float_comparison
end

module Test = struct
  type t =
      Itruetest
    | Ifalsetest
    | Iinttest of Integer.With_signedness.t
    | Iinttest_imm of Integer.With_signedness.t * int
    | Ifloattest of Float.t
    | Ioddtest
    | Ieventest

  let invert = function
      Itruetest -> Ifalsetest
    | Ifalsetest -> Itruetest
    | Iinttest(cmp) -> Iinttest(Integer.With_signedness.invert cmp)
    | Iinttest_imm(cmp, n) -> Iinttest_imm(Integer.With_signedness.invert cmp, n)
    | Ifloattest(cmp) -> Ifloattest(Float.negate cmp)
    | Ieventest -> Ioddtest
    | Ioddtest -> Ieventest
end
