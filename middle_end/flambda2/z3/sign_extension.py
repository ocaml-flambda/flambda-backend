#!/usr/bin/env python3
from z3 import *

ARCH_BITS = 64


class Op:
    def __init__(self, name):
        self.inner = BitVec(f"{name}.inner", ARCH_BITS)
        self.shift_right = BitVec(f"{name}.shift_right", ARCH_BITS)
        self.arith = Bool(f"{name}.arith")

    def as_ast(self) -> BitVecRef:
        return If(
            self.arith,
            self.inner >> self.shift_right,
            LShR(self.inner, self.shift_right),
        )

    def __repr__(self):
        return repr(self.as_ast())

    def size(self):
        return self.as_ast().size()

    def reference_sign_extend(self, bits):
        unused_bits = self.size() - bits
        return (self.as_ast() << unused_bits) >> unused_bits

    def experimental_sign_extend(self, bits) -> BitVecRef:
        unused_bits = self.size() - bits

        return If(
            self.shift_right == unused_bits,
            If(self.arith, self.as_ast(), self.inner >> unused_bits),
            If(
                self.shift_right > unused_bits,
                self.as_ast(),
                (self.inner << (unused_bits - self.shift_right)) >> unused_bits,
            ),
        )


if __name__ == "__main__":
    s = Solver()

    x = Op("x")
    bits = BitVec("bits", ARCH_BITS)  # Number of low bits to preserve

    # assumptions
    s.add(And(0 <= x.shift_right, x.shift_right < x.size()))
    s.add(And(0 < bits, bits <= x.size()))

    # sanity check that we haven't introduced something crazy
    assert s.check() == sat

    # falsify this
    s.add(x.reference_sign_extend(bits) != x.experimental_sign_extend(bits))

    print(s.to_smt2())

    print("Verifying sign_extend...")
    if s.check() == unsat:
        print("sign_extend optimization is correct.")
    else:
        print("sign_extend is incorrect.")
        model = s.model()
        print("Counterexample:", model)
        exit(1)


# ; benchmark generated from python API
# (set-info :status unknown)
# (declare-fun x.shift_right () (_ BitVec 64))
# (declare-fun bits () (_ BitVec 64))
# (declare-fun x.inner () (_ BitVec 64))
# (declare-fun x.arith () Bool)
# (assert
#  (and (bvsge x.shift_right (_ bv0 64)) (bvslt x.shift_right (_ bv64 64))))
# (assert
#  (let (($x36 (bvsle bits (_ bv64 64))))
#  (and (bvsgt bits (_ bv0 64)) $x36)))
# (assert
#  (let ((?x51 (bvsub (_ bv64 64) bits)))
#  (let ((?x47 (ite x.arith (bvashr x.inner x.shift_right) (bvlshr x.inner x.shift_right))))
#  (let ((?x66 (ite (bvsgt x.shift_right ?x51) ?x47 (bvashr (bvshl x.inner (bvsub ?x51 x.shift_right)) ?x51))))
#  (and (distinct (bvashr (bvshl ?x47 ?x51) ?x51) ?x66) true)))))
# (check-sat)
