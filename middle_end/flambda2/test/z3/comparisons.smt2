(define-sort ocaml_int  () (_ BitVec 63))
(define-sort tagged_int () (_ BitVec 64))

(define-fun shift ((x ocaml_int)) tagged_int
 (bvshl ((_ zero_extend 1) x) (_ bv1 64)))

(define-fun tag ((x ocaml_int)) tagged_int
 (bvor
  (bvshl ((_ zero_extend 1) x) (_ bv1 64))
  (_ bv1 64)))

(declare-const xc ocaml_int)
(declare-const yc ocaml_int)

(define-const x tagged_int (tag xc))
(define-const y tagged_int (tag yc))

(push)
(echo "comparing OCaml values or their tagged representation is the same")
(assert (not (=
 (bvslt x y)
 (bvslt xc yc))))
(check-sat)
(echo "")
(pop)

(push)
(echo "comparing OCaml values or their shifted representation is the same")
(assert (not (=
 (bvslt xc yc)
 (bvslt (shift xc) (shift yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "xc < yc  iff  x < (y & ~0x1)")
(assert (not (=
 (bvslt xc yc)
 (bvslt x (bvand y (bvnot (_ bv1 64)))))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Signed, Lt -> C.lt ~dbg x (C.ignore_low_bit_int y)")
(assert (not (=
 (bvslt xc yc)
 (bvslt (tag xc) (shift yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Signed, Le -> C.le ~dbg (C.ignore_low_bit_int x) y")
(assert (not (=
 (bvsle xc yc)
 (bvsle (shift xc) (tag yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Signed, Gt -> C.gt ~dbg (C.ignore_low_bit_int x) y")
(assert (not (=
 (bvsgt xc yc)
 (bvsgt (shift xc) (tag yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Signed, Ge -> C.ge ~dbg x (C.ignore_low_bit_int y)")
(assert (not (=
 (bvsge xc yc)
 (bvsge (tag xc) (shift yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Unsigned, Lt -> C.ult ~dbg x (C.ignore_low_bit_int y)")
(assert (not (=
 (bvult xc yc)
 (bvult (tag xc) (shift yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Unsigned, Le -> C.ule ~dbg (C.ignore_low_bit_int x) y")
(assert (not (=
 (bvule xc yc)
 (bvule (shift xc) (tag yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Unsigned, Gt -> C.ugt ~dbg (C.ignore_low_bit_int x) y")
(assert (not (=
 (bvugt xc yc)
 (bvugt (shift xc) (tag yc)))))
(check-sat)
(echo "")
(pop)

(push)
(echo "Unsigned, Ge -> C.uge ~dbg x (C.ignore_low_bit_int y))")
(assert (not (=
 (bvuge xc yc)
 (bvuge (tag xc) (shift yc)))))
(check-sat)
(echo "")
(pop)
