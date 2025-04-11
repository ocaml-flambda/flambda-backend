module Bool = struct

  external select
    :  bool
    -> ('a[@local_opt])
    -> ('a[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "caml_csel_value"
  [@@kind k = value] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

  external select_u64
    :  bool
    -> (int64#[@unboxed])
    -> (int64#[@unboxed])
    -> (int64#[@unboxed])
    @@ portable
    = "caml_csel_value" "caml_csel_int64_unboxed"
  [@@kind k = bits64] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

end

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) = "%box_int64" [@@warning "-187"]
  let[@inline always] equal x y = Int64.equal (to_int64 x) (to_int64 y)
end

let[@inline never][@local never] test_ite x = if x then 1 else 0
let[@inline never][@local never]  test_sel x = Bool.select x 1 0

let[@inline never][@local never]  test_ite_cond (x:int) x' = if x = x' then 1 else 0
let[@inline never][@local never]  test_sel_cond (x:int) x' = Bool.select (x = x') 1 0

let[@inline never][@local never]  test_ite_not x = if x then 0 else 1
let[@inline never][@local never]  test_sel_not x = Bool.select x 0 1

let[@inline never][@local never]  test_ite_eq x y = if x then y else y
let[@inline never][@local never]  test_sel_eq x y = Bool.select x y y

let[@inline never][@local never]  test_ite_eq1 x y z = if x then (y+z) else (y+z)
let[@inline never][@local never]  test_sel_eq1 x y z = Bool.select x (y+z) (y+z)

let[@inline never][@local never]  test_ite_eq2 x y z = if x then (z+y) else (y+z)
let[@inline never][@local never]  test_sel_eq2 x y z = Bool.select x (z+y) (y+z)

let[@inline never][@local never]  test_ite_u64 x = if x then #1L else #0L
let[@inline never][@local never]  test_sel_u64 x = Bool.select_u64 x #1L #0L

let[@inline never][@local never]  test_ite_not_u64 x = if x then #0L else #1L
let[@inline never][@local never]  test_sel_not_u64 x = Bool.select_u64 x #0L #1L

let[@inline never][@local never]  test_ite_eq_u64 x y = if x then y else y
let[@inline never][@local never]  test_sel_eq_u64 x y = Bool.select_u64 x y y


let () =
  assert ((test_ite true) = (Sys.opaque_identity 1));
  assert ((test_ite false) = (Sys.opaque_identity 0));
  assert ((test_sel true) = (Sys.opaque_identity 1));
  assert ((test_sel false) = (Sys.opaque_identity 0));

  assert ((test_ite_cond 42 42) = (Sys.opaque_identity 1));
  assert ((test_ite_cond 42 444) = (Sys.opaque_identity 0));
  assert ((test_sel_cond 42 42) = (Sys.opaque_identity 1));
  assert ((test_sel_cond 42 444) = (Sys.opaque_identity 0));

  assert ((test_ite_not true) = (Sys.opaque_identity 0));
  assert ((test_ite_not false) = (Sys.opaque_identity 1));
  assert ((test_sel_not true) = (Sys.opaque_identity 0));
  assert ((test_sel_not false) = (Sys.opaque_identity 1));

  assert ((test_ite_eq true 42 ) = (Sys.opaque_identity (42)));
  assert ((test_ite_eq false 42 ) = (Sys.opaque_identity (42)));
  assert ((test_sel_eq true 42 ) = (Sys.opaque_identity (42)));
  assert ((test_sel_eq false 42) = (Sys.opaque_identity (42)));

  assert ((test_ite_eq1 true 42 42) = (Sys.opaque_identity (42+42)));
  assert ((test_ite_eq1 false 42 444) = (Sys.opaque_identity (42+444)));
  assert ((test_sel_eq1 true 42 42) = (Sys.opaque_identity (42+42)));
  assert ((test_sel_eq1 false 42 444) = (Sys.opaque_identity (42+444)));

  assert ((test_ite_eq2 true 42 42) = (Sys.opaque_identity (42+42)));
  assert ((test_ite_eq2 false 42 444) = (Sys.opaque_identity (42+444)));
  assert ((test_sel_eq2 true 42 42) = (Sys.opaque_identity (42+42)));
  assert ((test_sel_eq2 false 42 444) = (Sys.opaque_identity (42+444)));

  assert (Int64_u.equal (test_ite_u64 true) (Sys.opaque_identity #1L));
  assert (Int64_u.equal (test_ite_u64 false) (Sys.opaque_identity #0L));
  assert (Int64_u.equal (test_sel_u64 true) (Sys.opaque_identity #1L));
  assert (Int64_u.equal (test_sel_u64 false) (Sys.opaque_identity #0L));

  assert (Int64_u.equal (test_ite_not_u64 true) (Sys.opaque_identity #0L));
  assert (Int64_u.equal (test_ite_not_u64 false) (Sys.opaque_identity #1L));
  assert (Int64_u.equal (test_sel_not_u64 true) (Sys.opaque_identity #0L));
  assert (Int64_u.equal (test_sel_not_u64 false) (Sys.opaque_identity #1L));

  let v = #42L in
  assert (Int64_u.equal (test_ite_eq_u64 true v) (Sys.opaque_identity v));
  assert (Int64_u.equal (test_ite_eq_u64 false v) (Sys.opaque_identity v));
  assert (Int64_u.equal (test_sel_eq_u64 true v) (Sys.opaque_identity v));
  assert (Int64_u.equal (test_sel_eq_u64 false v) (Sys.opaque_identity v));

  ()
