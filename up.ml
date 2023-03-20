external make_unboxed_pair_v_v : 'a -> 'b -> ('a, 'b) unboxed_pair = "%make_unboxed_pair_v_v"
external unboxed_pair_field_0_v_v : ('a, 'b) unboxed_pair -> 'a = "%unboxed_pair_field_0_v_v"
external unboxed_pair_field_1_v_v : ('a, 'b) unboxed_pair -> 'b = "%unboxed_pair_field_1_v_v"

let f i x y =
  let p = make_unboxed_pair_v_v x y in
  if i < 0 then unboxed_pair_field_0_v_v p
  else unboxed_pair_field_1_v_v p

external make_unboxed_pair_vup_vup :
  ('a, 'b) unboxed_pair -> ('c, 'd) unboxed_pair
  -> (('a, 'b) unboxed_pair, ('c, 'd) unboxed_pair) unboxed_pair
  = "%make_unboxed_pair_vup_vup"

external unboxed_pair_field_0_vup_vup :
  (('a, 'b) unboxed_pair, _) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_0_vup_vup"

let g i x y =
  let p = make_unboxed_pair_v_v x y in
  let q = make_unboxed_pair_vup_vup p p in
  let p_again = unboxed_pair_field_0_vup_vup q in
  if i < 0 then unboxed_pair_field_0_v_v p_again
  else unboxed_pair_field_1_v_v p_again

external unboxed_pair_field_1_vup_vup :
  (_, ('a, 'b) unboxed_pair) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_1_vup_vup"

let h i x y =
  let p = make_unboxed_pair_v_v x y in
  let p' = make_unboxed_pair_v_v y x in
  let q = make_unboxed_pair_vup_vup p p' in
  let r =
    if i < 0 then unboxed_pair_field_0_vup_vup q
    else unboxed_pair_field_1_vup_vup q
  in
  if i < 0 then unboxed_pair_field_0_v_v r
  else unboxed_pair_field_1_v_v r

external make_unboxed_pair_i_i : 'a -> 'b -> ('a, 'b) unboxed_pair = "%make_unboxed_pair_i_i"
external unboxed_pair_field_0_i_i : ('a, 'b) unboxed_pair -> 'a = "%unboxed_pair_field_0_i_i"
external unboxed_pair_field_1_i_i : ('a, 'b) unboxed_pair -> 'b = "%unboxed_pair_field_1_i_i"

let[@inline never] takes_unboxed_pair (p : (int, int) unboxed_pair) =
  let p0 = unboxed_pair_field_0_i_i p in
  let p1 = unboxed_pair_field_1_i_i p in
  p0 + p1

let caller x y =
  let p = make_unboxed_pair_i_i x y in
  takes_unboxed_pair p

let[@inline never] takes_two_unboxed_pairs
      (p : (int, int) unboxed_pair)
      (q : (int, int) unboxed_pair) =
  let p0 = unboxed_pair_field_0_i_i p in
  let p1 = unboxed_pair_field_1_i_i p in
  let q0 = unboxed_pair_field_0_i_i q in
  let q1 = unboxed_pair_field_1_i_i q in
  p0 + 10*p1 + 100*q0 + 1000*q1

let caller2 x y =
  let p = make_unboxed_pair_i_i x y in
  let q = make_unboxed_pair_i_i y x in
  takes_two_unboxed_pairs p q

let make_partial x y =
  let p = make_unboxed_pair_i_i x y in
  let partial = takes_two_unboxed_pairs p in
  let q = make_unboxed_pair_i_i y x in
  (Sys.opaque_identity partial) q

let () =
  Printf.printf "%d\n%!" (make_partial 1 2)

let[@inline never] takes_three_unboxed_pairs
      (p : (int, int) unboxed_pair)
      (q : (int, int) unboxed_pair)
      (r : (int, int) unboxed_pair) =
  let p0 = unboxed_pair_field_0_i_i p in
  let p1 = unboxed_pair_field_1_i_i p in
  let q0 = unboxed_pair_field_0_i_i q in
  let q1 = unboxed_pair_field_1_i_i q in
  let r0 = unboxed_pair_field_0_i_i r in
  let r1 = unboxed_pair_field_1_i_i r in
  p0 + 10*p1 + 100*q0 + 1000*q1 + 10000*r0 + 100000*r1

let make_partial3 a b c d e f =
  let p = make_unboxed_pair_i_i a b in
  let q = make_unboxed_pair_i_i c d in
  let r = make_unboxed_pair_i_i e f in
  let partial1 = takes_three_unboxed_pairs p in
  let partial2 = (Sys.opaque_identity partial1) q in
  (Sys.opaque_identity partial2) r

let () =
  Printf.printf "%d\n%!" (make_partial3 1 2 3 4 5 6)

let[@inline never] returns_unboxed_pair_not_inlined x =
  if x < 0 then make_unboxed_pair_i_i x x
  else make_unboxed_pair_i_i (x + 1) (x + 2)

let[@inline] returns_unboxed_pair_inlined x =
  if x < 0 then make_unboxed_pair_i_i x x
  else make_unboxed_pair_i_i (x + 1) (x + 2)

let call_function_returning_unboxed_pair x =
  let p1 = returns_unboxed_pair_not_inlined x in
  let p2 = returns_unboxed_pair_inlined x in
  let p1_0 = unboxed_pair_field_0_i_i p1 in
  let p1_1 = unboxed_pair_field_1_i_i p1 in
  let p2_0 = unboxed_pair_field_0_i_i p2 in
  let p2_1 = unboxed_pair_field_1_i_i p2 in
  p1_0 + p1_1 + p2_0 + p2_1

let () =
  Printf.printf "%d\n%!" (call_function_returning_unboxed_pair 42)

external void : unit -> void = "%void"

let void_const (v : void) = 42

let void0 (v : void) x = x

let void1a (v : void) x y = x
let void1b (v : void) x y = y

let void2a x (v : void) y = x
let void2b x (v : void) y = y

let void3a x y (v : void) = x
let void3b x y (v : void) = y

let apply_void1a_no_wrapper x y =
  let p = (Sys.opaque_identity void1a) (void ()) in
  p x y

let () =
  Printf.printf "%d (expected 1)\n%!" (apply_void1a_no_wrapper 1 2)

let[@inline never] two_voids_const (v : void) (v : void) = 42
let[@inline never] two_voids_const_side1 (v : void) =
  Printf.printf "foo\n%!"; fun (v : void) -> 42

let[@inline never] two_voids_const_side2 (v : void) (v : void) =
  Printf.printf "bar\n%!";
  42

(* With partial applications concealed from flambda *)
let () =
  let p = (Sys.opaque_identity two_voids_const) (void ()) in
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = (Sys.opaque_identity two_voids_const_side1) (void ()) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = (Sys.opaque_identity two_voids_const_side2) (void ()) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()))

(* With partial applications visible to flambda (From_lambda) *)
let () =
  let p = Sys.opaque_identity (two_voids_const (void ())) in
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = Sys.opaque_identity (two_voids_const_side1 (void ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = Sys.opaque_identity (two_voids_const_side2 (void ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()))

(* With partial applications visible to flambda (Simplify) *)
let[@inline] to_inline two_voids_const two_voids_const_side1
      two_voids_const_side2 =
  let p = Sys.opaque_identity (two_voids_const (void ())) in
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = Sys.opaque_identity (two_voids_const_side1 (void ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()));
  let p = Sys.opaque_identity (two_voids_const_side2 (void ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void ()))

let () =
  to_inline two_voids_const two_voids_const_side1 two_voids_const_side2

(* Overapplications concealed from flambda *)
let () =
  Printf.printf "OVERAPP1\n%!";
  let x = (Sys.opaque_identity two_voids_const) (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = (Sys.opaque_identity two_voids_const_side1) (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = (Sys.opaque_identity two_voids_const_side2) (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x

(* Overapplications visible to flambda *)
let () =
  Printf.printf "OVERAPP2\n%!";
  let x = two_voids_const (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = two_voids_const_side1 (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = two_voids_const_side2 (void ()) (void ()) in
  Printf.printf "%d (expected 42)\n%!" x

(* Overapplications visible to simplify only *)
let[@inline always] g (f : void -> void -> int) : int = f (void ()) (void ())
let () =
  Printf.printf "OVERAPP3\n%!";
  let x = g two_voids_const in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = g two_voids_const_side1 in
  Printf.printf "%d (expected 42)\n%!" x;
  let x = g two_voids_const_side2 in
  Printf.printf "%d (expected 42)\n%!" x

(* From above: with partial applications visible to flambda (From_lambda),
   but using void arguments extracted from unboxed products *)
external make_unboxed_pair_o_o : void -> void -> (void, void) unboxed_pair =
  "%make_unboxed_pair_o_o"
external unboxed_pair_field_0_o_o : (void, void) unboxed_pair -> void =
  "%unboxed_pair_field_0_o_o"
external unboxed_pair_field_1_o_o : (void, void) unboxed_pair -> void =
  "%unboxed_pair_field_1_o_o"

let[@inline never] void_from_product () =
  let p = make_unboxed_pair_o_o (void ()) (void ()) in
  unboxed_pair_field_0_o_o p

let[@inline] void_from_product_inlined () =
  let p = make_unboxed_pair_o_o (void ()) (void ()) in
  unboxed_pair_field_0_o_o p

let () =
  let p = Sys.opaque_identity (two_voids_const (void_from_product ())) in
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product ()));
  let p = Sys.opaque_identity (two_voids_const_side1 (void_from_product ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product ()));
  let p = Sys.opaque_identity (two_voids_const_side2 (void_from_product ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product ()))

let () =
  let p = Sys.opaque_identity (two_voids_const (void_from_product_inlined ())) in
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product_inlined ()));
  let p = Sys.opaque_identity (two_voids_const_side1 (void_from_product_inlined ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product_inlined ()));
  let p = Sys.opaque_identity (two_voids_const_side2 (void_from_product_inlined ())) in
  Printf.printf "after definition\n%!";
  Printf.printf "%d (expected 42)\n%!" (p (void_from_product_inlined ()))

(* version without printing to declutter Cmm *)
let[@inline never] returns_unboxed_pair_of_voids_not_inlined0 x =
  make_unboxed_pair_o_o (void ()) (void ())

let[@inline never] returns_unboxed_pair_of_voids_not_inlined x =
  if x < 0 then make_unboxed_pair_o_o (void ()) (void ())
  else (
    Printf.printf "foo\n%!";
    make_unboxed_pair_o_o (void ()) (void ())
  )

let[@inline] returns_unboxed_pair_of_voids_inlined x =
  if x < 0 then make_unboxed_pair_o_o (void ()) (void ())
  else (
    Printf.printf "foo2\n%!";
    make_unboxed_pair_o_o (void ()) (void ())
  )

let print_foo (_ : void) = Printf.printf "FOO\n%!"

let[@inline never] call_functions_returning_unboxed_pair_of_voids x =
  let p1 = returns_unboxed_pair_of_voids_not_inlined x in
  let p2 = returns_unboxed_pair_of_voids_inlined x in
  print_foo (unboxed_pair_field_0_o_o p1);
  print_foo (unboxed_pair_field_1_o_o p1);
  print_foo (unboxed_pair_field_0_o_o p2);
  print_foo (unboxed_pair_field_1_o_o p2)

let () = call_functions_returning_unboxed_pair_of_voids 100
let () = call_functions_returning_unboxed_pair_of_voids (-100)

external make_unboxed_triple_o_i_o
  : void -> int -> void -> (void, int, void) unboxed_triple =
  "%make_unboxed_triple_o_i_o"

let[@inline never] returns_unboxed_triple_of_void_int_void_not_inlined x =
  make_unboxed_triple_o_i_o (void ()) x (void ())
