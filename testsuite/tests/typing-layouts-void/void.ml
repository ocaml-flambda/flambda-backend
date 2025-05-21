(* TEST
 reference = "${test_source_directory}/void.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe no_extensions";
   compiler_reference = "${test_source_directory}/void_disabled.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 } {
   flags = "-extension layouts_alpha";
   native;
 } {
   flags = "-extension layouts_alpha -Oclassic";
   native;
 } {
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }
*)

type void : void
external void : unit -> void = "%unbox_unit"

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

let fst_u #((x : void), (_ : void)) = x
let snd_u #((_ : void), (x : void)) = x

let[@inline never] void_from_product () =
  let p = #(void (), void ()) in
  fst_u p

let[@inline] void_from_product_inlined () =
  let p = #(void (), void ()) in
  fst_u p

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
  #(void (), void ())

let[@inline never] returns_unboxed_pair_of_voids_not_inlined x =
  if x < 0 then #(void (), void ())
  else (
    Printf.printf "foo\n%!";
    #(void (), void ())
  )

let[@inline] returns_unboxed_pair_of_voids_inlined x =
  if x < 0 then #(void (), void ())
  else (
    Printf.printf "foo2\n%!";
    #(void (), void ())
  )

let print_foo (_ : void) = Printf.printf "FOO\n%!"

let[@inline never] call_functions_returning_unboxed_pair_of_voids x =
  let p1 = returns_unboxed_pair_of_voids_not_inlined x in
  let p2 = returns_unboxed_pair_of_voids_inlined x in
  print_foo (fst_u p1);
  print_foo (snd_u p1);
  print_foo (fst_u p2);
  print_foo (snd_u p2)

let () = call_functions_returning_unboxed_pair_of_voids 100
let () = call_functions_returning_unboxed_pair_of_voids (-100)

let[@inline never] returns_unboxed_triple_of_void_int_void_not_inlined x =
  #(void (), x, void ())
