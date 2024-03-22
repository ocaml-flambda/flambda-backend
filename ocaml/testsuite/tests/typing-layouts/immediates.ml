(* TEST
   reference = "${test_source_directory}/immediates.reference"
   * flambda2
   ** native
   ** bytecode
   ** native
     flags = "-extension layouts_alpha"
   ** bytecode
     flags = "-extension layouts_alpha"
   ** native
     flags = "-extension layouts_beta"
   ** bytecode
     flags = "-extension layouts_beta"
   ** setup-ocamlc.byte-build-env
     ocamlc_byte_exit_status = "2"
   *** ocamlc.byte
     flags = "-universe no_extensions"
     compiler_reference = "${test_source_directory}/immediates_disabled.compilers.reference"
   **** check-ocamlc.byte-output


*)

(* This tests some example uses of immediates in both native and bytecode. *)

(*****************************************)
(* Prelude: Some immediate types. *)

type a : immediate = A | B | C

(*********************************)
(* Test 1: higher-order function *)

let[@inline never] test1 (f : ('a : immediate). 'a -> 'a) =
  match f 4 + f 5, f A with
  | x, A -> Printf.printf "Test 1: %d\n" (x + 1)
  | _, (B | C) -> assert false

let () = test1 (fun x -> x)

(****************************************)
(* Test 2: exercising the write barrier *)

type 'a mut =
  { mutable x : 'a }

type ('a : immediate) mut_imm =
  { mutable x_imm : 'a }

type ('a : immediate64) mut_imm64 =
  { mutable x_imm64 : 'a }

let[@inline never] update_with_write_barrier (type a) (m : a mut) (x : a) =
  m.x <- x
;;

let[@inline never] update_1 (type (a : immediate)) (m : a mut) (x : a) =
  m.x <- x
;;

let[@inline never] update_2 (type (a : immediate64)) (m : a mut) (x : a) =
  m.x <- x
;;

let[@inline never] update_imm (type (a : immediate)) (m : a mut_imm) (x : a) =
  m.x_imm <- x
;;

let[@inline never] update_imm64 (type (a : immediate64)) (m : a mut_imm64) (x : a) =
  m.x_imm64 <- x
;;

let[@inline never] test2 x =
  Printf.printf "Test 2: original value: %d\n" x;
  let mut_non_imm = { x = [||] } in
  let mut = { x } in
  let mut_imm = { x_imm = x } in
  let mut_imm64 = { x_imm64 = x } in
  Gc.full_major ();
  (* Exercise the write barrier by making something on the major heap point
     back to the minor heap.
  *)
  update_with_write_barrier mut_non_imm [| x |];
  Printf.printf " 1. mut_non_imm.x.(0): %d\n" mut_non_imm.x.(0);
  update_with_write_barrier mut x;
  Printf.printf " 2. mut.x: %d\n" mut.x;
  update_1 mut x;
  Printf.printf " 3. mut.x: %d\n" mut.x;
  update_2 mut (x+1);
  update_imm mut_imm (x+2);
  update_imm64 mut_imm64 (x+3);
  Gc.full_major ();
  Printf.printf " 4. mut_non_imm.x.(0): %d\n" mut_non_imm.x.(0);
  Printf.printf " 5. mut.x: %d\n" mut.x;
  Printf.printf " 6. mut_imm.x_imm: %d\n" mut_imm.x_imm;
  Printf.printf " 7. mut_imm64.x_imm64: %d\n" mut_imm64.x_imm64;
;;

let () = test2 123_456_789_000
