(* TEST
 reference = "${test_source_directory}/unboxed_records.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compiler_reference = "${test_source_directory}/unboxed_records_stable.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe upstream_compatible";
   compiler_reference = "${test_source_directory}/unboxed_records_stable.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe no_extensions";
   compiler_reference = "${test_source_directory}/unboxed_records_disabled.compilers.reference";
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
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
   }
*)

type t = #{ i : int ; j : int }
let add #{ i ; j } = i + j
let () =
  let t = #{i = 2; j = 3} in
  let res = add t in
  Printf.printf "Test 1: %d\n" res

let add2 x y = add x + add y
let () =
  let t1 = #{i = 2; j = 3} in
  let t2 = #{i = 20; j = 30} in
  let res = add2 t1 t2 in
  Printf.printf "Test 2: %d\n" res

let add t = t.#i + t.#j
let () =
  let t = #{i = 200; j = 300} in
  let res = add t in
  Printf.printf "Test 3: %d\n" res


let copy_i_to_j #{ i ; j } = #{ i; j = i }
let () =
  let t = #{i = 1000; j = 2} in
  let res = add (copy_i_to_j t) in
  Printf.printf "Test 4: %d\n" res

let copy_i_to_j r = #{ r with j = r.#i }
let () =
  let t = #{i = 1000; j = 2} in
  let res = add (copy_i_to_j t) in
  Printf.printf "Test 5: %d\n" res
