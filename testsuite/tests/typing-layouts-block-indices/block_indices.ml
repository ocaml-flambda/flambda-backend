(* TEST
 reference = "${test_source_directory}/block_indices.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "";
   compiler_reference = "${test_source_directory}/block_indices_disabled.compilers.reference";
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

type pt = { x : int; y : int }
type r = { p : pt#; q : pt# }
let f () = (.p.#x)

external imm_idx_to_int64 : 'base ('a: any) . ('base, 'a) imm_idx -> int64 = "%box_int64"
external mut_idx_to_int64 : _ mut_idx -> int64 = "%box_int64"

let () =
  Printf.printf "%d\n" (Int64.to_int (imm_idx_to_int64 (f ())))
