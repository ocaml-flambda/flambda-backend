(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/unboxed_return2.reference";
 include stdlib_stable;
 flambda2;
 arch_amd64;
 {
   native;
 }
 {
   bytecode;
 }
*)

(* CR mshinwell: enable this for arm64, but see to_cmm_expr.ml *)

external ui64_f64_make : unit -> #(int64# * float#) = "ui64_f64_make_bytecode" "ui64_f64_make"
external f64_ui64_make : unit -> #(float# * int64#) = "f64_ui64_make_bytecode" "f64_ui64_make"

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"


let () =
  let #(x, y) = ui64_f64_make () in
  Printf.eprintf "%Ld %f\n%!" (box_int64 x) (box_float y)

let () =
  let #(x, y) = f64_ui64_make () in
  Printf.eprintf "%f %Ld\n%!" (box_float x) (box_int64 y)
