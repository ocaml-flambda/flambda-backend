(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/unboxed_return.reference";
 include stdlib_stable;
 flambda2;
 {
   native;
 }
 {
   bytecode;
 }
*)

external ui64_ui64_make : unit -> #(int64# * int64#) = "ui64_ui64_make_bytecode" "ui64_ui64_make"
external f64_f64_make : unit -> #(float# * float#) = "f64_f64_make_bytecode" "f64_f64_make"

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"


let () =
  let #(x, y) = ui64_ui64_make () in
  Printf.eprintf "%Ld %Ld\n%!" (box_int64 x) (box_int64 y)

let () =
  let #(x, y) = f64_f64_make () in
  Printf.eprintf "%f %f\n%!" (box_float x) (box_float y)
