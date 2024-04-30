(* TEST
 flags = "-extension-universe alpha";
 include alpha;
 {
   bytecode;
 }{
   native;
 }
*)

open Alpha

(* Test that [Float32] is exported. *)

let () =
  let one = Float32.of_float 1.0 in
  assert (Float32.to_float one = 1.0)
;;
