(* TEST
   flags = "-disable-all-extensions"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This test checks that the include functor feature doesn't work without the
   extension flag.

   Test 2: [include functor] at the top level *)

module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

type t = int
let x : t = 3
let x : t = 5
include functor F1

let () = assert (Int.(equal y 5));;
