(* TEST
   flags = "-universe no_extensions"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This test checks that the include functor feature doesn't work without the
   extension flag.

   Test 1: [include functor] in a structure *)

module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int
  let x = 5

  include functor F1
end

let () = assert Int.(equal M1.y 5);;
