(* TEST
   flags = "-extension-universe no_extensions"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This test checks that the include functor feature doesn't work without the
   extension flag.

   Test 2: [include functor] in a signature *)

module type S = sig
  type t
  val x : t
end

module type T = sig
  type s
  val f : s -> bool
end

module type F2 = functor (X : S) -> T with type s = X.t

module type M2_sig = sig
  type t
  val x : t

  include functor F2
end

module M2_impl : M2_sig = struct
  type t = int
  type s = t

  let x = 5
  let f s = x = s
end

let () = assert (M2_impl.f M2_impl.x);;
