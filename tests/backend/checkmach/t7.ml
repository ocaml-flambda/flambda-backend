(* Check that entry function and functors are ignored with  [@@@zero_alloc all] *)
[@@@zero_alloc all]

let[@zero_alloc ignore] foo x = (x,x)

let[@inline never][@zero_alloc ignore] print x =
  print_int (fst x)

let () =
  print (Sys.opaque_identity (foo (Sys.opaque_identity 5)))

module type A = sig
  type t
  val foo : t -> int
end

module Make (A : A) = struct
  include A
  let f _t = ()
  let g _t = ()
end
[@@inline always]
