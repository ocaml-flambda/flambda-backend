(* Tests showing how [zero_alloc] information from a signature can be used in
   checking uses of the corresponding functions.

   This is part of a test of separate compilation. This file is a library used
   by [test_signatures_separate_b.ml].  The dune rules compile only its [cmi]
   file - no executable artifacts - showing we're inferring the correct
   signature and using the zero_alloc information in it.
*)

(* Basic behavior with strict and assume. *)
let[@zero_alloc] f_id x = x

let[@zero_alloc strict] f_id_strict x = x

let[@zero_alloc assume] f_tuple_assume x = (x,x)

let f_tuple_no_assume x = (x,x)

(* opt behavior - these should be available to the client only when the client
   is compiled with -zero-alloc-check all *)
let[@zero_alloc opt] f_id_opt x = x

let[@zero_alloc strict opt] f_id_strict_opt x = x

(* arity *)
let[@zero_alloc] f_arity_one x = x

let[@zero_alloc] f_arity_two x y =
  if x = y+1 then fun z -> (z,z) else fun z -> (z,0)

let[@zero_alloc assume] f_arity_three x y z = (x,y,z)

module M_arity : sig
  val[@zero_alloc arity 2] f_arity_two_sig : int -> int -> int -> int*int
end = struct
  let[@zero_alloc] f_arity_two_sig x y =
    if x = y+1 then fun z -> (z,z) else fun z -> (z,0)
end
