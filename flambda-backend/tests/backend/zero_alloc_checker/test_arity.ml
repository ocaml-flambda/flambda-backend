
(* These tests check that the backend check does what we expect on functions
   used in an interesting example in the typing tests for zero_alloc in
   signatures (see [ocaml/testsuite/tests/typing-zero-alloc/signatures.ml]). *)

let[@zero_alloc] f_zero_alloc x y =
  if x = y+1 then fun z -> (z,z) else fun z -> (z,0)

let[@zero_alloc] f_not_zero_alloc x y z =
  if x = y+1 then (z,z) else (z,0)
