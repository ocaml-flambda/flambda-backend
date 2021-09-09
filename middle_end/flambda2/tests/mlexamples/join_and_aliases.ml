(* Example provided by Pierre Chambart and Guillaume Bury This example was
   produced to test cases where a variant can be unboxed but there isn't any
   existing variable that refers to the field of the block. However, it also
   triggered a few bugs in the join algorithm, fixed by PRs #327 and #329, so it
   is included here.

   The goal here is to propagate enough information to remove the last assert
   false. The main issue was that the type for v was Top, for two reasons: - The
   fact that v is an alias to r was not correctly propagated, because its type
   was the result of joining an alias with Bottom (this is fixed by #327) - Even
   without the alias, the join should have expanded the type at the use site and
   found that it only had Foo (0) as possible tag, but because the expansion was
   done in the wrong environment no type was found and Top was returned
   instead. *)

type t =
  | Foo of int
  | Bar of string

let f r =
  let v = match r with Foo x' as res -> res | Bar _ -> raise Exit in
  let v' = if Sys.opaque_identity false then v else Foo 42 in
  match v' with Foo i -> i | Bar _ -> assert false
