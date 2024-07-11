(* TEST
 flags = "-less-tco -dtypedtree -dlambda -dno-unique-ids";
 native;
*)

(* These calls should be inferred as tail-calls because they call, in tail 
   position, a function defined in some ancestor let rec. *)
let rec foo n =
  if n > 0 then foo (n - 2) else ()

let rec bar n =
  if n > 0 then baz (n - 2) else ()
and baz n = bar (n + 1)

