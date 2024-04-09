[@@@zero_alloc all]

(* check fails with -check-zero-alloc opt/all, passes with -check-zero-alloc default *)
let[@zero_alloc opt] foo x = (x,x)

(* check fails with -check-zero-alloc default/all, passes with "-check-zero-alloc opt" *)
let[@zero_alloc] bar x = [x;x]

let baz x = [|x;x+1|]
(* CR-soon gyorsh:
   The check fails on [baz] with -check-zero-alloc default/all/opt.
   This shows that the current interpretation of [@@@zero_alloc all] on functions
   that do not have an explicit "zero_alloc" attribute
   is to require "zero all" with both "opt" and "default" flags.
   This behavior behavior exposes an implementation detail that does not matter in
   practice because "opt" flag is only intended for testing, and the behavior
   with "default" and "all" flags is indistinguishable.
   Fix it in the next commit.
*)
