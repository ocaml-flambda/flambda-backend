let[@zero_alloc assume][@inline never][@specialise never][@local never] bar x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo x = bar x


let[@zero_alloc assume][@inline always] bar' x =
  if x > 0 then failwith (Printf.sprintf "BOO %d!" x);
  (x+1,x)

let[@zero_alloc strict] foo' x = bar' x

let[@inline always] test46 x = if x > 0 then failwith (Printf.sprintf "%d" x) else (x,x)

let[@zero_alloc strict] test48 x =
  (test46[@zero_alloc assume never_returns_normally]) x

(* The analysis checks the debug info associated with an allocation to see if the
   allocation came from an inlined function annotated with [@zero_alloc assume
   never_returns_normally].  If so, the analysis treats this allocation
   as if it was a call that never returns normally but may raise.

   This is needed for analysis to give the same results regardless of inlining of
   allocations annotated with "assume". See test50-53 below.
*)
let[@zero_alloc] test49 x =
  try let y = (test46[@zero_alloc assume never_returns_normally]) x in [y;(x,x+1)]
  with _ -> failwith (Printf.sprintf "%d" x)

(* strict check should fail *)
let[@inline always][@zero_alloc assume never_returns_normally] test50 x = (x, x)

let[@zero_alloc strict] test51 x = test50 x

let[@inline never][@zero_alloc assume never_returns_normally] test52 x = (x, x)

let[@zero_alloc strict] test53 x = test52 x

(* relaxed check should pass *)
let[@inline always][@zero_alloc assume never_returns_normally] test54 x = (x, x)

let[@zero_alloc] test55 x = test54 x

let[@inline never][@zero_alloc assume never_returns_normally] test56 x = (x, x)

let[@zero_alloc] test57 x = test56 x

(* strict assume should pass *)
let[@inline always][@zero_alloc assume strict never_returns_normally] test58 x = (x, x)

let[@zero_alloc strict] test59 x = test58 x

let[@inline never][@zero_alloc assume never_returns_normally strict] test60 x = (x, x)

let[@zero_alloc strict] test61 x = test60 x

(* allocations on the path to a call labeled [@zero_alloc assume never_returns_normally]
   do not count for the normal check, but do count for the strict check *)

let[@inline never] test62 x = if List.length x > 0 then [x;x] else failwith (string_of_int (List.length x))

let[@zero_alloc] test63 x =
  let p = [x;x+1] in
  (test62[@zero_alloc assume never_returns_normally]) p

let[@zero_alloc strict] test64 x =
  let p = [x;x+1] in
  (test62[@zero_alloc assume never_returns_normally]) p

(* The check passes on test66 and fails on test68. The analsysis is overly-conservative
   when test67 is inlined into test68: the analysis loses the "assume" annotation because
   after inlining and optimizations there is nothing to attach the assume annotation
   onto. *)
let[@inline never][@local never] test65 x = x

let[@zero_alloc strict] test66 x =
  let z = (test65[@zero_alloc assume never_returns_normally strict]) x
  in z, z

let[@inline always] test67 x = x

let[@zero_alloc strict] test68 x =
  let z = (test67[@zero_alloc assume never_returns_normally strict]) x
  in z, z

(* CR-soon gyorsh: For a more precise handling of "assume" annotation,
   compute the meet of the function summary (or the effect of an operation)
   with the most precise abstract value that represents the
   meaning of "assume". Then check will pass on test50-53.
   This is no going to change the results of test65-68 and won't fix
   their sensitivity to inlining. To fixed them, we can add a pseudo-instruciton that
   carries the "assume" information of operations that were optimized away.
   So far, we haven't seen the need for it in a real example.
*)

(* CR gyorsh: the check passes on g0,g2,g3 and fails on g1 below, even though the
   generated code is exactly the same in all four cases.  The difference is because
   "assume" is not propagated on partial applications. There is no misplaced
   attribute warning. This is the same behavior as for "@inlined" annotations.
*)
let[@inline never][@local never] f x y = (x,y)

let[@zero_alloc] g0 () =
  let x = (f[@zero_alloc assume]) () () in
  x

let[@zero_alloc] g1 () =
  let x = ((f[@zero_alloc assume]) ()) () in
  x

let[@zero_alloc] g2 () =
  let x = ((f ())[@zero_alloc assume]) () in
  x

let[@zero_alloc] g3 () =
  let x = (((f[@zero_alloc assume]) ())[@zero_alloc assume]) () in
  x
