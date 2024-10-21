(* These checks fail with closure but pass with flambda and flambda2, so we have a
   separate file for them. *)
let[@inline always] test4 n = (n+1,n)

let[@zero_alloc strict][@inline never] test5 n =
  let first,_ = (test4 (n + 1)) in
  first

exception Exn_string of string
exception Exn_int of int
exception Exn

let[@zero_alloc strict][@inline never] test8 n =
(* strict check passes because all exceptions are statically allocated *)
  match n with
  | 1 -> raise_notrace Exn
  | 2 -> raise_notrace (Exn_int 1)
  | 3 -> raise_notrace (Exn_string "foo")
  | _ -> 10

(* the following function is zero_alloc strict *)
let[@zero_alloc strict] test9 n =
  try test8 n
  with _ -> 0

(* This should work with closure too, but there is a problem with stdlib: stdlib is not
   compiled with -zero-alloc-check and with closure "failwith" does not get inlined.
   With flambda, ( ^ ) does not get inlined.
*)
(* let[@zero_alloc] test33 s = failwith ("msg"^s) *)

let[@zero_alloc] test34 s = failwith s

(* [test37] allocates on exn return in closure but not in flambda or flambda2,
   and conservative handling of callees causes test36 to fail zero_alloc check even
   though it does not allocate on normal return. *)
let[@zero_alloc] rec test36 x =
  if Sys.opaque_identity true then
    try test37 x with _ -> ()
  else raise (Failure x)

and[@zero_alloc]  test37 x = assert (String.length x > 0)
