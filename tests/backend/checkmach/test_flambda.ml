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
