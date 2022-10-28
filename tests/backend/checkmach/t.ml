(* CR gyorsh: @assert are currently ignored by the compiler.
   Support for them will be handled in a separate PR. They are here to show
   which functions we expect to pass the noalloc check. *)
let[@inline never] test1 n =
  let rec create n =
    if n = 0 then []
    else
      (n :: create (n -1))
  in
  create n

let[@inline never] test2 n  = List.length(test1 n)

let[@noalloc][@inline never] test3 l  = List.length l

let[@noalloc][@inline never] test6 n = n + 37

let[@noalloc][@inline never] test7 n m = (test6 n) * (test6 m)

exception Exn_string of string
exception Exn_int of int
exception Exn
exception Exn2

let[@inline never] test8 n =
  match n with
  | 1 -> raise_notrace Exn
  | 2 -> raise_notrace (Exn_int 1)
  | 3 -> raise_notrace (Exn_string "foo")
  | _ -> 10

let[@inline never] test9 n =
  try test8 n
  with _ -> 0

let[@noalloc][@inline never] test10 n =
  match n with
  | 1 -> raise_notrace Exn
  | _ -> 10

let[@noalloc][@inline never] test11 n =
  try test10 n
  with Exn -> 10

let[@noalloc][@inline never] test12 n =
  let test n =
    match n with
    | 1 -> raise Exn
    | _ -> 10
  in
  try test n
  with Exn -> raise Exn2

let[@inline never] test13 n =
  print_int n;
  print_newline ()

let test14 n = Float.of_int n

let[@noalloc][@inline never] test15 n = Int64.to_int (Int64.of_float (test14 n))

let[@noalloc] test16 n m = S.foo n m
let[@noalloc] test17 n m = S.foo (S.foo n n) m
let test18 n m = S.foo n m

exception Exn3 of (int * int)
type boo =
  | A
  | B of int

(* CR gyorsh: analysis for noalloc_exn is not yet implemented.
   It ignores allocations post-dominated by a raise. *)
let(* [@noalloc_exn] *)[@inline never] test18 n boo =
  if n > 0 then n + n
  else begin
    let k = 45 in
    let p =
      match boo with
      | A -> (n,k)
      | B x -> (n,x)
    in
    raise (Exn3 p)
  end

let[@inline never] test19 n =
  let rec create n =
    if n = 0 then []
    else
      (n :: create (n -1))
  in
  create n

let(* [@noalloc_exn] *) rec foo n =
  bar (n-1)
and(* [@noalloc_exn] *) bar n =
  foo (n-1)
