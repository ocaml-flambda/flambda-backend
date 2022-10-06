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

let[@noalloc_strict][@inline never] test3 l  = List.length l

let[@noalloc_strict][@inline never] test6 n = n + 37

let[@noalloc_strict][@inline never] test7 n m = (test6 n) * (test6 m)

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

let[@noalloc_strict][@inline never] test10 n =
  match n with
  | 1 -> raise_notrace Exn
  | _ -> 10

let[@noalloc_strict][@inline never] test11 n =
  try test10 n
  with Exn -> 10

let[@noalloc_strict][@inline never] test12 n =
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

let[@noalloc_strict][@inline never] test15 n = Int64.to_int (Int64.of_float (test14 n))

let[@noalloc_strict] test16 n m = S.foo n m
let[@noalloc_strict] test17 n m = S.foo (S.foo n n) m
let test18 n m = S.foo n m

exception Exn3 of (int * int)
type boo =
  | A
  | B of int

let[@noalloc][@inline never] test18 n boo =
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

let[@noalloc] rec foo n =
  bar (n-1)
and[@noalloc] bar n =
  foo (n-1)

let[@noalloc] always_allocate_and_raise n = raise (Exn3 (4,n))

let[@inline never] f n = (n,n)

(* compiles to a recursive catch that the check can handle *)
let[@inline never] test20 n = Sys.opaque_identity n
let[@noalloc] test21 n =
  let i = ref 0 in
  let j = ref 0 in
  while !j < n do
    while !i < n do
      i := test20 !i
    done;
    if n > 0 then raise (Exn_int !i);
  done;


(*
The check is too conservative about recursive catch to handle the following:

let[@noalloc] test22 n =
  let e = (Exn_int n) in
  for i = 0 to n do
    ();
  done;
  raise e
*)
