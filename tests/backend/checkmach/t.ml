let[@inline never] test1 n =
  let rec create n =
    if n = 0 then []
    else
      (n :: create (n -1))
  in
  create n

let[@inline never] test2 n  = List.length(test1 n)

let[@zero_alloc strict][@inline never] test3 l  = List.length l

let[@zero_alloc strict][@inline never] test6 n = n + 37

let[@zero_alloc strict][@inline never] test7 n m = (test6 n) * (test6 m)

exception Exn_string of string
exception Exn_int of int
exception Exn
exception Exn2

let[@zero_alloc][@inline never] test8b n =
  match n with
  | 1 -> raise Exn
  | 2 -> raise (Exn_int n)
  | 3 -> raise (Exn_string "foo")
  | _ -> 10

let[@zero_alloc strict][@inline never] test10 n =
  match n with
  | 1 -> raise_notrace Exn
  | _ -> 10

let[@zero_alloc strict][@inline never] test11 n =
  try test10 n
  with Exn -> 10

let[@zero_alloc strict][@inline never] test12 n =
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

let[@zero_alloc strict][@inline never] test15 n = Int64.to_int (Int64.of_float (test14 n))

let[@zero_alloc strict] test16 n m = S.foo n m
let[@zero_alloc strict] test17 n m = S.foo (S.foo n n) m
let test18 n m = S.foo n m

exception Exn3 of (int * int)
type boo =
  | A
  | B of int

let[@zero_alloc][@inline never] test18 n boo =
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

let[@zero_alloc] rec foo n =
  bar (n-1)
and[@zero_alloc] bar n =
  foo (n-1)

let[@zero_alloc] always_allocate_and_raise n = raise (Exn3 (4,n))

let[@inline never] f n = (n,n)

(* compiles to a recursive catch that the check can handle *)
let[@inline never] test20 n = Sys.opaque_identity n
let[@zero_alloc] test21 n =
  let i = ref 0 in
  let j = ref 0 in
  while !j < n do
    while !i < n do
      i := test20 !i
    done;
    if n > 0 then raise (Exn_int !i);
  done

let[@zero_alloc] test22 n =
  let e = (Exn_int n) in
  for i = 0 to n do
    ();
  done;
  raise e


let[@zero_alloc] test23 n =
  let n =
    try raise (Exn_int n) with Exn_int n -> n
  in
  raise (Exn_int (n*n))

let[@zero_alloc] test24 n =
  try raise (Exn_int n)
  with Exn_int n -> raise (Exn_int (n+1))

let[@zero_alloc] test25 n =
  try
    S.baz n
  with
  | _ -> raise (Exn3 (n,n))

let[@zero_alloc strict] test26 n =
  try raise Exn
  with _ -> n

let[@zero_alloc] test27 n =
  try raise Exn
  with _ -> n

let[@zero_alloc strict][@inline never] forever () = while true do ignore (1,1) done
let[@zero_alloc strict] test28 g x =
  try
    while true do
      forever ()
    done;
    x
  with _ ->
    g x;
    x

let[@zero_alloc strict][@inline never] rec forever2 () = while true do () done
and[@zero_alloc strict][@inline never] test29 () =
  forever2 ();
  (2, 3)

let[@zero_alloc strict][@inline never] rec test30 () =
  forever3 ();
  (2, 3)
and[@zero_alloc strict][@inline never] forever3 () = while true do () done

(* test31+test32 show that tracking "div" component separately from "nor" improves
   precision. *)
let[@zero_alloc][@inline never] test31 y =
  if y > 0 then raise (Exn_int y)
  else
    while true do
      Sys.opaque_identity ()
    done

let[@zero_alloc] test32 x y=
   test31 y;
   (x,x)

let[@inline never] test33 x = Printf.eprintf "%d\n%!" x
let[@zero_alloc] test34 x =
  if x > 0 then (test33 x; raise Exn) else x + 1

let[@inline never] allocate x = (x,x)
let[@zero_alloc] test35 cond x =
  while true do
    (Sys.opaque_identity (allocate x)) |> ignore;
    if Sys.opaque_identity cond then raise Exn
  done

let[@zero_alloc] test36 a i : int = a.(i)


module Params = struct
  exception E

  (* A bit of testing for function arguments: pattern match, mutable fields, optional *)
  type t = { x : int; y : int }

  let[@zero_alloc] test1 {x; y}  = Printf.eprintf "%d\n%!" y; raise E
  let[@zero_alloc] test2 t = test1 t

  let[@zero_alloc] test3 t () =
    let {x; y} = t in
    Printf.eprintf "%d\n%!" y; raise E
  let[@zero_alloc] test4 t = test3 t ()


  type s = { a : int; mutable b : int }
  let[@zero_alloc] test5 {a; b}  = Printf.eprintf "%d\n%!" b; raise E

  let[@zero_alloc] test6 {b; a}  = Printf.eprintf "%d\n%!" b; raise E

  let[@zero_alloc] test7 ?t ~d () =
    let b =
      match (t:t option) with
      | None -> d
      | Some {x; y } -> y
    in
    Printf.eprintf "%d\n%!" b; raise E


  let[@zero_alloc] test8 ?t =
    test7 ?t ~d:42 ()

  let[@zero_alloc] test9 ?s ~d () =
    let b =
      match (s:s option) with
      | None -> d
      | Some {a; b } -> b
    in
    Printf.eprintf "%d\n%!" b; raise E

  let[@zero_alloc] test10 ?s =
    test9 ?s ~d:42 ()

  let[@zero_alloc] test11 s =
    Printf.eprintf "%d\n%!" s.b; raise E

  let test12 ?(s= {a = 4; b = 5}) ~d () =
    test11 s

  let[@zero_alloc] test13 () =
    test12 ~d:42 ()
end

let[@zero_alloc assume][@inline always] test40 x = (x,x)

let[@zero_alloc] test41 b x =
  if b then test40 (x+1) else test40 (x*2)


module Never_returns_normally = struct
  let[@inline never][@zero_alloc assume never_returns_normally] failwithf fmt =
   Printf.ksprintf (fun s () -> failwith s) fmt

  let[@inline never][@zero_alloc assume never_returns_normally] invalid_argf fmt =
    Printf.ksprintf (fun s () -> invalid_arg s) fmt

 let[@zero_alloc] foo x = failwithf "%d" x
 let[@zero_alloc] bar x y = invalid_argf "%d" (x+y)
end


module Opt = struct
  let[@zero_alloc opt] test x = x,x
end
