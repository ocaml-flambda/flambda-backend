(* TEST
 flags += "-extension unique";
 expect;
*)


(*
 * Overapplication (functions that return functions)
 *)
let g : local_ 'a -> int -> _ = fun _ _ -> (fun[@curry] (local_ _) (x : int) -> x)
[%%expect{|
val g : local_ 'a -> int -> (local_ 'b -> int -> int) = <fun>
|}]
let apply1 x = g x
[%%expect{|
Line 1, characters 15-18:
1 | let apply1 x = g x
                   ^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let apply2 x = g x x
[%%expect{|
val apply2 : int -> local_ 'a -> int -> int = <fun>
|}]
let apply3 x = g x x x
[%%expect{|
Line 1, characters 15-20:
1 | let apply3 x = g x x x
                   ^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let apply3_wrapped x = (g x x) x
[%%expect{|
Line 1, characters 23-32:
1 | let apply3_wrapped x = (g x x) x
                           ^^^^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let apply4 x = g x x x x
[%%expect{|
Line 1, characters 15-20:
1 | let apply4 x = g x x x x
                   ^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let apply4_wrapped x = (g x x) x x
[%%expect{|
val apply4_wrapped : int -> int = <fun>
|}]
let ill_typed () = g 1 2 3 4 5
[%%expect{|
Line 1, characters 19-30:
1 | let ill_typed () = g 1 2 3 4 5
                       ^^^^^^^^^^^
Error: The function g has type local_ 'a -> int -> (local_ 'b -> int -> int)
       It is applied to too many arguments
Line 1, characters 29-30:
1 | let ill_typed () = g 1 2 3 4 5
                                 ^
  This extra argument is not expected.
|}]

(*
 * Defaulting of modes in printing, similar to mli-less files
 *)

let f g = g (local_ (1, 2)) 1 2 3 [@nontail]
[%%expect{|
val f : (local_ int * int -> int -> int -> int -> 'a) -> 'a = <fun>
|}]

(*
 * Labels and reordering
 *)

let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
[%%expect{|
Line 1, characters 64-79:
1 | let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
                                                                    ^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: It is captured by a partial application.
|}]
let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 64-79:
1 | let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
                                                                    ^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: It is captured by a partial application.
|}]
let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: It is captured by a partial application.
|}]
let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]
let app42 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2 ~c:4
[%%expect{|
Line 2, characters 2-21:
2 |   f ~a:(local_ ref 1) 2 ~c:4
      ^^^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app42_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(local_ ref 1)) 2 ~c:4
[%%expect{|
val app42_wrapped :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> unit = <fun>
|}]
let app43 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2
[%%expect{|
Line 2, characters 7-21:
2 |   f ~a:(local_ ref 1) 2
           ^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]
let app5 (f : b:local_ int ref -> a:int -> unit) = f ~a:42
[%%expect{|
val app5 : (b:local_ int ref -> a:int -> unit) -> b:local_ int ref -> unit =
  <fun>
|}]
let app6 (f : a:local_ int ref -> b:local_ int ref -> c:int -> unit) = f ~c:42
[%%expect{|
val app6 :
  (a:local_ int ref -> b:local_ int ref -> c:int -> unit) ->
  a:local_ int ref -> b:local_ int ref -> unit = <fun>
|}]

let app1' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42) ()
[%%expect{|
val app1' : (a:int -> b:local_ int ref -> unit -> unit) -> a:int -> unit =
  <fun>
|}]
let app2' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42)
[%%expect{|
val app2' :
  (a:int -> b:local_ int ref -> unit -> unit) ->
  a:int -> local_ (unit -> unit) = <fun>
|}]
let app3' (f : a:int -> b:local_ int ref -> unit) = f ~b:(ref 42)
[%%expect{|
val app3' : (a:int -> b:local_ int ref -> unit) -> a:int -> unit = <fun>
|}]
let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 52-65:
1 | let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
                                                        ^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let app42' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2 ~c:4
[%%expect{|
Line 2, characters 2-14:
2 |   f ~a:(ref 1) 2 ~c:4
      ^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app42'_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(ref 1)) 2 ~c:4
[%%expect{|
val app42'_wrapped :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> unit = <fun>
|}]
let app43' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2
[%%expect{|
Line 2, characters 2-14:
2 |   f ~a:(ref 1) 2
      ^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app43'_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(ref 1)) 2
[%%expect{|
val app43'_wrapped :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> c:int -> unit = <fun>
|}]

let rapp1 (f : a:int -> unit -> local_ int ref) = f ()
[%%expect{|
val rapp1 : (a:int -> unit -> local_ int ref) -> a:int -> local_ int ref =
  <fun>
|}]
let rapp2 (f : a:int -> unit -> local_ int ref) = f ~a:1
[%%expect{|
val rapp2 : (a:int -> unit -> local_ int ref) -> unit -> local_ int ref =
  <fun>
|}]
let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
[%%expect{|
Line 1, characters 50-59:
1 | let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
                                                      ^^^^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let bug1 () =
  let foo : a:local_ string -> b:local_ string -> c:int -> unit =
    fun ~a ~b ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 7, characters 2-5:
7 |   res
      ^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]
let bug2 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 5, characters 19-33:
5 |   let bar = local_ foo ~b:"hello" in
                       ^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after a in the function's type should be applied separately.
|}]
let bug3 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
  in
  let[@stack] bar = foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 3, characters 63-64:
3 |     fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
                                                                   ^
Error: The value a is local, so cannot be used inside a closure that might escape.
|}]
let overapp ~(local_ a) ~b = (); fun ~c ~d -> ()

let () = overapp ~a:1 ~b:2 ~c:3 ~d:4
[%%expect{|
val overapp : a:local_ 'a -> b:'b -> (c:'c -> d:'d -> unit) = <fun>
Line 3, characters 9-26:
3 | let () = overapp ~a:1 ~b:2 ~c:3 ~d:4
             ^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let () = overapp ~b:2 ~a:1 ~c:3 ~d:4
[%%expect{|
Line 1, characters 20-21:
1 | let () = overapp ~b:2 ~a:1 ~c:3 ~d:4
                        ^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after this one in the function's type should be applied separately.
|}]

let () = overapp ~c:1 ~b:2
[%%expect{|
Line 1, characters 25-26:
1 | let () = overapp ~c:1 ~b:2
                             ^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after this one in the function's type should be applied separately.
|}]

let () = overapp ~d:1 ~a:2
[%%expect{|
Line 1, characters 9-26:
1 | let () = overapp ~d:1 ~a:2
             ^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after b in the function's type should be applied separately.
|}]


(* Regression test for bug with mishandled regional function modes *)
let bug4 : local_ (string -> foo:string -> unit) -> (string -> unit) =
  fun f -> f ~foo:"hello"
[%%expect{|
Line 2, characters 11-25:
2 |   fun f -> f ~foo:"hello"
               ^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]

(* The fixed version. Note that in the printed type, local returning is implicit
    *)
let bug4_fixed : local_ (string -> foo:string -> unit) -> local_ (string -> unit) =
  fun f -> exclave_ f ~foo:"hello"
[%%expect{|
val bug4_fixed : local_ (string -> foo:string -> unit) -> string -> unit =
  <fun>
|}]


let bug4' () =
  let local_ f arg ~foo = () in
  let local_ perm ~foo = f ~foo in
  perm ~foo:"foo" "Optional"
[%%expect{|
Line 3, characters 25-31:
3 |   let local_ perm ~foo = f ~foo in
                             ^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
  Hint: This is a partial application
        Adding 1 more argument may make the value non-local
|}]

(* The above tests for the locality axis exhaust cases wrt
  [Known_arg/Unknown_arg/Omitted_optional_arg/Omitted],
  Tests below only try to exhaust mode axes. *)

(* Uniqueness & linearity *)

let _ =
  let f : _ @ unique -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : _ @ once -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : _ -> (_ -> _) @@ once = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : _ -> (_ -> _) @@ unique = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

(* portability and contention, due to the choice of legacy modes, don't have
   the same problem. *)
let _ =
  let f : _ @ uncontended -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : _ @ nonportable -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : _ -> (_ -> _) @@ nonportable = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : _ -> (_ -> _) @@ uncontended = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

(* printing is similar to generating [cmi] without [mli]. Tests below show that
   the inferred type of [g] doesn't contain extra parenthesis, as they would
   if pushed to legacy; this is prevented by [check_curried_application_complete]. *)
let f g x =
  g (x: _ @@ once) x [@nontail]
[%%expect{|
val f : (once_ 'a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]

let f g x y =
  g (x: _ @@ unique) y [@nontail]
[%%expect{|
val f : ('a -> 'b -> 'c) -> unique_ 'a -> 'b -> 'c = <fun>
|}]

let f (g @ unique) x =
  g x x [@nontail]
[%%expect{|
val f : unique_ ('a -> 'a -> 'b) -> ('a -> 'b) = <fun>
|}, Principal{|
val f : unique_ ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]

let f (g @ once) x =
  g x x [@nontail]
[%%expect{|
val f : once_ ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]

(* portability and contention is not affected due to the choice of legacy modes. *)
let f g x =
  g (x: _ @@ nonportable) x [@nontail]
[%%expect{|
val f : ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]

let f g x y =
  g (x: _ @@ uncontended) y [@nontail]
[%%expect{|
val f : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
|}]

let f (g @ uncontended) x =
  g x x [@nontail]
[%%expect{|
val f : ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]

let f (g @ nonportable) x =
  g x x [@nontail]
[%%expect{|
val f : ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
|}]
