(* TEST
 flags = "-extension comprehensions";
 expect;
*)

(******************************************************************************
 *                        ******** ATTENTION! ********                        *
 *                                                                            *
 * This file should be kept in sync with the files                            *
 * "list_comprehensions_pure.ml" and "iarray_comprehensions_pure.ml".  If     *
 * you're adding a test to one, add it to the others as well; if the test     *
 * output changes in one file and not the others (except as documented in     *
 * comments), this is a bug.                                                  *
 ******************************************************************************)

(******************************************************************************)
(**** Basic behavior ****)

[|i for i = 0 to 9|];;
[%%expect{|
- : int array = [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|]
|}];;

[|i for i = 9 downto 0|];;
[%%expect{|
- : int array = [|9; 8; 7; 6; 5; 4; 3; 2; 1; 0|]
|}];;

[|s for s in [|"hello"; "world"; "!"|]|];;
[%%expect{|
- : string array = [|"hello"; "world"; "!"|]
|}];;

[|i,s for i = 0 to 3 and s in [|'a'; 'b'; 'c'|]|];;
[%%expect{|
- : (int * char) array =
[|(0, 'a'); (0, 'b'); (0, 'c'); (1, 'a'); (1, 'b'); (1, 'c'); (2, 'a');
  (2, 'b'); (2, 'c'); (3, 'a'); (3, 'b'); (3, 'c')|]
|}];;

[|i,s for i = 0 to 3 for s in [|'a'; 'b'; 'c'|]|];;
[%%expect{|
- : (int * char) array =
[|(0, 'a'); (0, 'b'); (0, 'c'); (1, 'a'); (1, 'b'); (1, 'c'); (2, 'a');
  (2, 'b'); (2, 'c'); (3, 'a'); (3, 'b'); (3, 'c')|]
|}];;

[|i,j for i = 1 to 3 for j in [|i*10; i*100|]|];;
[%%expect{|
- : (int * int) array =
[|(1, 10); (1, 100); (2, 20); (2, 200); (3, 30); (3, 300)|]
|}];;

[|x for xs in [|[|"this"; "is"; "one"|];
                [|"way"|];
                [|"to"; "flatten"|];
                [|"a"; "nested"; "array"|]|]
    for x in xs|];;
[%%expect{|
- : string array =
[|"this"; "is"; "one"; "way"; "to"; "flatten"; "a"; "nested"; "array"|]
|}];;

[|i for i = 0 to 10 when i mod 2 = 0|];;
[%%expect{|
- : int array = [|0; 2; 4; 6; 8; 10|]
|}];;

[|() for _ = 1 to 10|];;
[%%expect{|
- : unit array = [|(); (); (); (); (); (); (); (); (); ()|]
|}];;

(******************************************************************************)
(**** More complex behavior ****)

let pythagorean_triples n =
  [|a,b,c for a = 1 to n for b = a to n for c = b to n when a*a + b*b = c*c|]
in
pythagorean_triples 10;;
[%%expect{|
- : (int * int * int) array = [|(3, 4, 5); (6, 8, 10)|]
|}];;

let tails xs =
  let len = Array.length xs in
  Array.init (len + 1) (fun i -> Array.sub xs i (len - i))
in
let sum = Array.fold_left ( + ) 0 in
[|sum xs for xs in tails [|1; 20; 300; 4_000; 50_000; 600_000; 7_000_000|]|];;
[%%expect{|
- : int array =
[|7654321; 7654320; 7654300; 7654000; 7650000; 7600000; 7000000; 0|]
|}];;

let xs = [|2;7;18;28|] in
[|x + 1000*y for x in xs and y in xs|];;
[%%expect{|
- : int array =
[|2002; 7002; 18002; 28002; 2007; 7007; 18007; 28007; 2018; 7018; 18018;
  28018; 2028; 7028; 18028; 28028|]
|}];;

(******************************************************************************)
(**** Edge cases ****)

[|42 when true|];;
[%%expect{|
- : int array = [|42|]
|}];;

[|42 when false|];;
[%%expect{|
- : int array = [||]
|}];;

[|x for x in [||]|];;
[%%expect{|
- : '_weak1 array = [||]
|}];;

[|i for i = 0 to -1|];;
[%%expect{|
- : int array = [||]
|}];;

[|i for i = 0 downto 1|];;
[%%expect{|
- : int array = [||]
|}];;

[|i for i = 0 to 0|];;
[%%expect{|
- : int array = [|0|]
|}];;

[|i for i = 0 downto 0|];;
[%%expect{|
- : int array = [|0|]
|}];;

(* This would take ~forever if the empty array were iterated over later;
   however, for arrays, using [and] lets us get more flexibility (see below). *)
[|i,j,k for i in [||] for j = 0 to Int.max_int for k = 0 downto Int.min_int|];;
[%%expect{|
- : ('_weak2 * int * int) array = [||]
|}];;

(* This works no matter where the empty array is, but would take ~forever for
   lists if the empty list were iterated over later; see
   "(i)array_comprehensions_special.ml" for more nuance on what can happen here
   with arrays. *)
[|i,j,k for i in [||] and j = 0 to Int.max_int and k = 0 downto Int.min_int|];;
[%%expect{|
- : ('_weak3 * int * int) array = [||]
|}];;

[|x for x in [|"one"; "two"; "three"|] for x in [|10; 20; 30|]|];;
[%%expect{|
Line 1, characters 8-9:
1 | [|x for x in [|"one"; "two"; "three"|] for x in [|10; 20; 30|]|];;
            ^
Warning 26 [unused-var]: unused variable x.

- : int array = [|10; 20; 30; 10; 20; 30; 10; 20; 30|]
|}];;

(******************************************************************************)
(**** Variable shadowing ****)

(* QuickCheck found that Python doesn't shadow variables in list comprehensions;
   instead, using the same variable name as the binder in two `for`-clauses
   doesn't shadow, but rather overwrites the same mutable cell.  To confirm that
   we handle the subtle issue of shadowing correctly, we preserve here the cases
   that Python does not, as found by QuickCheck. *)

(* Python: {v
     [a for a in [0] for a in [1]] == [1]
   v} *)
[|a for a in [|0|] for a in [|1|]|];;
[%%expect{|
Line 1, characters 8-9:
1 | [|a for a in [|0|] for a in [|1|]|];;
            ^
Warning 26 [unused-var]: unused variable a.

- : int array = [|1|]
|}];;

(* Python: {v
     [(a, b)
        for b in [0]
        for _ in [0, 0]
        for a in [b]
        for b in range(0, -2, -1)]
     == [(0, 0), (0, -1), (-1, 0), (-1, -1)]
   v} *)
[|(a, b) for b in [|0|] for _ in [|0; 0|] for a in [|b|] and b = 0 downto -1|];;
[%%expect{|
- : (int * int) array = [|(0, 0); (0, -1); (0, 0); (0, -1)|]
|}];;

(* Python: {v
     [(a, b) for b in [1] for b in [0] for a in [b]] == [(0, 0)]
   v} *)
[|(a, b) for b in [|1|] for b in [|0|] and a in [|b|]|];;
[%%expect{|
- : (int * int) array = [|(1, 0)|]
|}];;

(* Python: {v
     [a for a in [1] for _ in [0, 0] if a > 0 for a in [0]] == [0]
   v} *)
[|a for a in [|1|] and _ in [|0; 0|] when a > 0 for a in [|0|]|];;
[%%expect{|
- : int array = [|0; 0|]
|}];;

(* Python: {v
     [a for a in [0] for _ in [0, 0] for a in [a, 1]] == [0, 1, 1, 1]
   v} *)
[|a for a in [|0|] and _ in [|0; 0|] for a in [|a; 1|]|];;
[%%expect{|
- : int array = [|0; 1; 0; 1|]
|}];;

(******************************************************************************)
(**** Bugs found by QuickCheck ****)

(* At one time, this was incorrectly returning the empty array as an array
   comprehension, but correctly returning a singleton list as a list
   comprehension. *)
[|() for _ = 0 to 0|];
[%%expect{|
- : unit array = [|()|]
|}];;

(******************************************************************************)
(**** Errors ****)

(* Can't iterate over non-arrays *)

[|x for x in 100|];;
[%%expect{|
Line 1, characters 13-16:
1 | [|x for x in 100|];;
                 ^^^
Error: This expression has type "int" but an expression was expected of type
         "'a array"
       because it is in a for-in iterator in an array comprehension
|}];;

(* No mixing arrays and lists *)

(* It's unclear why these are different based on principality *)
[|x for x in []|];;
[%%expect{|
Line 1, characters 13-15:
1 | [|x for x in []|];;
                 ^^
Error: This expression has type "'a list"
       but an expression was expected of type "'b array"
|}, Principal{|
Line 1, characters 13-15:
1 | [|x for x in []|];;
                 ^^
Error: This expression has type "'a list"
       but an expression was expected of type "'b array"
       because it is in a for-in iterator in an array comprehension
|}];;

(* As above, but don't trigger type-based disambiguation; this affects the error
   message for array comprehensions, but is invisible for list comprehensions *)
let empty = [] in
[|x for x in empty|];;
[%%expect{|
Line 2, characters 13-18:
2 | [|x for x in empty|];;
                 ^^^^^
Error: This expression has type "'a list"
       but an expression was expected of type "'b array"
       because it is in a for-in iterator in an array comprehension
|}];;

List.length [|i for i = 0 to 3|];;
[%%expect{|
Line 1, characters 12-32:
1 | List.length [|i for i = 0 to 3|];;
                ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a array"
       but an expression was expected of type "'b list"
|}];;

(* to/downto are only for ints *)

[|x for x = 1.5 to 4.2|];;
[%%expect{|
Line 1, characters 12-15:
1 | [|x for x = 1.5 to 4.2|];;
                ^^^
Error: This expression has type "float" but an expression was expected of type
         "int"
       because it is in a range-based for iterator start index in a comprehension
|}];;

[|x for x = 4.2 downto 1.5|];;
[%%expect{|
Line 1, characters 12-15:
1 | [|x for x = 4.2 downto 1.5|];;
                ^^^
Error: This expression has type "float" but an expression was expected of type
         "int"
       because it is in a range-based for iterator start index in a comprehension
|}];;

(* Using first-class module patterns isn't supported yet *)

module type S = sig
  type t
  val x : t
end;;

let t = (module struct
  type t = int
  let x = 3
end : S);;
[%%expect {|
module type S = sig type t val x : t end
val t : (module S) = <module>
|}];;

[| M.x for (module M : S) in [| t |] |];;
[%%expect {|
Line 1, characters 19-20:
1 | [| M.x for (module M : S) in [| t |] |];;
                       ^
Error: Modules are not allowed in this pattern.
|}];;

[| M.x
   for (module M : S) in
   [| (module struct
         type t = int
         let x = 3
       end : S)
   |]
|];;
[%%expect {|
Line 2, characters 15-16:
2 |    for (module M : S) in
                   ^
Error: Modules are not allowed in this pattern.
|}];;

[| M.x
   for (module M : S) in
   [| (let t = t in
       t)
   |]
|];;
[%%expect {|
Line 2, characters 15-16:
2 |    for (module M : S) in
                   ^
Error: Modules are not allowed in this pattern.
|}];;

(* No duplicating variables in a for-and clause *)

[|i for i = 1 to 3 and i = 3 downto 1|];;
[%%expect{|
Line 1, characters 0-39:
1 | [|i for i = 1 to 3 and i = 3 downto 1|];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Variable "i" is bound several times in this matching
|}];;

[|i for i = 1 to 3 and i in [|10; 20; 30|]|];;
[%%expect{|
Line 1, characters 23-24:
1 | [|i for i = 1 to 3 and i in [|10; 20; 30|]|];;
                           ^
Error: Variable "i" is bound several times in this matching
|}];;

[|i for i in [|1; 2; 3|] and i = 3 downto 1|];;
[%%expect{|
Line 1, characters 0-45:
1 | [|i for i in [|1; 2; 3|] and i = 3 downto 1|];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Variable "i" is bound several times in this matching
|}];;

[|i for i in [|1; 2; 3|] and i in [|10; 20; 30|]|];;
[%%expect{|
Line 1, characters 29-30:
1 | [|i for i in [|1; 2; 3|] and i in [|10; 20; 30|]|];;
                                 ^
Error: Variable "i" is bound several times in this matching
|}];;

[|i for i, j in [|1, 10; 2, 20; 3, 30|] and k, i in [|10, 1; 20, 2; 30, 3|]|];
[%%expect{|
Line 1, characters 47-48:
1 | [|i for i, j in [|1, 10; 2, 20; 3, 30|] and k, i in [|10, 1; 20, 2; 30, 3|]|];
                                                   ^
Error: Variable "i" is bound several times in this matching
|}];;

(* Variables bind from left to right, not right to left *)

[|outer,inner for outer = inner to 3 for inner = 1 to 3|];;
[%%expect{|
Line 1, characters 26-31:
1 | [|outer,inner for outer = inner to 3 for inner = 1 to 3|];;
                              ^^^^^
Error: Unbound value "inner"
Hint: Did you mean "incr"?
|}];;

(* The element type is handled correctly *)

Array.append [|true|] [|i for i = 0 to 10|];;
[%%expect{|
Line 1, characters 24-25:
1 | Array.append [|true|] [|i for i = 0 to 10|];;
                            ^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}];;

(******************************************************************************)
(**** Test that types are being propagated inwards correctly ****)

(* Prepare a module for later use *)
module M = struct
  type t = A | B
end;;
[%%expect{|
module M : sig type t = A | B end
|}];;

let x : M.t array  = [|A for _ = 1 to 3|];;
[%%expect{|
val x : M.t array = [|M.A; M.A; M.A|]
|}];;

[|A for _ = 1 to 3|];;
[%%expect{|
Line 1, characters 2-3:
1 | [|A for _ = 1 to 3|];;
      ^
Error: Unbound constructor "A"
|}];;

Array.append [|M.B|] [|A for _ = 1 to 3|];;
[%%expect{|
- : M.t array = [|M.B; M.A; M.A; M.A|]
|}, Principal{|
Line 1, characters 23-24:
1 | Array.append [|M.B|] [|A for _ = 1 to 3|];;
                           ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.

- : M.t array = [|M.B; M.A; M.A; M.A|]
|}];;
