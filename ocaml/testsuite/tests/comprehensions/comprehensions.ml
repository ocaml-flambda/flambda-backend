(* TEST
  flags = "-extensions Comprehensions"
   * expect
*)
(*Type checking tests.*)
true::[i for i = 10 downto 0];;
[%%expect{|
Line 1, characters 7-8:
1 | true::[i for i = 10 downto 0];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

module M = struct type t = A | B end;;
let x : M.t list  = [A for i = 1 to 1];;
[%%expect{|
module M : sig type t = A | B end
val x : M.t list = [M.A]
|}];;

[A for i = 1 to 1];;
[%%expect{|
Line 1, characters 1-2:
1 | [A for i = 1 to 1];;
     ^
Error: Unbound constructor A
|}];;

M.B::[A for i = 1 to 1];;
[%%expect{|
- : M.t list = [M.B; M.A]
|}, Principal{|
Line 1, characters 6-7:
1 | M.B::[A for i = 1 to 1];;
          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
- : M.t list = [M.B; M.A]
|}];;

let y = 10;;
[i for i in y];;
[%%expect{|
val y : int = 10
Line 2, characters 12-13:
2 | [i for i in y];;
                ^
Error: This expression has type int but an expression was expected of type
         'a list
       because it is in the iteration argument of a comprehension
|}];;

let y = [1;2;3];;
true::[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
Line 2, characters 7-8:
2 | true::[i for i in y];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

let y = [[1]];;
true::[i for i in z for z in y];;
[%%expect{|
val y : int list list = [[1]]
Line 2, characters 7-8:
2 | true::[i for i in z for z in y];;
           ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

let y = [[]];;
[i for i in z and z in y];;
[%%expect{|
val y : 'a list list = [[]]
Line 2, characters 12-13:
2 | [i for i in z and z in y];;
                ^
Error: Unbound value z
|}];;

(*List construction tests.*)

[i for i = 0 to 10];;
[%%expect{|
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
|}];;

[i for i = 10 downto 0];;
[%%expect{|
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
|}];;

let y = [1;2;3];;
[i for i in y];;
[%%expect{|
val y : int list = [1; 2; 3]
- : int list = [1; 2; 3]
|}];;

let y = [0;1;2;3];;
[ (k*4*4 + j*4 + i) for i in y for j in y for k in y];;
[%%expect{|
val y : int list = [0; 1; 2; 3]
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
 59; 60; 61; 62; 63]
|}];;

let y = [0;1;2;3];;
[ (k*4*4 + j*4 + i) for i in y and j in y and k in y];;
[%%expect{|
val y : int list = [0; 1; 2; 3]
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
 59; 60; 61; 62; 63]
|}];;

(*Array construction tests*)

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y for j in y for k in y |];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y and j in y and k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y for j in y and k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

let y = [|0;1;2;3|];;
[| (k*4*4 + j*4 + i) for i in y and j in y for k in y|];;
[%%expect{|
val y : int array = [|0; 1; 2; 3|]
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;


[| (k*4*4 + j*4 + i) for i = 0 to 3 and j = 0 to 3  for k = 0 to 3 |];;
[%%expect{|
- : int array =
[|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
  21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
  59; 60; 61; 62; 63|]
|}];;

[| (float_of_int (k*4*4 + j*4 + i)) for i = 0 to 3 and j = 0 to 3  for k = 0 to 3 |];;
[%%expect{|
- : float array =
[|0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11.; 12.; 13.; 14.; 15.; 16.;
  17.; 18.; 19.; 20.; 21.; 22.; 23.; 24.; 25.; 26.; 27.; 28.; 29.; 30.; 31.;
  32.; 33.; 34.; 35.; 36.; 37.; 38.; 39.; 40.; 41.; 42.; 43.; 44.; 45.; 46.;
  47.; 48.; 49.; 50.; 51.; 52.; 53.; 54.; 55.; 56.; 57.; 58.; 59.; 60.; 61.;
  62.; 63.|]
|}];;

let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| i for i in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : int array = [|1; 2; 3; 4; 5; 6; 7; 8|]
|}];;

let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| (i,j) for i in x and j in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : (int * int) array =
[|(1, 1); (2, 1); (1, 2); (2, 2); (3, 3); (4, 3); (3, 4); (4, 4); (5, 5);
  (6, 5); (5, 6); (6, 6); (7, 7); (8, 7); (7, 8); (8, 8)|]
|}];;

let y = [| [| [| 1;2;|]; [| 3;4; |] |]; [| [| 5;6;|]; [| 7;8; |] |] |];;
[| (string_of_int i,j) for i in x and j in x for x in z for z in y |];;
[%%expect{|
val y : int array array array =
  [|[|[|1; 2|]; [|3; 4|]|]; [|[|5; 6|]; [|7; 8|]|]|]
- : (string * int) array =
[|("1", 1); ("2", 1); ("1", 2); ("2", 2); ("3", 3); ("4", 3); ("3", 4);
  ("4", 4); ("5", 5); ("6", 5); ("5", 6); ("6", 6); ("7", 7); ("8", 7);
  ("7", 8); ("8", 8)|]
|}];;


(*Testcase with empty intermediate array.*)
[|  ( j * 3 + i)  for i = 0 to (j-1) for j = 0 to 2|];;
[%%expect{|
- : int array = [|3; 6; 7|]
|}];;

(* When clauses*)

[(j,i) for j = 0 to i when (i >= 4 && j >= 4) for i = 0 to 9 when (i mod 2 = 0)];;
[%%expect{|
- : (int * int) list =
[(4, 4); (4, 6); (5, 6); (6, 6); (4, 8); (5, 8); (6, 8); (7, 8); (8, 8)]
|}];;

[| (j,i) for j = 0 to i when (i >= 4 && j >= 4) for i = 0 to 9 when (i mod 2 = 0) |];;
[%%expect{|
- : (int * int) array =
[|(4, 4); (4, 6); (5, 6); (6, 6); (4, 8); (5, 8); (6, 8); (7, 8); (8, 8)|]
|}];;

[ (j,i) for j = 0 to i when (i > 4) for i = 0 to 10 when (j = 0) ];;
[%%expect{|
Line 1, characters 58-59:
1 | [ (j,i) for j = 0 to i when (i > 4) for i = 0 to 10 when (j = 0) ];;
                                                              ^
Error: Unbound value j
|}];;

[| (i,j) for i = 0 to 10 when (i mod 2 = 0) for j = 0 to 5 when (j = 1 || j = 2)|];;
[%%expect{|
- : (int * int) array =
[|(0, 1); (2, 1); (4, 1); (6, 1); (8, 1); (10, 1); (0, 2); (2, 2); (4, 2);
  (6, 2); (8, 2); (10, 2)|]
|}];;

[| (i) for i = 0 to 10 when (i mod 2 = 0)|];;
[%%expect{|
- : int array = [|0; 2; 4; 6; 8; 10|]
|}];;

[ (i,j,k) for i = 0 to 5 when (i mod 2 = 0)  for j = 0 to 5 when (j mod 2 = 0)  for k = 0 to 5 when (k mod 2 = 0)];;
[%%expect{|
- : (int * int * int) list =
[(0, 0, 0); (2, 0, 0); (4, 0, 0); (0, 2, 0); (2, 2, 0); (4, 2, 0); (0, 4, 0);
 (2, 4, 0); (4, 4, 0); (0, 0, 2); (2, 0, 2); (4, 0, 2); (0, 2, 2); (2, 2, 2);
 (4, 2, 2); (0, 4, 2); (2, 4, 2); (4, 4, 2); (0, 0, 4); (2, 0, 4); (4, 0, 4);
 (0, 2, 4); (2, 2, 4); (4, 2, 4); (0, 4, 4); (2, 4, 4); (4, 4, 4)]
|}];;

[| (i,j,k) for i = 0 to 5 when (i mod 2 = 0)  for j = 0 to 5 when (j mod 2 = 0)  for k = 0 to 5 when (k mod 2 = 0)|];;
[%%expect{|
- : (int * int * int) array =
[|(0, 0, 0); (2, 0, 0); (4, 0, 0); (0, 2, 0); (2, 2, 0); (4, 2, 0);
  (0, 4, 0); (2, 4, 0); (4, 4, 0); (0, 0, 2); (2, 0, 2); (4, 0, 2);
  (0, 2, 2); (2, 2, 2); (4, 2, 2); (0, 4, 2); (2, 4, 2); (4, 4, 2);
  (0, 0, 4); (2, 0, 4); (4, 0, 4); (0, 2, 4); (2, 2, 4); (4, 2, 4);
  (0, 4, 4); (2, 4, 4); (4, 4, 4)|]
|}];;

[ (i,j,k) for i = 0 to 5  and j = 0 to 5 and k = 0 to 5 when ((k mod 2 = 0) && (i mod 2 = 0) && (j mod 2 = 0))];;
[%%expect{|
- : (int * int * int) list =
[(0, 0, 0); (2, 0, 0); (4, 0, 0); (0, 2, 0); (2, 2, 0); (4, 2, 0); (0, 4, 0);
 (2, 4, 0); (4, 4, 0); (0, 0, 2); (2, 0, 2); (4, 0, 2); (0, 2, 2); (2, 2, 2);
 (4, 2, 2); (0, 4, 2); (2, 4, 2); (4, 4, 2); (0, 0, 4); (2, 0, 4); (4, 0, 4);
 (0, 2, 4); (2, 2, 4); (4, 2, 4); (0, 4, 4); (2, 4, 4); (4, 4, 4)]
|}];;

[| (i,j,k) for i = 0 to 5  and j = 0 to 5 and k = 0 to 5 when ((k mod 2 = 0) && (i mod 2 = 0) && (j mod 2 = 0)  )|];;
[%%expect{|
- : (int * int * int) array =
[|(0, 0, 0); (2, 0, 0); (4, 0, 0); (0, 2, 0); (2, 2, 0); (4, 2, 0);
  (0, 4, 0); (2, 4, 0); (4, 4, 0); (0, 0, 2); (2, 0, 2); (4, 0, 2);
  (0, 2, 2); (2, 2, 2); (4, 2, 2); (0, 4, 2); (2, 4, 2); (4, 4, 2);
  (0, 0, 4); (2, 0, 4); (4, 0, 4); (0, 2, 4); (2, 2, 4); (4, 2, 4);
  (0, 4, 4); (2, 4, 4); (4, 4, 4)|]
|}];;

[| (i,j,k) for i = 0 to 5 when ((k mod 2 = 0) && (i mod 2 = 0) && (j mod 2 = 0)) for j = 0 to 5 for k = 0 to 5 |];;
[%%expect{|
- : (int * int * int) array =
[|(0, 0, 0); (2, 0, 0); (4, 0, 0); (0, 2, 0); (2, 2, 0); (4, 2, 0);
  (0, 4, 0); (2, 4, 0); (4, 4, 0); (0, 0, 2); (2, 0, 2); (4, 0, 2);
  (0, 2, 2); (2, 2, 2); (4, 2, 2); (0, 4, 2); (2, 4, 2); (4, 4, 2);
  (0, 0, 4); (2, 0, 4); (4, 0, 4); (0, 2, 4); (2, 2, 4); (4, 2, 4);
  (0, 4, 4); (2, 4, 4); (4, 4, 4)|]
|}];;

let f f =
  [ (f i j k) for i = 0 to 3 when (i mod 2 = 0)  for j = 0 to 3 when (j mod 2 = 0)  for k = 0 to 3 when (k mod 2 = 0)];;
[%%expect{|
val f : (int -> int -> int -> 'a) -> 'a list = <fun>
|}];;

f (fun i j k -> (i,j,k) )
[%%expect{|
- : (int * int * int) list =
[(0, 0, 0); (2, 0, 0); (0, 2, 0); (2, 2, 0); (0, 0, 2); (2, 0, 2); (0, 2, 2);
 (2, 2, 2)]
|}];;

f (fun i j k -> i )
[%%expect{|
- : int list = [0; 2; 0; 2; 0; 2; 0; 2]
|}];;

f (fun i j k -> float_of_int i )
[%%expect{|
- : float list = [0.; 2.; 0.; 2.; 0.; 2.; 0.; 2.]
|}];;

f (fun i j k -> [|string_of_int i|] )
[%%expect{|
- : string array list =
[[|"0"|]; [|"2"|]; [|"0"|]; [|"2"|]; [|"0"|]; [|"2"|]; [|"0"|]; [|"2"|]]
|}];;


(*Make sure stuff is called in correct order/ correct number of times.*)

let var = ref [];;
let f x = var := x::!var; x;;
[%%expect{|
val var : '_weak1 list ref = {contents = []}
val f : '_weak2 -> '_weak2 = <fun>
|}];;

let z = [|1;2;3|];;
[| i  for  i in (f z) |];;
List.rev !var;;
[%%expect{|
val z : int array = [|1; 2; 3|]
- : int array = [|1; 2; 3|]
- : int array list = [[|1; 2; 3|]]
|}];;

var := [];;
let z = [|1;2;3|];;
[| i  for  i in (f z) for i = 0 to 1 |];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int array = [|1; 2; 3|]
- : int array = [|1; 2; 3; 1; 2; 3|]
- : int array list = [[|1; 2; 3|]; [|1; 2; 3|]]
|}];;

var := [];;
let z = [|1;2;3|];;
[| i  for  i in (f z) for i = 0 to 6 when (i mod 2 = 0) |];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int array = [|1; 2; 3|]
- : int array = [|1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3|]
- : int array list = [[|1; 2; 3|]; [|1; 2; 3|]; [|1; 2; 3|]; [|1; 2; 3|]]
|}];;

var := [];;
let z = [|1;2;3|];;
[| i  for  i in (f z) and i = 0 to 1 |];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int array = [|1; 2; 3|]
- : int array = [|1; 2; 3; 1; 2; 3|]
- : int array list = [[|1; 2; 3|]]
|}];;

var := [];;
let z = [|1;2;3|];;
[| i  for  i in (f z) and  i = 0 to 6 when (i mod 2 = 0) |];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int array = [|1; 2; 3|]
- : int array = [|2; 2; 2; 2; 2; 2; 2|]
- : int array list = [[|1; 2; 3|]]
|}];;

var := [];;
let z = [|[|1;2;3|];[|4;5;6|]|];;
[| i  for  i in (f y) for y in z |];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int array array = [|[|1; 2; 3|]; [|4; 5; 6|]|]
- : int array = [|1; 2; 3; 4; 5; 6|]
- : int array list = [[|1; 2; 3|]; [|4; 5; 6|]]
|}];;


let var = ref [];;
let f x = var := x::!var; x;;
[%%expect{|
val var : '_weak3 list ref = {contents = []}
val f : '_weak4 -> '_weak4 = <fun>
|}];;

let z = [1;2;3];;
[ i  for  i in (f z) ];;
List.rev !var;;
[%%expect{|
val z : int list = [1; 2; 3]
- : int list = [1; 2; 3]
- : int list list = [[1; 2; 3]]
|}];;

var := [];;
let z = [1;2;3];;
[ i  for  i in (f z) for i = 0 to 1 ];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int list = [1; 2; 3]
- : int list = [1; 2; 3; 1; 2; 3]
- : int list list = [[1; 2; 3]; [1; 2; 3]]
|}];;

var := [];;
let z = [1;2;3];;
[ i  for  i in (f z) for i = 0 to 6 when (i mod 2 = 0) ];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int list = [1; 2; 3]
- : int list = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1; 2; 3]
- : int list list = [[1; 2; 3]; [1; 2; 3]; [1; 2; 3]; [1; 2; 3]]
|}];;

var := [];;
let z = [1;2;3];;
[ i  for  i in (f z) and i = 0 to 1 ];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int list = [1; 2; 3]
- : int list = [1; 2; 3; 1; 2; 3]
- : int list list = [[1; 2; 3]]
|}];;

var := [];;
let z = [1;2;3];;
[ i  for  i in (f z) and  i = 0 to 6 when (i mod 2 = 0) ];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int list = [1; 2; 3]
- : int list = [2; 2; 2; 2; 2; 2; 2]
- : int list list = [[1; 2; 3]]
|}];;

var := [];;
let z = [[1;2;3];[4;5;6]];;
[ i  for  i in (f y) for y in z ];;
List.rev !var;;
[%%expect{|
- : unit = ()
val z : int list list = [[1; 2; 3]; [4; 5; 6]]
- : int list = [1; 2; 3; 4; 5; 6]
- : int list list = [[1; 2; 3]; [4; 5; 6]]
|}];;

let var = ref [];;
let f x = var := x::!var; x;;
[%%expect{|
val var : '_weak5 list ref = {contents = []}
val f : '_weak6 -> '_weak6 = <fun>
|}];;


var := [];;
[ i  for i = (f 0) to (f 5) ];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int list = [0; 1; 2; 3; 4; 5]
- : int list = [0; 5]
|}];;

var := [];;
[ i  for i = (f 0) to (f 5) and j = (f 3) to (f 4) ];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int list = [0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5]
- : int list = [3; 4; 0; 5]
|}];;

var := [];;
[ i  for i = (f 0) to (f 5) for j = (f 3) to (f 4) ];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int list = [0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5]
- : int list = [3; 4; 0; 5; 0; 5]
|}];;

var := [];;
[| i  for i = (f 0) to (f 5) |];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int array = [|0; 1; 2; 3; 4; 5|]
- : int list = [0; 5]
|}];;

var := [];;
[| i  for i = (f 0) to (f 5) and j = (f 3) to (f 4)  |];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int array = [|0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5|]
- : int list = [3; 4; 0; 5]
|}];;

var := [];;
[| i  for i = (f 0) to (f 5) for j = (f 3) to (f 4)  |];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int array = [|0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5|]
- : int list = [3; 4; 0; 5; 0; 5]
|}];;

var := [];;
[| i  for i = (f 5) downto (f 0) for j = (f 3) to (f 4) |];;
List.rev !var;;
[%%expect{|
- : unit = ()
- : int array = [|5; 4; 3; 2; 1; 0; 5; 4; 3; 2; 1; 0|]
- : int list = [3; 4; 5; 0; 5; 0]
|}];;
