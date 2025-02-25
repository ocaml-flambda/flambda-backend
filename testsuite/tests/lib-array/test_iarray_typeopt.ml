(* TEST
   arch_amd64;
   include stdlib_stable;
   flags = "-dlambda";
   expect;
*)

(* CR dkalinichenko: this test generates slightly different code
   on ARM64, so I disabled it there for now. Ideally, we'd split
   this for different architectures. *)

module Array = Stdlib.Array
open Stdlib_stable.Iarray

[%%expect {|
0
module Array = Array
0
|}]

(* Regression test showing that an [i]array of iarrays
   has element kind [addr].
 *)

let _ = [: [: :] :];;

[%%expect {|
(makearray_imm[addr] (makearray_imm[gen]))
- : 'a iarray iarray = [:[::]:]
|}]

let _ = [| [: :] |];;

[%%expect {|
(makearray[addr] (makearray_imm[gen]))
- : '_weak1 iarray array = [|[::]|]
|}]

(* Test that reading from an iarray generates an immutable load (iarray.get) *)

let arr = [: 1; 2; 3 :];;
[%%expect {|
(let (arr/379 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/379))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/379 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/379 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/379 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/379 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/379 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/379 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/381 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/381))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/381 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/381 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/381 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/381 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/381 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/381 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/383 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/383))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/383 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/383 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/383 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/383 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/383 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/383 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/385 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/385))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/385 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/385 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/385 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/385 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/385 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/385 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/387 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/387))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/387 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/387 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/387 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/387 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/387 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/387 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/388 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/388))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/388 = (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/389 =
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/388))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/389))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/389 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/389 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/389 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int, array kind int] arr/389 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/389 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int, array kind int] arr/389 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/390 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/390))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/390 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/390 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/390 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/390 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/390 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int, array kind int] mut_arr/390 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/442 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/442))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/442 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/442 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/442 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/442 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/442 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int, array kind int] mut_arr/442 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/443 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/443))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/443 = (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/444 =[intarray]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/443))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/444))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/444 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/444 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/444 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/444 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/444 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int, array kind int] mut_arr/444 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/445 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/445))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/445 = (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/446 =[intarray]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/445))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/446))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/446 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/446 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/446 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int, array kind int] mut_arr/446 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/446 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int, array kind int] mut_arr/446 1))
- : int = 2
|}]
