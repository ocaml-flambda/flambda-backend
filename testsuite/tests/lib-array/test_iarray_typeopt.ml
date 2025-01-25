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
(let (arr/374 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/374))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/374 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/374 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/374 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/374 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/374 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/374 1))
- : int = 2
|}]

type 'a alias = 'a iarray
let arr : int alias = [: 1; 2; 3 :];;
[%%expect {|
0
type 'a alias = 'a iarray
(let (arr/376 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/376))
val arr : int alias = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/376 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/376 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/376 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/376 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/376 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/376 1))
- : int = 2
|}]

let mut_arr = [| 1; 2; 3 |];;
let arr = of_array mut_arr;;
[%%expect {|
(let (mut_arr/377 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/377))
val mut_arr : int array = [|1; 2; 3|]
(let
  (mut_arr/377 = (apply (field_imm 0 (global Toploop!)) "mut_arr")
   arr/378 =
     (apply (field_imm 13 (global Stdlib_stable__Iarray!)) mut_arr/377))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/378))
val arr : int iarray = [:1; 2; 3:]
|}];;

let _ = arr.:(1);;
[%%expect {|
(let (arr/378 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/378 1))
- : int = 2
|}]

let _ = get arr 1;;
[%%expect {|
(let (arr/378 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.get[int indexed by int] arr/378 1))
- : int = 2
|}]

let _ = unsafe_get arr 1;;
[%%expect {|
(let (arr/378 = (apply (field_imm 0 (global Toploop!)) "arr"))
  (iarray.unsafe_get[int indexed by int] arr/378 1))
- : int = 2
|}]

(* And check that arrays are still mutable loads (array.get) *)

let mut_arr = [| 1; 2; 3 |];;
[%%expect {|
(let (mut_arr/379 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/379))
val mut_arr : int array = [|1; 2; 3|]
|}]

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/379 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/379 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/379 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/379 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/379 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/379 1))
- : int = 2
|}]

type 'a alias = 'a array
let mut_arr : int alias = [| 1; 2; 3 |];;
[%%expect {|
0
type 'a alias = 'a array
(let (mut_arr/431 =[intarray] (makearray[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/431))
val mut_arr : int alias = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/431 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/431 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/431 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/431 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/431 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/431 1))
- : int = 2
|}]

let arr = [: 1; 2; 3 :];;
let mut_arr = to_array arr;;
[%%expect {|
(let (arr/432 = (makearray_imm[int] 1 2 3))
  (apply (field_imm 1 (global Toploop!)) "arr" arr/432))
val arr : int iarray = [:1; 2; 3:]
(let
  (arr/432 = (apply (field_imm 0 (global Toploop!)) "arr")
   mut_arr/433 =[intarray]
     (apply (field_imm 12 (global Stdlib_stable__Iarray!)) arr/432))
  (apply (field_imm 1 (global Toploop!)) "mut_arr" mut_arr/433))
val mut_arr : int array = [|1; 2; 3|]
|}];;

let _ = mut_arr.(1);;
[%%expect {|
(let (mut_arr/433 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/433 1))
- : int = 2
|}]

let _ = Array.get mut_arr 1;;
[%%expect {|
(let (mut_arr/433 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.get[int indexed by int] mut_arr/433 1))
- : int = 2
|}]

let _ = Array.unsafe_get mut_arr 1;;
[%%expect {|
(let (mut_arr/433 = (apply (field_imm 0 (global Toploop!)) "mut_arr"))
  (array.unsafe_get[int indexed by int] mut_arr/433 1))
- : int = 2
|}]
