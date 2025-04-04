(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

type r = { i : int; j : int }
type t = (r# array, r#) idx
[%%expect{|
type r = { i : int; j : int; }
type t = (r# array, r#) idx
|}]

type 'a r = { a : 'a; j : int }

let f () = idx_ .a
[%%expect{|
type 'a r = { a : 'a; j : int; }
>> Fatal error: transl unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type t1 = { mutable a : string; b : int }
type t2 = { mutable a : string; b : int; c : string }

let a () = idx_ .a
let b () = idx_ .b
[%%expect{|
type t1 = { mutable a : string; b : int; }
type t2 = { mutable a : string; b : int; c : string; }
>> Fatal error: transl unimplemented
Uncaught exception: Misc.Fatal_error

|}]

type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
type ('c, 'b, 'a) i = ('a, 'b) idx
[%%expect{|
type ('c, 'b, 'a) mi = ('a, 'b) mut_idx
type ('c, 'b, 'a) i = ('a, 'b) idx
|}]

let a () : (t1, _) mut_idx = idx_ .a
let b () : (float, int, t1) i = idx_ .b
[%%expect{|
>> Fatal error: transl unimplemented
Uncaught exception: Misc.Fatal_error

|}]

let b () : (_, _, t1) i = idx_ .c
[%%expect{|
Line 38, characters 32-33:
38 | let b () : (_, _, t1) i = idx_ .c
                                     ^
Error: This expression is a field index with base  type "t1"
       There is no field "c" within type "t1"
|}, Principal{|
Line 51, characters 32-33:
51 | let b () : (_, _, t1) i = idx_ .c
                                     ^
Error: This expression is a field index with base  type "t1"
       There is no field "c" within type "t1"
|}]

let f a = idx_ .i .# blah
[%%expect{|
>> Fatal error: unimplemented: type unboxed accesses
Uncaught exception: Misc.Fatal_error

|}]

type pt = { x : int; y : int }
type 'a r = { p : pt#; q : pt# }
let f () = idx_.start.#x
[%%expect{|
type pt = { x : int; y : int; }
Line 2, characters 0-32:
2 | type 'a r = { p : pt#; q : pt# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "pt#" has layout "value & value".
       Records may not yet contain types of this layout.
|}]
