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

let f a = idx_ .i .# blah
[%%expect{|
>> Fatal error: transl unimplemented
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
