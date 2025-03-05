(* TEST
   expect;
*)

(* Test that "with" syntax isn't allowed in locations that expect a right-kind *)

(* CR layouts v2.8: these errors should talk about kinds, not modes *)
type ('a : immutable_data with int) t
[%%expect {|
Line 1, characters 31-34:
1 | type ('a : immutable_data with int) t
                                   ^^^
Error: 'with' syntax is not allowed on a right mode.
|}]

type 'a t = { x : ('a : value mod portable with int) }
[%%expect {|
Line 1, characters 48-51:
1 | type 'a t = { x : ('a : value mod portable with int) }
                                                    ^^^
Error: 'with' syntax is not allowed on a right mode.
|}]

val foo : ('a : value mod portable with 'a). 'a -> unit
[%%expect {|
Line 1, characters 40-42:
1 | val foo : ('a : value mod portable with 'a). 'a -> unit
                                            ^^
Error: 'with' syntax is not allowed on a right mode.
|}]

let foo (type a : value mod portable with int option) (x : a) = ()
[%%expect {|
Line 1, characters 42-52:
1 | let foo (type a : value mod portable with int option) (x : a) = ()
                                              ^^^^^^^^^^
Error: 'with' syntax is not allowed on a right mode.
|}]

let foo (x : ('a : value mod portable with 'b)) (y : 'b) = ()
[%%expect {|
Line 1, characters 43-45:
1 | let foo (x : ('a : value mod portable with 'b)) (y : 'b) = ()
                                               ^^
Error: 'with' syntax is not allowed on a right mode.
|}]
