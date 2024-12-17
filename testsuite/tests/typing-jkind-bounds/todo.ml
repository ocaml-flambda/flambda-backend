(* TEST
    expect;
*)

(* TODO: find a home for these *)

type ('a : immutable_data) t : immutable_data = { x : 'a list }
[%%expect {|
Line 1, characters 0-63:
1 | type ('a : immutable_data) t : immutable_data = { x : 'a list }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type ('a : immutable_data) t : immutable_data = { x : 'a option }
type ('a : immutable_data) t : immutable_data = { x : 'a }
type ('a : immutable_data) t : immutable_data = 'a list
[%%expect {|
type ('a : immutable_data) t = { x : 'a option; }
type ('a : immutable_data) t = { x : 'a; }
type ('a : immutable_data) t = 'a list
|}]
