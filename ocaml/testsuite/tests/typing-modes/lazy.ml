(* TEST
    expect;
*)

(* lazy expression is legacy *)
let x @ portable = lazy "hello"
(* CR zqian: this should fail. *)
[%%expect{|
val x : string lazy_t = lazy "hello"
|}]

(* lazy body is legacy *)
let x = lazy ("hello" : _ @@ local)
[%%expect{|
Line 1, characters 13-35:
1 | let x = lazy ("hello" : _ @@ local)
                 ^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* Forcing lazy gives legacy, but that's in stdlib and not compiler *)
