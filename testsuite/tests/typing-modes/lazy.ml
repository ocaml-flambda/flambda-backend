(* TEST
    expect;
*)

(* lazy expression is legacy *)
let u =
    let _x @ portable = lazy "hello" in
    ()
[%%expect{|
Line 2, characters 24-36:
2 |     let _x @ portable = lazy "hello" in
                            ^^^^^^^^^^^^
Error: This value is nonportable but expected to be portable.
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
