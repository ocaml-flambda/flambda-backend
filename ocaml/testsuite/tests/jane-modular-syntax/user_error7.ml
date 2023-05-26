(* TEST
   * expect
*)

(* If we use extension nodes outside of the context they are expected,
   they aren't interpreted by the modular syntax machinery and fail with
   a normal OCaml error *)

(* Extension node in an interpreted context would be:

include struct
  [%%jane]
  let f = ()
end

We don't include this test as it's a parsing error.
*)

[%%jane];;
[%%expect{|
Line 1, characters 3-7:
1 | [%%jane];;
       ^^^^
Error: Uninterpreted extension 'jane'.
|}];;

[%%jane "payload"];;
[%%expect{|
Line 1, characters 3-7:
1 | [%%jane "payload"];;
       ^^^^
Error: Uninterpreted extension 'jane'.
|}];;
