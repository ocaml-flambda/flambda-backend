(* TEST
   expect;
*)

(* For backwards-compatibility, we want to keep the same error message when the extension
   is not enabled and an underscore is detected outside of a pattern. Note, however,
   that the new implementation errors in the type checker rather than the parser. *)

let underscore () = _
[%%expect{|
Line 1, characters 20-21:
1 | let underscore () = _
                        ^
Error: Syntax error: wildcard "_" not expected.
|}]

let overwriting t = overwrite_ t with (a, b)
[%%expect{|
Line 1, characters 20-44:
1 | let overwriting t = overwrite_ t with (a, b)
                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The overwriting extension is disabled
       To enable it, pass the '-extension overwriting' flag
|}]
