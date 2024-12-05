(* TEST
 expect;
*)

(* Types *)
type t = #{ a : int }
[%%expect{|
Line 1, characters 0-21:
1 | type t = #{ a : int }
    ^^^^^^^^^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Construction *)
let _ = #{ u = () }
[%%expect{|
Line 1, characters 8-19:
1 | let _ = #{ u = () }
            ^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Field *)
let get r = r.#x
[%%expect{|
Line 1, characters 12-16:
1 | let get r = r.#x
                ^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

(* Patterns *)
let #{ u = () } = ()
[%%expect{|
Line 1, characters 4-15:
1 | let #{ u = () } = ()
        ^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]
