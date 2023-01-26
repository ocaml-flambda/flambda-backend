(* TEST
   * expect
*)

(* What happens if the user tries to write one of the ocaml-jst extensions in
   terms of extension nodes but messes up?  In practice we don't expect to ever
   see these errors, but one never knows (and a bug in our desugaring could
   cause them).  Each let-binding is named after the constructor in
   [extensions.ml] representing this particular error. *)

let _malformed_extension_has_payload = [%extension.something "no payloads"] ();;
[%%expect{|
Line 1, characters 41-60:
1 | let _malformed_extension_has_payload = [%extension.something "no payloads"] ();;
                                             ^^^^^^^^^^^^^^^^^^^
Error: Extension extension nodes are not allowed to have a payload, but "extension.something" does
|}];;

let _malformed_extensions_wrong_arguments = [%extension.something] "two" "arguments";;
[%%expect{|
Line 1, characters 46-65:
1 | let _malformed_extensions_wrong_arguments = [%extension.something] "two" "arguments";;
                                                  ^^^^^^^^^^^^^^^^^^^
Error: Extension extension nodes must be applied to exactly one unlabeled argument, but "extension.something" was applied to 2 arguments
|}];;

let _unknown_extension = [%extension.this_extension_doesn't_exist] ();;
[%%expect{|
Line 1, characters 25-69:
1 | let _unknown_extension = [%extension.this_extension_doesn't_exist] ();;
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unknown extension "this_extension_doesn't_exist" referenced via an [%extension.this_extension_doesn't_exist] extension node
|}];;

let _disabled_extension = [%extension.comprehensions] ();;
[%%expect{|
Line 1, characters 26-56:
1 | let _disabled_extension = [%extension.comprehensions] ();;
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions" is disabled and cannot be used
|}];;

let _unnamed_extension = [%extension] ();;
[%%expect{|
Line 1, characters 25-40:
1 | let _unnamed_extension = [%extension] ();;
                             ^^^^^^^^^^^^^^^
Error: Cannot have an extension node named [%extension]
|}];;

let _bad_introduction = [%extension.something.nested] ();;
[%%expect{|
Line 1, characters 24-56:
1 | let _bad_introduction = [%extension.something.nested] ();;
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "something" was referenced improperly; it started with an [%extension.something.nested] extension node, not an [%extension.something] one
|}];;

(* If you don't use these as applications, they don't pass through the modular
   extensions machinery and fail with a normal OCaml error *)

let _ = [%extension];;
[%%expect{|
Line 1, characters 10-19:
1 | let _ = [%extension];;
              ^^^^^^^^^
Error: Uninterpreted extension 'extension'.
|}];;

let _ = [%extension "payload"];;
[%%expect{|
Line 1, characters 10-19:
1 | let _ = [%extension "payload"];;
              ^^^^^^^^^
Error: Uninterpreted extension 'extension'.
|}];;
