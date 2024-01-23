(* TEST
   * expect
*)

type r = string @ local -> string @ local
[%%expect{|
type r = local_ string -> local_ string
|}]

let foo () =
  let e @ local = "hello" in
  ref e
[%%expect{|
Line 3, characters 6-7:
3 |   ref e
          ^
Error: This value escapes its region
|}]

let foo (x @ local) =
  ref x
[%%expect{|
Line 2, characters 6-7:
2 |   ref x
          ^
Error: This value escapes its region
|}]

let foo @ local = fun (x @ local) -> ()
[%%expect{|
Line 1, characters 4-32:
1 | let foo @ local (x @ local) = ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(* Wrong mode names is a type error, not a parse error *)
type r = string @ foo -> string @ bar
[%%expect{|
Line 1, characters 18-21:
1 | type r = string @ foo -> string @ bar
                      ^^^
Error: Unrecognized mode name.
|}]

let foo () =
  let e @ foo bar = "hello" in ()
[%%expect{|
Line 2, characters 10-13:
2 |   let e @ foo = "hello" in ()
              ^^^
Error: Unrecognized mode for coercion.
|}]
;;
(* In the following, "unique" is parsed as function argument *)
let r @ local unique = "hello"
[%%expect{|
Line 1, characters 4-30:
1 | let r @ local unique = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

let foo () =
  let r @ local unique = "hello" in
  let _ = r in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Mode expression longer than a single LIDENT must be enclosed in parens. *)
let foo () =
  let e @local unique = "hello" in
  ref e
[%%expect{|
Line 3, characters 6-7:
3 |   ref e
          ^
Error: This value escapes its region
|}]

(* TODO: maybe this syntax would be better?
let (e @ local unique) arg = ... in
*)

(* Each axis can only be specified once. This error should evolve in the future
   after we introduce proper mode expressions *)
type r = string @ local local -> string @ local
[%%expect{|
Line 1, characters 25-30:
1 | type r = string @ (local local) -> string @ local
                             ^^^^^
Error: The locality axis has already been specified.
|}]

(* TODO: move modalities to the right:
type r = {x : string @ global}
type r = {x : (string @ local -> string) @ global }
This requires refactoring the parser.
*)
type r = {x @ global : string }
[%%expect{|
type r = { global_ x : string; }
|}]

type r = {x @ global global : string}
[%%expect{|
Line 1, characters 22-28:
1 | type r = {x @ (global global) : string}
                          ^^^^^^
Error: Modality in the locality axis has already be used.
|}]

type r = {x @ foo : string}
[%%expect{|
Line 1, characters 14-17:
1 | type r = {x @ foo : string}
                  ^^^
Error: Unrecognized modality name.
|}]

(* Mode coercion *)
let foo () =
  let x = ref "hello" in
  (x : _ @ local)
[%%expect{|
val foo : unit -> local_ string ref = <fun>
|}]

let foo () = @local unique
  let x = ref "hello" in
  x
[%%expect{|
Line 3, characters 2-3:
3 |   x
      ^
Error: Found a shared value where a unique value was expected
|}]


(* Mixing legacy syntax and new syntax are allowed for simplicity *)

(* Examples that would trigger syntax error (not type error): *)
type r = local_ string @ unique -> unique_ string @ local
[%%expect{|
type r = local_ unique_ string -> local_ unique_ string
|}]

let local_ x @ unique = "hello"
[%%expect{|
Line 1, characters 4-31:
1 | let local_ x @ unique = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

type r = {global_ x @ global : string}
[%%expect{|
Line 1, characters 22-28:
1 | type r = {global_ x @ global : string}
                          ^^^^^^
Error: Modality in the locality axis has already be used.
|}]

type r = local_ string -> string @ local -> string @ local
[%%expect{|
type r = local_ string -> local_ string -> local_ string
|}]