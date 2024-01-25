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


(* Wrong mode names is a type error, not a parse error *)
type r = string @ foo -> string @ bar
[%%expect{|
Line 1, characters 18-21:
1 | type r = string @ foo -> string @ bar
                      ^^^
Error: Unrecognized mode name.
|}]

let foo () =
  let e @ foo = "hello" in ()
[%%expect{|
Line 2, characters 10-13:
2 |   let e @ foo = "hello" in ()
              ^^^
Error: Unrecognized mode for coercion.
|}]
;;

(* in the following, unique is parsed as mode on [foo], instead of argument to
   the function [foo]. *)
let r @ local unique = "hello"
[%%expect{|
Line 1, characters 4-30:
1 | let r @ local unique = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(* If you want [foo] to take argument, write like this: *)
let foo @ local = fun unique-> ()
[%%expect{|
Line 1, characters 4-33:
1 | let foo @ local = fun unique-> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]


let foo () =
  let r @ local unique = "hello" in
  let _ = r in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let e @local unique = "hello" in
  ref e
[%%expect{|
Line 3, characters 6-7:
3 |   ref e
          ^
Error: This value escapes its region
|}]

let foo () =
  let e : string @ local unique = "hello" in
  ref e
[%%expect{|
Line 3, characters 6-7:
3 |   ref e
          ^
Error: This value escapes its region
|}]

(* Each axis can only be specified once. This error should evolve in the future
   after we introduce proper mode expressions *)
type r = string @ local local -> string @ local
[%%expect{|
Line 1, characters 24-29:
1 | type r = string @ local local -> string @ local
                            ^^^^^
Error: The locality axis has already been specified.
|}]

type r = {x : string @ global }
[%%expect{|
type r = { global_ x : string; }
|}]

type r = {x : string @ global global }
[%%expect{|
Line 1, characters 30-36:
1 | type r = {x : string @ global global }
                                  ^^^^^^
Error: Modality in the locality axis has already be used.
|}]

type r = {x : string @ foo}
[%%expect{|
Line 1, characters 23-26:
1 | type r = {x : string @ foo}
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

let foo () =
  let x = ref "hello" in
  (x : _ @ local unique)
[%%expect{|
Line 3, characters 3-4:
3 |   (x : _ @ local unique)
       ^
Error: Found a shared value where a unique value was expected
|}]


(* Mixing legacy syntax and new syntax are allowed, just for the simplicity of
   parsing *)
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

type r = {global_ x : string @ global}
[%%expect{|
Line 1, characters 31-37:
1 | type r = {global_ x : string @ global}
                                   ^^^^^^
Error: Modality in the locality axis has already be used.
|}]

type r = local_ string -> string @ local -> string @ local
[%%expect{|
type r = local_ string -> local_ string -> local_ string
|}]