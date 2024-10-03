(* TEST
   include stdlib_stable;
   expect;
*)

(* Let bindings *)
let foo @ deterministic = ()
[%%expect{|
val foo : unit = ()
|}]

let foo =
  let _ @ constant = () in ()
[%%expect{|
val foo : unit = ()
|}]

let foo =
  let _ @ constant deterministic = () in ()
[%%expect{|
val foo : unit = ()
|}]

let foo : unit @@ deterministic = ()
[%%expect{|
val foo : unit = ()
|}]

let foo =
  let _ : unit @@ constant = () in ()
[%%expect{|
val foo : unit = ()
|}]

type r = {
  x : string @@ deterministic
}
[%%expect{|
type r = { x : string @@ portable coordinate_nothing; }
|}]

type r = {
  x : string @@ constant
}
[%%expect{|
type r = { x : string @@ contended coordinated_none; }
|}]

(* Error with aliases *)

(* CR modes: error message could give a hint with the unfolding of the alias *)
let foo @ constant = ()
[%%expect{|
Line 1, characters 4-23:
1 | let foo @ constant = ()
        ^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

type r = {mutable a : int}

let foo =
  let r = {a = 42} in
  let bar () = r.a <- 2 in
  let _ @ deterministic = bar in
  ()
[%%expect{|
type r = { mutable a : int; }
Line 6, characters 26-29:
6 |   let _ @ deterministic = bar in
                              ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo @ deterministic portable = ()
[%%expect{|
Line 1, characters 24-32:
1 | let foo @ deterministic portable = ()
                            ^^^^^^^^
Error: The portability axis has already been specified.
|}]

let foo @ deterministic deterministic = ()
[%%expect{|
Line 1, characters 24-37:
1 | let foo @ deterministic deterministic = ()
                            ^^^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

let foo @ portable deterministic = ()
[%%expect{|
Line 1, characters 19-32:
1 | let foo @ portable deterministic = ()
                       ^^^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

let foo : type a. a -> a @@ constant = fun x -> x
[%%expect{|
Line 1, characters 4-49:
1 | let foo : type a. a -> a @@ constant = fun x -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]
