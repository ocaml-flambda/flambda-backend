(* TEST

readonly_files = "original.ml middle.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "original.ml"
*** ocamlc.byte
module = "middle.ml"
**** script
script = "rm -f original.cmi"
***** expect
*)


#directory "ocamlc.byte";;
#load "original.cmo"
#load "middle.cmo"

let x:'a. 'a Middle.t =
  let _r = ref 0 in
  Middle.T
[%%expect {|
val x : 'a Middle.t = Middle.T
|}]


let () = Middle.(g x)
[%%expect {|
|}]

let () = Middle.(f x)
[%%expect {|
Line 1, characters 19-20:
1 | let () = Middle.(f x)
                       ^
Error: This expression has type (module Original.T)
       but an expression was expected of type
         (module Original.T with type t = int)
|}]

let () = Middle.f (module struct end)
[%%expect {|
Line 1, characters 26-36:
1 | let () = Middle.f (module struct end)
                              ^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig end is not included in Original.T
|}]

let foo (x : Middle.pack1) =
  let module M = (val x) in
  ()
[%%expect {|
Line 2, characters 17-24:
2 |   let module M = (val x) in
                     ^^^^^^^
Error: The type of this packed module refers to Original.T, which is missing
|}]

let foo (x : Middle.pack2) =
  let module M = (val x) in
  ()
[%%expect {|
Line 2, characters 17-24:
2 |   let module M = (val x) in
                     ^^^^^^^
Error: The type of this packed module refers to Original.T, which is missing
|}]

module type T1 = sig type t = int end
let foo x = (x : Middle.pack1 :> (module T1))
[%%expect {|
module type T1 = sig type t = int end
Line 2, characters 12-45:
2 | let foo x = (x : Middle.pack1 :> (module T1))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type Middle.pack1 = (module Original.T with type t = int)
       is not a subtype of (module T1)
|}]

module type T2 = sig module M : sig type t = int end end
let foo x = (x : Middle.pack2 :> (module T2))
[%%expect {|
module type T2 = sig module M : sig type t = int end end
Line 2, characters 12-45:
2 | let foo x = (x : Middle.pack2 :> (module T2))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type Middle.pack2 = (module Middle.T with type M.t = int)
       is not a subtype of (module T2)
|}]

(* Check the detection of type kind in type-directed disambiguation . *)
let t = Middle.r.Middle.x
[%%expect {|
val t : unit = ()
|}]

let k = match Middle.s with Middle.S -> ()
[%%expect {|
val k : unit = ()
|}]

let f (x: Middle.u) = x
[%%expect {|
Line 1, characters 6-23:
1 | let f (x: Middle.u) = x
          ^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of Original.u is any, because
         the .cmi file for Original.u is missing.
       But the layout of Original.u must be a sublayout of value, because
         it has to be value for the V1 safety check.
       No .cmi file found containing Original.u.
       Hint: Adding "original" to your dependencies might help.
|}]
