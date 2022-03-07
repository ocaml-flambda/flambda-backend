(* TEST
   * expect *)

type fn = string -> int
type lfn = (string[@ocaml.local]) -> int
type lfn' = local_ string -> int
[%%expect{|
type fn = string -> int
type lfn = (string [@ocaml.local]) -> int
Line 3, characters 12-25:
3 | type lfn' = local_ string -> int
                ^^^^^^^^^^^^^
Error: The type constructor string expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]

let cast (x : fn) = (x : lfn)
[%%expect{|
Line 1, characters 21-22:
1 | let cast (x : fn) = (x : lfn)
                         ^
Error: This expression has type fn = string -> int
       but an expression was expected of type
         lfn = (string [@ocaml.local]) -> int
|}]

let local_ref (f : lfn -> unit) =
  f (fun s -> let _ = [|s;s;s|] in 1)

[%%expect{|
Line 2, characters 22-31:
2 |   f (fun s -> let _ = [|s;s;s|] in 1)
                          ^^^^^^^^^
Error: Local allocation required but '-extension local' not enabled
|}]
