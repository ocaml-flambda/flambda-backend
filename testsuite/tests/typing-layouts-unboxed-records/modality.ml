(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* This test is adapted from
   [testsuite/tests/typing-local/local.ml].

   CR layouts v7.2: once unboxed records are in stable, fold this test back into
   the original or move it to [typing-layouts-products]. *)
type 'a gbl = #{ global_ gbl : 'a }
[%%expect{|
type 'a gbl = #{ global_ gbl : 'a; }
|}]

let foo (local_ x) = x.#gbl
[%%expect{|
val foo : local_ 'a gbl -> 'a = <fun>
|}]
let foo y =
  let x = local_ #{ gbl = y } in
  x.#gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ #{ gbl }) = gbl
[%%expect{|
val foo : local_ 'a gbl -> 'a = <fun>
|}]
let foo y =
  let #{ gbl } = local_ #{ gbl = y } in
  gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ gbl) =
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 2, characters 13-16:
2 |   let _ = #{ gbl } in
                 ^^^
Error: This value escapes its region.
|}]
let foo () =
  let gbl = local_ ref 5 in
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 3, characters 13-16:
3 |   let _ = #{ gbl } in
                 ^^^
Error: This value escapes its region.
|}]

(* Global fields are preserved in module inclusion *)
module M : sig
  type t = #{ global_ foo : string }
end = struct
  type t = #{ foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ foo : string; } end
       is not included in
         sig type t = #{ global_ foo : string; } end
       Type declarations do not match:
         type t = #{ foo : string; }
       is not included in
         type t = #{ global_ foo : string; }
       Fields do not match:
         "foo : string;"
       is not the same as:
         "global_ foo : string;"
       The second is global_ and the first is not.
|}]

module M : sig
  type t = #{ foo : string }
end = struct
  type t = #{ global_ foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ global_ foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ global_ foo : string; } end
       is not included in
         sig type t = #{ foo : string; } end
       Type declarations do not match:
         type t = #{ global_ foo : string; }
       is not included in
         type t = #{ foo : string; }
       Fields do not match:
         "global_ foo : string;"
       is not the same as:
         "foo : string;"
       The first is global_ and the second is not.
|}]
