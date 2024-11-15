(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta -dlambda";
 {
   expect;
 }
*)

(* We implement the same functionality with pattern matching, projection,
   and check the lambda for correctness against unboxed records. *)

type t = #{ i : int ; j : int }
let add (#{ i ; j = _} as r) = i + r.#j
[%%expect{|
0
type t = #{ i : int; j : int; }
(let
  (add/280 =
     (function {nlocal = 0} r/283#(*, *) : int
       (+ (unboxed_product_field 0 #(*, *) r/283)
         (unboxed_product_field 1 #([int], [int]) r/283))))
  (apply (field_imm 1 (global Toploop!)) "add" add/280))
val add : t -> int = <fun>
|}]

let add2 x y = add x + add y
[%%expect{|
(let
  (add/280 = (apply (field_imm 0 (global Toploop!)) "add")
   add2/285 =
     (function {nlocal = 0} x/287#(*, *) y/288#(*, *) : int
       (+ (apply add/280 x/287) (apply add/280 y/288))))
  (apply (field_imm 1 (global Toploop!)) "add2" add2/285))
val add2 : t -> t -> int = <fun>
|}]


let add #( i, j ) = i + j
[%%expect{|
(let
  (add/289 =
     (function {nlocal = 0} param/292#(*, *) : int
       (+ (unboxed_product_field 0 #(*, *) param/292)
         (unboxed_product_field 1 #(*, *) param/292))))
  (apply (field_imm 1 (global Toploop!)) "add" add/289))
val add : #(int * int) -> int = <fun>
|}]
let add2 x y = add x + add y
[%%expect{|
(let
  (add/289 = (apply (field_imm 0 (global Toploop!)) "add")
   add2/293 =
     (function {nlocal = 0} x/294#(*, *) y/295#(*, *) : int
       (+ (apply add/289 x/294) (apply add/289 y/295))))
  (apply (field_imm 1 (global Toploop!)) "add2" add2/293))
val add2 : #(int * int) -> #(int * int) -> int = <fun>
|}]

let copy_i_to_j #{ i ; j } = #{ i; j = i }
[%%expect{|
(let
  (copy_i_to_j/296 =
     (function {nlocal = 0} param/300#(*, *)
       : #(*, *)(let (i/298 =a (unboxed_product_field 0 #(*, *) param/300))
                  (make_unboxed_product #([int], [int]) i/298 i/298))))
  (apply (field_imm 1 (global Toploop!)) "copy_i_to_j" copy_i_to_j/296))
val copy_i_to_j : t -> t = <fun>
|}]

(* this one is slightly different than the above and below - it
   calls unboxed_product_field twice rather than let-binding, but
   this should be inconsequential*)
let copy_i_to_j r = #{ r with j = r.#i }
[%%expect{|
(let
  (copy_i_to_j/302 =
     (function {nlocal = 0} r/303#(*, *)
       : #(*, *)(make_unboxed_product #([int], [int])
                  (unboxed_product_field 0 #([int], [int]) r/303)
                  (unboxed_product_field 0 #([int], [int]) r/303))))
  (apply (field_imm 1 (global Toploop!)) "copy_i_to_j" copy_i_to_j/302))
val copy_i_to_j : t -> t = <fun>
|}]

let copy_i_to_j (#(i, _j) : #(int * int)) = #(i, i)
[%%expect{|
(let
  (copy_i_to_j/305 =
     (function {nlocal = 0} param/308#(*, *)
       : #(*, *)(let (i/306 =a (unboxed_product_field 0 #(*, *) param/308))
                  (make_unboxed_product #([int], [int]) i/306 i/306))))
  (apply (field_imm 1 (global Toploop!)) "copy_i_to_j" copy_i_to_j/305))
val copy_i_to_j : #(int * int) -> #(int * int) = <fun>
|}]

(* compare against @@unboxed records *)
type t = #{ s : string }

let t = #{ s = "hi" }
[%%expect{|
0
type t = #{ s : string; }
(let (t/311 = "hi") (apply (field_imm 1 (global Toploop!)) "t" t/311))
val t : t = #{s = "hi"}
|}]

let s = #{ s = "hi" }.#s
[%%expect{|
(let (s/313 = "hi") (apply (field_imm 1 (global Toploop!)) "s" s/313))
val s : string = "hi"
|}]

type t = { s : string } [@@unboxed]
let t = { s = "hi" }
[%%expect{|
0
type t = { s : string; } [@@unboxed]
(let (t/319 = "hi") (apply (field_imm 1 (global Toploop!)) "t" t/319))
val t : t = {s = "hi"}
|}]

let s = { s = "hi"}.s
[%%expect{|
(let (s/321 = "hi") (apply (field_imm 1 (global Toploop!)) "s" s/321))
val s : string = "hi"
|}]

