(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

type 'a r = #{ a : 'a }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : 'a r -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
and 'a ok = F : { x : 'a r } -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a ok = F : { x : 'a r; } -> 'a ok [@@unboxed]
|}]

type 'a r = #{ a : 'a }
type bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-38:
2 | type bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
type bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-46:
2 | type bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and 'a r2 = #{ a : 'a r }
and bad = F : 'a r2 -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-38:
3 | and bad = F : 'a r2 -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-45:
2 | and bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* CR layouts v12: Once we allow products containing void in unboxed GADTs,
   we'll have to make sure the below fails separability checking: *)
type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-35:
2 | and 'a r = #{ a : 'a ; v : t_void }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'a r is '_representable_layout_1 & '_representable_layout_2
         because it is an unboxed record.
       But the layout of 'a r must be a sublayout of value
         because it's the type of a constructor field.
|}]

type t_void : void
and 'a r = #{ a : 'a ; v : t_void }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-35:
2 | and 'a r = #{ a : 'a ; v : t_void }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'a r is '_representable_layout_3 & '_representable_layout_4
         because it is an unboxed record.
       But the layout of 'a r must be a sublayout of value
         because it is the type of record field x.
|}]
