(* TEST
 expect;
*)

type unit_u : void

(* All-void records are not allowed *)
type u1 = #{ a: unit_u }
type u2 = #{ a: unit_u; b: unit_u }
type u3 = { a : unit_u } [@@unboxed]
type u4 = #{ a: u2 }
type u5 = #{ a: u3 }
[%%expect{|
type unit_u : void
type u1 = #{ a : unit_u; }
type u2 = #{ a : unit_u; b : unit_u; }
type u3 = { a : unit_u; } [@@unboxed]
type u4 = #{ a : u2; }
type u5 = #{ a : u3; }
|}]

type bad = { a : unit_u }
[%%expect{|
Line 1, characters 0-25:
1 | type bad = { a : unit_u }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : #(unit_u * unit_u) }
[%%expect{|
Line 1, characters 0-37:
1 | type bad = { a : #(unit_u * unit_u) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u1 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u1 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u2 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u2 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u3 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u3 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u4 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u4 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
type bad = { a : u5 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { a : u5 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
