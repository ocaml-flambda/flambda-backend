(* TEST
 expect;
*)

type t = (unit, unit, unit, unit) bar
;;
(* PR#7315: we expect the error location on "bar" instead of "(...) bar" *)
[%%expect{|
Line 1, characters 34-37:
1 | type t = (unit, unit, unit, unit) bar
                                      ^^^
Error: Unbound type constructor "bar"
|}];;

function (x :
#bar) -> ();;
(* we expect the location on "bar" instead of "#bar" *)
[%%expect{|
Line 2, characters 1-4:
2 | #bar) -> ();;
     ^^^
Error: Unbound class type "bar"
|}];;

function
#bar -> ()
;;
(* we expect the location on "bar" instead of "#bar" *)
[%%expect{|
Line 2, characters 1-4:
2 | #bar -> ()
     ^^^
Error: Unbound type constructor "bar"
|}];;

new bar;;
(* we expect the location on "bar" instead of "new bar" *)
[%%expect{|
Line 1, characters 4-7:
1 | new bar;;
        ^^^
Error: Unbound class "bar"
|}];;

type t =
  | Foo of unit [@deprecated]
  | Bar;;
#warnings "@3";;
let x =
Foo ();;

[%%expect{|
type t = Foo of unit | Bar
Unknown directive "warnings".
|}];;
function
Foo _ -> () | Bar -> ();;

[%%expect{|
Line 2, characters 0-3:
2 | Foo _ -> () | Bar -> ();;
    ^^^
Alert deprecated: Foo

- : t -> unit = <fun>
|}];;


open Foo;;
(* the error location should be on "Foo" *)
[%%expect{|
Line 1, characters 5-8:
1 | open Foo;;
         ^^^
Error: Unbound module "Foo"
|}];;

#warnings "@33";; (* unused open statement *)
include (struct
open List
end);;
(* here we expect the error location to be
   on "open List" as whole rather than "List" *)
[%%expect{|
Unknown directive "warnings".
|}];;

type unknown += Foo;;
(* unknown, not the whole line *)
[%%expect{|
Line 1, characters 5-12:
1 | type unknown += Foo;;
         ^^^^^^^
Error: Unbound type constructor "unknown"
|}];;

type t = ..;;
type t +=
Foo = Foobar;;
(* Foobar, not the whole line *)
[%%expect{|
type t = ..
Line 3, characters 6-12:
3 | Foo = Foobar;;
          ^^^^^^
Error: Unbound constructor "Foobar"
|}];;
