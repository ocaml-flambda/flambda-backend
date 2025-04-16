(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* This file is meant to read in tandem with [magic_is_a_value.ml],
   showing that our special treatment of [Obj.magic] is internal-only
   and can't be used in upstream-compatible code.
 *)

(* not really an application, since Obj.magic doesn't do anything *)
(* Nonetheless, in upstream-compatible code, we expect the type vars
   to not be generalized. *)
let f : 'a. 'a -> 'a = Obj.magic Fun.id
[%%expect {|
Line 1, characters 23-39:
1 | let f : 'a. 'a -> 'a = Obj.magic Fun.id
                           ^^^^^^^^^^^^^^^^
Error: This definition has type "'a -> 'a" which is less general than
         "'a0. 'a0 -> 'a0"
|}]


(* not a value, since it's actually an application *)
let g : 'a -> 'a = Obj.magic Fun.id Fun.id
[%%expect {|
val g : '_a -> '_a = <fun>
|}]
