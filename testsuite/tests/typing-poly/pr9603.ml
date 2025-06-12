(* TEST
 expect;
*)

type 'p pair = 'a * 'b constraint 'p = < left:'a; right:'b>

(* New in 4.11 *)
let error: 'left 'right.
  <left:'left; right:'right> pair -> <left:'right; right:'left> pair =
  fun (x,y) -> (y,x)
[%%expect{|
type 'c pair = 'a * 'b constraint 'c = < left : 'a; right : 'b >
val error :
  < left : 'left; right : 'right > pair ->
  < left : 'right; right : 'left > pair = <fun>
|}]

(* Known problem with polymorphic methods *)
let foo :
  < m : 'left 'right. <left:'left; right:'right> pair >
   -> < m : 'left 'right. <left:'left; right:'right> pair >
= fun x -> x
(* CR layouts v2.8: This got worse once we enabled with-kinds for tuples, because of
   Cannot_subst turning into Missing_cmi in estimate_type_jkind. When we fix that, this
   should change back to a better error message. *)
[%%expect{|
Line 2, characters 4-53:
2 |   < m : 'left 'right. <left:'left; right:'right> pair >
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "'left 'right. < left : 'left; right : 'right > pair" is any
         because the .cmi file for pair is missing.
       But the layout of "'left 'right. < left : 'left; right : 'right > pair" must be a sublayout of value
         because it's the type of an object field.
       No .cmi file found containing pair.
|}]
