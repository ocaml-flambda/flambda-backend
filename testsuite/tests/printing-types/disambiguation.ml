(* TEST
 expect;
*)

type 'a x = private [> `x] as 'a;;
[%%expect {|
Line 1:
Error: Type declarations do not match:
         type !'a x = private [> `x ] constraint 'a = 'a x
       is not included in
         type 'a x
<<<<<<< HEAD
       Their parameters differ:
       The type 'b x as 'b is not equal to the type 'a
||||||| 121bedcfd2
       Their parameters differ
       The type 'b x as 'b is not equal to the type 'a
=======
       Their parameters differ
       The type "'b x as 'b" is not equal to the type "'a"
>>>>>>> 5.2.0
|}, Principal{|
Line 1:
Error: Type declarations do not match:
         type !'a x = private 'a constraint 'a = [> `x ]
       is not included in
         type 'a x
<<<<<<< HEAD
       Their parameters differ:
       The type [> `x ] is not equal to the type 'a
||||||| 121bedcfd2
       Their parameters differ
       The type [> `x ] is not equal to the type 'a
=======
       Their parameters differ
       The type "[> `x ]" is not equal to the type "'a"
>>>>>>> 5.2.0
|}];;


type int;;
[%%expect {|
type int
|}];;

let x = 0;;
[%%expect {|
val x : int/2 = 0
|}];;


type float;;
[%%expect {|
type float
|}];;

0.;;
[%%expect {|
- : float/2 = 0.
|}];;
