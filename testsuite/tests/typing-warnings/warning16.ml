(* TEST
 expect;
*)
let foo ?x = ()
[%%expect{|
Line 1, characters 9-10:
1 | let foo ?x = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val foo : ?x:'a -> unit @@ global many = <fun>
|}]

let foo ?x ~y = ()
[%%expect{|
Line 1, characters 9-10:
1 | let foo ?x ~y = ()
             ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val foo : ?x:'a -> y:'b -> unit @@ global many = <fun>
|}]

let foo ?x () = ()
[%%expect{|
val foo : ?x:'a -> unit -> unit @@ global many = <fun>
|}]

let foo ?x ~y () = ()
[%%expect{|
val foo : ?x:'a -> y:'b -> unit -> unit @@ global many = <fun>
|}]

class bar ?x = object end
[%%expect{|
Line 1, characters 11-12:
1 | class bar ?x = object end
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

class bar : ?x:'a -> object  end
|}]

class bar ?x ~y = object end
[%%expect{|
Line 1, characters 11-12:
1 | class bar ?x ~y = object end
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

class bar : ?x:'a -> y:'b -> object  end
|}]

class bar ?x () = object end
[%%expect{|
class bar : ?x:'a -> unit -> object  end
|}]

class foo ?x ~y () = object end
[%%expect{|
class foo : ?x:'a -> y:'b -> unit -> object  end
|}]
