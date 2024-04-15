(* TEST
 flags += "-extension unique";
 expect;
*)

(* This file tests the interaction between a module/class and its surrounding environment *)

let unique_id (unique_ x) = ignore x


(* you cannot use env vars as unique in classes/objects  *)
let texp_object () =
  let x = "foo" in
  object (self)
  val bar = unique_ x
  end;
[%%expect{|
val unique_id : unique_ 'a -> unit = <fun>
Line 8, characters 20-21:
8 |   val bar = unique_ x
                        ^
Error: Found a shared value where a unique value was expected
  Hint: This identifier cannot be used uniquely,
  because it is defined in a class.
|}]

(* you can use env vars as shared and many, but they might collide with the external uses *)
let texp_object () =
  let x = "foo" in
  unique_id x;
  object (self)
  val bar = x
  end;
[%%expect{|
Line 5, characters 12-13:
5 |   val bar = x
                ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 12-13:
3 |   unique_id x;
                ^

|}]

(* you are not allowed to use x uniquely inside the module *)
let texp_letmodule () =
  let x = "foo" in
  let module Bar = struct
    let y = unique_ x
  end
  in
  ()
[%%expect{|
Line 4, characters 12-21:
4 |     let y = unique_ x
                ^^^^^^^^^
Error: This value is shared but used as unique.
Hint: This value comes from outside the current module or class.
|}]

(* you can use x as shared and many, but it might collide with external uses. *)
let texp_letmodule () =
  let x = "foo" in
  unique_id x;
  let module Bar = struct
    let y = x
  end
  in
  ()
[%%expect{|
Line 5, characters 12-13:
5 |     let y = x
                ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 12-13:
3 |   unique_id x;
                ^

|}]

let texp_open () =
  let x = "foo" in
  let open (struct let y = unique_ x end) in
  ()
[%%expect{|
Line 3, characters 27-36:
3 |   let open (struct let y = unique_ x end) in
                               ^^^^^^^^^
Error: This value is shared but used as unique.
Hint: This value comes from outside the current module or class.
|}]

let texp_open () =
  let x = "foo" in
  unique_id x;
  let open (struct let y = x end) in
  ()
[%%expect{|
Line 4, characters 27-28:
4 |   let open (struct let y = x end) in
                               ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 12-13:
3 |   unique_id x;
                ^

|}]

module type bar = sig val y : string end

let texp_pack () =
  let x = "foo" in
  let z = (module struct let y = unique_ x end : bar) in
  ()
[%%expect{|
module type bar = sig val y : string end
Line 5, characters 33-42:
5 |   let z = (module struct let y = unique_ x end : bar) in
                                     ^^^^^^^^^
Error: This value is shared but used as unique.
Hint: This value comes from outside the current module or class.
|}]

let texp_pack () =
  let x = "foo" in
  unique_id x;
  let z = (module struct let y = x end : bar) in
  ()
[%%expect{|
Line 4, characters 33-34:
4 |   let z = (module struct let y = x end : bar) in
                                     ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 12-13:
3 |   unique_id x;
                ^

|}]

module M = struct
  let foo = "hello"
end


let value_from_module () =
  unique_id M.foo
[%%expect{|
module M : sig val foo : string end
Line 7, characters 12-17:
7 |   unique_id M.foo
                ^^^^^
Error: Found a shared value where a unique value was expected
|}]