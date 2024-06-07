(* TEST
    flags += "-extension unique";
    expect;
*)

(* This file tests that classes/objects are sound wrt modes. *)

let unique_use : 'a @ unique -> unit = fun _ -> ()

let portable_use : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val unique_use : unique_ 'a -> unit = <fun>
val portable_use : 'a @ portable -> unit = <fun>
|}]

(* There is a closure_lock of legacy around a class. We test for comonadic and
   monadic axes separately. *)

(* class cannot refer to external local things *)
let foo () =
    let local_ s = "hello" in
    let module M = struct
    class cla = object
        val k = s
    end
    end in ()
[%%expect{|
Line 5, characters 16-17:
5 |         val k = s
                    ^
Error: The value s is local, so cannot be used inside a class.
|}]

(* class can refer to external unique things, but only as shared. *)
let foo () =
    let unique_ s = "hello" in
    let module M = struct
    class cla = object
        val k = unique_use s
    end
    end in ()
[%%expect{|
Line 5, characters 27-28:
5 |         val k = unique_use s
                               ^
Error: This value is shared but expected to be unique.
  Hint: This identifier cannot be used uniquely,
  because it is defined in a class.
|}]

(* instance variables need to be defined as legacy *)
class cla = object
    val x = ("world" : _ @@ local)
end
[%%expect{|
Line 2, characters 12-34:
2 |     val x = ("world" : _ @@ local)
                ^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* instance variables are available as legacy to methods *)
class cla = object
    val x = ("world" : _ @@ portable)

    method foo = portable_use x
end
[%%expect{|
Line 4, characters 30-31:
4 |     method foo = portable_use x
                                  ^
Error: This value is nonportable but expected to be portable.
|}]

(* values written to instance variables need to be legacy *)
class cla = object
    val mutable x = "hello"

    method foo = x <- ("world" : _ @@ local)
end
[%%expect{|
Line 4, characters 22-44:
4 |     method foo = x <- ("world" : _ @@ local)
                          ^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

class cla = object
    method m = "hello"
end

(* For Pexp_send, the object needs to be legacy *)
let foo () =
    let local_ obj = new cla in
    obj#m
[%%expect{|
class cla : object method m : string end
Line 8, characters 4-7:
8 |     obj#m
        ^^^
Error: This value escapes its region.
|}]

(* methods are available as legacy *)
let u =
    let obj = new cla in
    portable_use obj#m
(* CR zqian: this should fail. *)
[%%expect{|
val u : unit = ()
|}]

(* for methods, arguments can be of any modes *)
class cla = object
    method foo (x : string) = portable_use x
end
[%%expect{|
class cla : object method foo : string @ portable -> unit end
|}]

(* the argument mode is soundly required during application *)
let foo () =
    let x @ nonportable = "hello" in
    let o = new cla in
    o#foo x
[%%expect{|
Line 4, characters 10-11:
4 |     o#foo x
              ^
Error: This value is nonportable but expected to be portable.
|}]


(* Closing over classes affects closure's mode *)
let u =
    let foo () = new cla in
    portable_use foo
(* CR zqian: this should fail. *)
[%%expect{|
val u : unit = ()
|}]

module type SC = sig
    class cla : object end
end
[%%expect{|
module type SC = sig class cla : object  end end
|}]

let u =
    let foo () =
        let m = (module struct class cla = object end end : SC) in
        let module M = (val m) in
        new M.cla
    in
    portable_use foo
[%%expect{|
val u : unit = ()
|}]

(* objects are always legacy *)
let u =
    let obj = new cla in
    portable_use obj
(* CR zqian: this should fail. *)
[%%expect{|
val u : unit = ()
|}]
