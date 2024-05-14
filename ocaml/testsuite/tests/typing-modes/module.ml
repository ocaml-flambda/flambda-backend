(* TEST
   expect;
*)

(* This file tests that modules are sound wrt modes. *)

module type S = sig val x : string end

module type Empty = sig end

module M = struct
    let x = "string"
end
[%%expect{|
module type S = sig val x : string end
module type Empty = sig end
module M : sig val x : string end
|}]

(* Closing over modules affects closure's modes *)
let (foo @ portable) () =
    let _ = (module M : S) in
    ()
(* CR zqian: This should fail *)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* File-level modules are looked up differently and need to be tested
separately. *)
let (foo @ portable) () =
    let _ = (module List : Empty) in
    ()
(* CR zqian: this should fail *)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Values in modules are defined as legacy *)
module M = struct
    let x = local_ "hello"
end
[%%expect{|
Line 2, characters 8-9:
2 |     let x = local_ "hello"
            ^
Error: This value escapes its region.
|}]

(* Values from modules are available as legacy *)
let (foo @ portable) () = M.x
(* CR zqian: this should fail *)
[%%expect{|
val foo : unit -> string = <fun>
|}]

let (foo @ portable) () = List.length
(* CR zqian: this should fail *)
[%%expect{|
val foo : unit -> 'a list -> int = <fun>
|}]

let (foo @ portable) () =
    let m = (module struct let x = "hello" end : S) in
    let module M = (val m) in
    M.x
[%%expect{|
val foo : unit -> string = <fun>
|}]

(* first class modules are produced at legacy *)
let x = ((module M : Empty) : _ @@ portable)
(* CR zqian: this should fail *)
[%%expect{|
val x : (module Empty) = <module>
|}]

(* first class modules are consumed at legacy *)
let foo () =
    let m @ local = (module M : Empty) in
    let module M = (val m) in
    ()
[%%expect{|
Line 3, characters 24-25:
3 |     let module M = (val m) in
                            ^
Error: This value escapes its region.
|}]
