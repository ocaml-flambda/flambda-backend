(* TEST
   expect;
*)

(* This file tests that modules are sound wrt modes. *)

let portable_use : 'a @ portable -> unit = fun _ -> ()

module type S = sig val x : 'a -> unit end

module type SL = sig type 'a t end

module M = struct
    type 'a t = int
    let x _ = ()
end
[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
module type S = sig val x : 'a -> unit end
module type SL = sig type 'a t end
module M : sig type 'a t = int val x : 'a -> unit end
|}]

(* Closing over modules affects closure's modes *)
let u =
    let foo () =
        let _ = (module M : S) in
        ()
    in
    portable_use foo
(* CR zqian: This should fail *)
[%%expect{|
val u : unit = ()
|}]

(* File-level modules are looked up differently and need to be tested
separately. *)
let u =
    let foo () =
        let _ = (module List : SL) in
        ()
    in
    portable_use foo
(* CR zqian: this should fail *)
[%%expect{|
val u : unit = ()
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
let u =
    let foo () = M.x in
    portable_use foo
(* CR zqian: this should fail *)
[%%expect{|
val u : unit = ()
|}]

let u =
    let foo () = List.length in
    portable_use foo
(* CR zqian: this should fail *)
[%%expect{|
val u : unit = ()
|}]

let u =
    let foo () =
        let m = (module struct let x _ = () end : S) in
        let module M = (val m) in
        M.x
    in
    portable_use foo
[%%expect{|
val u : unit = ()
|}]

(* first class modules are produced at legacy *)
let x = ((module M : SL) : _ @@ portable)
(* CR zqian: this should fail *)
[%%expect{|
val x : (module SL) = <module>
|}]

(* first class modules are consumed at legacy *)
let foo () =
    let m @ local = (module M : SL) in
    let module M = (val m) in
    ()
[%%expect{|
Line 3, characters 24-25:
3 |     let module M = (val m) in
                            ^
Error: This value escapes its region.
|}]
