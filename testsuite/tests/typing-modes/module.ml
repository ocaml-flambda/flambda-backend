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
module F (X : S) = struct
    type t = int
    let x = X.x
end
[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
module type S = sig val x : 'a -> unit end
module type SL = sig type 'a t end
module M : sig type 'a t = int val x : 'a -> unit end
module F : functor (X : S) -> sig type t = int val x : 'a -> unit end
|}]

(* Closing over modules affects closure's modes *)
let u =
    let foo () =
        let _ = (module M : S) in
        ()
    in
    portable_use foo
[%%expect{|
Line 6, characters 17-20:
6 |     portable_use foo
                     ^^^
Error: This value is nonportable but expected to be portable.
|}]

let u =
    let foo () =
        let module X = struct
            let x _ = ()
        end
        in
        let module R = F(X) in
        ()
    in
    portable_use foo
[%%expect{|
Line 10, characters 17-20:
10 |     portable_use foo
                      ^^^
Error: This value is nonportable but expected to be portable.
|}]

(* File-level modules are looked up differently and need to be tested
separately. *)
let u =
    let foo () =
        let _ = (module List : SL) in
        ()
    in
    portable_use foo
[%%expect{|
Line 6, characters 17-20:
6 |     portable_use foo
                     ^^^
Error: This value is nonportable but expected to be portable.
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
[%%expect{|
Line 3, characters 17-20:
3 |     portable_use foo
                     ^^^
Error: This value is nonportable but expected to be portable.
|}]

let u =
    let foo () = List.length in
    portable_use foo
[%%expect{|
Line 3, characters 17-20:
3 |     portable_use foo
                     ^^^
Error: This value is nonportable but expected to be portable.
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

let foo () =
    let bar () =
        let _ : F(M).t = 42 in
        ()
    in
    let _ = (bar : _ @@ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : sig
            open M
        end = struct end
        in
        ()
    in
    let _ = (bar : _ @@ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : (sig
            module M' : sig  end
        end with module M' := M) = struct
        end
        in
        ()
    in
    let _ = (bar : _ @@ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Replacing [:=] in the above example with [=] should work similarly, but I
   couldn't construct an example to test this properly. *)

let foo () =
    let bar () =
        let module _ : module type of M = struct
            type 'a t = int
            let x _ = ()
        end
        in
        ()
    in
    let _ = (bar : _ @@ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
    let bar () =
        let module _ : (sig
            module M' := M
        end) = struct
        end
        in
        ()
    in
    let _ = (bar : _ @@ portable) in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Pmty_alias is not testable *)
