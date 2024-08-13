(* TEST
 readonly_files = "original.ml middle.ml";
 setup-ocamlc.byte-build-env;
 module = "original.ml";
 ocamlc.byte;
 module = "middle.ml";
 ocamlc.byte;
 script = "rm -f original.cmi";
 script;
 expect;
*)


#directory "ocamlc.byte";;
#load "original.cmo"
#load "middle.cmo"

let x:'a. 'a Middle.t =
  let _r = ref 0 in
  Middle.T
[%%expect {|
Unknown directive "directory".
|}]


let () = Middle.(g x)
[%%expect {|
Line 1, characters 9-15:
1 | let () = Middle.(g x)
             ^^^^^^
Error: Unbound module "Middle"
|}]

let () = Middle.(f x)
[%%expect {|
Line 1, characters 9-15:
1 | let () = Middle.(f x)
             ^^^^^^
Error: Unbound module "Middle"
|}]

let () = Middle.f (module struct end)
[%%expect {|
Line 1, characters 9-17:
1 | let () = Middle.f (module struct end)
             ^^^^^^^^
Error: Unbound module "Middle"
|}]

let foo (x : Middle.pack1) =
  let module M = (val x) in
  ()
[%%expect {|
Line 1, characters 13-25:
1 | let foo (x : Middle.pack1) =
                 ^^^^^^^^^^^^
Error: Unbound module "Middle"
|}]

let foo (x : Middle.pack2) =
  let module M = (val x) in
  ()
[%%expect {|
Line 1, characters 13-25:
1 | let foo (x : Middle.pack2) =
                 ^^^^^^^^^^^^
Error: Unbound module "Middle"
|}]

module type T1 = sig type t = int end
let foo x = (x : Middle.pack1 :> (module T1))
[%%expect {|
module type T1 = sig type t = int end
Line 2, characters 17-29:
2 | let foo x = (x : Middle.pack1 :> (module T1))
                     ^^^^^^^^^^^^
Error: Unbound module "Middle"
|}]

module type T2 = sig module M : sig type t = int end end
let foo x = (x : Middle.pack2 :> (module T2))
[%%expect {|
module type T2 = sig module M : sig type t = int end end
Line 2, characters 17-29:
2 | let foo x = (x : Middle.pack2 :> (module T2))
                     ^^^^^^^^^^^^
Error: Unbound module "Middle"
|}]

(* Check the detection of type kind in type-directed disambiguation . *)
let t = Middle.r.Middle.x
[%%expect {|
Line 1, characters 8-16:
1 | let t = Middle.r.Middle.x
            ^^^^^^^^
Error: Unbound module "Middle"
|}]

let k = match Middle.s with Middle.S -> ()
[%%expect {|
Line 1, characters 14-22:
1 | let k = match Middle.s with Middle.S -> ()
                  ^^^^^^^^
Error: Unbound module "Middle"
|}]

(* #11560: gadts and missing cmis *)

let  f : type a b. (a Middle.ti -> unit) -> (a,b) Middle.gadt -> b -> unit =
  fun call Middle.G x -> call x
[%%expect {|
Line 1, characters 22-31:
1 | let  f : type a b. (a Middle.ti -> unit) -> (a,b) Middle.gadt -> b -> unit =
                          ^^^^^^^^^
Error: Unbound module "Middle"
|}]

(* Check re-exportation of GADTs *)

let f : type a. a Middle.is_int -> a -> int = fun Middle.Is_int x -> x
let g : bool Middle.is_int -> 'a = function _ -> .
[%%expect{|
Line 1, characters 18-31:
1 | let f : type a. a Middle.is_int -> a -> int = fun Middle.Is_int x -> x
                      ^^^^^^^^^^^^^
Error: Unbound module "Middle"
|}]

let f (x: Middle.u) = x
[%%expect {|
Line 1, characters 10-18:
1 | let f (x: Middle.u) = x
              ^^^^^^^^
Error: Unbound module "Middle"
|}]
