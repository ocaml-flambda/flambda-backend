(* TEST
 expect;
*)

(* CR dkalinichenko: allow [yielding] at toplevel? *)
let my_effect : unit -> unit @@ yielding = print_endline "Hello, world!"
[%%expect{|
Line 1, characters 4-72:
1 | let my_effect : unit -> unit @@ yielding = print_endline "Hello, world!"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "yielding" but expected to be "unyielding".
|}]

let storage = ref ""

let with_effect : ((string -> unit) @ local yielding -> 'a) -> 'a =
  fun f -> f ((:=) storage)

[%%expect{|
val storage : string ref = {contents = ""}
val with_effect : (local_ (string -> unit) -> 'a) -> 'a = <fun>
|}]

let () = with_effect (fun k -> k "Hello, world!")

let _ = !storage

[%%expect{|
- : string = "Hello, world!"
|}]

let run_yielding : (string -> unit) @ local yielding -> unit = fun f -> f "my string"

let () = with_effect (fun k -> run_yielding k)

let _ = !storage

[%%expect{|
val run_yielding : local_ (string -> unit) -> unit = <fun>
- : string = "my string"
|}]

let run_unyielding : (string -> unit) @ local unyielding -> unit = fun f -> f "another string"

let () = with_effect (fun k -> run_unyielding k)

[%%expect{|
val run_unyielding : local_ (string -> unit) @ unyielding -> unit = <fun>
Line 3, characters 46-47:
3 | let () = with_effect (fun k -> run_unyielding k)
                                                  ^
Error: This value is "yielding" but expected to be "unyielding".
|}]

let run_default : (string -> unit) @ local -> unit = fun f -> f "some string"

let () = with_effect (fun k -> run_default k)

[%%expect{|
val run_default : local_ (string -> unit) -> unit = <fun>
|}]

(* A closure over a [yielding] value must be [yielding]. *)

let () = with_effect (fun k ->
  let closure @ local unyielding = fun () -> k () in
  run_unyielding k)

[%%expect{|
Line 2, characters 45-46:
2 |   let closure @ local unyielding = fun () -> k () in
                                                 ^
Error: The value "k" is yielding, so cannot be used inside a function that may not yield.
|}]


type 'a t1 = Mk1 of 'a @@ global

type 'a t2 = Mk2 of 'a @@ global yielding

type 'a t3 = Mk3 of 'a @@ unyielding

type 'a t4 = Mk4 of 'a @@ yielding

let with_global_effect : ((string -> unit) @ yielding -> 'a) -> 'a =
  fun f -> f ((:=) storage)

[%%expect{|
type 'a t1 = Mk1 of global_ 'a
type 'a t2 = Mk2 of global_ 'a @@ yielding
type 'a t3 = Mk3 of 'a @@ unyielding
type 'a t4 = Mk4 of 'a
val with_global_effect : ((string -> unit) @ yielding -> 'a) -> 'a = <fun>
|}]

(* [global] modality implies [unyielding]. *)
let _ = with_global_effect (fun k -> let _ = Mk1 k in ())

[%%expect{|
Line 1, characters 49-50:
1 | let _ = with_global_effect (fun k -> let _ = Mk1 k in ())
                                                     ^
Error: This value is "yielding" but expected to be "unyielding".
|}]

(* [global yielding] works: *)
let _ = with_global_effect (fun k -> let _ = Mk2 k in ())

[%%expect{|
- : unit = ()
|}]

(* [unyielding] and [yielding] modalities: *)
let _ = with_global_effect (fun k -> let _ = Mk3 k in ())

[%%expect{|
Line 1, characters 49-50:
1 | let _ = with_global_effect (fun k -> let _ = Mk3 k in ())
                                                     ^
Error: This value is "yielding" but expected to be "unyielding".
|}]

let _ = with_global_effect (fun k -> let _ = Mk4 k in ())

[%%expect{|
- : unit = ()
|}]

(* [@local_opt] and [yielding]: *)

external id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"

let f1 x = id x
let f2 (x @ local) = exclave_ id x
let f3 (x @ yielding) = id x
let f4 (x @ local unyielding) = exclave_ id x

[%%expect{|
external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
val f1 : 'a -> 'a = <fun>
val f2 : local_ 'a -> local_ 'a = <fun>
val f3 : 'a @ yielding -> 'a @ yielding = <fun>
val f4 : local_ 'a @ unyielding -> local_ 'a @ unyielding = <fun>
|}]

(* [mod global] implies [mod unyielding] by default. *)

type ('a : value mod global) u1

type ('a : value mod global yielding) u2

type w1 : value mod global yielding

type w2 : value mod global unyielding

[%%expect{|
type ('a : value mod global) u1
type ('a : value mod global) u2
type w1 : value mod global
type w2 : value mod global
|}]

type _z1 = w1 u1

[%%expect{|
Line 1, characters 11-13:
1 | type _z1 = w1 u1
               ^^
Error: This type "w1" should be an instance of type "('a : value mod global)"
       The kind of w1 is value mod global
         because of the definition of w1 at line 5, characters 0-35.
       But the kind of w1 must be a subkind of value mod global
         because of the definition of u1 at line 1, characters 0-31.
|}]

type _z2 = w2 u1

[%%expect{|
type _z2 = w2 u1
|}]

type _z3 = w1 u2

[%%expect{|
type _z3 = w1 u2
|}]

type _z4 = w2 u2

[%%expect{|
type _z4 = w2 u2
|}]
