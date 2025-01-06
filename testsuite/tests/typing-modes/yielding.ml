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
val with_effect : (local_ (string -> unit) @ yielding -> 'a) -> 'a = <fun>
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
val run_yielding : local_ (string -> unit) @ yielding -> unit = <fun>
- : string = "my string"
|}]

let run_unyielding : (string -> unit) @ local unyielding -> unit = fun f -> f "another string"

let () = with_effect (fun k -> run_unyielding k)

[%%expect{|
val run_unyielding : local_ (string -> unit) -> unit = <fun>
Line 3, characters 46-47:
3 | let () = with_effect (fun k -> run_unyielding k)
                                                  ^
Error: This value is "yielding" but expected to be "unyielding".
|}]

(* CR dkalinichenko: default [local] arguments to [yielding]. *)

let run_default : (string -> unit) @ local -> unit = fun f -> f "some string"

let () = with_effect (fun k -> run_default k)

[%%expect{|
val run_default : local_ (string -> unit) -> unit = <fun>
Line 3, characters 43-44:
3 | let () = with_effect (fun k -> run_default k)
                                               ^
Error: This value is "yielding" but expected to be "unyielding".
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
