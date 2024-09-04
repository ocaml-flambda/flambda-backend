(* TEST
    flags += "-allow-illegal-crossing";
    expect;
*)

type t = int option
[%%expect{|
type t = int option
|}]

let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
|}]


let foo (t : int option @@ contended nonportable) =
    use_uncontended t;
    use_portable t
[%%expect{|
val foo : int option @ contended -> unit = <fun>
|}]

(* crosses contention but not portability *)
let foo (t : ('a -> 'a) option @@ contended) =
    use_uncontended t
[%%expect{|
val foo : ('a : any). ('a -> 'a) option @ contended -> unit = <fun>
|}]

let foo (t : ('a -> 'a) option @@ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* crosses portability but not contention *)
type int_ref : value mod portable = {mutable a : int}
[%%expect{|
type int_ref : value mod portable = { mutable a : int; }
|}]

let foo (t : int_ref option @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int_ref option @@ nonportable) =
    use_portable t
[%%expect{|
val foo : int_ref option -> unit = <fun>
|}]

(* crosses nothing *)
let foo (t : 'a option @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : 'a option @@ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]
