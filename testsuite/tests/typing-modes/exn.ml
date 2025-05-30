(* TEST
   expect;
*)

(* [exn] currently crosses portability. To make it safe, exception constructors
are portable iff all its arguments are portable. *)

exception Nonportable of (unit -> unit)
exception Portable of unit
exception Portable' of (unit -> unit) @@ portable

[%%expect{|
exception Nonportable of (unit -> unit)
exception Portable of unit
exception Portable' of (unit -> unit) @@ portable
|}]

let x : exn = Nonportable (fun x -> x)
[%%expect{|
val x : exn = Nonportable <fun>
|}]

let (foo @ portable) () =
    match x with
    | Nonportable g -> ()
    | _ -> ()
[%%expect{|
Line 3, characters 6-17:
3 |     | Nonportable g -> ()
          ^^^^^^^^^^^
Error: The constructor "Nonportable" is nonportable, so cannot be used inside a function that is portable.
|}]

let (foo @ portable) () =
    match x with
    | Portable g -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Portable' g -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    raise (Nonportable (fun () -> ()))
[%%expect{|
Line 2, characters 11-22:
2 |     raise (Nonportable (fun () -> ()))
               ^^^^^^^^^^^
Error: The constructor "Nonportable" is nonportable, so cannot be used inside a function that is portable.
|}]

let (foo @ portable) () =
    raise (Portable ())
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Portable' (fun () -> ()))
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

(* rebinding counts as usage *)
let (foo @ portable) () =
    let module M = struct
        exception Nonportable' = Nonportable
    end in
    ()
[%%expect{|
Line 3, characters 33-44:
3 |         exception Nonportable' = Nonportable
                                     ^^^^^^^^^^^
Error: The constructor "Nonportable" is nonportable, so cannot be used inside a function that is portable.
|}]


(* CR zqian: the following should be allowed, but requires a completely different
approach (coportable). *)
exception SemiPortable of string * (unit -> unit)

let (foo @ portable) () =
    try () with
    SemiPortable (s, _) -> print_endline s
[%%expect{|
exception SemiPortable of string * (unit -> unit)
Line 5, characters 4-16:
5 |     SemiPortable (s, _) -> print_endline s
        ^^^^^^^^^^^^
Error: The constructor "SemiPortable" is nonportable, so cannot be used inside a function that is portable.
|}]

(* [exn] also crosses contention. To make it safe, exception constructors
are uncontended iff all its arguments are uncontended. *)
exception Uncontended of unit
exception Uncontended' of int ref @@ contended
exception Contended of int ref
[%%expect{|
exception Uncontended of unit
exception Uncontended' of int ref @@ contended
exception Contended of int ref
|}]

let (foo @ portable) () =
    match x with
    | Uncontended () -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Uncontended' _ -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | Contended _ -> ()
    | _ -> ()
[%%expect{|
Line 3, characters 6-15:
3 |     | Contended _ -> ()
          ^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

let (foo @ portable) () =
    raise (Uncontended ())
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Uncontended' (ref 42))
[%%expect{|
val foo : unit -> 'a = <fun>
|}]

let (foo @ portable) () =
    raise (Contended (ref 42))
[%%expect{|
Line 2, characters 11-20:
2 |     raise (Contended (ref 42))
               ^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* rebinding counts as usage *)
let (foo @ portable) () =
    let module M = struct
        exception Contended' = Contended
    end in
    ()
[%%expect{|
Line 3, characters 31-40:
3 |         exception Contended' = Contended
                                   ^^^^^^^^^
Error: This constructor is at mode "contended", but expected to be at mode "uncontended".
|}, Principal{|
Line 3, characters 31-40:
3 |         exception Contended' = Contended
                                   ^^^^^^^^^
Error: The constructor "Contended" is nonportable, so cannot be used inside a function that is portable.
|}]

exception Fields : { x : int } -> exn
exception MutFields : { mutable x : string } -> exn
exception MutFields': { mutable z : int ref @@ contended } -> exn
[%%expect{|
exception Fields : { x : int; } -> exn
exception MutFields : { mutable x : string; } -> exn
exception MutFields' : { mutable z : int ref @@ contended; } -> exn
|}]

let (foo @ portable) () =
    match x with
    | Fields _ -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let (foo @ portable) () =
    match x with
    | MutFields _ -> ()
    | _ -> ()
[%%expect{|
Line 3, characters 6-15:
3 |     | MutFields _ -> ()
          ^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

let (foo @ portable) () =
    match x with
    | MutFields' _ -> ()
    | _ -> ()
[%%expect{|
Line 3, characters 6-16:
3 |     | MutFields' _ -> ()
          ^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* built-in exceptions can be raised arbitrarily inside portable functions - we
show that's safe. *)
let (foo @ portable) () =
    match x with
    | Assert_failure _ -> ()
    | Match_failure _ -> ()
    | _ -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]


(* other extensible types are not affected *)
(* CR zqian: support other extensible types. *)
type t = ..

type t += Foo of int

let x : t = Foo 42

let (foo @ portable) () =
    ignore (x : _ @ portable)

[%%expect{|
type t = ..
type t += Foo of int
val x : t = Foo 42
Line 8, characters 12-13:
8 |     ignore (x : _ @ portable)
                ^
Error: The value "x" is nonportable, so cannot be used inside a function that is portable.
|}]
