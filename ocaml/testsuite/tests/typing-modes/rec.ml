(* TEST
 expect;
*)

(* for entirely-function definitions, they will share the same mode that can be
   at any mode *)
let te (local_ x) =
    let use_portable (_ @ portable) = () in
    let rec foo x' y =
        use_portable bar;
        bar x y
    and bar x' y =
        use_portable foo;
        foo x y
    in
    use_portable bar;
    use_portable foo;
    ()
[%%expect{|
val te : local_ 'a @ portable -> unit = <fun>
|}]

(* for mixed definitions, they will still share the same mode, but the locality
   must be global. *)

let te (local_ x) =
    let rec foo x' y =
        bar x y
    and bar x' y =
        foo x y
    and baz = "hello"
    in
    ()
[%%expect{|
Line 3, characters 12-13:
3 |         bar x y
                ^
Error: The value x is local, so cannot be used inside a closure that might escape.
|}]

(* for mixed definitions, the other axes are not constrained. *)
let te (x) =
    let use_portable (_ @ portable) = () in
    let rec foo x' y =
        use_portable bar;
        bar x y
    and bar x' y =
        use_portable foo;
        foo x y
    and baz = "hello"
    in
    use_portable bar;
    use_portable foo;
    use_portable baz;
    ()
[%%expect{|
val te : 'a @ portable -> unit = <fun>
|}]
