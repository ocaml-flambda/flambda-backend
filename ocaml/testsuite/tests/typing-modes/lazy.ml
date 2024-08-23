(* TEST
    expect;
*)

let use_portable : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

(* The thunk and the result are required to be global *)
let foo () =
    lazy (let x @ local = "hello" in x)
[%%expect{|
Line 2, characters 37-38:
2 |     lazy (let x @ local = "hello" in x)
                                         ^
Error: This value escapes its region.
  Hint: It is the result of a lazy expression.
|}]

let foo (local_ x) =
    lazy (let _ = x in ())
[%%expect{|
Line 2, characters 18-19:
2 |     lazy (let _ = x in ())
                      ^
Error: The value "x" is local, so cannot be used inside a lazy expression.
|}]

(* lazy expression is constructed as global *)
let foo () =
    lazy ("hello")
[%%expect{|
val foo : unit -> string lazy_t = <fun>
|}]

(* result of lazy is available as global always *)
let foo (x @ local) =
    match x with
    | lazy y -> y
[%%expect{|
val foo : local_ 'a lazy_t -> 'a = <fun>
|}]

(* one can construct portable lazy, if both the thunk and the result are
   portable *)
let foo () =
    let l = lazy (let x @ nonportable = fun x -> x in x) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x @ nonportable) =
    let l = lazy (let _ = x in ()) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x @ portable) =
    let l = lazy (let _ = x in let y = fun () -> () in y) in
    use_portable l
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

(* inside a portable lazy, things are available as contended *)
let foo (x @ uncontended) =
    let l @ portable = lazy ( let x' @ uncontended = x in ()) in
    use_portable l
[%%expect{|
Line 2, characters 53-54:
2 |     let l @ portable = lazy ( let x' @ uncontended = x in ()) in
                                                         ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* Portable lazy gives portable result *)
let foo (x @ portable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
val foo : 'a lazy_t @ portable -> unit = <fun>
|}]

let foo (x @ nonportable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
Line 3, characters 29-30:
3 |     | lazy r -> use_portable x
                                 ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* forcing a lazy is not concurrency-safe; therefore, we require uncontended
   access *)
let foo (x @ contended) =
    match x with
    | lazy _ -> ()
[%%expect{|
Line 3, characters 6-12:
3 |     | lazy _ -> ()
          ^^^^^^
Error: This value is contended but expected to be uncontended.
  Hint: In order to force the lazy expression,
  the lazy needs to be uncontended.
|}]

(* stdlib's [Lazy.force] is a special case of lazy pattern *)
