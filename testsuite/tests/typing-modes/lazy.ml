(* TEST
    expect;
*)

let use_portable : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

(* The thunk and the result are required to be global. This is only because we
don't support allocating lazy values on the stack. *)
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

(* For simplicity, we also require them to be [unyielding]. *)
let foo (x @ yielding) =
    lazy (let _ = x in ())
[%%expect{|
Line 2, characters 18-19:
2 |     lazy (let _ = x in ())
                      ^
Error: The value "x" is yielding, so cannot be used inside a lazy expression that may not yield.
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

(* one can construct [portable] lazy only if the result is [portable] *)
let foo () =
    let l = lazy (let x @ nonportable = fun x -> x in x) in
    use_portable l
[%%expect{|
Line 3, characters 17-18:
3 |     use_portable l
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* thunk is evaluated only when [uncontended] lazy is forced, so the thunk can be
    [nonportable] even if the lazy is [portable]. *)
let foo (x @ nonportable) =
    let l = lazy (let _ = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a -> unit = <fun>
|}]

(* For the same reason, [portable] lazy can close over things at [uncontended]. *)
let foo (x @ uncontended) =
    let l @ portable = lazy ( let _x @ uncontended = x in ()) in
    use_portable l
[%%expect{|
val foo : 'a -> unit = <fun>
|}]

(* Portable lazy gives portable result *)
let foo (x @ portable) =
    match x with
    | lazy r -> use_portable x
[%%expect{|
val foo : 'a lazy_t @ portable -> unit = <fun>
|}]

(* Nonportable lazy gives nonportable result *)
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
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to force the lazy expression,
  the lazy needs to be uncontended.
|}]

(* stdlib's [Lazy.force] is a special case of lazy pattern *)

(* thunk can close over [unique] values and be [once], even if the lazy itself is [many]. *)
let use_unique (_ @ unique) = ()
let use_many (_ @ many) = ()
[%%expect{|
val use_unique : 'a @ unique -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

let foo () =
    let x = "hello" in
    let t = lazy (use_unique x) in
    use_many t
[%%expect{|
val foo : unit -> unit = <fun>
|}]
