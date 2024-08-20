(* TEST
    expect;
*)

let portable_use : _ @ portable -> unit = fun _ -> ()

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let ( and* ) a b =
  match a, b with
  | Some a, Some b -> Some (a, b)
  | _ -> None

[%%expect{|
val portable_use : 'a @ portable -> unit = <fun>
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = <fun>
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option = <fun>
|}]

(* bindings are required to be legacy *)
let foo () =
    let* a = local_ "hello" in
    ()
[%%expect{|
Line 2, characters 13-27:
2 |     let* a = local_ "hello" in
                 ^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo () =
    let* a = Some "hello"
    and* b = local_ "hello" in
    ()
[%%expect{|
Line 3, characters 13-27:
3 |     and* b = local_ "hello" in
                 ^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* Bindings are avialable as legacy *)
let foo () =
    let* a = Some (fun x -> x)
    and* b = Some (fun x -> x) in
    portable_use a
[%%expect{|
Line 4, characters 17-18:
4 |     portable_use a
                     ^
Error: This value is nonportable but expected to be portable.
|}]

let foo () =
    let* a = Some (fun x -> x)
    and* b = Some (fun x -> x) in
    portable_use b
[%%expect{|
Line 4, characters 17-18:
4 |     portable_use b
                     ^
Error: This value is nonportable but expected to be portable.
|}]

(* Body required to be legacy *)
let foo () =
    let _ =
        let* a = Some (fun x -> x) in
        local_ "hello"
    in
    ()
[%%expect{|
Line 4, characters 8-22:
4 |         local_ "hello"
            ^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* The whole letop is available as legacy *)
let foo () =
    portable_use (
        let* a = Some (fun x -> x) in
        fun x -> x
    )
[%%expect{|
Lines 2-5, characters 17-5:
2 | .................(
3 |         let* a = Some (fun x -> x) in
4 |         fun x -> x
5 |     )
Error: This value is nonportable but expected to be portable.
|}]
