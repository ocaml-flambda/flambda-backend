(* TEST
  flags = "-no-always-tco";
  expect;
*)

let [@inline never] f (local_ str) = let _ = str in ()

(* ----- Basic tests ----- *)
module M = struct
  let f = f
end

[%%expect {|
val f : local_ 'a -> unit = <fun>
module M : sig val f : local_ 'a -> unit end
|}]


let implicit_nontail_M () =
  let local_ str = "hello" in
  M.f str

[%%expect {|
val implicit_nontail_M : unit -> unit = <fun>
|}]


let explicit_nontail_M () =
  let local_ str = "hello" in
  M.f str [@nontail]

[%%expect {|
val explicit_nontail_M : unit -> unit = <fun>
|}]


let implicit_nontail_M_overriden_by_tail () =
  let local_ str = "hello" in
  M.f str [@tail]

[%%expect {|
Line 3, characters 6-9:
3 |   M.f str [@tail]
          ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]


let implicit_nontail () =
  let local_ str = "hello" in
  f str

[%%expect {|
val implicit_nontail : unit -> unit = <fun>
|}]

let explicit_nontail () =
  let local_ str = "hello" in
  f str [@nontail]

[%%expect {|
val explicit_nontail : unit -> unit = <fun>
|}]

let implicit_nontail_overriden_by_tail () =
  let local_ str = "hello" in
  f str [@tail]

[%%expect {|
Line 3, characters 4-7:
3 |   f str [@tail]
        ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]


let rec implicit_tail n (local_ str) =
  let local_ str = "hello" in
  if n > 0 then implicit_tail (n - 1) str else f str

[%%expect {|
Line 3, characters 38-41:
3 |   if n > 0 then implicit_tail (n - 1) str else f str
                                          ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let rec explicit_tail n (local_ str) =
  let local_ str = "hello" in
  if n > 0 then explicit_tail (n - 1) str [@tail] else f str

[%%expect {|
Line 3, characters 38-41:
3 |   if n > 0 then explicit_tail (n - 1) str [@tail] else f str
                                          ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let rec implicit_tail_overriden_by_nontail n (local_ str) =
  let local_ str = "hello" in
  if n > 0 then
    implicit_tail_overriden_by_nontail (n - 1) str [@nontail]
  else f str

[%%expect {|
val implicit_tail_overriden_by_nontail : int -> local_ string -> unit = <fun>
|}]

module rec List : sig
  val map : local_ ('a -> 'b) -> 'a list -> 'b list
end = List

let call_local_map_in_tail_pos lst =
  let local_ str = "Hello, world!" in
  let local_ f x = let _ = str in () in
  List.map f lst

[%%expect {|
module rec List : sig val map : local_ ('a -> 'b) -> 'a list -> 'b list end
val call_local_map_in_tail_pos : 'a list -> unit list = <fun>
|}]


(* ----- Basic letrec tests ----- *)
(* Check that locals cannot be passed into a function defined in the same let rec
   (`implicit_tail_foo`) if that function is applied in tail position. *)
let rec implicit_tail_foo n (local_ str) =
  if n > 0 then
    implicit_tail_bar (n - 2) str
  else f str
and implicit_tail_bar n (local_ str) =
  implicit_tail_foo (n + 1) str

and call_foo n =
  let local_ str = "hello" in
  implicit_tail_foo n str

[%%expect {|
Line 10, characters 22-25:
10 |   implicit_tail_foo n str
                           ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Same as above ("Check that locals cannot be passed into a function...") except
   with a parent expression. *)
let () =
  let rec implicit_tail_foo n (local_ str) =
    if n > 0 then
      implicit_tail_bar (n - 2) str
    else f str
  and implicit_tail_bar n (local_ str) =
    implicit_tail_foo (n + 1) str

  and call_foo n =
    let local_ str = "hello" in
    implicit_tail_foo n str
  in ignore call_foo

[%%expect {|
Line 11, characters 24-27:
11 |     implicit_tail_foo n str
                             ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]


(* On the other hand,once we are no longer inside of the definition of
   `implicit_tail_foo`'s let rec, we should be able to pass it locals. *)
let rec implicit_tail_foo n (local_ str) =
  if n > 0 then
    implicit_tail_bar (n - 2) str
  else f str
and implicit_tail_bar n (local_ str) =
  implicit_tail_foo (n + 1) str

let call_foo n =
  let local_ str = "hello" in
  implicit_tail_foo n str

[%%expect {|
val implicit_tail_foo : int -> local_ 'a -> unit = <fun>
val implicit_tail_bar : int -> local_ 'a -> unit = <fun>
val call_foo : int -> unit = <fun>
|}]


(* Same as the test above, except with let expressions. *)
let () =
  let rec implicit_tail_foo n (local_ str) =
    if n > 0 then
      implicit_tail_bar (n - 2) str
    else f str
  and implicit_tail_bar n (local_ str) =
    implicit_tail_foo (n + 1) str
  in
  let call_foo n =
    let local_ str = "hello" in
    implicit_tail_foo n str
  in ignore call_foo

[%%expect {|
|}]

(* Two level nested letrec with a third let helper: Parent structure *)
let rec outer1 (local_ str) = outer2 str

and outer2 (local_ str) = begin
  let rec inner1 (local_ str) = inner2 str
  and inner2 (local_ str) = begin
    let helper (local_ str) = if Sys.opaque_identity true then "goodbye" else outer1 str
    in helper str
  end in
  let local_ str = "hello" in
  inner1 str
end

[%%expect {|
val outer1 : local_ string -> string = <fun>
val outer2 : local_ string -> string = <fun>
|}]


(* Two level nested letrec with a third let helper: Parent expression *)
let _ =
  let rec _outer1 (local_ str) = outer2 str

  and outer2 (local_ str) = begin
    let rec inner1 (local_ str) = inner2 str
    and inner2 (local_ str) = begin
      let helper (local_ str) = if Sys.opaque_identity true then "goodbye" else outer1 str
      in helper str
    end in inner1 str
  end in
  let local_ str = "hello" in
  _outer1 str;

[%%expect {|
- : string = "goodbye"
|}]

(* ----- Nested letrec (in body) tests ----- *)
(* Calls to functions defined in a parent letrec should be nontail *)
let () =
  let rec outer1 (local_ str) = outer2 str
  and outer2 (local_ str) =
    if Sys.opaque_identity true then "goodbye" else outer1 str
  in 
  let local_ str = "hello" in
  ignore (outer1 str)
[%%expect {|
|}]

(* They should still be nontail if letrecs are nested. *)
let _ =
  let rec outer1 (local_ str) = outer2 str
  and outer2 (local_ str) = if Sys.opaque_identity true then "goodbye" else outer1 str in
  let rec inner1 (local_ str) = inner2 str
  and inner2 (local_ str) = if Sys.opaque_identity true then "goodbye" else outer1 str in
  let local_ str = "hello" in
  inner1 str

[%%expect {|
- : string = "goodbye"
|}]
