(* TEST
 flags = "-w +53";
 expect;
*)

(* This file tests the typing around mutable() logic. *)

(* By default, mutable implies all legacy modalities *)
type r = {mutable s : string}
let foo (local_ s) = exclave_ {s}
[%%expect{|
type r = { mutable s : string; }
Line 2, characters 31-32:
2 | let foo (local_ s) = exclave_ {s}
                                   ^
Error: This value escapes its region.
|}]

(* you can override those implied modalities *)
type r = {mutable s : string @@ local}
let foo (local_ s) = exclave_ {s}
[%%expect{|
type r = { mutable s : string @@ local; }
val foo : local_ string -> local_ r = <fun>
|}]

type r = {mutable s : string @@ global}
[%%expect{|
type r = { mutable s : string; }
|}]

type r = {mutable s : string @@ global yielding}
[%%expect{|
type r = { mutable s : string @@ yielding; }
|}]

type r = {mutable s : string @@ yielding global}
[%%expect{|
type r = { mutable s : string @@ yielding; }
|}]

type r = {mutable s : string @@ yielding}
[%%expect{|
type r = { mutable s : string @@ yielding; }
|}]

type r = {mutable s : string @@ local yielding}
[%%expect{|
type r = { mutable s : string @@ local; }
|}]

type r = {mutable s : string @@ yielding local}
[%%expect{|
type r = { mutable s : string @@ local; }
|}]

type r = {mutable s : string @@ local unyielding}
[%%expect{|
type r = { mutable s : string @@ local unyielding; }
|}]

type r = {mutable s : string @@ unyielding local}
[%%expect{|
type r = { mutable s : string @@ local unyielding; }
|}]

type 'a r = {mutable s : 'a @@ local}
[%%expect{|
type 'a r = { mutable s : 'a @@ local; }
|}]

(* We can now construct a local record using a local field. *)
let foo (local_ s) = exclave_ {s}
[%%expect{|
val foo : local_ 'a -> local_ 'a r = <fun>
|}]

(* Mutation needs to be global *)
let foo (local_ r) =
  r.s <- (local_ "hello")
[%%expect{|
Line 2, characters 9-25:
2 |   r.s <- (local_ "hello")
             ^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo (local_ r) = ref r.s
[%%expect{|
Line 1, characters 25-28:
1 | let foo (local_ r) = ref r.s
                             ^^^
Error: This value escapes its region.
|}]

let foo (local_ r) =
  r.s <- "hello"
[%%expect{|
val foo : local_ string r -> unit = <fun>
|}]

(* We can still add modalities explicitly. But they might be omitted if they are
  the same as the mutable-implied ones. *)
type r' = {mutable s' : string @@ global}
[%%expect{|
type r' = { mutable s' : string; }
|}]

let foo (local_ s') = exclave_ {s'}
[%%expect{|
Line 1, characters 32-34:
1 | let foo (local_ s') = exclave_ {s'}
                                    ^^
Error: This value escapes its region.
|}]

(* mutable defaults to mutable(legacy = nonportable), so currently we can't construct a
   portable record (ignoring mode-crossing). *)
let foo (s @ portable) = ({s} : _ @ portable)
[%%expect{|
Line 1, characters 26-29:
1 | let foo (s @ portable) = ({s} : _ @ portable)
                              ^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* This attribute doesn't disable implied modalities on monadic axes. For
   example, there is a [aliased] modality on the [s] field, which allows the
   following to type check. Otherwise, new values in mutation are required to be
   [unique]. *)
let foo r (s @ aliased) = r.s <- s
[%%expect{|
val foo : 'a r -> 'a -> unit = <fun>
|}]

let foo (s @ aliased) = ({s} : _ @ unique)
[%%expect{|
val foo : 'a -> 'a r = <fun>
|}]

let foo (r @ unique) = (r.s : _ @ unique)
[%%expect{|
Line 1, characters 24-27:
1 | let foo (r @ unique) = (r.s : _ @ unique)
                            ^^^
Error: This value is "aliased" but expected to be "unique".
|}]

module M : sig
  type t = { mutable s : string @@ local }
end = struct
  type t = { mutable s : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable s : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable s : string; } end
       is not included in
         sig type t = { mutable s : string @@ local; } end
       Type declarations do not match:
         type t = { mutable s : string; }
       is not included in
         type t = { mutable s : string @@ local; }
       Fields do not match:
         "mutable s : string;"
       is not the same as:
         "mutable s : string @@ local;"
       The first is global and the second is not.
|}]

module M : sig
  type t = { mutable s : string }
end = struct
  type t = { mutable s : string @@ local}
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable s : string @@ local}
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable s : string @@ local; } end
       is not included in
         sig type t = { mutable s : string; } end
       Type declarations do not match:
         type t = { mutable s : string @@ local; }
       is not included in
         type t = { mutable s : string; }
       Fields do not match:
         "mutable s : string @@ local;"
       is not the same as:
         "mutable s : string;"
       The second is global and the first is not.
|}]

type r =
  { f : string -> string;
    mutable a : int }
let r @ portable =
  { f = (fun x -> x);
    a = 42 }
(* mutable default to mutable(nonportable), but the field is integer and crosses
modes *)
[%%expect{|
type r = { f : string -> string; mutable a : int; }
val r : r = {f = <fun>; a = 42}
|}]

let r @ portable =
  let r = {f = (fun x -> x); a = 42} in
  {r with f = (fun x -> x)}
[%%expect{|
val r : r = {f = <fun>; a = 42}
|}]

type r =
  { f : string -> string;
    mutable g : string -> string @@ portable }
let r @ portable =
  { f = (fun x -> x);
    g = fun x -> x }
(* mutable defaults to mutable(nonportable), but the field has modality and crosses
modes. *)
[%%expect{|
type r = { f : string -> string; mutable g : string -> string @@ portable; }
val r : r = {f = <fun>; g = <fun>}
|}]

let r @ portable =
  let r = {f = (fun x -> x); g = fun x -> x} in
  {r with f = fun x -> x}
[%%expect{|
val r : r = {f = <fun>; g = <fun>}
|}]

let r : int array @ portable = [| 42; 24 |]
[%%expect{|
val r : int array = [|42; 24|]
|}]

(* CR zqian: the following should pass but does not. Would have to shuffle
things in [type_expect_record]. *)
type 'a r =
  { f : string -> string;
    mutable a : 'a
  }
let r : int r @ portable =
  { f = (fun x -> x);
    a = 42 }
[%%expect{|
type 'a r = { f : string -> string; mutable a : 'a; }
Lines 6-7, characters 2-12:
6 | ..{ f = (fun x -> x);
7 |     a = 42 }
Error: This value is "nonportable" but expected to be "portable".
|}]
