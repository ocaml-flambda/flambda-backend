(* TEST
 flags = "-extension unique -w +53";
 expect;
*)

(* This file tests the typing around mutable() logic. *)

(* By default, mutable implies [global many shared] modalities *)
type r = {mutable s : string}
let foo (local_ s) = local_ {s}
[%%expect{|
type r = { mutable s : string; }
Line 2, characters 29-30:
2 | let foo (local_ s) = local_ {s}
                                 ^
Error: This value escapes its region.
|}]

(* [@no_mutable_implied_modalities] disables those implied modalities, and
   allows us to test [mutable] alone *)

(* Note the attribute is not printed back, which might be confusing.
   Considering this is a short-term workaround, let's not worry too much. *)
type 'a r = {mutable s : 'a [@no_mutable_implied_modalities]}
[%%expect{|
type 'a r = { mutable s : 'a; }
|}]

(* We can now construct a local record using a local field. *)
let foo (local_ s) = local_ {s}
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

(* We can still add modalities explicitly. Of course, the print-back is
   confusing. *)
type r' = {mutable s' : string @@ global [@no_mutable_implied_modalities]}
[%%expect{|
type r' = { mutable global_ s' : string; }
|}]

let foo (local_ s') = local_ {s'}
[%%expect{|
Line 1, characters 30-32:
1 | let foo (local_ s') = local_ {s'}
                                  ^^
Error: This value escapes its region.
|}]

(* mutable defaults to mutable(legacy = nonportable), so currently we can't construct a
   portable record (ignoring mode-crossing). *)
let foo (s @ portable) = ({s} : _ @@ portable)
[%%expect{|
Line 1, characters 26-29:
1 | let foo (s @ portable) = ({s} : _ @@ portable)
                              ^^^
Error: This value is nonportable but expected to be portable.
|}]

(* For monadic axes, mutable defaults to mutable(min). So currently we can't
   write a [contended] value to a mutable field. *)
let foo (r @ uncontended) (s @ contended) = r.s <- s
[%%expect{|
Line 1, characters 51-52:
1 | let foo (r @ uncontended) (s @ contended) = r.s <- s
                                                       ^
Error: This value is contended but expected to be uncontended.
|}]

module M : sig
  type t = { mutable s : string [@no_mutable_implied_modalities] }
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
         sig type t = { mutable s : string; } end
       Type declarations do not match:
         type t = { mutable s : string; }
       is not included in
         type t = { mutable s : string; }
       Fields do not match:
         mutable s : string;
       is not the same as:
         mutable s : string;
       The second is empty and the first is shared.
|}]

module M : sig
  type t = { mutable s : string }
end = struct
  type t = { mutable s : string [@no_mutable_implied_modalities] }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable s : string [@no_mutable_implied_modalities] }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable s : string; } end
       is not included in
         sig type t = { mutable s : string; } end
       Type declarations do not match:
         type t = { mutable s : string; }
       is not included in
         type t = { mutable s : string; }
       Fields do not match:
         mutable s : string;
       is not the same as:
         mutable s : string;
       The second is global_ and the first is not.
|}]

type r =
  { f : string -> string;
    mutable a : int }
let r @ portable =
  { f = (fun x -> x);
    a = 42 }
(* CR mode-crossing: The [m0] in mutable should cross modes upon construction. *)
[%%expect{|
type r = { f : string -> string; mutable a : int; }
Lines 5-6, characters 2-12:
5 | ..{ f = (fun x -> x);
6 |     a = 42 }
Error: This value is nonportable but expected to be portable.
|}]

type r =
  { f : string -> string;
    mutable g : string -> string @@ portable }
let r @ portable =
  { f = (fun x -> x);
    g = fun x -> x }
(* CR mode-crossing: The [m0] in mutable corresponds to the field type wrapped
   in modality; as a result, it enjoys mode crossing enabled by the modality. *)
[%%expect{|
type r = { f : string -> string; mutable g : string -> string @@ portable; }
Lines 5-6, characters 2-20:
5 | ..{ f = (fun x -> x);
6 |     g = fun x -> x }
Error: This value is nonportable but expected to be portable.
|}]
