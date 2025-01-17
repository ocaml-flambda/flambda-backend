(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Non-existent #-type *)
type bad = a#
[%%expect{|
Line 1, characters 11-13:
1 | type bad = a#
               ^^
Error: Unbound type constructor "a"
|}]

type t = float#
[%%expect{|
type t = float#
|}]

type t = float
[%%expect{|
type t = float
|}]

(* Alias gets its own #-type *)
type u = t#
[%%expect{|
type u = t#
|}]

(* Shadowing hides #-type *)
type float
[%%expect{|
type float
|}]

type bad = float#

(* But it's still accessible via the alias *)
type u2 = t#
[%%expect{|
Line 1, characters 11-17:
1 | type bad = float#
               ^^^^^^
Error: "float" has no unboxed version.
|}]

module M = struct
  type t = int32
end
[%%expect{|
module M : sig type t = int32 end
|}]

type int32_u = M.t#
[%%expect{|
type int32_u = M.t#
|}]

(* Restore float *)
type float = t
[%%expect{|
type float = t
|}]

module type S = sig
  type t = float
  type u = t#
end with type t := float
[%%expect{|
module type S = sig type u = float# end
|}]

module S : sig
  type t = float
  type u = t#
  val f : u -> t#
end with type u := float# = struct
  type t = float
  let f x = x
end
[%%expect{|
module S : sig type t = float val f : float# -> t# end
|}]

type 'a t = float
type s = int t#
type v = string t#

let (_ : s -> v) = fun x -> x
[%%expect{|
type 'a t = float
type s = int t#
type v = string t#
- : t/2# -> t/2# = <fun>
|}, Principal{|
type 'a t = float
type s = int t#
type v = string t#
- : s -> v = <fun>
|}]

(* The difference between principal and non-principal above is normal. See: *)

type 'a bar = float
type intbar = int bar
type stringbar = string bar
let (_ : intbar * intbar -> stringbar * stringbar) = fun x -> x
[%%expect{|
type 'a bar = float
type intbar = int bar
type stringbar = string bar
- : stringbar * stringbar -> stringbar * stringbar = <fun>
|}, Principal{|
type 'a bar = float
type intbar = int bar
type stringbar = string bar
- : intbar * intbar -> stringbar * stringbar = <fun>
|}]

type t = float
and s = t#
[%%expect{|
type t = float
and s = t#
|}]

type t = int
and s = t#
[%%expect{|
Line 2, characters 8-10:
2 | and s = t#
            ^^
Error: "t" has no unboxed version. (typedecl.ml)
|}]

type t = s#
and s = t#
[%%expect{|
Line 1, characters 9-11:
1 | type t = s#
             ^^
Error: "s" has no unboxed version. (typedecl.ml)
|}]

type t = s#
and s = float
[%%expect{|
type t = s#
and s = float
|}]

type t = float
and s = t#
[%%expect{|
type t = float
and s = t#
|}]

module rec M : sig
  type t = M2.t#
end = struct
  type t = M2.t#
end
and M2 : sig
  type t = float
end = struct
  type t = float
end
[%%expect{|
module rec M : sig type t = M2.t# end
and M2 : sig type t = float end
|}]

module rec M : sig
  type t = M2.t#
end = struct
  type t = M2.t#
end
and M2 : sig
  type t = int
end = struct
  type t = int
end
[%%expect{|
Line 2, characters 11-16:
2 |   type t = M2.t#
               ^^^^^
Error: "M2.t" has no unboxed version.
|}]

module rec Bad1 : sig
  type t = Bad2.t#
end = struct
  type t = Bad2.t#
end
and Bad2 : sig
  type t = Bad1.t#
end = struct
  type t = Bad1.t#
end
[%%expect{|
Line 2, characters 11-18:
2 |   type t = Bad2.t#
               ^^^^^^^
Error: "Bad2.t" has no unboxed version.
|}]

type t = { i : int ; j : int }
type s = t#
[%%expect{|
type t = { i : int; j : int; }
type s = t#
|}]

type t = { i : int ; j : int }
and s = t#
[%%expect{|
type t = { i : int; j : int; }
and s = t#
|}]

let x () = #{ i = 1 ; j = 2 }
[%%expect{|
val x : unit -> t# = <fun>
|}]

type t = { s : s# }
and s = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { s : s# }
    ^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "s#",
         "s#" contains "t#"
|}]

(* Many cases from the [recursive.ml] test to try... *)

(* Do we give a type a hash type in enter type based on manifest, kind or both?
   I think we give it if *either* has a hash type, in order for both of the
   following to give good errors:

     type a = { i : int }
     type b = a = { i : int } [@@unboxed]
     and s = b#

     type a = { i : int } [@@unboxed]
     type b = a = { i : int }
     and s = b#

   (Also consider if there are other variations than boxed/unboxed versions of
   records, consider abstravct types with unboxed versions (only possible with
   [boxes]). A further challenge: float records; we need to type these to
   determine whether or not to give them dummy unboxed versions. Maybe we give
   all records dummy unboxed versions, then have a check at the end that it's
   [Record_boxed]?)

   Also... these dummy declarations need to have good-enough jkinds for
   typechecking. E.g. products need the right arity (remember the
   product-of-[any]s hack.)

   Another tricky case:
   (Maybe not necessary for exactly this case, but it's worth thinking about
   what dummy jkind [s] has)

   type t = { i : int ; j : int }

   type s = t
   and m = s# = #{ i : int ; j : int }

   (It's fine if we disallow this, I think.)

   Also need to check that unused type error are eliminated by using the hash
   version.
*)

module M = struct
  type t = {x:int}
  module N = struct
    type b = t
    type u = t#
  end
end
[%%expect{||}]
