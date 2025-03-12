(* TEST
 expect;
*)

(* mode crossing during inclusion check is according to the written type, not
the inferred type. *)

(* In this example, the inferred type does not allow mode crossing, but the
written type does. *)
module M : sig
    val f : int @ nonportable -> int @ portable
end = struct
    let f (x @ portable) = (x : _ @@ nonportable)
end
[%%expect{|
module M : sig val f : int -> int @ portable end
|}]

(* In this example, the inferred type allows crossing to portable, but the
written type does not. *)
module M : sig
    val f : unit -> [`A | `B of 'a -> 'a] @ portable
end = struct
    let f () = (`A : _ @@ nonportable)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |     let f () = (`A : _ @@ nonportable)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> [> `A ] end
       is not included in
         sig val f : unit -> [ `A | `B of 'a -> 'a ] @ portable end
       Values do not match:
         val f : unit -> [> `A ]
       is not included in
         val f : unit -> [ `A | `B of 'a -> 'a ] @ portable
       The type "unit -> [ `A | `B of 'a -> 'a ]"
       is not compatible with the type
         "unit -> [ `A | `B of 'a -> 'a ] @ portable"
|}]

(* In this example, the inferred type does not allow crossing portability, but
the written type does. *)
module M : sig
    val f : [`A] @ nonportable -> unit
end = struct
    let f (x : [< `A | `B of string -> string] @@ portable) =
        match x with
        | `A -> ()
        | `B f -> ()
end
[%%expect{|
module M : sig val f : [ `A ] -> unit end
|}]

module M : sig
    val f : unit -> int
end = struct
    let f () = exclave_ 42
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |     let f () = exclave_ 42
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> local_ int end
       is not included in
         sig val f : unit -> int end
       Values do not match:
         val f : unit -> local_ int
       is not included in
         val f : unit -> int
       The type "unit -> local_ int" is not compatible with the type
         "unit -> int"
|}]

module M : sig
    val f : local_ int -> int
end = struct
    let f (_ @ global) = 42
end
[%%expect{|
module M : sig val f : local_ int -> int end
|}]

(* Check all the mode crossing coercions *)

type cross_global : value mod global
type cross_local : value mod local
type cross_many : value mod many
type cross_once : value mod once
type cross_portable : value mod portable
type cross_nonportable : value mod nonportable
type cross_unyielding : value mod unyielding
type cross_yielding : value mod yielding
type cross_aliased : value mod aliased
type cross_unique : value mod unique
type cross_contended : value mod contended
type cross_shared : value mod shared
type cross_uncontended : value mod uncontended
[%%expect{|
type cross_global : value mod global
type cross_local
type cross_many : value mod many
type cross_once
type cross_portable : value mod portable
type cross_nonportable
type cross_unyielding : value mod unyielding
type cross_yielding
type cross_aliased : value mod aliased
type cross_unique
type cross_contended : value mod contended
type cross_shared : value mod shared
type cross_uncontended
|}]

let cross_global (x : cross_global @@ local) : _ @@ global = x
[%%expect{|
val cross_global : local_ cross_global -> cross_global = <fun>
|}]

let cross_local (x : cross_local @@ local) : _ @@ global = x
[%%expect{|
Line 1, characters 59-60:
1 | let cross_local (x : cross_local @@ local) : _ @@ global = x
                                                               ^
Error: This value escapes its region.
|}]

let cross_many (x : cross_many @@ once) : _ @@ many = x
[%%expect{|
val cross_many : cross_many @ once -> cross_many = <fun>
|}]

let cross_once (x : cross_once @@ once) : _ @@ many = x
[%%expect{|
Line 1, characters 54-55:
1 | let cross_once (x : cross_once @@ once) : _ @@ many = x
                                                          ^
Error: This value is "once" but expected to be "many".
|}]

let cross_portable (x : cross_portable @@ nonportable) : _ @@ portable = x
[%%expect{|
val cross_portable : cross_portable -> cross_portable @ portable = <fun>
|}]

let cross_nonportable (x : cross_nonportable @@ nonportable) : _ @@ portable = x
[%%expect{|
Line 1, characters 79-80:
1 | let cross_nonportable (x : cross_nonportable @@ nonportable) : _ @@ portable = x
                                                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let cross_unyielding (x : cross_unyielding @@ yielding) : _ @@ unyielding = x
[%%expect{|
val cross_unyielding : cross_unyielding @ yielding -> cross_unyielding =
  <fun>
|}]

let cross_yielding (x : cross_yielding @@ yielding) : _ @@ unyielding = x
[%%expect{|
Line 1, characters 72-73:
1 | let cross_yielding (x : cross_yielding @@ yielding) : _ @@ unyielding = x
                                                                            ^
Error: This value is "yielding" but expected to be "unyielding".
|}]

let cross_aliased (x : cross_aliased @@ aliased) : _ @@ unique = x
[%%expect{|
val cross_aliased : cross_aliased -> cross_aliased @ unique = <fun>
|}]

let cross_unique (x : cross_unique @@ aliased) : _ @@ unique = x
[%%expect{|
Line 1, characters 63-64:
1 | let cross_unique (x : cross_unique @@ aliased) : _ @@ unique = x
                                                                   ^
Error: This value is "aliased" but expected to be "unique".
|}]

let cross_contended1 (x : cross_contended @@ shared) : _ @@ uncontended = x
[%%expect{|
val cross_contended1 : cross_contended @ shared -> cross_contended = <fun>
|}]

let cross_contended2 (x : cross_contended @@ contended) : _ @@ shared = x
[%%expect{|
val cross_contended2 :
  cross_contended @ contended -> cross_contended @ shared = <fun>
|}]

let cross_shared1 (x : cross_shared @@ shared) : _ @@ uncontended = x
[%%expect{|
val cross_shared1 : cross_shared @ shared -> cross_shared = <fun>
|}]

let cross_shared2 (x : cross_shared @@ contended) : _ @@ shared = x
[%%expect{|
Line 1, characters 66-67:
1 | let cross_shared2 (x : cross_shared @@ contended) : _ @@ shared = x
                                                                      ^
Error: This value is "contended" but expected to be "shared".
|}]

let cross_uncontended1 (x : cross_uncontended @@ shared) : _ @@ uncontended = x
[%%expect{|
Line 1, characters 78-79:
1 | let cross_uncontended1 (x : cross_uncontended @@ shared) : _ @@ uncontended = x
                                                                                  ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let cross_uncontended2 (x : cross_uncontended @@ contended) : _ @@ shared = x
[%%expect{|
Line 1, characters 76-77:
1 | let cross_uncontended2 (x : cross_uncontended @@ contended) : _ @@ shared = x
                                                                                ^
Error: This value is "contended" but expected to be "shared".
|}]

(* Check that all modalities cross modes *)

type t
type s : value mod global = { v : t @@ global } [@@unboxed]
[%%expect{|
type t
type s = { global_ v : t; } [@@unboxed]
|}]
type s : value mod many = { v : t @@ many } [@@unboxed]
[%%expect{|
type s = { v : t @@ many; } [@@unboxed]
|}]
type s : value mod portable = { v : t @@ portable } [@@unboxed]
[%%expect{|
type s = { v : t @@ portable; } [@@unboxed]
|}]
type s : value mod unyielding = { v : t @@ unyielding } [@@unboxed]
[%%expect{|
type s = { v : t; } [@@unboxed]
|}]
type s : value mod aliased = { v : t @@ aliased } [@@unboxed]
[%%expect{|
type s = { v : t @@ aliased; } [@@unboxed]
|}]
type s : value mod contended = { v : t @@ contended } [@@unboxed]
[%%expect{|
type s = { v : t @@ contended; } [@@unboxed]
|}]
type s : value = { v : t @@ shared } [@@unboxed]
(* CR layouts: Ideally, this should have a better jkind than [value], but we
   don't yet support the interaction between middle modes (like [shared]) and
   modal kinds. *)
[%%expect{|
type s = { v : t @@ shared; } [@@unboxed]
|}]
