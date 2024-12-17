(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()

type ('a : value mod global) require_global
type ('a : value mod unique) require_unique
type ('a : value mod uncontended) require_uncontended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type ('a : value mod non_null) require_nonnull
type ('a : value mod external_) require_external
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod global) require_global
type ('a : value mod unique) require_unique
type ('a : value mod uncontended) require_uncontended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type 'a require_nonnull
type ('a : value mod external_) require_external
|}]

(***********************************************************************)
type t =
  | Foo : 'a -> t
[%%expect {|
type t = Foo : 'a -> t
|}]

let foo (t : t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 46-47:
1 | let foo (t : t @@ nonportable) = use_portable t
                                                  ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 40-41:
1 | let foo (t : t @@ aliased) = use_unique t
                                            ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************************************************************)
type t =
  | Foo : ('a : immutable_data) -> t
[%%expect {|
type t = Foo : ('a : immutable_data). 'a -> t
|}]

let foo (t : t @@ contended) = use_uncontended t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : t @@ contended) = use_uncontended t
                                                   ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 38-39:
1 | let foo (t : t @@ local) = use_global t [@nontail]
                                          ^
Error: This value escapes its region.
|}]

(***********************************************************************)
type 'a t =
  | Foo : 'a -> 'a t
[%%expect {|
type 'a t = Foo : 'a -> 'a t
|}]

let foo (t : int t @@ once) = use_many t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 39-40:
1 | let foo (t : int t @@ once) = use_many t
                                           ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 13-14:
1 | let foo (t : t @@ aliased) = use_unique t
                 ^
Error: The type constructor "t" expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

(* CR layouts v2.8: write more gadt tests *)
