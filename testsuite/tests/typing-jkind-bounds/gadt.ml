(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()

type ('a : value mod global) require_global
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
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
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
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

(***********************************************************************)
(* This test is about trying to avoid inconsistent contexts.
   See
   https://github.com/ocaml-flambda/flambda-backend/pull/3284#discussion_r1920019049
*)

module type S = sig
  type t1
  type t2
end

type (_ : any, _ : any) eq =
  Refl : ('a : any). ('a, 'a) eq

let use_portable (_ : _ @@ portable) = ()

module F (X : S) = struct
  type t3 : value mod portable with X.t1
  type t4 : value mod portable with X.t2
end

module Arg1 = struct
  type t1 = int -> int
  type t2 = string
end

module M1 = F(Arg1)

let f (witness : (M1.t3, M1.t4) eq)
      (t3 : M1.t3 @@ nonportable) (t4 : M1.t4 @@ nonportable) =
  match witness with
  | Refl ->
    use_portable t3;
    use_portable t4

(* CR layouts v2.8: This is obviously terrible. But at least it's not
   a soundness problem. *)
[%%expect{|
module type S = sig type t1 type t2 end
type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq
val use_portable : 'a @ portable -> unit = <fun>
module F :
  functor (X : S) ->
    sig
      type t3 : value mod portable with X.t1
      type t4 : value mod portable with X.t2
    end
module Arg1 : sig type t1 = int -> int type t2 = string end
module M1 : sig type t3 = F(Arg1).t3 type t4 = F(Arg1).t4 end
>> Fatal error: Abstract kind with [with]: value mod portable
with Arg1.t2
Uncaught exception: Misc.Fatal_error

|}]

(*****************************************)
(* CR layouts v2.8: write more gadt tests *)
