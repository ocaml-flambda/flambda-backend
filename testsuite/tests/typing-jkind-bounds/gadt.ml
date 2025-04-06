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
[%%expect {|
val foo : t @ contended -> unit = <fun>
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
[%%expect {|
val foo : int t @ once -> unit = <fun>
|}]

let foo (t : int t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 44-45:
1 | let foo (t : int t @@ aliased) = use_unique t
                                                ^
Error: This value is "aliased" but expected to be "unique".
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

type ('a, 'b) t : immutable_data with 'a = { inner : 'a }
[%%expect{|
type ('a, 'b) t = { inner : 'a; }
|}]

(* We can have existential variables, as long as they don't end up in our with-bounds *)
type 'a u : immutable_data with 'a =
| P1 : ('a1, 'b) t -> 'a1 u
| P2 : ('a2, 'b) t -> 'a2 u
[%%expect{|
type 'a u = P1 : ('a1, 'b) t -> 'a1 u | P2 : ('a2, 'b) t -> 'a2 u
|}]

(* Any existentials directly in the with-bounds get treated as if they're [Best]
   (this test intentionally causes a subkind error to make sure that the existentials
   don't show up in the with-bounds)
*)
type 'x u : immediate =
| P1 : ('b, 'a1) t -> 'a1 u
[%%expect{|
Lines 1-2, characters 0-27:
1 | type 'x u : immediate =
2 | | P1 : ('b, 'a1) t -> 'a1 u
Error: The kind of type "u" is value
         because it's a boxed variant type.
       But the kind of type "u" must be a subkind of immediate
         because of the annotation on the declaration of the type u.
|}]

type 'a u : immutable_data =
| P1 : ('b, 'a) t -> 'a u
[%%expect{|
Lines 1-2, characters 0-25:
1 | type 'a u : immutable_data =
2 | | P1 : ('b, 'a) t -> 'a u
Error: The kind of type "u" is value
         because it's a boxed variant type.
       But the kind of type "u" must be a subkind of immutable_data
         because of the annotation on the declaration of the type u.
|}]

type ('x, 'y) t : immutable_data with 'x with 'y =
  | T : 'a -> ('a, 'a) t
  | U : 'c -> ('b,  'c) t
[%%expect{|
type ('x, 'y) t = T : 'a -> ('a, 'a) t | U : 'c -> ('b, 'c) t
|}]

type 'a t : immediate =
  | A : 'b -> 'b option t
[%%expect{|
Lines 1-2, characters 0-25:
1 | type 'a t : immediate =
2 |   | A : 'b -> 'b option t
Error: The kind of type "t" is value
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immediate
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data =
  | A : ('b : immutable_data). 'b -> 'b option t
[%%expect{|
type 'a t = A : ('b : immutable_data). 'b -> 'b option t
|}]

type 'a t : immediate =
  | A : ('b : immutable_data). 'b -> 'b option t
[%%expect{|
Lines 1-2, characters 0-48:
1 | type 'a t : immediate =
2 |   | A : ('b : immutable_data). 'b -> 'b option t
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immediate
         because of the annotation on the declaration of the type t.
|}]

type 'a cell : mutable_data with 'a =
  | Nil : 'a cell
  | Cons of { value : 'a; mutable next: 'a cell }
[%%expect{|
type 'a cell =
    Nil : 'a cell
  | Cons of { value : 'a; mutable next : 'a cell; }
|}]

type 'a cell : mutable_data with 'a =
  | Nil
  | Cons : { value : 'a; mutable next: 'a cell } -> 'a cell
[%%expect{|
type 'a cell =
    Nil
  | Cons : { value : 'a; mutable next : 'a cell; } -> 'a cell
|}]

(* Abstract types over existentials shouldn't show up in the with-bounds - they should
   just get treated as best.

   This test intentionally triggers a kind error to check this via the printed kind in the
   error message.
*)
type 'a abstract : value mod portable
type existential_abstract : immediate =
  | P : ('a : value mod portable). 'a abstract -> existential_abstract
[%%expect{|
type 'a abstract : value mod portable
Lines 2-3, characters 0-70:
2 | type existential_abstract : immediate =
3 |   | P : ('a : value mod portable). 'a abstract -> existential_abstract
Error: The kind of type "existential_abstract" is value mod portable
         because it's a boxed variant type.
       But the kind of type "existential_abstract" must be a subkind of
         immediate
         because of the annotation on the declaration of the type existential_abstract.
|}]
