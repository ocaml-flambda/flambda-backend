(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

(* Simple cases : closed polymorphic variants with fields *)

type 'a t : immutable_data with 'a = [ `A of 'a ]

[%%expect{|
type 'a t = [ `A of 'a ]
|}]

type 'a u : mutable_data with 'a = [ `B of 'a ref ]
[%%expect{|
type 'a u = [ `B of 'a ref ]
|}]

type 'a v : value mod contended with 'a = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
[%%expect{|
type 'a v = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

type 'a w : value mod contended with 'a = [ 'a t | 'a v ]
[%%expect{|
type 'a w = [ `A of 'a | `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

let cross_contention (x : int t @ contended) = use_uncontended x
let cross_portability (x : int t @ nonportable) = use_portable x
let cross_linearity (x : int t @ once) = use_many x
[%%expect{|
val cross_contention : int t @ contended -> unit = <fun>
val cross_portability : int t -> unit = <fun>
val cross_linearity : int t @ once -> unit = <fun>
|}]

let don't_cross_unique (x : int t @ aliased) = use_unique x
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_unique (x : int t @ aliased) = use_unique x
                                                              ^
Error: This value is "aliased" but expected to be "unique".
|}]

let don't_cross_locality (x : int t @ local) = use_global x [@nontail]
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_locality (x : int t @ local) = use_global x [@nontail]
                                                              ^
Error: This value escapes its region.
|}]


let cross_contention (x : int w @ contended) = use_uncontended x
[%%expect{|
val cross_contention : int w @ contended -> unit = <fun>
|}]

let don't_cross_portability (x : int w @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 69-70:
1 | let don't_cross_portability (x : int w @ nonportable) = use_portable x
                                                                         ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Quality *)

module type S = sig
  type 'a polyvar = [ `A of 'a ]
  type 'a abstract : immutable_data with 'a polyvar
end
[%%expect{|
module type S =
  sig
    type 'a polyvar = [ `A of 'a ]
    type 'a abstract : immutable_data with 'a
  end
|}]

(* Since the jkind of [ `A of 'a ] has best quality, we can substitute with another type *)
type 'a simple : immutable_data with 'a
module type S2 = S with type 'a abstract = 'a simple
[%%expect{|
type 'a simple : immutable_data with 'a
module type S2 =
  sig type 'a polyvar = [ `A of 'a ] type 'a abstract = 'a simple end
|}]

(* Contrast: jkinds of abstract types always have non-best quality. *)
type 'a test : immutable_data with 'a

module type S = sig
  type 'a abstract : immutable_data with 'a test
end

module type S2 = S with type 'a abstract = 'a simple
[%%expect{|
type 'a test : immutable_data with 'a
module type S = sig type 'a abstract : immutable_data with 'a test end
Line 7, characters 24-52:
7 | module type S2 = S with type 'a abstract = 'a simple
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a simple" is immutable_data with 'a
         because of the definition of simple at line 1, characters 0-39.
       But the kind of type "'a simple" must be a subkind of immutable_data
         with 'a test
         because of the definition of abstract at line 4, characters 2-48.
|}]

(* When we give open polymorphic variants more precise kinds, we should make sure to give them not-best quality *)
(* CR reisenberg: This test output should probably mention [polyvar] still *)
module type S = sig
  type 'a polyvar = private [< `A of 'a | `B of int ref | `C ]
  type 'a abstract : immutable_data with 'a polyvar
end
[%%expect{|
module type S =
  sig
    type 'a polyvar = private [< `A of 'a | `B of int ref | `C ]
    type 'a abstract : immutable_data with 'a polyvar
  end
|}]

(* CR reisenberg: In the output, [abstract] should have kind [immutable_data] *)
module type S2 = S with type 'a polyvar = [ `C ]
[%%expect{|
module type S2 =
  sig
    type 'a polyvar = [ `C ]
    type 'a abstract : immutable_data with 'a polyvar
  end
|}]

(* CR layouts v2.8: ['a abstract] should get the kind [immutable_data with 'a polyvar]
   and this should fail: *)
module type S3 = S with type 'a record = 'a simple
[%%expect{|
Line 1, characters 17-50:
1 | module type S3 = S with type 'a record = 'a simple
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The signature constrained by "with" has no component named "record"
|}]


(* Harder cases: row variables *)

(* CR layouts v2.8: These are both correct, but we could probably infer a more precise kind for both. *)
type ('a, 'b) t : immutable_data with 'a = [< `X | `Y of 'a] as 'b
[%%expect{|
Line 1, characters 0-66:
1 | type ('a, 'b) t : immutable_data with 'a = [< `X | `Y of 'a] as 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[< `X | `Y of 'a ]" is value mod non_float
         because it's a polymorphic variant type.
       But the kind of type "[< `X | `Y of 'a ]" must be a subkind of
         immutable_data with 'a
         because of the definition of t at line 1, characters 0-66.
|}]
type ('a, 'b) u : immutable_data with 'a = [> `X | `Y of 'a] as 'b
[%%expect{|
Line 1, characters 0-66:
1 | type ('a, 'b) u : immutable_data with 'a = [> `X | `Y of 'a] as 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[> `X | `Y of 'a ]" is value mod non_float
         because it's a polymorphic variant type.
       But the kind of type "[> `X | `Y of 'a ]" must be a subkind of
         immutable_data with 'a
         because of the definition of u at line 1, characters 0-66.
|}]

(* less-than rows *)

let f (x : [< `A of int | `B of string] @ contended) =
  use_uncontended x
(* CR layouts v2.8: This should be accepted  *)
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* CR layouts v2.8: This should also be accepted, but not with a best quality. *)
module M : sig
  type 'a t : immutable_data with 'a = private [< `A of 'a | `B of ('a * 'a) | `C ]
end = struct
  type 'a t = [ `C ]
end
[%%expect{|
Line 2, characters 2-83:
2 |   type 'a t : immutable_data with 'a = private [< `A of 'a | `B of ('a * 'a) | `C ]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[< `A of 'a | `B of 'a * 'a | `C ]" is
         value mod non_float
         because it's a polymorphic variant type.
       But the kind of type "[< `A of 'a | `B of 'a * 'a | `C ]" must be a subkind of
         immutable_data with 'a
         because of the definition of t at line 2, characters 2-83.
|}]

(* Tunivar-ified row variables *)

(* With [> `Foo of int] as 'a, this should not be accepted
   -- we can't restrict the row variable to be [value mod portable]. *)

type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
[%%expect{|
Line 1, characters 64-65:
1 | type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[> `Foo of int ]"
       but is used as an instance of type "('a : value mod portable)"
       The kind of [> `Foo of int ] is value mod non_float
         because it's a polymorphic variant type.
       But the kind of [> `Foo of int ] must be a subkind of
         value mod portable
         because of the annotation on the universal variable 'a.
|}]

type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 64-65:
1 | type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[< `Foo of int ]"
       but is used as an instance of type "('a : value mod portable)"
       The kind of [< `Foo of int ] is value mod non_float
         because it's a polymorphic variant type.
       But the kind of [< `Foo of int ] must be a subkind of
         value mod portable
         because of the annotation on the universal variable 'a.
|}]

(* Recursive polymorphic variants. *)
type trec1 : immutable_data = [ `A of string | `B of 'a ] as 'a
[%%expect{|
type trec1 = [ `A of string | `B of 'a ] as 'a
|}]

type trec2 : immutable_data = [ `A | `B of 'a list ] as 'a
[%%expect{|
type trec2 = [ `A | `B of 'a list ] as 'a
|}]

type trec_fails : immutable_data = [ `C | `D of 'a * unit -> 'a ] as 'a

[%%expect{|
Line 1, characters 0-71:
1 | type trec_fails : immutable_data = [ `C | `D of 'a * unit -> 'a ] as 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `C | `D of 'a * unit -> 'a ] as 'a" is
         value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `C | `D of 'a * unit -> 'a ] as 'a" must be a subkind of
         immutable_data
         because of the definition of trec_fails at line 1, characters 0-71.
|}]

type trec_succeeds : value mod immutable = [ `C | `D of 'a * unit -> 'a ] as 'a

[%%expect{|
type trec_succeeds = [ `C | `D of 'a * unit -> 'a ] as 'a
|}]

type trec_rec_fails : immutable_data =
  [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a

[%%expect{|
Lines 1-2, characters 0-80:
1 | type trec_rec_fails : immutable_data =
2 |   [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a
Error: The kind of type "[ `X of
                            [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ] as 'b
                        | `Y of 'b ] as 'a" is value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `X of
                                [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ]
                                as 'b
                            | `Y of 'b ] as 'a" must be a subkind of
         immutable_data
         because of the definition of trec_rec_fails at lines 1-2, characters 0-80.
|}]

type trec_rec_succeeds : value mod immutable =
  [ `X of 'b | `Y of [ `Z of ('a -> 'b) | `W of 'a | `Loop of 'b ] as 'b ] as 'a

[%%expect{|
type trec_rec_succeeds =
    [ `X of [ `Loop of 'b | `W of 'a | `Z of 'a -> 'b ] as 'b | `Y of 'b ]
    as 'a
|}]

(* Future tests for when we start adding row variables to with-bounds. *)

type 'a t1 = [< `A of string | `B of int ] as 'a
type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
[%%expect{|
type 'a t1 = 'a constraint 'a = [< `A of string | `B of int ]
Line 2, characters 0-47:
2 | type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immediate
         with [< `A of string | `B of int ] t1
         because of the annotation on the declaration of the type t2.
|}]
type t3 : immediate with [ `A of string] t1 = C of string  (* should be accepted *)
[%%expect{|
type t3 = C of string
|}]

type 'a t1 = [> `A of string | `B of int ] as 'a
type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected *)
[%%expect{|
type 'a t1 = 'a constraint 'a = [> `A of string | `B of int ]
Line 2, characters 0-47:
2 | type 'a t2 : immediate with 'a t1 = C of string  (* should be rejected *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immediate
         with [> `A of string | `B of int ] t1
         because of the annotation on the declaration of the type t2.
|}]
type t3 : immediate with [ `A of string | `B of int | `C ] t1 = C of string  (* should be accepted *)
[%%expect{|
type t3 = C of string
|}]

module type S = sig
  type t = private [< `A of string | `B of int ]
end
module M1 : S = struct
  type t = [ `A of string ]
end
type t2 : immediate with M1.t = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
[%%expect{|
module type S = sig type t = private [< `A of string | `B of int ] end
module M1 : S
Line 7, characters 0-43:
7 | type t2 : immediate with M1.t = C of string  (* should be rejected, at least until we sort out closed-but-not-static bestness *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immediate with M1.t
         because of the annotation on the declaration of the type t2.
|}]

module M2 : S with type t = [ `A of string ] = struct
  type t = [ `A of string ]
end
type t3 : immediate with M2.t = C of string (* should be accepted *)
[%%expect{|
module M2 : sig type t = [ `A of string ] end
type t3 = C of string
|}]

type (_, _) eq = Refl : ('a, 'a) eq

(* I'm not sure whether module substitution over a non-static private row type preserves the Tvariant structure; so I made this harder case, too *)
let sneaky (x : (M1.t, [ `A of string ]) eq) = match x with
  | Refl -> let open struct
    type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
  end in ()
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
Line 6, characters 4-47:
6 |     type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t4" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t4" must be a subkind of immediate with M1.t
         because of the annotation on the declaration of the type t4.
|}]

module type S = sig
  type t = private [> `A of string | `B of int ]
end
module M1 : S = struct
  type t = [ `A of string | `B of int | `C of (int -> int) ref ]
end
type t2 : immediate with M1.t = C of string  (* should be rejected *)
[%%expect{|
module type S = sig type t = private [> `A of string | `B of int ] end
module M1 : S
Line 7, characters 0-43:
7 | type t2 : immediate with M1.t = C of string  (* should be rejected *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immediate with M1.t
         because of the annotation on the declaration of the type t2.
|}]

module M2 : S with type t = [ `A of string | `B of int ] = struct
  type t = [ `A of string | `B of int ]
end
type t3 : immediate with M2.t = C of string (* should be accepted *)
[%%expect{|
module M2 : sig type t = [ `A of string | `B of int ] end
type t3 = C of string
|}]

let sneaky (x : (M1.t, [ `A of string | `B of int ]) eq) = match x with
  | Refl -> let open struct
    type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
  end in ()
[%%expect{|
Line 3, characters 4-47:
3 |     type t4 : immediate with M1.t = C of string  (* not sure what will happen, but we should eventually accept *)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t4" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t4" must be a subkind of immediate with M1.t
         because of the annotation on the declaration of the type t4.
|}]
