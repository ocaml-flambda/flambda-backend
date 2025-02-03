(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

let use_as_value : ('a : value) -> 'a = fun x -> x
let use_uncontended : 'a @ uncontended -> 'a = fun x -> x

(* Baseline: if the jkind doesn't match, we should get an error. *)
type t : value mod uncontended = { mutable contents : string }
[%%expect{|
val use_as_value : 'a -> 'a = <fun>
val use_uncontended : ('a : value_or_null). 'a -> 'a = <fun>
Line 5, characters 0-62:
5 | type t : value mod uncontended = { mutable contents : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod uncontended
         because of the annotation on the declaration of the type t.
|}]

(* On the other hand, if we set the attribute, we shouldn't get an error. *)
type t : value mod uncontended = { mutable contents : string }
[@@unsafe_allow_any_mode_crossing]
let f (x : t @@ contended) = use_uncontended x
[%%expect{|
type t : value mod uncontended = { mutable contents : string; }
[@@unsafe_allow_any_mode_crossing]
val f : t @ contended -> t = <fun>
|}]

(* If we set the attribute but *don't* get a kind mismatch, we ought to be fine *)
type t : value mod many portable uncontended = string
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
Lines 1-2, characters 0-34:
1 | type t : value mod many portable uncontended = string
2 | [@@unsafe_allow_any_mode_crossing]
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]

(* The attribute shouldn't allow us to change the layout *)
type t : float64 mod uncontended = { mutable contents : string }
[@@unsafe_allow_any_mode_crossing]
[%%expect{|
Lines 1-2, characters 0-34:
1 | type t : float64 mod uncontended = { mutable contents : string }
2 | [@@unsafe_allow_any_mode_crossing]
Error: The layout of type "t" is value
         because it's a boxed record type.
       But the layout of type "t" must be a sublayout of float64
         because of the annotation on the declaration of the type t.
|}]

(* Abstract types in signatures should work with the unsafe kind *)
module M : sig
  type t : value mod uncontended
end = struct
  type t : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @@ contended) = use_uncontended x
end
[%%expect{|
module M : sig type t : value mod uncontended end
|}]

(* Setting the attribute on an open or abstract type is not allowed *)
module type S = sig
  type abstract [@@unsafe_allow_any_mode_crossing]
end
[%%expect{|
Line 2, characters 2-50:
2 |   type abstract [@@unsafe_allow_any_mode_crossing]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]

type open_ = .. [@@unsafe_allow_any_mode_crossing]
[%%expect{|
Line 1, characters 0-50:
1 | type open_ = .. [@@unsafe_allow_any_mode_crossing]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: [@@unsafe_allow_any_mode_crossing] is not allowed on this kind of type declaration.
       Only records, unboxed products, and variants are supported.
|}]


module M1 : sig
  type t : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @@ contended) = use_uncontended x
end
module M2 : sig
  type t : value mod uncontended = M1.t = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod uncontended = M1.t = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @@ contended) = use_uncontended x
end
[%%expect{|
module M1 :
  sig
    type t : value mod uncontended = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M2 :
  sig
    type t = M1.t : value mod uncontended = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* Private types still require the attribute *)
module Private : sig
  type t : value mod uncontended = private { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t  : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f (x : t @@ contended) = use_uncontended x
end
[%%expect{|
module Private :
  sig
    type t : value mod uncontended = private { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* Non-abstract types in signatures should work as long as they specify the attribute *)
module M : sig
  type t1 : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t2 : value mod uncontended = private { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t3 : value mod uncontended =
    | Immut of string
    | Mut of { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t1 : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t2 : value mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  type t3 : value mod uncontended =
    | Immut of string
    | Mut of { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  let f1 (x : t1 @@ contended) = use_uncontended x
  let f2 (x : t2 @@ contended) = use_uncontended x
  let f3 (x : t3 @@ contended) = use_uncontended x
end
[%%expect{|
module M :
  sig
    type t1 : value mod uncontended = { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
    type t2 : value mod uncontended = private { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
    type t3
      : value mod uncontended =
        Immut of string
      | Mut of { mutable contents : string; }
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

(* [@@unsafe_allow_any_mode_crossing] should not allow you to weaken the modal bounds on a
   kind in module inclusion *)
module M : sig
  type t : value mod uncontended = { mutable x : int } [@@unsafe_allow_any_mode_crossing]
end = struct
  type t = { mutable x : int }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable x : int }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; } end
       is not included in
         sig
           type t : value mod uncontended = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       Type declarations do not match:
         type t = { mutable x : int; }
       is not included in
         type t : value mod uncontended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       the second has [@@unsafe_allow_any_mode_crossing], but the first does not
|}]


module type S = sig
  type t : value mod uncontended = { mutable x : int } [@@unsafe_allow_any_mode_crossing]
end

module M = struct
    type t = { mutable x : int }
end

module _ = (M : S)
[%%expect{|
module type S =
  sig
    type t : value mod uncontended = { mutable x : int; }
    [@@unsafe_allow_any_mode_crossing]
  end
module M : sig type t = { mutable x : int; } end
Line 9, characters 12-13:
9 | module _ = (M : S)
                ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t = { mutable x : int; } end
       is not included in
         S
       Type declarations do not match:
         type t = M.t = { mutable x : int; }
       is not included in
         type t : value mod uncontended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       the second has [@@unsafe_allow_any_mode_crossing], but the first does not
|}]

module type S2 = S with type t = M.t
[%%expect{|
Line 1, characters 24-36:
1 | module type S2 = S with type t = M.t
                            ^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "M.t"
       They have different unsafe mode crossing behavior:
       this has [@@unsafe_allow_any_mode_crossing], but the original does not
|}]

(** The mod-bounds must be equal if the attribute is specified in both the sig and the
    struct *)
module M : sig
  type t : value mod uncontended = { mutable x : int }
  [@@unsafe_allow_any_mode_crossing]
end = struct
  type t : value mod portable uncontended = { mutable x : int }
  [@@unsafe_allow_any_mode_crossing]
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t : value mod portable uncontended = { mutable x : int }
6 |   [@@unsafe_allow_any_mode_crossing]
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t : value mod uncontended portable = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       is not included in
         sig
           type t : value mod uncontended = { mutable x : int; }
           [@@unsafe_allow_any_mode_crossing]
         end
       Type declarations do not match:
         type t : value mod uncontended portable = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       is not included in
         type t : value mod uncontended = { mutable x : int; }
       [@@unsafe_allow_any_mode_crossing]
       They have different unsafe mode crossing behavior:
       Both specify [@@unsafe_allow_any_mode_crossing], but their mod-bounds are not equal
|}]

module A : sig
  type t : value mod external_ global portable many uncontended unique
end = struct
  type t = int
end

module B = struct
  type t : value mod portable uncontended = { a : A.t }
  [@@unsafe_allow_any_mode_crossing]

  let a t = t.a
end
[%%expect{|
module A : sig type t : immediate end
module B :
  sig
    type t : value mod uncontended portable = { a : A.t; }
    [@@unsafe_allow_any_mode_crossing]
    val a : t -> A.t
  end
|}]
