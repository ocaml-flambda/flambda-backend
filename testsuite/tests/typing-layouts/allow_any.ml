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
type t = string
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

(* If the kind annotation on a (non-abstract) type has a more general layout than the one
   inferred from the declaration, we should be able to use the type as if it had the
   inferred layout - but also able to use it as if it had the modal bounds on the
   annotation.
*)
module Weaker_layout_stronger_modes : sig
  type t : any mod uncontended
end = struct
  type t : any mod uncontended = { mutable contents : string }
  [@@unsafe_allow_any_mode_crossing]

  (* The actual kind here looks more like [value mod uncontended] *)
  let f1 (x : t @@ contended) = use_uncontended x
  let _ = use_as_value ({ contents = "foo" } : t)
end
[%%expect{|
module Weaker_layout_stronger_modes : sig type t : any mod uncontended end
|}]
