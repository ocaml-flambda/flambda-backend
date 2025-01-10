(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Baseline: if the jkind doesn't match, we should get an error. *)
module Mismatched_no_attrs : sig
  type t : float64
end = struct
  type t = string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t : float64 end
       Type declarations do not match:
         type t = string
       is not included in
         type t : float64
       The layout of the first is value
         because it is the primitive type string.
       But the layout of the first must be a sublayout of float64
         because of the definition of t at line 2, characters 2-18.
|}]

(* On the other hand, if we set the correct attributes on both the impl and the intf, we
   shouldn't get an error (though, obviously, this is completely unsound!) *)
module Mismatched_with_both_attrs : sig
  type t : float64
  [@@unsafe_allow_any_kind_in_impl "I love segfaults"]
end = struct
  type t = string
  [@@unsafe_allow_any_kind_in_intf "I love segfaults"]
end
[%%expect{|
module Mismatched_with_both_attrs : sig type t : float64 end
|}]

(* If we set the attributes but *don't* get a kind mismatch, we ought to be fine *)
module Matching : sig
  type t : value
  [@@unsafe_allow_any_kind_in_impl "I love segfaults"]
end = struct
  type t = string
  [@@unsafe_allow_any_kind_in_intf "I love segfaults"]
end
[%%expect{|
Lines 2-3, characters 2-54:
2 | ..type t : value
3 |   [@@unsafe_allow_any_kind_in_impl "I love segfaults"]
Warning 212 [unnecessary-allow-any-kind]: [@@allow_any_kind_in_intf] and [@@allow_any_kind_in_impl] set on a
type, but the kind matches. The attributes can be removed.

module Matching : sig type t end
|}]

(* If the attr is only on the signature we should get an error *)
module Mismatched_with_attr_on_intf : sig
  type t : float64
  [@@unsafe_allow_any_kind_in_impl "I love segfaults"]
end = struct
  type t = string
end
[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t = string
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t : float64 end
       Type declarations do not match:
         type t = string
       is not included in
         type t : float64
       The layout of the first is value
         because it is the primitive type string.
       But the layout of the first must be a sublayout of float64
         because of the definition of t at lines 2-3, characters 2-54.
|}]

(* If the attr is only on the struct we should get an error *)
module Mismatched_with_attr_on_impl : sig
  type t : float64
end = struct
  type t = string
  [@@unsafe_allow_any_kind_in_intf "I love segfaults"]
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type t = string
5 |   [@@unsafe_allow_any_kind_in_intf "I love segfaults"]
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t : float64 end
       Type declarations do not match:
         type t = string
       is not included in
         type t : float64
       The layout of the first is value
         because it is the primitive type string.
       But the layout of the first must be a sublayout of float64
         because of the definition of t at line 2, characters 2-18.
|}]

(* Some more complex stuff with functors *)

module type S1 = sig
  type t : value
end

module type S2 = sig
  type t : float64
  [@@unsafe_allow_any_kind_in_impl]
end

module type S1 = sig
  type t : value
  [@@unsafe_allow_any_kind_in_intf]
end

module F1 (X : S1) : S2 = X

[%%expect{|
module type S1 = sig type t end
module type S2 = sig type t : float64 end
module type S1 = sig type t end
module F1 : functor (X : S1) -> S2
|}]

module F2 (X : S2) : S1 = X
[%%expect{|
Line 1, characters 26-27:
1 | module F2 (X : S2) : S1 = X
                              ^
Error: Signature mismatch:
       Modules do not match: sig type t = X.t end is not included in S1
       Type declarations do not match: type t = X.t is not included in type t
       The layout of the first is float64
         because of the definition of t at lines 6-7, characters 2-35.
       But the layout of the first must be a sublayout of value
         because of the definition of t at lines 11-12, characters 2-35.
|}]

(* Non-abstract types can be annotated with [@@unsafe_allow_any_kind_in_intf] too, and get
   checked against signatures during inclusion. *)

module M1 : sig
  type t : value = string [@@unsafe_allow_any_kind_in_intf]
end = struct
  type t = string
end

module M2 : S2 = M1

[%%expect{|
module M1 : sig type t = string end
module M2 : S2
|}]

module type S3 = sig
  type t : value
  [@@unsafe_allow_any_kind_in_impl]
end

module M3 : S3 = M1
(* CR aspsmith: This is somewhat unfortunate, if S3 and M1 are defined far away, but it's
   unclear how to squash the warning *)
[%%expect{|
module type S3 = sig type t end
Lines 2-3, characters 2-35:
2 | ..type t : value
3 |   [@@unsafe_allow_any_kind_in_impl]
Warning 212 [unnecessary-allow-any-kind]: [@@allow_any_kind_in_intf] and [@@allow_any_kind_in_impl] set on a
type, but the kind matches. The attributes can be removed.

module M3 : S3
|}]
