(* TEST
 include stdlib_upstream_compatible;
 {
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }
*)

type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any   : any;;
[%%expect {|
type t_value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any
|}];;

type t_void  : void;;
[%%expect{|
Line 1, characters 15-19:
1 | type t_void  : void;;
                   ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(*********************************************************)
(* Test 1: Simple with type constraints respect jkinds. *)

(* CR layouts v5: parts of this test moved to [modules_alpha.ml] because they
   need void.  Bring back here when we have it. *)
module type S1 = sig
  type ('a : void) t
  type s
end;;
[%%expect {|
Line 2, characters 13-17:
2 |   type ('a : void) t
                 ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

module type S1f = sig
  type ('a : float64) t
  type s
end;;

type ('a : float64) t1;;

module type S1f' = S1f with type 'a t = t_float64 t1 and type s = t_float64 t1;;

[%%expect {|
module type S1f = sig type ('a : float64) t type s end
type ('a : float64) t1
module type S1f' =
  sig type ('a : float64) t = t_float64 t1 type s = t_float64 t1 end
|}];;

module type S1f'' = S1f with type 'a t = 'a list;;
[%%expect {|
Line 1, characters 34-36:
1 | module type S1f'' = S1f with type 'a t = 'a list;;
                                      ^^
Error: The type constraints are not consistent.
       Type "('a : value)" is not compatible with type "('a0 : float64)"
       The layout of 'a is float64
         because of the definition of t at line 2, characters 2-23.
       But the layout of 'a must overlap with value
         because the type argument of list has layout value.
|}];;

module type S1f'' = S1f with type s = t_float64;;

[%%expect{|
Line 1, characters 29-47:
1 | module type S1f'' = S1f with type s = t_float64;;
                                 ^^^^^^^^^^^^^^^^^^
Error: The layout of type "t_float64" is float64
         because of the definition of t_float64 at line 4, characters 0-24.
       But the layout of type "t_float64" must be a sublayout of value
         because of the definition of s at line 3, characters 2-8.
|}]

module type S1_2 = sig
  type ('a : immediate) t
end

module type S1_2' = S1_2 with type 'a t = 'a list

module M1_2' : S1_2' = struct
  type ('a : immediate) t = 'a list
end;;
[%%expect{|
module type S1_2 = sig type ('a : immediate) t end
module type S1_2' = sig type ('a : immediate) t = 'a list end
module M1_2' : S1_2'
|}]

module M1_2'': S1_2' = struct
  type 'a t = 'a list
end;;
[%%expect{|
module M1_2'' : S1_2'
|}]

module M1_2''' : S1_2 = struct
  type 'a t = 'a list
end;;
[%%expect{|
module M1_2''' : S1_2
|}]

(************************************************************************)
(* Test 2: with type constraints for fixed types (the complicated case of
   Type_mod.merge_constraint) *)
module type S2 = sig
  type ('a : immediate) t
end

type ('a : immediate) r2 = R
type ('a : immediate) s2 = private [> `A of 'a r2]

module type T2 = S2 with type 'a t = 'a s2

module F2 (X : T2) = struct
  let f () : 'a X.t = `A R
end;;
[%%expect{|
module type S2 = sig type ('a : immediate) t end
type ('a : immediate) r2 = R
type (!'a : immediate) s2 = private [> `A of 'a r2 ]
module type T2 = sig type ('a : immediate) t = 'a s2 end
module F2 :
  functor (X : T2) -> sig val f : ('a : immediate). unit -> 'a X.t end
|}]

type ('a : immediate) s2' = private [> `B of 'a]
module type T2' = S2 with type 'a t = 'a s2'

module F2' (X : T2') = struct
  let f () : 'a X.t = `B "bad"
end
[%%expect{|
type (!'a : immediate) s2' = private [> `B of 'a ]
module type T2' = sig type ('a : immediate) t = 'a s2' end
Line 5, characters 25-30:
5 |   let f () : 'a X.t = `B "bad"
                             ^^^^^
Error: This expression has type "string" but an expression was expected of type
         "('a : immediate)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of t at line 2, characters 2-25.
|}]

(******************************************************************)
(* Test 3: Recursive modules, with and without jkind annotations *)

(* CR layouts v5: Some parts of this test need void. *)

module rec Foo3 : sig
  val create : Bar3.t -> unit
end = struct
  let create _ = ()
end

and Bar3 : sig
  type t
end = struct
  type t = unit
end;;
[%%expect {|
module rec Foo3 : sig val create : Bar3.t -> unit end
and Bar3 : sig type t end
|}];;

module rec Foo3 : sig
  val create : Bar3.t -> unit
end = struct
  let create _ = ()
end

and Bar3 : sig
  type t : void
end = struct
  type t : void
end;;
[%%expect {|
Line 8, characters 11-15:
8 |   type t : void
               ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

module rec Foo3f : sig
  val create : Bar3f.t -> unit
end = struct
  let create _ = ()
end

and Bar3f : sig
  type t : float64
end = struct
  type t : float64
end;;
[%%expect {|
module rec Foo3f : sig val create : Bar3f.t -> unit end
and Bar3f : sig type t : float64 end
|}];;

module rec Foo3 : sig
  type t : immediate = Bar3.t
end = struct
  type t = Bar3.t
end

and Bar3 : sig
  type t : value
end = struct
  type t = A
end;;
[%%expect {|
Line 2, characters 2-29:
2 |   type t : immediate = Bar3.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "Bar3.t" is value
         because of the annotation on the declaration of the type t.
       But the kind of type "Bar3.t" must be a subkind of immediate
         because of the definition of t at line 2, characters 2-29.
|}];;

module rec Foo3 : sig
  type t : immediate = Bar3.t
end = struct
  type t = Bar3.t
end

and Bar3 : sig
  type t : immediate
end = struct
  type t = A
end;;
[%%expect {|
module rec Foo3 : sig type t = Bar3.t end
and Bar3 : sig type t : immediate end
|}];;

module rec Foo3f : sig
  type 'a t = 'a Bar3f.t * 'a list
end = struct
  type t = 'a Bar3f.t * 'a list
end

and Bar3f : sig
  type ('a : float64) t
end = struct
  type 'a t
end;;
[%%expect {|
Line 2, characters 27-29:
2 |   type 'a t = 'a Bar3f.t * 'a list
                               ^^
Error: This type "('a : float64)" should be an instance of type "('b : value)"
       The layout of 'a is float64
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because the type argument of list has layout value.
|}];;

type t3f : float64

module rec Foo3f : sig
  type t = t3f
end = struct
  type t = t3f
end

and Bar3f : sig
  type ('a : float64) t

  type s = Foo3f.t t
end = struct
  type ('a : float64) t
  type s = Foo3f.t t
end;;
[%%expect {|
type t3f : float64
Line 12, characters 11-18:
12 |   type s = Foo3f.t t
                ^^^^^^^
Error: This type "Foo3f.t" should be an instance of type "('a : float64)"
       The layout of Foo3f.t is value
         because an abstract type has the value layout by default.
       But the layout of Foo3f.t must be a sublayout of float64
         because of the definition of t at line 10, characters 2-23.
|}];;

(* Previous example works with annotation *)
module rec Foo3f : sig
  type t : float64 = t3f
end = struct
  type t = t3f
end

and Bar3 : sig
  type ('a : float64) t

  type s = Foo3f.t t
end = struct
  type ('a : float64) t
  type s = Foo3f.t t
end;;
[%%expect {|
module rec Foo3f : sig type t = t3f end
and Bar3 : sig type ('a : float64) t type s = Foo3f.t t end
|}];;

(*************************************************************************)
(* Test 4: Nondep typedecl jkind approximation in the Nondep_cannot_erase
   case. *)

(* CR layouts v5: Bring back the void part of this test. *)
module F4(X : sig type t end) = struct
  type s = Foo of X.t
end

module M4 = F4(struct type t = T end)

type ('a : value) t4_val

type t4 = M4.s t4_val;;
[%%expect {|
module F4 : functor (X : sig type t end) -> sig type s = Foo of X.t end
module M4 : sig type s end
type 'a t4_val
type t4 = M4.s t4_val
|}]

type ('a : float64) t4_float64
type t4f' = M4.s t4_float64;;
[%%expect {|
type ('a : float64) t4_float64
Line 2, characters 12-16:
2 | type t4f' = M4.s t4_float64;;
                ^^^^
Error: This type "M4.s" should be an instance of type "('a : float64)"
       The layout of M4.s is value
         because of the definition of s at line 2, characters 2-21.
       But the layout of M4.s must be a sublayout of float64
         because of the definition of t4_float64 at line 1, characters 0-30.
|}]

module F4'(X : sig type t : immediate end) = struct
  type s : immediate = Foo of X.t [@@unboxed]
end

module M4' = F4'(struct type t = T end)

type ('a : immediate) t4_imm

type t4 = M4'.s t4_imm;;
[%%expect{|
module F4' :
  functor (X : sig type t : immediate end) ->
    sig type s : immediate = Foo of X.t [@@unboxed] end
module M4' : sig type s : immediate end
type ('a : immediate) t4_imm
type t4 = M4'.s t4_imm
|}];;

type t4 = M4'.s t4_float64;;
[%%expect{|
Line 1, characters 10-15:
1 | type t4 = M4'.s t4_float64;;
              ^^^^^
Error: This type "M4'.s" should be an instance of type "('a : float64)"
       The layout of M4'.s is value
         because of the definition of s at line 2, characters 2-45.
       But the layout of M4'.s must be a sublayout of float64
         because of the definition of t4_float64 at line 1, characters 0-30.
|}];;


(************************************)
(* Test 5: Destructive substitution *)
module type S3_1 = sig
  type ('a : immediate) t
  val f : 'a -> 'a t
end

module type S3_1' = S3_1 with type 'a t := 'a list

module M3_1 : S3_1' = struct
  let f x = [x]
end

let x3 = M3_1.f 42

let x3' = M3_1.f "test";;
[%%expect{|
module type S3_1 =
  sig type ('a : immediate) t val f : ('a : immediate). 'a -> 'a t end
module type S3_1' = sig val f : ('a : immediate). 'a -> 'a list end
module M3_1 : S3_1'
val x3 : int list = [42]
Line 14, characters 17-23:
14 | let x3' = M3_1.f "test";;
                      ^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "('a : immediate)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of f at line 3, characters 2-20.
|}]

module type S3_2 = sig
  type t : immediate
end

module type S3_2' = S3_2 with type t := string;;
[%%expect{|
module type S3_2 = sig type t : immediate end
Line 5, characters 30-46:
5 | module type S3_2' = S3_2 with type t := string;;
                                  ^^^^^^^^^^^^^^^^
Error: The kind of type "string" is immutable_data
         because it is the primitive type string.
       But the kind of type "string" must be a subkind of immediate
         because of the definition of t at line 2, characters 2-20.
|}]

(*****************************************)
(* Test 6: With constraints on packages. *)

(* CR layouts v5: Bring over void versions of these tests from modules_alpha *)
module type S6_1 = sig
  type t : void
end
[%%expect{|
Line 2, characters 11-15:
2 |   type t : void
               ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

module type S6_1f = sig
  type t : float64
end

module type S6_2f = sig
  val m : (module S6_1f with type t = int)
end;;
[%%expect{|
module type S6_1f = sig type t : float64 end
Line 6, characters 10-42:
6 |   val m : (module S6_1f with type t = int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = int
       is not included in
         type t : float64
       The layout of the first is value
         because it is the primitive immediate type int.
       But the layout of the first must be a sublayout of float64
         because of the definition of t at line 2, characters 2-18.
|}];;

module type S6_3 = sig
  type t : value
end

module type S6_4f = sig
  val m : (module S6_3 with type t = t_float64)
end;;
[%%expect{|
module type S6_3 = sig type t end
Line 6, characters 10-47:
6 |   val m : (module S6_3 with type t = t_float64)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = t_float64
       is not included in
         type t
       The layout of the first is float64
         because of the definition of t_float64 at line 4, characters 0-24.
       But the layout of the first must be a sublayout of value
         because of the definition of t at line 2, characters 2-16.
|}];;

module type S6_5 = sig
  type t : immediate
end

module type S6_6 = sig
  val m : (module S6_5 with type t = string)
end
[%%expect{|
module type S6_5 = sig type t : immediate end
Line 6, characters 10-44:
6 |   val m : (module S6_5 with type t = string)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       The kind of the first is immutable_data
         because it is the primitive type string.
       But the kind of the first must be a subkind of immediate
         because of the definition of t at line 2, characters 2-20.
|}];;

module type S6_6' = sig
  type s
  val m : (module S6_5 with type t = s)
end
[%%expect{|
Line 3, characters 10-39:
3 |   val m : (module S6_5 with type t = s)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = s
       is not included in
         type t : immediate
       The kind of the first is value
         because of the definition of s at line 2, characters 2-8.
       But the kind of the first must be a subkind of immediate
         because of the definition of t at line 2, characters 2-20.
|}];;

module type S6_6'' = sig
  type s = int
  val m : (module S6_5 with type t = int)
end;;
[%%expect{|
module type S6_6'' =
  sig type s = int val m : (module S6_5 with type t = int) end
|}];;

(*****************************************)
(* Test 7: "any" is allowed in Tstr_eval *)
module F (_ : sig end) = struct
  assert false
end;;
[%%expect {|
module F : sig end -> sig end
|}];;

(****************************************)
(* Test 8: [val]s must be representable *)

module type S = sig val x : t_any end

module M = struct
  let x : t_void = assert false
end

[%%expect{|
Line 1, characters 28-33:
1 | module type S = sig val x : t_any end
                                ^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type t_any is any
         because of the definition of t_any at line 5, characters 0-18.
       But the layout of type t_any must be a sublayout of value
         because it's the type of something stored in a module structure.
|}]

(****************************************************************)
(* Test 9: Non-values temporarily banned in recmod safety check *)
module type S = sig
  val f : ('a : float64). 'a -> 'a
end

module rec M : S = M

[%%expect{|
module type S = sig val f : ('a : float64). 'a -> 'a end
Line 5, characters 19-20:
5 | module rec M : S = M
                       ^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: M -> M.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 2-34:
2 |   val f : ('a : float64). 'a -> 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Module "M" defines a function whose first argument is not a value, "f" .
|}]

(*******************************)
(* Test 10: Specializing [any] *)

(* see also the test any_in_types.ml, which makes sure running code
   that looks like this produces the right result *)

module type S = sig
  type t : any

  val one : unit -> t
  val print : t -> unit
end

module Floaty : S with type t := float# = struct
  let one () = Stdlib_upstream_compatible.Float_u.of_float 1.  (* CR layouts: use literal syntax *)
  let print t = Printf.printf "%f" (Stdlib_upstream_compatible.Float_u.to_float t)
end

module Inty : S with type t := int = struct
  let one () = 1
  let print t = Printf.printf "%d" t
end

module Stringy : S with type t := string = struct
  let one () = "one"
  let print t = Printf.printf "%s" t
end

[%%expect{|
module type S =
  sig type t : any val one : unit -> t val print : t -> unit end
module Floaty : sig val one : unit -> float# val print : float# -> unit end
module Inty : sig val one : unit -> int val print : int -> unit end
module Stringy : sig val one : unit -> string val print : string -> unit end
|}]

module F1 (X : S with type t := float#) : sig
  val print_one : unit -> unit
end = struct
  let print_one () = X.print (X.one ())
end

module F2 (X : S with type t := string) : sig
  val print_one : unit -> unit
end = struct
  let print_one () = X.print (X.one ())
end

[%%expect{|
module F1 :
  functor (X : sig val one : unit -> float# val print : float# -> unit end)
    -> sig val print_one : unit -> unit end
module F2 :
  functor (X : sig val one : unit -> string val print : string -> unit end)
    -> sig val print_one : unit -> unit end
|}]

module F_bad (X : S) : sig
  val print_one : unit -> unit
end = struct
  let print_one () = X.print (X.one ())
end

[%%expect{|
Line 4, characters 29-39:
4 |   let print_one () = X.print (X.one ())
                                 ^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of X.t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of X.t must be representable
         because we must know concretely how to pass a function argument.
|}]

(***********************************)
(* Test 11: [any] in package types *)

module type S = sig
  type t : any
end

module C : S = struct
  type t = float
end

let x = (module C : S with type t = 'a)

[%%expect{|
module type S = sig type t : any end
module C : S
val x : (module S with type t = C.t) = <module>
|}]

(*************************************************)
(* Test 12: equations on layout of packaged type *)

module Repr32 = Stdlib.Sys.Immediate64.Make (Int) (Int32)

module type S = sig
  type t = Repr32.t
end

module Choice1 = struct
  type t = int
end
module Choice2 = struct
  type t = int32
end

[%%expect{|
module Repr32 :
  sig
    type t = Sys.Immediate64.Make(Int)(Int32).t
    type 'a repr =
      'a Sys.Immediate64.Make(Int)(Int32).repr =
        Immediate : Int.t repr
      | Non_immediate : Int32.t repr
    val repr : t repr
  end
module type S = sig type t = Repr32.t end
module Choice1 : sig type t = int end
module Choice2 : sig type t = int32 end
|}]

(* Failure: attempt to package without the needed
   equation.
*)

let m : (module S) = (module Choice2)

[%%expect{|
Line 1, characters 29-36:
1 | let m : (module S) = (module Choice2)
                                 ^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig type t = int32 end is not included in S
       Type declarations do not match:
         type t = int32
       is not included in
         type t = Repr32.t
       The type "int32" is not equal to the type
         "Repr32.t" = "Sys.Immediate64.Make(Int)(Int32).t"
|}]

(* Pass: package with needed equation. *)

let m : (module S) =
  match Repr32.repr with
  | Immediate -> (module Choice1)
  | Non_immediate -> (module Choice2)

[%%expect{|
val m : (module S) = <module>
|}]

(*********************************************)
(* Test 13: contravariance of type arguments *)

module M : sig
  type 'a t
end = struct
  type ('a : any) t
end

[%%expect{|
module M : sig type 'a t end
|}]

module M : sig
  type ('a : any) t
end = struct
  type 'a t
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t end
       is not included in
         sig type ('a : any) t end
       Type declarations do not match:
         type 'a t
       is not included in
         type ('a : any) t
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-19.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-11.
|}]


module M : sig
  type 'a t
end = struct
  type ('a : any) t = 'a
end

[%%expect{|
module M : sig type 'a t end
|}]

module M : sig
  type ('a : any) t
end = struct
  type 'a t = 'a
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type ('a : any) t end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type ('a : any) t
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-19.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-16.
|}]


(* this should still fail, in order to remain upstream-compatible *)
module M : sig
  type 'a t constraint 'a = int
end = struct
  type 'a t
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t end
       is not included in
         sig type 'a t constraint 'a = int end
       Type declarations do not match:
         type 'a t
       is not included in
         type 'a t constraint 'a = int
       Their parameters differ:
       The type "'a" is not equal to the type "int"
|}]

module M : sig
  type ('a : value) t = 'a
end = struct
  type ('a : any) t = 'a
end

[%%expect {|
module M : sig type 'a t = 'a end
|}]

module M : sig
  type ('a : any) t = 'a
end = struct
  type ('a : value) t = 'a
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type ('a : any) t = 'a end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type ('a : any) t = 'a
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-24.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-26.
|}]

module M : sig
  type ('a : value) t2
  type ('a : value) t = 'a t2
end = struct
  type ('a : any) t
  type ('a : any) t2 = 'a t
end

[%%expect {|
module M : sig type 'a t2 type 'a t = 'a t2 end
|}]

module M : sig
  type ('a : any) t2
  type ('a : any) t = 'a t2
end = struct
  type ('a : value) t
  type ('a : value) t2 = 'a t
end

[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type ('a : value) t
6 |   type ('a : value) t2 = 'a t
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t type 'a t2 = 'a t end
       is not included in
         sig type ('a : any) t2 type ('a : any) t = 'a t2 end
       Type declarations do not match:
         type 'a t2 = 'a t
       is not included in
         type ('a : any) t2
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t2 at line 2, characters 2-20.
       But the layout of 'a must be a sublayout of value
         because of the definition of t2 at line 6, characters 2-29.
|}]

module M : sig
  type ('a : value) t = Mk of ('a -> 'a)
end = struct
  type ('a : any) t = Mk of ('a -> 'a)
end

[%%expect {|
module M : sig type 'a t = Mk of ('a -> 'a) end
|}]

module M : sig
  type ('a : any) t = Mk of ('a -> 'a)
end = struct
  type ('a : value) t = Mk of ('a -> 'a)
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = Mk of ('a -> 'a)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Mk of ('a -> 'a) end
       is not included in
         sig type ('a : any) t = Mk of ('a -> 'a) end
       Type declarations do not match:
         type 'a t = Mk of ('a -> 'a)
       is not included in
         type ('a : any) t = Mk of ('a -> 'a)
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-38.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-40.
|}]

module M : sig
  type ('a : value) t = { x : 'a -> 'a }
end = struct
  type ('a : any) t = { x : 'a -> 'a }
end

[%%expect {|
module M : sig type 'a t = { x : 'a -> 'a; } end
|}]

module M : sig
  type ('a : any) t = { x : 'a -> 'a }
end = struct
  type ('a : value) t = { x : 'a -> 'a }
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = { x : 'a -> 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { x : 'a -> 'a; } end
       is not included in
         sig type ('a : any) t = { x : 'a -> 'a; } end
       Type declarations do not match:
         type 'a t = { x : 'a -> 'a; }
       is not included in
         type ('a : any) t = { x : 'a -> 'a; }
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-38.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-40.
|}]

module M : sig
  type ('a : value) t = Mk of { x : 'a -> 'a }
end = struct
  type ('a : any) t = Mk of { x : 'a -> 'a }
end

[%%expect {|
module M : sig type 'a t = Mk of { x : 'a -> 'a; } end
|}]

module M : sig
  type ('a : any) t = Mk of { x : 'a -> 'a }
end = struct
  type ('a : value) t = Mk of { x : 'a -> 'a }
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = Mk of { x : 'a -> 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Mk of { x : 'a -> 'a; } end
       is not included in
         sig type ('a : any) t = Mk of { x : 'a -> 'a; } end
       Type declarations do not match:
         type 'a t = Mk of { x : 'a -> 'a; }
       is not included in
         type ('a : any) t = Mk of { x : 'a -> 'a; }
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-44.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-46.
|}]

module M : sig
  type _ t = Mk : 'a. 'a -> 'a t
end = struct
  type (_ : any) t = Mk : 'a. 'a -> 'a t
end

[%%expect {|
module M : sig type _ t = Mk : 'a -> 'a t end
|}]

module M : sig
  type (_ : any) t = Mk : 'a. 'a -> 'a t
end = struct
  type _ t = Mk : 'a. 'a -> 'a t
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type _ t = Mk : 'a. 'a -> 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type _ t = Mk : 'a -> 'a t end
       is not included in
         sig type (_ : any) t = Mk : 'a -> 'a t end
       Type declarations do not match:
         type _ t = Mk : 'a -> 'a t
       is not included in
         type (_ : any) t = Mk : 'a -> 'a t
       The problem is in the kinds of a parameter:
       The layout of _ is any
         because of the definition of t at line 2, characters 2-40.
       But the layout of _ must be a sublayout of value
         because of the definition of t at line 4, characters 2-32.
|}]

module M : sig
  type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
end = struct
  type (_ : any) t = Mk : ('a : value). 'a -> 'a t
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type (_ : any) t = Mk : ('a : value). 'a -> 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type (_ : any) t = Mk : 'a -> 'a t end
       is not included in
         sig type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t end
       Type declarations do not match:
         type (_ : any) t = Mk : 'a -> 'a t
       is not included in
         type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
       Constructors do not match:
         "Mk : 'a -> 'a t"
       is not the same as:
         "Mk : ('a : immediate). 'a -> 'a t"
       The type "'a t" is not equal to the type "'a0 t"
       because the layouts of their variables are different.
       The layout of 'a is value
         because of the definition of t at line 4, characters 2-50.
       The layout of 'a0 is immediate
         because of the definition of t at line 2, characters 2-54.
|}]

module M : sig
  type (_ : any) t = Mk : ('a : value). 'a -> 'a t
end = struct
  type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t end
       is not included in
         sig type (_ : any) t = Mk : 'a -> 'a t end
       Type declarations do not match:
         type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
       is not included in
         type (_ : any) t = Mk : 'a -> 'a t
       Constructors do not match:
         "Mk : ('a : immediate). 'a -> 'a t"
       is not the same as:
         "Mk : 'a -> 'a t"
       The type "'a t" is not equal to the type "'a0 t"
       because the layouts of their variables are different.
       The layout of 'a is immediate
         because of the definition of t at line 4, characters 2-54.
       The layout of 'a0 is value
         because of the definition of t at line 2, characters 2-50.
|}]

module M : sig
  type (_ : any) t = Mk : ('a : value). 'a -> 'a t
end = struct
  type (_ : any) t = Mk : ('a : value). 'a -> 'a t
end

[%%expect {|
module M : sig type (_ : any) t = Mk : 'a -> 'a t end
|}]

module M : sig
  type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
end = struct
  type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t
end

[%%expect {|
module M : sig type (_ : any) t = Mk : ('a : immediate). 'a -> 'a t end
|}]

module M : sig
  type t = Mk : ('a : any). ('a -> 'a) -> t
end = struct
  type t = Mk : ('a : any). ('a -> 'a) -> t
end

[%%expect {|
module M : sig type t = Mk : ('a : any). ('a -> 'a) -> t end
|}]

module M : sig
  type t = Mk : ('a : value). ('a -> 'a) -> t
end = struct
  type t = Mk : ('a : any). ('a -> 'a) -> t
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Mk : ('a : any). ('a -> 'a) -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Mk : ('a : any). ('a -> 'a) -> t end
       is not included in
         sig type t = Mk : ('a -> 'a) -> t end
       Type declarations do not match:
         type t = Mk : ('a : any). ('a -> 'a) -> t
       is not included in
         type t = Mk : ('a -> 'a) -> t
       Constructors do not match:
         "Mk : ('a : any). ('a -> 'a) -> t"
       is not the same as:
         "Mk : ('a -> 'a) -> t"
       The type "'a -> 'a" is not equal to the type "'a0 -> 'a0"
       because the layouts of their variables are different.
       The layout of 'a is any
         because of the definition of t at line 4, characters 2-43.
       The layout of 'a0 is value
         because of the definition of t at line 2, characters 2-45.
|}]

module M : sig
  type t = Mk : ('a : any). ('a -> 'a) -> t
end = struct
  type t = Mk : ('a : value). ('a -> 'a) -> t
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Mk : ('a : value). ('a -> 'a) -> t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Mk : ('a -> 'a) -> t end
       is not included in
         sig type t = Mk : ('a : any). ('a -> 'a) -> t end
       Type declarations do not match:
         type t = Mk : ('a -> 'a) -> t
       is not included in
         type t = Mk : ('a : any). ('a -> 'a) -> t
       Constructors do not match:
         "Mk : ('a -> 'a) -> t"
       is not the same as:
         "Mk : ('a : any). ('a -> 'a) -> t"
       The type "'a -> 'a" is not equal to the type "'a0 -> 'a0"
       because the layouts of their variables are different.
       The layout of 'a is value
         because of the definition of t at line 4, characters 2-45.
       The layout of 'a0 is any
         because of the definition of t at line 2, characters 2-43.
|}]

module M : sig
  type t = Mk : ('a : value). ('a -> 'a) -> t
end = struct
  type t = Mk : ('a : value). ('a -> 'a) -> t
end

[%%expect {|
module M : sig type t = Mk : ('a -> 'a) -> t end
|}]

(* This would be safe to accept. *)
module M : sig
  type 'a t constraint 'a = ('b : immediate) -> ('c : immediate)
end = struct
  type 'a t constraint 'a = ('b : value) -> ('c : value)
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t constraint 'a = ('b : value) -> ('c : value)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t constraint 'a = 'b -> 'c end
       is not included in
         sig type 'a t constraint 'a = 'b -> 'c end
       Type declarations do not match:
         type 'a t constraint 'a = 'b -> 'c
       is not included in
         type 'a t constraint 'a = 'b -> 'c
       Their parameters differ:
       The type "'b -> 'c" is not equal to the type "'b0 -> 'c0"
       because the layouts of their variables are different.
       The layout of 'b is value
         because of the definition of t at line 4, characters 2-56.
       The layout of 'b0 is immediate
         because of the definition of t at line 2, characters 2-64.
|}]

module M : sig
  type 'a t constraint 'a = ('b : value) -> ('c : value)
end = struct
  type 'a t constraint 'a = ('b : immediate) -> ('c : immediate)
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t constraint 'a = ('b : immediate) -> ('c : immediate)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t constraint 'a = 'b -> 'c end
       is not included in
         sig type 'a t constraint 'a = 'b -> 'c end
       Type declarations do not match:
         type 'a t constraint 'a = 'b -> 'c
       is not included in
         type 'a t constraint 'a = 'b -> 'c
       Their parameters differ:
       The type "'b -> 'c" is not equal to the type "'b0 -> 'c0"
       because the layouts of their variables are different.
       The layout of 'b is immediate
         because of the definition of t at line 4, characters 2-64.
       The layout of 'b0 is value
         because of the definition of t at line 2, characters 2-56.
|}]

module M : sig
  type ('a : value) t = private 'a
end = struct
  type ('a : any) t = 'a
end

[%%expect {|
module M : sig type 'a t = private 'a end
|}]

module M : sig
  type ('a : any) t = private 'a
end = struct
  type ('a : value) t = 'a
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type ('a : any) t = private 'a end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type ('a : any) t = private 'a
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-32.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-26.
|}]

module M : sig
  type ('a : value) t = private 'a
end = struct
  type ('a : any) t = private 'a
end

[%%expect {|
module M : sig type 'a t = private 'a end
|}]

module M : sig
  type ('a : any) t = private 'a
end = struct
  type ('a : value) t = private 'a
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = private 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = private 'a end
       is not included in
         sig type ('a : any) t = private 'a end
       Type declarations do not match:
         type 'a t = private 'a
       is not included in
         type ('a : any) t = private 'a
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-32.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-34.
|}]

type ('a : any) iddy = private 'a

[%%expect {|
type ('a : any) iddy = private 'a
|}]

module M : sig
  type ('a : value) t = private 'a
end = struct
  type ('a : any) t = 'a iddy
end

[%%expect {|
module M : sig type 'a t = private 'a end
|}]

module M : sig
  type ('a : any) t = private 'a
end = struct
  type ('a : value) t = 'a iddy
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = 'a iddy
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a iddy end
       is not included in
         sig type ('a : any) t = private 'a end
       Type declarations do not match:
         type 'a t = 'a iddy
       is not included in
         type ('a : any) t = private 'a
       The problem is in the kinds of a parameter:
       The layout of 'a is any
         because of the definition of t at line 2, characters 2-32.
       But the layout of 'a must be a sublayout of value
         because of the definition of t at line 4, characters 2-31.
|}]
