(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)
(* CR layouts v2.9: all error messages below here are unreviewed *)

type t_any   : any
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_void  : void;;

(*********************************************************)
(* Test 1: Simple with type constraints respect layouts. *)
module type S1 = sig
  type ('a : void) t
  type s
end;;

type ('a : void) t1;;

module type S1' = S1 with type 'a t = t_void t1 and type s = t_void t1;;
[%%expect {|
type t_any : any
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_void : void
module type S1 = sig type ('a : void) t type s end
type ('a : void) t1
module type S1' = sig type ('a : void) t = t_void t1 type s = t_void t1 end
|}];;

module type S1'' = S1 with type 'a t = 'a list;;
[%%expect {|
Line 1, characters 32-34:
1 | module type S1'' = S1 with type 'a t = 'a list;;
                                    ^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type ('b : void)
       The layout of 'a is void, because
         of the definition of t at line 10, characters 2-20.
       But the layout of 'a must overlap with value, because
         the type argument of list has layout value.
|}];;

module type S1'' = S1 with type s = t_void;;

[%%expect{|
Line 1, characters 27-42:
1 | module type S1'' = S1 with type s = t_void;;
                               ^^^^^^^^^^^^^^^
Error: The layout of type t_void is void, because
         of the definition of t_void at line 5, characters 0-19.
       But the layout of type t_void must be a sublayout of value, because
         of the definition of s at line 11, characters 2-8.
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

(* CR layouts - annoyingly, the immediate annotation on 'a is required.  We
   can probably relax this so you don't have to label the parameter explcitly
   and the layout is determined from the signature.  But we anticipate it'll
   require non-trivial refactoring of eqtype, so we've put it off for now. *)
module M1_2'': S1_2' = struct
  type 'a t = 'a list
end;;
[%%expect{|
Lines 1-3, characters 23-3:
1 | .......................struct
2 |   type 'a t = 'a list
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a list end
       is not included in
         S1_2'
       Type declarations do not match:
         type 'a t = 'a list
       is not included in
         type ('a : immediate) t = 'a list
       The type ('a : value) is not equal to the type ('a0 : immediate)
       because their layouts are different.
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
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-25.
|}]

(******************************************************************)
(* Test 3: Recursive modules, with and without layout annotations *)
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
Line 4, characters 13-19:
4 |   let create _ = ()
                 ^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       'a. Please report this error to the Jane Street compilers team.
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
Error: The layout of type Bar3.t is value, because
         of the annotation on the declaration of the type t.
       But the layout of type Bar3.t must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-29.
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

module rec Foo3 : sig
  type 'a t = 'a Bar3.t * 'a list
end = struct
  type t = 'a Bar3.t * 'a list
end

and Bar3 : sig
  type ('a : void) t
end = struct
  type 'a t
end;;
[%%expect {|
Line 2, characters 26-28:
2 |   type 'a t = 'a Bar3.t * 'a list
                              ^^
Error: This type ('a : void) should be an instance of type ('b : value)
       The layout of 'a is void, because
         of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value, because
         the type argument of list has layout value.
|}];;

(* One downside of the current approach - this could be allowed, but isn't.  You
   need to annotate types declared in recursive modules if they need to have
   layouts other than value, even if it's obvious from the manifest *)
type t3 : void

module rec Foo3 : sig
  type t = t3
end = struct
  type t = t3
end

and Bar3 : sig
  type ('a : void) t

  type s = Foo3.t t
end = struct
  type ('a : void) t
  type s = Foo3.t t
end;;
[%%expect {|
type t3 : void
Line 12, characters 11-17:
12 |   type s = Foo3.t t
                ^^^^^^
Error: This type Foo3.t should be an instance of type ('a : void)
       The layout of Foo3.t is value, because
         an abstract type has the value layout by default.
       But the layout of Foo3.t must be a sublayout of void, because
         of the definition of t at line 10, characters 2-20.
|}];;

(* Previous example works with annotation *)
module rec Foo3 : sig
  type t : void = t3
end = struct
  type t = t3
end

and Bar3 : sig
  type ('a : void) t

  type s = Foo3.t t
end = struct
  type ('a : void) t
  type s = Foo3.t t
end;;
[%%expect {|
module rec Foo3 : sig type t = t3 end
and Bar3 : sig type ('a : void) t type s = Foo3.t t end
|}];;

(*************************************************************************)
(* Test 4: Nondep typedecl layout approximation in the Nondep_cannot_erase
   case. *)
module F4(X : sig type t end) = struct
  type s = Foo of X.t
end

module M4 = F4(struct type t = T end)

type ('a : value) t4_val
type ('a : void) t4_void

type t4 = M4.s t4_val;;
[%%expect {|
module F4 : functor (X : sig type t end) -> sig type s = Foo of X.t end
module M4 : sig type s end
type 'a t4_val
type ('a : void) t4_void
type t4 = M4.s t4_val
|}]

type t4' = M4.s t4_void;;
[%%expect {|
Line 1, characters 11-15:
1 | type t4' = M4.s t4_void;;
               ^^^^
Error: This type M4.s should be an instance of type ('a : void)
       The layout of M4.s is value, because
         of the definition of s at line 2, characters 2-21.
       But the layout of M4.s must be a sublayout of void, because
         of the definition of t4_void at line 8, characters 0-24.
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

type t4 = M4'.s t4_void;;
[%%expect{|
Line 1, characters 10-15:
1 | type t4 = M4'.s t4_void;;
              ^^^^^
Error: This type M4'.s should be an instance of type ('a : void)
       The layout of M4'.s is immediate, because
         of the definition of s at line 2, characters 2-45.
       But the layout of M4'.s must be a sublayout of void, because
         of the definition of t4_void at line 8, characters 0-24.
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
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of f at line 3, characters 2-20.
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
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}]

(*****************************************)
(* Test 6: With constraints on packages. *)
module type S6_1 = sig
  type t : void
end

module type S6_2 = sig
  val m : (module S6_1 with type t = int)
end;;
[%%expect{|
module type S6_1 = sig type t : void end
Line 6, characters 10-41:
6 |   val m : (module S6_1 with type t = int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : void
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of void, because
         of the definition of t at line 2, characters 2-15.
|}];;

module type S6_3 = sig
  type t : value
end

module type S6_4 = sig
  val m : (module S6_3 with type t = t_void)
end;;
[%%expect{|
module type S6_3 = sig type t : value end
Line 6, characters 33-34:
6 |   val m : (module S6_3 with type t = t_void)
                                     ^
Error: Signature package constraint types must have layout value.
       The layout of t_void is void, because
         of the definition of t_void at line 5, characters 0-19.
       But the layout of t_void must be a sublayout of value, because
         it's used as an element in a first-class module.
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
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}];;

module type S6_6' = sig
  type s
  val m : (module S6_5 with type t = s)
end
[%%expect{|
Line 3, characters 10-39:
3 |   val m : (module S6_5 with type t = s)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}];;

(* CR layouts: S6_6'' should be fixed *)
module type S6_6'' = sig
  type s = int
  val m : (module S6_5 with type t = int)
end;;
[%%expect{|
Line 3, characters 10-41:
3 |   val m : (module S6_5 with type t = int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
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
Error: This type signature for x is not a value type.
       The layout of x is any, because
         of the definition of t_any at line 1, characters 0-18.
       But the layout of x must be a sublayout of value, because
         it's stored in a module structure.
|}]
