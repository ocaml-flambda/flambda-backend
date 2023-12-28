(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

type t_any   : any
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_void  : void;;

(*********************************************************)
(* Test 1: Simple with type constraints respect jkinds. *)
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
       'a has layout void, which does not overlap with value.
|}];;

module type S1'' = S1 with type s = t_void;;

[%%expect{|
Line 1, characters 27-42:
1 | module type S1'' = S1 with type s = t_void;;
                               ^^^^^^^^^^^^^^^
Error: Type t_void has layout void, which is not a sublayout of value.
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
   and the jkind is determined from the signature.  But we anticipate it'll
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
       string has layout value, which is not a sublayout of immediate.
|}]

(******************************************************************)
(* Test 3: Recursive modules, with and without jkind annotations *)
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
Line 4, characters 13-14:
4 |   let create _ = ()
                 ^
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
Error: Type Bar3.t has layout value, which is not a sublayout of immediate.
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
       'a has layout void, which does not overlap with value.
|}];;

(* One downside of the current approach - this could be allowed, but isn't.  You
   need to annotate types declared in recursive modules if they need to have
   jkinds other than value, even if it's obvious from the manifest *)
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
       Foo3.t has layout value, which is not a sublayout of void.
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
(* Test 4: Nondep typedecl jkind approximation in the Nondep_cannot_erase
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
       M4.s has layout value, which is not a sublayout of void.
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
       M4'.s has layout immediate, which is not a sublayout of void.
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
       string has layout value, which is not a sublayout of immediate.
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
Error: Type string has layout value, which is not a sublayout of immediate.
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
       the first has layout value, which is not a sublayout of void.
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
        t_void has layout void, which is not a sublayout of value.
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
       the first has layout value, which is not a sublayout of immediate.
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
       the first has layout value, which is not a sublayout of immediate.
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
       the first has layout value, which is not a sublayout of immediate.
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
       x has layout any, which is not a sublayout of value.
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
  Module M defines a function whose first argument is not a value, f .
|}]

(*******************************)
(* Test 10: Specializing [any] *)

(* in modules.ml only *)
