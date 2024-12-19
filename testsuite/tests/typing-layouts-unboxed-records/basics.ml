(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

open Stdlib_upstream_compatible

(**************************************************************************)
(* Basic examples: construction, functional updates, projection, matching *)

(* We can change the type of an unboxed record with a functional update. *)

type ('a : value & value) t = #{ x : 'a ; y : string }
let f : #(int * string) t -> #(string * int) t =
  fun (#{ x = #(i, s); y } as r) -> #{ r with x = #(s, i) }
[%%expect{|
type ('a : value & value) t = #{ x : 'a; y : string; }
val f : #(int * string) t -> #(string * int) t = <fun>
|}]

(* Patterns, as-patterns, partial patterns *)

type t = #{ i: int; j : int }
let add (#{ i; _} as r) = i + r.#j
[%%expect{|
type t = #{ i : int; j : int; }
val add : t -> int = <fun>
|}]

let bad_match (x : t) =
  match x with
  | _ -> .
[%%expect{|
Line 3, characters 4-5:
3 |   | _ -> .
        ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "#{ _ }"
|}]

(* Unboxed records are not subject to the mixed-block restriction *)

type t = #{ f : float# ; i : int }
[%%expect{|
type t = #{ f : float#; i : int; }
|}]

let mk_t () =
  #{ f = #3.14; i = 0 }
[%%expect{|
val mk_t : unit -> t = <fun>
|}]

let take_t #{ f; i } =
  #{ f; i }
[%%expect{|
val take_t : t -> t = <fun>
|}]

let combine_ts #{ f = _f1; i = i1 } #{ f = f2; i = _i2 } =
   #{ f = f2 ; i = i1 }
[%%expect{|
val combine_ts : t -> t -> t = <fun>
|}]

(* We still cannot have top-level products *)

let disallowed = #{ f = #3.14; i = 0 }
[%%expect{|
Line 1, characters 4-14:
1 | let disallowed = #{ f = #3.14; i = 0 }
        ^^^^^^^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "disallowed" has layout "float64 & value".
|}]

;;
#{ f = #3.14; i = 0};;
[%%expect{|
Line 1, characters 0-20:
1 | #{ f = #3.14; i = 0};;
    ^^^^^^^^^^^^^^^^^^^^
Error: Types of unnamed expressions must have layout value when using
       the toplevel, but this expression has layout "float64 & value".
|}]

(* However, we can have a top-level unboxed record if its kind is value *)

type m_record = #{ i1 : int }
module M = struct
  let x = #{ i1 = 1 }
end
[%%expect{|
type m_record = #{ i1 : int; }
module M : sig val x : m_record end
|}]

type wrap_int = #{ i : int }
type wrap_wrap_int = #{ wi : wrap_int}
let w5 = #{ i = 5 }
let ww5 = #{ wi = #{ i = 5 }}
[%%expect{|
type wrap_int = #{ i : int; }
type wrap_wrap_int = #{ wi : wrap_int; }
val w5 : wrap_int = #{i = 5}
val ww5 : wrap_wrap_int = #{wi = #{i = 5}}
|}]

type t = #{ s : string }
let s = #{ s = "hi" }
[%%expect{|
type t = #{ s : string; }
val s : t = #{s = "hi"}
|}]

;;
#{ i1 = 1 };;
[%%expect{|
- : m_record = #{i1 = 1}
|}]

(* Accessing inner products *)

type t = #{ is: #(int * int) }

let add t =
  let #(x, y) = t.#is in
  x + y
[%%expect{|
type t = #{ is : #(int * int); }
val add : t -> int = <fun>
|}]

(* An unboxed record is not an allocation, but a regular record is *)

type ('a, 'b) ab = { left : 'a ; right : 'b }
type ('a, 'b) ab_u = #{ left : 'a ; right : 'b }

let f_unboxed_record (local_ left) (local_ right) =
  let t = #{ left; right } in
  let #{ left = left'; _ } = t in
  left'
[%%expect{|
type ('a, 'b) ab = { left : 'a; right : 'b; }
type ('a, 'b) ab_u = #{ left : 'a; right : 'b; }
val f_unboxed_record : local_ 'a -> local_ 'b -> local_ 'a = <fun>
|}]

let f_boxed_record (local_ left) (local_ right) =
  let t = { left; right } in
  let { left = left'; _ } = t in
  left'
[%%expect{|
Line 4, characters 2-7:
4 |   left'
      ^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

(* Mutable fields are not allowed *)

type mut = #{ mutable i : int }
[%%expect{|
Line 1, characters 14-29:
1 | type mut = #{ mutable i : int }
                  ^^^^^^^^^^^^^^^
Error: Unboxed record labels cannot be mutable
|}]

(*********************************)
(* Parameterized unboxed records *)

(* Checks of constrain_type_jkind *)

type 'a r = #{ i: 'a }
type int_r : immediate = int r
[%%expect{|
type 'a r = #{ i : 'a; }
type int_r = int r
|}]

type ('a : float64) t = #{ i: 'a }
type floatu_t : float64 = float# t
[%%expect{|
type ('a : float64) t = #{ i : 'a; }
type floatu_t = float# t
|}]

type 'a t = #{ i : 'a ; j : 'a }
type int_t : immediate & immediate = int t
[%%expect{|
type 'a t = #{ i : 'a; j : 'a; }
type int_t = int t
|}]

type ('a : float64) t = #{ i : 'a ; j : 'a }
type floatu_t : float64 & float64 = float# t
[%%expect{|
type ('a : float64) t = #{ i : 'a; j : 'a; }
type floatu_t = float# t
|}]

type 'a t = 'a list
type s = #{ lbl : s t }
[%%expect{|
type 'a t = 'a list
type s = #{ lbl : s t; }
|}]

type ('a : float64) t = #{ x : string; y : 'a }
[%%expect{|
type ('a : float64) t = #{ x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b; }
|}];;

type ('a : value & float64 & value) t1
type ('a : value) t2
[%%expect{|
type ('a : value & float64 & value) t1
type 'a t2
|}]

type s = r t1
and r = #{ x : int; y : float#; z : s t2 }
[%%expect{|
type s = r t1
and r = #{ x : int; y : float#; z : s t2; }
|}]

type s = r_bad t1
and r_bad = #{ y : float#; z : s t2 }
[%%expect{|
Line 2, characters 0-37:
2 | and r_bad = #{ y : float#; z : s t2 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of r_bad is any & any
         because it is an unboxed record.
       But the layout of r_bad must be a sublayout of value & float64 & value
         because of the definition of t1 at line 1, characters 0-38.
|}]

(* CR layouts v7.2: the following should typecheck. *)
type 'a t = #{ a : 'a ; a' : 'a } constraint 'a = r
and r = #{ i : int ; f : float# }
[%%expect{|
type 'a t = #{ a : 'a; a' : 'a; } constraint 'a = r
and r = #{ i : int; f : float#; }
|}]

(*******************)
(* Types with [as] *)

let f (x : < m: 'a. ([< `Foo of int & float] as 'a) -> unit>)
         : < m: 'a. ([< `Foo of int & float] as 'a) -> unit> = x;;

type t = #{ x : 'a. ([< `Foo of int & float ] as 'a) -> unit };;
let f t = #{ x = t.#x };;
[%%expect{|
val f :
  < m : 'a. ([< `Foo of int & float ] as 'a) -> unit > ->
  < m : 'b. ([< `Foo of int & float ] as 'b) -> unit > = <fun>
type t = #{ x : 'a. ([< `Foo of int & float ] as 'a) -> unit; }
val f : t -> t = <fun>
|}]

module Bad : sig
  type t = #{ i : int ; a: (<x:'a> as 'a) }
end = struct
  type t = #{ i : int ; a: (<x:'a * 'a> as 'a) }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ i : int ; a: (<x:'a * 'a> as 'a) }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ i : int; a : < x : 'a * 'a > as 'a; } end
       is not included in
         sig type t = #{ i : int; a : < x : 'a > as 'a; } end
       Type declarations do not match:
         type t = #{ i : int; a : < x : 'a * 'a > as 'a; }
       is not included in
         type t = #{ i : int; a : < x : 'a > as 'a; }
       Fields do not match:
         "a : < x : 'a * 'a > as 'a;"
       is not the same as:
         "a : < x : 'a > as 'a;"
       The type "< x : 'a * 'a > as 'a" is not equal to the type
         "< x : 'b > as 'b"
       The method "x" has type "< x : 'c > * < x : 'c > as 'c",
       but the expected method type was "< x : 'b > as 'b"
|}]

(**********************)
(* Signature checking *)

(* Must expose non-value kind *)
module M : sig
  type t
end = struct
  type t = #{ s: string; r: string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ s: string; r: string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ s : string; r : string; } end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = #{ s : string; r : string; }
       is not included in
         type t
       The layout of the first is value & value
         because of the definition of t at line 4, characters 2-36.
       But the layout of the first must be a sublayout of value
         because of the definition of t at line 2, characters 2-8.
|}]

module M : sig
  type t = #{ f : float# ; s : string }
end = struct
  type t = #{ f : float# ; s : string }
end
[%%expect{|
module M : sig type t = #{ f : float#; s : string; } end
|}]

module M2 : sig
  type t : float64 & value
end = struct
  include M
end
[%%expect{|
module M2 : sig type t : float64 & value end
|}]

module M : sig
  type t : float64 & value
end = struct
  type t = #{ i : float# ; s : string }
end
[%%expect{|
module M : sig type t : float64 & value end
|}]

module M : sig
  type t : float64 & value
end = struct
  type t = #{ i : float# ; s : string }
end
[%%expect{|
module M : sig type t : float64 & value end
|}]

module M : sig
  type t
end = struct
  type t = #{ s : string }
end
[%%expect{|
module M : sig type t end
|}]

(*************************************)
(* Types that mode cross externality *)

type ('a : value mod external_) t = #{ x : float#; y : 'a }
type ('a : immediate) t = #{ x : float#; y : 'a }
[%%expect {|
type ('a : value mod external_) t = #{ x : float#; y : 'a; }
type ('a : immediate) t = #{ x : float#; y : 'a; }
|}]

type u : value mod external_
type t = #{ x : float#; y : u }
[%%expect {|
type u : value mod external_
type t = #{ x : float#; y : u; }
|}]

type u : immediate
type t = #{ x : float#; y : u }
[%%expect {|
type u : immediate
type t = #{ x : float#; y : u; }
|}]

(* Recursive groups *)

type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
[%%expect{|
type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a, 'b, 'ptr) t =
  #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
Line 4, characters 28-38:
4 |   #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
                                ^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float64
           because of the definition of t_float64_id at line 1, characters 0-37.
         But the layout of 'a must overlap with value
           because it instantiates an unannotated type parameter of t,
           chosen to have layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t =
  #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
type ('a : float64) t_float = 'a t_float64_id
and ('a : immediate) t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t = #{
  ptr : 'ptr;
  x : 'a;
  y : 'a t_float;
  z : 'b;
  w : 'b t_imm;
}
|}];;

(* We don't yet have syntax for setting an unboxed record field.
   However, the below, using a boxed set field, will never work. *)

type r = #{ i : int }
let f = #{ i = 1 }
[%%expect{|
type r = #{ i : int; }
val f : r = #{i = 1}
|}]

let () = f.i <- 2
[%%expect{|
Line 1, characters 9-10:
1 | let () = f.i <- 2
             ^
Error: This expression has type "r",
       which is an unboxed record rather than a boxed one.
|}]

(********************************)
(* Private unboxed record types *)

module M : sig
  type t = private #{ x : int; y : bool }
end = struct
  type t = #{ x : int; y : bool }
end;;
[%%expect{|
module M : sig type t = private #{ x : int; y : bool; } end
|}]

module M : sig
  type t = #{ x : int; y : bool }
end = struct
  type t = private #{ x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; y : bool; } end
       is not included in
         sig type t = #{ x : int; y : bool; } end
       Type declarations do not match:
         type t = private #{ x : int; y : bool; }
       is not included in
         type t = #{ x : int; y : bool; }
       A private unboxed record constructor would be revealed.
|}];;


module M : sig
  type t = #{ x : int }
end = struct
  type t = private #{ x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; y : bool; } end
       is not included in
         sig type t = #{ x : int; } end
       Type declarations do not match:
         type t = private #{ x : int; y : bool; }
       is not included in
         type t = #{ x : int; }
       A private unboxed record constructor would be revealed.
|}];;

module M : sig
  type t = #{ x : int; y : bool }
end = struct
  type t = private #{ x : int }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; } end
       is not included in
         sig type t = #{ x : int; y : bool; } end
       Type declarations do not match:
         type t = private #{ x : int; }
       is not included in
         type t = #{ x : int; y : bool; }
       A private unboxed record constructor would be revealed.
|}];;

(*****************************************************)
(* Special-cased errors for boxed/unboxed mismatches *)

type t_u = #{ u : int }
type t = { b : int }
[%%expect{|
type t_u = #{ u : int; }
type t = { b : int; }
|}]

let f () : t_u = { b = 1 }
[%%expect{|
Line 1, characters 17-26:
1 | let f () : t_u = { b = 1 }
                     ^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "t_u"
|}]

let f () : t = #{ u = 2 }
[%%expect{|
Line 1, characters 15-25:
1 | let f () : t = #{ u = 2 }
                   ^^^^^^^^^^
Error: This unboxed record expression should be boxed instead,
       the expected type is "t"
|}]

let ({ b } : t_u) = assert false
[%%expect{|
Line 1, characters 5-10:
1 | let ({ b } : t_u) = assert false
         ^^^^^
Error: This boxed record pattern should be unboxed instead,
       the expected type is "t_u"
|}]

let (#{ u } : t) = assert false
[%%expect{|
Line 1, characters 5-11:
1 | let (#{ u } : t) = assert false
         ^^^^^^
Error: This unboxed record pattern should be boxed instead,
       the expected type is "t"
|}]

let bad_get (t_u : t_u) = t_u.u
[%%expect{|
Line 1, characters 26-29:
1 | let bad_get (t_u : t_u) = t_u.u
                              ^^^
Error: This expression has type "t_u",
       which is an unboxed record rather than a boxed one.
|}]

let bad_get (t : t) = t.#b
[%%expect{|
Line 1, characters 22-23:
1 | let bad_get (t : t) = t.#b
                          ^
Error: This expression has type "t",
       which is a boxed record rather than an unboxed one.
|}]

let _ = #{ b = 5 }
[%%expect{|
Line 1, characters 11-12:
1 | let _ = #{ b = 5 }
               ^
Error: Unbound unboxed record field "b"
Hint: There is a boxed record field with this name.
|}]

let _ = { u = 5 }
[%%expect{|
Line 1, characters 10-11:
1 | let _ = { u = 5 }
              ^
Error: Unbound record field "u"
Hint: There is an unboxed record field with this name.
|}]

let bad_get t_u = t_u.u
[%%expect{|
Line 1, characters 22-23:
1 | let bad_get t_u = t_u.u
                          ^
Error: Unbound record field "u"
Hint: There is an unboxed record field with this name.
|}]

let bad_get t = t.#b
[%%expect{|
Line 1, characters 19-20:
1 | let bad_get t = t.#b
                       ^
Error: Unbound unboxed record field "b"
Hint: There is a boxed record field with this name.
|}]

(* Initial expressions for functionally updated records are always evaluated *)

type t = #{ x : string }

let [@warning "-23"] update_t t =
  let updated = ref false in
  let _ = #{ (updated := true; t) with x = "" } in
  assert !updated

let _ = update_t #{ x = "x" }
[%%expect{|
type t = #{ x : string; }
val update_t : t -> unit = <fun>
- : unit = ()
|}]

type t = #{ x : string ; y : float# ; z : unit}

let [@warning "-23"] update_t t =
  let counter = ref 0 in
  let _ = #{ (incr counter; t) with x = ""; y = #0.0 ; z = ()} in
  assert (!counter = 1);
  let _ = #{ (incr counter; t) with y = #0.0 } in
  assert (!counter = 2)

let _ = update_t #{ x = "x" ; y = #1.0 ; z = ()}
[%%expect{|
type t = #{ x : string; y : float#; z : unit; }
val update_t : t -> unit = <fun>
- : unit = ()
|}]

(************************************************************)
(* Basic tests for construction/projection representability *)

type ('a : any) t = #{ x : int; y : 'a }
[%%expect{|
type ('a : value_or_null) t = #{ x : int; y : 'a; }
|}]

(* CR layouts v7.2: once we allow record declarations with unknown kind (right
   now, ['a] in the decl above is defaulted to value), then this should give an
   error saying that records being projected from must be representable. *)
let f : ('a : any). 'a t -> 'a = fun t -> t.#y
[%%expect{|
Line 1, characters 8-30:
1 | let f : ('a : any). 'a t -> 'a = fun t -> t.#y
            ^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind any.
       But it was inferred to have kind value_or_null
         because of the definition of t at line 1, characters 0-40.
|}]

(* CR layouts v7.2: once we allow record declarations with unknown kind
   (right now, ['a] in the decl above is defaulted to value), then this should
   give an error saying that records used in functional updates must be
   representable.
*)
let f : ('a : any). 'a -> 'a t = fun a -> #{ x = 1; y = a }
[%%expect{|
Line 1, characters 8-30:
1 | let f : ('a : any). 'a -> 'a t = fun a -> #{ x = 1; y = a }
            ^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind any.
       But it was inferred to have kind value_or_null
         because of the definition of t at line 1, characters 0-40.
|}]


(************************************************************)

(* This is a regression test. Previously, we hit a fatal error because the [any]
   annotation obscured the fact that the unboxed record is a product, breaking
   an invariant assumed by [Ctype.constrain_type_jkind].

   [a] was necessary to trigger the error because it called
   [check_representable] on [b], constraining its kind to be a sort variable. *)
type a = B of b
and b : any = #{ i : int ; j : int }
[%%expect{|
Line 1, characters 9-15:
1 | type a = B of b
             ^^^^^^
Error: Type "b" has layout "value & value".
       Variants may not yet contain types of this layout.
|}]
