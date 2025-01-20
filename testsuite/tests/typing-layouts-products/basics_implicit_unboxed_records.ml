(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* NOTE: When adding tests to this file, also update
   [typing-layouts-products/basics_unboxed_records.ml] *)

open Stdlib_upstream_compatible

(**************************************************************************)
(* Basic examples: construction, functional updates, projection, matching *)

(* We can change the type of an unboxed record with a functional update. *)

type 'a t = { x : 'a ; y : string }
let f : (int * string) t# -> (string * int) t# =
  fun (#{ x = (i, s); y } as r) -> #{ r with x = (s, i) }
[%%expect{|
type 'a t = { x : 'a; y : string; }
val f : (int * string) t# -> (string * int) t# = <fun>
|}]

(* Patterns, as-patterns, partial patterns *)

type t = { i: int; j : int }
let add (#{ i; _} as r) = i + r.#j
[%%expect{|
type t = { i : int; j : int; }
val add : t# -> int = <fun>
|}]

let bad_match (x : t#) =
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

type t = { f : float# ; i : int }
[%%expect{|
type t = { f : float#; i : int; }
|}]

let mk_t () =
  #{ f = #3.14; i = 0 }
[%%expect{|
val mk_t : unit -> t# = <fun>
|}]

let take_t #{ f; i } =
  #{ f; i }
[%%expect{|
val take_t : t# -> t# = <fun>
|}]

let combine_ts #{ f = _f1; i = i1 } #{ f = f2; i = _i2 } =
   #{ f = f2 ; i = i1 }
[%%expect{|
val combine_ts : t# -> t# -> t# = <fun>
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

type m_record = { i1 : int }
module M = struct
  let x = #{ i1 = 1 }
end
[%%expect{|
type m_record = { i1 : int; }
module M : sig val x : m_record# end
|}]

type wrap_int = { i : int }
type wrap_wrap_int = { wi : wrap_int# }
let w5 = #{ i = 5 }
let ww5 = #{ wi = #{ i = 5 }}
[%%expect{|
type wrap_int = { i : int; }
type wrap_wrap_int = { wi : wrap_int#; }
val w5 : wrap_int# = #{i = 5}
val ww5 : wrap_wrap_int# = #{wi = #{i = 5}}
|}]

type t = { s : string }
let s = #{ s = "hi" }
[%%expect{|
type t = { s : string; }
val s : t# = #{s = "hi"}
|}]

;;
#{ i1 = 1 };;
[%%expect{|
- : m_record# = #{i1 = 1}
|}]

(* Accessing inner products *)

(* CR layouts v5: this should work once we allow non-value record fields *)
type t = { is: #(int * int) }

let add t =
  let #(x, y) = t.#is in
  x + y
[%%expect{|
Line 1, characters 0-29:
1 | type t = { is: #(int * int) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "#(int * int)" has layout "value & value".
       Records may not yet contain types of this layout.
|}]

(* An unboxed record is not an allocation, but a regular record is *)

type ('a, 'b) ab = { left : 'a ; right : 'b }
type ('a, 'b) ab_u = { left : 'a ; right : 'b }

let f_unboxed_record (local_ left) (local_ right) =
  let t = #{ left; right } in
  let #{ left = left'; _ } = t in
  left'
[%%expect{|
type ('a, 'b) ab = { left : 'a; right : 'b; }
type ('a, 'b) ab_u = { left : 'a; right : 'b; }
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

(*********************************)
(* Parameterized unboxed records *)

(* Checks of constrain_type_jkind *)

type 'a r = { i: 'a }
type int_r : immediate = int r#
[%%expect{|
type 'a r = { i : 'a; }
type int_r = int r#
|}]

(* CR layouts v5: this should work once we allow non-value record fields *)
type ('a : float64) t = { i: 'a }
type floatu_t : float64 = float t#
[%%expect{|
type ('a : float64) t = { i : 'a; }
Line 2, characters 26-31:
2 | type floatu_t : float64 = float t#
                              ^^^^^
Error: This type "float" should be an instance of type "('a : float64)"
       The layout of float is value
         because it is the primitive type float.
       But the layout of float must be a sublayout of float64
         because of the definition of t at line 1, characters 0-33.
|}]

type 'a t = { i : 'a ; j : 'a }
type int_t : immediate & immediate = int t#
[%%expect{|
type 'a t = { i : 'a; j : 'a; }
type int_t = int t#
|}]

type ('a : float64) t = { i : 'a ; j : 'a }
type floatu_t : float64 & float64 = float t#
[%%expect{|
type ('a : float64) t = { i : 'a; j : 'a; }
Line 2, characters 36-41:
2 | type floatu_t : float64 & float64 = float t#
                                        ^^^^^
Error: This type "float" should be an instance of type "('a : float64)"
       The layout of float is value
         because it is the primitive type float.
       But the layout of float must be a sublayout of float64
         because of the definition of t at line 1, characters 0-43.
|}]

type 'a t = 'a list
type s = { lbl : s t# }
[%%expect{|
type 'a t = 'a list
Line 2, characters 19-21:
2 | type s = { lbl : s t# }
                       ^^
Error: "t" has no unboxed version.
|}]

type ('a : float64) t = { x : string; y : 'a }
[%%expect{|
type ('a : float64) t = { x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = { x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = { x : string; y : 'a; z : 'b; }
|}];;

type ('a : value & float64 & value) t1
type ('a : value) t2
[%%expect{|
type ('a : value & float64 & value) t1
type 'a t2
|}]

(* CR layouts v5: this should work once we reorder record fields *)
type s = r# t1
and r = { x : int; y : float#; z : s t2 }
[%%expect{|
Line 2, characters 31-39:
2 | and r = { x : int; y : float#; z : s t2 }
                                   ^^^^^^^^
Error: Expected all flat fields after non-value field, "y",
       but found boxed field, "z".
|}]

type s = r_bad# t1
and r_bad = { y : float#; z : s t2 }
[%%expect{|
Line 2, characters 0-36:
2 | and r_bad = { y : float#; z : s t2 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of r_bad# is any & any
         because it is an unboxed record.
       But the layout of r_bad# must be a sublayout of value & float64 & value
         because of the definition of t1 at line 1, characters 0-38.
|}]

(* CR layouts v5: this should work once we allow non-value record fields *)
type 'a t = { a : 'a ; a' : 'a } constraint 'a = r#
and r = { i : int ; f : float# }
[%%expect{|
Line 1, characters 0-51:
1 | type 'a t = { a : 'a ; a' : 'a } constraint 'a = r#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "r#" has layout "value & float64".
       Records may not yet contain types of this layout.
|}]

type 'a t = { a : 'a ; a' : 'a } constraint 'a = r#
and r = { i : int }
[%%expect{|
type 'a t = { a : 'a; a' : 'a; } constraint 'a = r#
and r = { i : int; }
|}]

(*******************)
(* Types with [as] *)

let f (x : < m: 'a. ([< `Foo of int & float] as 'a) -> unit>)
         : < m: 'a. ([< `Foo of int & float] as 'a) -> unit> = x;;

type t = { x : 'a. ([< `Foo of int & float ] as 'a) -> unit };;
let f t = #{ x = t.#x };;
[%%expect{|
val f :
  < m : 'a. ([< `Foo of int & float ] as 'a) -> unit > ->
  < m : 'b. ([< `Foo of int & float ] as 'b) -> unit > = <fun>
type t = { x : 'a. ([< `Foo of int & float ] as 'a) -> unit; }
val f : t# -> t# = <fun>
|}]

module Bad : sig
  type t = { i : int ; a: (<x:'a> as 'a) }
end = struct
  type t = { i : int ; a: (<x:'a * 'a> as 'a) }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { i : int ; a: (<x:'a * 'a> as 'a) }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { i : int; a : < x : 'a * 'a > as 'a; } end
       is not included in
         sig type t = { i : int; a : < x : 'a > as 'a; } end
       Type declarations do not match:
         type t = { i : int; a : < x : 'a * 'a > as 'a; }
       is not included in
         type t = { i : int; a : < x : 'a > as 'a; }
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
module Bad : sig
  type u
end = struct
  type t = { s: string; r: string }
  type nonrec u = t#
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type t = { s: string; r: string }
5 |   type nonrec u = t#
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { s : string; r : string; } type nonrec u = t# end
       is not included in
         sig type u end
       Type declarations do not match: type u = t# is not included in type u
       The layout of the first is value & value
         because it is an unboxed record.
       But the layout of the first must be a sublayout of value
         because of the definition of u at line 2, characters 2-8.
|}]

module M : sig
  type t = { s : string; f : float# }
end = struct
  type t = { s : string; f : float# }
end
[%%expect{|
module M : sig type t = { s : string; f : float#; } end
|}]

module M2 : sig
  type u : value & float64
end = struct
  include M
  type nonrec u = t#
end
[%%expect{|
module M2 : sig type u : value & float64 end
|}]

module M : sig
  type u : value & float64
end = struct
  type t = { s : string; f : float# }
  type nonrec u = t#
end
[%%expect{|
module M : sig type u : value & float64 end
|}]

module M : sig
  type u
end = struct
  type t = #{ s : string }
  type nonrec u = t#
end
[%%expect{|
Line 5, characters 18-20:
5 |   type nonrec u = t#
                      ^^
Error: "t" has no unboxed version.
|}]

(*************************************)
(* Types that mode cross externality *)

(* No corresponding tests for implicit unboxed records *)

(********************)
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
  {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
and ('a, 'b, 'ptr) u = ('a, 'b, 'ptr) t#
[%%expect{|
Line 4, characters 27-37:
4 |   {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
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
  {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
and ('a : float64, 'b : immediate, 'ptr) u =
  ('a, 'b, 'ptr) t#
[%%expect{|
type ('a : float64) t_float = 'a t_float64_id
and ('a : immediate) t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t = {
  ptr : 'ptr;
  x : 'a;
  y : 'a t_float;
  z : 'b;
  w : 'b t_imm;
}
and ('a : float64, 'b : immediate, 'ptr) u = ('a, 'b, 'ptr) t#
|}];;

(* We don't yet have syntax for setting an unboxed record field.
   However, the below, using a boxed set field, will never work. *)

type r = { i : int }
let f = #{ i = 1 }
[%%expect{|
type r = { i : int; }
val f : r# = #{i = 1}
|}]

let () = f.i <- 2
[%%expect{|
Line 1, characters 9-10:
1 | let () = f.i <- 2
             ^
Error: This expression has type "r#",
       which is an unboxed record rather than a boxed one.
|}]

(*****************************************)
(* Private implicit unboxed record types *)

module M : sig
  type u = private #{ x : int; y : bool }
end = struct
  type t = { x : int; y : bool }
  type u = t# = #{ x : int ; y : bool }
end
[%%expect{|
module M : sig type u = private #{ x : int; y : bool; } end
|}]

module Bad : sig
  type u = #{ x : int; y : bool }
end = struct
  type t = private { x : int; y : bool }
  type u = t# = #{ x : int; y : bool }
end
[%%expect{|
Line 5, characters 2-38:
5 |   type u = t# = #{ x : int; y : bool }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t#"
       A private unboxed record constructor would be revealed.
|}]

module Bad = struct
  type t = private { x : int; y : bool }
  type u = t# = #{ x : int; y : bool }
end
[%%expect{|
Line 3, characters 2-38:
3 |   type u = t# = #{ x : int; y : bool }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t/2#"
       A private unboxed record constructor would be revealed.
|}]

module Bad : sig
  type u = #{ x : int }
end = struct
  type t = private { x : int; y : bool }
  type u = t# = private #{ x : int; y : bool }
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type t = private { x : int; y : bool }
5 |   type u = t# = private #{ x : int; y : bool }
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = private { x : int; y : bool; }
           type u = t# = private #{ x : int; y : bool; }
         end
       is not included in
         sig type u = #{ x : int; } end
       Type declarations do not match:
         type u = t# = private #{ x : int; y : bool; }
       is not included in
         type u = #{ x : int; }
       A private unboxed record constructor would be revealed.
|}];;

module Bad : sig
  type u = #{ x : int; y : bool }
end = struct
  type t = private { x : int }
  type u = t# = private #{ x : int }
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type t = private { x : int }
5 |   type u = t# = private #{ x : int }
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = private { x : int; }
           type u = t# = private #{ x : int; }
         end
       is not included in
         sig type u = #{ x : int; y : bool; } end
       Type declarations do not match:
         type u = t# = private #{ x : int; }
       is not included in
         type u = #{ x : int; y : bool; }
       A private unboxed record constructor would be revealed.
|}];;

(*****************************************************)
(* Special-cased errors for boxed/unboxed mismatches *)

(* Doesn't apply to implicit unboxed records *)

(*****************************************************************************)
(* Initial expressions for functionally updated records are always evaluated *)

type t = { x : string }

let [@warning "-23"] update_t t =
  let updated = ref false in
  let _ = #{ (updated := true; t) with x = "" } in
  assert !updated

let _ = update_t #{ x = "x" }
[%%expect{|
type t = { x : string; }
val update_t : t# -> unit = <fun>
- : unit = ()
|}]

type t = { x : string ; y : float# ; z : unit}

let [@warning "-23"] update_t t =
  let counter = ref 0 in
  let _ = #{ (incr counter; t) with x = ""; y = #0.0 ; z = ()} in
  assert (!counter = 1);
  let _ = #{ (incr counter; t) with y = #0.0 } in
  assert (!counter = 2)

let _ = update_t #{ x = "x" ; y = #1.0 ; z = ()}
[%%expect{|
type t = { x : string; y : float#; z : unit; }
val update_t : t# -> unit = <fun>
- : unit = ()
|}]

(************************************************************)
(* Basic tests for construction/projection representability *)

type ('a : any) t = { x : int; y : 'a }
[%%expect{|
type 'a t = { x : int; y : 'a; }
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
         because of the definition of t at line 1, characters 0-39.
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
         because of the definition of t at line 1, characters 0-39.
|}]


(************************************************************)

(* This is a regression test. Previously, we hit a fatal error because the [any]
   annotation obscured the fact that the unboxed record is a product, breaking
   an invariant assumed by [Ctype.constrain_type_jkind].

   [a] was necessary to trigger the error because it called
   [check_representable] on [b], constraining its kind to be a sort variable. *)
type a = B of b
and b : any = r#
and r = { i : int ; j : int }
[%%expect{|
Line 1, characters 9-15:
1 | type a = B of b
             ^^^^^^
Error: Type "b" has layout "value & value".
       Variants may not yet contain types of this layout.
|}]
type a = B of b_portable
and b_portable : any mod portable = r#
and r = { i : int ; j : int }
[%%expect{|
Line 1, characters 9-24:
1 | type a = B of b_portable
             ^^^^^^^^^^^^^^^
Error: Type "b_portable" has layout "value & value".
       Variants may not yet contain types of this layout.
|}]
type a = B of b
and b : any & any & any = r#
and r = { i : int ; j : int }
[%%expect{|
Line 2, characters 0-28:
2 | and b : any & any & any = r#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of b is any & any
         because it is an unboxed record.
       But the layout of b must be representable
         because it's the type of a constructor field.
|}]
