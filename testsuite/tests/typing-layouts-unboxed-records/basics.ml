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

module With_index : sig
  type 'a t : value & immediate
  type 'a t_boxed : value

  val unbox_t : 'a t_boxed -> 'a t
  val box_t : 'a t -> 'a t_boxed
  val inc : 'a t -> 'a t
end = struct
  type 'a t = #{ data : 'a ; i : int }
  type 'a t_boxed = { data : 'a ; i : int }

  let unbox_t { data ; i = idx } = #{ data ; i = idx }
  let box_t #{ data ; i = idx } = { data ; i = idx }
  let inc t = #{ t with i = t.#i + 1 }
end
[%%expect{|
module With_index :
  sig
    type 'a t : value & immediate
    type 'a t_boxed
    val unbox_t : 'a t_boxed -> 'a t
    val box_t : 'a t -> 'a t_boxed
    val inc : 'a t -> 'a t
  end
|}]

(* We can change the type of an unboxed record with a functional update. *)

type ('a : value & value) t = #{ x : 'a ; y : string }
let f : #(int * string) t -> #(string * int) t =
  fun (#{ x = #(i, s); y } as r) -> #{ r with x = #(s, i) }
[%%expect{|
type ('a : value & value) t = #{ x : 'a; y : string; }
val f : #(int * string) t -> #(string * int) t = <fun>
|}]

(* As-patterns, partial patterns *)

type t = #{ i: int; j : int }
let add (#{ i; _} as r) = i + r.#j
[%%expect{|
type t = #{ i : int; j : int; }
val add : t -> int = <fun>
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

(* However, we can have a top-level unboxed record if its kind is value *)

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

(* Accessing inner products *)

type t = #{ is: #(int * int) }

let add t =
  let #(x, y) = t.#is in
  x + y
[%%expect{|
type t = #{ is : #(int * int); }
val add : t -> int = <fun>
|}]

(******************************)
(* Basic unboxed record types *)

type t1 = #{ i1 : int }
type t2 = #{ f2: float# ; i2: int ; s2 : string}
type t3 = #{ f3: float# }
[%%expect{|
type t1 = #{ i1 : int; }
type t2 = #{ f2 : float#; i2 : int; s2 : string; }
type t3 = #{ f3 : float#; }
|}]

(* You can put unboxed and normal products inside unboxed products *)

type t4 = #(string * t2)
type t5 = #{ r5 : t1 ; r5_ : t2 ; s5 : string}
[%%expect{|
type t4 = #(string * t2)
type t5 = #{ r5 : t1; r5_ : t2; s5 : string; }
|}]

(* But you can't put unboxed products into normal tuples and records (yet) *)

type bad = { r : t2 }
[%%expect{|
Line 1, characters 0-21:
1 | type bad = { r : t2 }
    ^^^^^^^^^^^^^^^^^^^^^
Error: Type "t2" has layout "float64 & value & value".
       Records may not yet contain types of this layout.
|}]

type bad = t2 * t2
[%%expect{|
Line 1, characters 11-13:
1 | type bad = t2 * t2
               ^^
Error: Tuple element types must have layout value.
       The layout of "t2" is float64 & value & value
         because of the definition of t2 at line 2, characters 0-48.
       But the layout of "t2" must be a sublayout of value
         because it's the type of a tuple element.
|}]

(*********************************)
(* Parameterized unboxed records *)

type 'a t = #{ x : 'a }
let convert (r : int t) : int t =
  { r with x = string }
[%%expect{|
type 'a t = #{ x : 'a; }
Line 3, characters 2-23:
3 |   { r with x = string }
      ^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a record, the expected type is "int t"
|}]

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
Line 2, characters 0-23:
2 | type s = #{ lbl : s t }
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "s" contains a cycle:
         "s t" = "s list",
         "s list" contains "s t",
         "s t" = "s list",
         "s list" contains "s t"
|}]

type ('a : float64) t = #{ x : string; y : 'a }
[%%expect{|
type ('a : float64) t = #{ x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b; }
|}];;

(***************************************************)
(* Simple kind annotations on unboxed record types *)

type t1 : immediate = #{ i1 : int }
type t2 : float64 & immediate & value = #{ f2: float# ; i2: int ; s2 : string}
type t3 : float64 = #{ f3: float# }
type t5 : immediate & (float64 & immediate & value) & value = #{ r5 : t1 ; r5_ : t2 ; s5 : string}
[%%expect{|
type t1 = #{ i1 : int; }
type t2 = #{ f2 : float#; i2 : int; s2 : string; }
type t3 = #{ f3 : float#; }
type t5 = #{ r5 : t1; r5_ : t2; s5 : string; }
|}]

type t5_bad : immediate & float64 & immediate & value & value = #{ r5 : t1 ; r5_ : t2 ; s5 : string}
[%%expect{|
Line 1, characters 0-100:
1 | type t5_bad : immediate & float64 & immediate & value & value = #{ r5 : t1 ; r5_ : t2 ; s5 : string}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t5_bad" is value & (float64 & value & value) & value
         because it is an unboxed record.
       But the layout of type "t5_bad" must be a sublayout of value & float64 & value & value & value
         because of the annotation on the declaration of the type t5_bad.
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
  type tup = t * t
end = struct
  type t = #{ s : string }
  type tup = t * t
end
[%%expect{|
module M : sig type t type tup = t * t end
|}]

(****************************)
(* Types with external mode *)

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
           defaulted to layout value.
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

(* Fields can be mutable, but we can't set them yet *)

type r = #{ mutable i : int }

let f = #{ i = 1 }

(* We don't yet have syntax for setting an unboxed record field.
   However, the below, using a boxed set field, will never work. *)

let () = f.i <- 2
[%%expect{|
type r = #{ mutable i : int; }
val f : r = #{i = 1}
Line 8, characters 9-10:
8 | let () = f.i <- 2
             ^
Error: This expression has type "r" which is not a record type.
|}]

(********************************)
(* Private unboxed record types *)

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
