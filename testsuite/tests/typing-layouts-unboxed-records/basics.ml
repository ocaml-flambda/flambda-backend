(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

open Stdlib_upstream_compatible


(* Recursive unboxed records *)

type bad = #{ bad : bad }
[%%expect{|
Line 6, characters 0-25:
6 | type bad = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of bad must be representable
         because it is the type of record field bad.
|}]

type a_bad = #{ b_bad : b_bad }
and b_bad = #{ a_bad : a_bad }
[%%expect{|
Line 1, characters 0-31:
1 | type a_bad = #{ b_bad : b_bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of a_bad is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of a_bad must be representable
         because it is the type of record field a_bad.
|}]

type bad : any = #{ bad : bad }
[%%expect{|
Line 1, characters 0-31:
1 | type bad : any = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad is any
         because of the annotation on the declaration of the type bad.
       But the layout of bad must be representable
         because it is the type of record field bad.
|}]

type 'a id = #{ a : 'a }
type cycle = cycle id
[%%expect{|
type 'a id = #{ a : 'a; }
Line 2, characters 0-21:
2 | type cycle = cycle id
    ^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "cycle" is cyclic:
         "cycle" = "cycle id",
         "cycle id" contains "cycle"
|}]

(* Maybe we can allow the next three...
   I want to disallow them, but
      type t = { t : t} [@@unboxed]
   is allowed upstream. See the discussion in chat, in which
   Leo mentions the "Unboxed data constructors" paper.
*)

type t : value = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]

type t : float64 = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]


type t : value = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]

type bad = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-34:
1 | type bad = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad is any & any
         because it is an unboxed record.
       But the layout of bad must be representable
         because it is the type of record field bad.
|}]

(* We can change the type with a functional update.
   Once we have layout polymorphism, we may be able to change the kind too. *)
type ('a : value & value) t = #{ x : 'a ; y : string }
let f : #(int * string) t -> #(string * int) t =
  fun (#{ x = #(i, s); y } as r) -> #{ r with x = #(s, i) }
[%%expect{|
type ('a : value & value) t = #{ x : 'a; y : string; }
val f : #(int * string) t -> #(string * int) t = <fun>
|}]

type t = #{ i: int; j : int }
let add (#{ i; j=_} as r) = i + r.#j
[%%expect{|
type t = #{ i : int; j : int; }
val add : t -> int = <fun>
|}]

type t = #{ f : float# }
[%%expect{|
type t = #{ f : float#; }
|}]
type t = { f : float# }
[%%expect{|
type t = { f : float#; }
|}]


type t = #{ i : int ; j : int }
let add #{ i ; j } = i + j
let () =
  let t = #{i = 1; j = 2} in
  let res = add t in
  Printf.printf "%d\n" res
[%%expect{|
type t = #{ i : int; j : int; }
val add : t -> int = <fun>
|}]

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

type t = #{ s : string ; i : int }
type t_boxed = { s : string ; i : int }

let unbox_t { s ; i }  = #{ s ; i }
let box_t #{ s ; i }  = { s ; i }
let id_t #{ s ; i }  = #{ s ; i }
[%%expect{|
type t = #{ s : string; i : int; }
type t_boxed = { s : string; i : int; }
val unbox_t : t_boxed -> t = <fun>
val box_t : t -> t_boxed = <fun>
val id_t : t -> t = <fun>
|}]


type t = #{ s : string }
(* This is allowed at the top level because has [t] has kind [value] *)
let s = #{ s = "hi" }
[%%expect{|
type t = #{ s : string; }
val s : t = #{s = "hi"}
|}]

(* Basic checks of constrain_type_jkind: *)
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

(**********************************************************)
(* Basic unboxed record types *)

type t1 = #{ i1 : int }
type t2 = #{ f2: float# ; i2: int ; s2 : string}
type t3 = #{ f3: float# }
[%%expect{|
type 'a t = 'a list
type s = #{ lbl : s t; }
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
         because of the definition of t2 at line 8, characters 0-48.
       But the layout of "t2" must be a sublayout of value
         because it's the type of a tuple element.
|}]

(**********************************************************)
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


(**********************************************************)

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


(* Parameterized types *)

type ('a : float64) t = #{ x : string; y : 'a }
[%%expect{|
type ('a : float64) t = #{ x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b; }
|}];;

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

