(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

open Stdlib_upstream_compatible

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
type t_box = { s : string ; i : int }

let unbox_t { s ; i }  = #{ s ; i }
let box_t #{ s ; i }  = { s ; i }
let id_t #{ s ; i }  = #{ s ; i }
[%%expect{|
type t = #{ s : string; i : int; }
type t_box = { s : string; i : int; }
val unbox_t : t_box -> t = <fun>
val box_t : t -> t_box = <fun>
val id_t : t -> t = <fun>
|}]


type t = #{ s : string }
(* This is allowed at the top level because has [t] has kind [value] *)
let s = #{ s = "hi" }


(* Basic checks of constrain_type_jkind: *)
type 'a r = #{ i: 'a }
type int_r : immediate = int r
[%%expect{|
type t = #{ s : string; }
val s : t = #{s = "hi"}
type 'a r = #{ i : 'a; }
type int_r = int r
|}]

type ('a : float64) r = #{ i: 'a }
type floatH_r : float64 = float# r
[%%expect{|
type ('a : float64) r = #{ i : 'a; }
type floatH_r = float# r
|}]

type 'a rcd = #{ i : 'a ; j : 'a }
type int_rcd : immediate & immediate = int rcd
[%%expect{|
type 'a rcd = #{ i : 'a; j : 'a; }
type int_rcd = int rcd
|}]

type ('a : float64) rcd = #{ i : 'a ; j : 'a }
type floatH_rcd : float64 & float64 = float# rcd
[%%expect{|
type ('a : float64) rcd = #{ i : 'a; j : 'a; }
type floatH_rcd = float# rcd
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

(* CR rtjoa: this error should actually complain that the boxiness doesn't match *)
type 'a t = #{ x : 'a }
let convert (r : int t) : int t =
  { r with x = string }
[%%expect{|
type 'a t = #{ x : 'a; }
Line 3, characters 11-12:
3 |   { r with x = string }
               ^
Error: This record expression is expected to have type "int t"
       There is no field "x" within type "t"
|}]
