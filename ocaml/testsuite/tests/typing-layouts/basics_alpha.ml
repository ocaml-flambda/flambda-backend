(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

type t_any   : any
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_void  : void

type void_variant = VV of t_void
type void_record = {vr_void : t_void; vr_int : int}
type void_unboxed_record = { vur_void : t_void } [@@unboxed];;

[%%expect{|
type t_any : any
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_void : void
type void_variant = VV of t_void
type void_record = { vr_void : t_void; vr_int : int; }
type void_unboxed_record = { vur_void : t_void; } [@@unboxed]
|}];;

(************************************************************)
(* Test 1: Disallow non-representable function args/returns *)
module type S1 = sig
  val f : int -> t_any
end;;
[%%expect {|
Line 2, characters 17-22:
2 |   val f : int -> t_any
                     ^^^^^
Error: Function return types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

module type S1 = sig
  val f : t_any -> int
end;;
[%%expect {|
Line 2, characters 10-15:
2 |   val f : t_any -> int
              ^^^^^
Error: Function argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

module type S1 = sig
  type t : any

  type 'a s = 'a -> int constraint 'a = t
end;;
[%%expect{|
Line 4, characters 35-41:
4 |   type 'a s = 'a -> int constraint 'a = t
                                       ^^^^^^
Error: The type constraints are not consistent.
       Type ('a : '_representable_layout_1) is not compatible with type t
       t has layout any, which is not representable.
|}]

module type S1 = sig
  type t : any

  type 'a s = int -> 'a constraint 'a = t
end;;
[%%expect{|
Line 4, characters 35-41:
4 |   type 'a s = int -> 'a constraint 'a = t
                                       ^^^^^^
Error: The type constraints are not consistent.
       Type ('a : '_representable_layout_2) is not compatible with type t
       t has layout any, which is not representable.
|}]

let f1 () : t_any = assert false;;
[%%expect{|
Line 1, characters 10-32:
1 | let f1 () : t_any = assert false;;
              ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_3)
       t_any has layout any, which is not representable.
|}];;

let f1 (x : t_any) = ();;
[%%expect{|
Line 1, characters 7-18:
1 | let f1 (x : t_any) = ();;
           ^^^^^^^^^^^
Error: This pattern matches values of type t_any
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_4)
       t_any has layout any, which is not representable.
|}];;

(*****************************************************)
(* Test 2: Permit representable function arg/returns *)

(* Presently, the typechecker will allow any representable layout as a function
   arg or return type.  The translation to lambda rejects functions with
   void args / returns. *)
(* CR layouts v2: Once we have another sort that can make it through lambda
   (float#), add tests showing the way sort variables will be instantiated.

   1) [let f x = x] roughly has type [('a : '_sort) -> ('a : '_sort)].
      Test that you can apply it to a value or to a #float, but once you've
      done that you can't apply it to the other.

   2) If [f] has a type in the mli (and isn't used in the ml) we get the sort
      from there.

   I think that all already works, but for the lack of float# *)

module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|
module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

module type S2 = sig
  val f : void_unboxed_record -> int
end
[%%expect {|
module type S2 = sig val f : void_unboxed_record -> int end
|}];;

module type S2 = sig
  val f : int -> void_unboxed_record
end
[%%expect {|
module type S2 = sig val f : int -> void_unboxed_record end
|}];;

module type S2 = sig
  type t : void

  type s = r -> int
  and r = t
end;;
[%%expect{|
module type S2 = sig type t : void type s = r -> int and r = t end
|}]

module type S2 = sig
  val f : int -> t_void
end;;
[%%expect {|
module type S2 = sig val f : int -> t_void end
|}];;

module type S = sig
  type t : void

  type 'a s = 'a -> int constraint 'a = t
end;;
[%%expect{|
module type S = sig type t : void type 'a s = 'a -> int constraint 'a = t end
|}]

module F2 (X : sig val x : t_void end) = struct
  let f () = X.x
end;;
[%%expect{|
Line 1, characters 27-33:
1 | module F2 (X : sig val x : t_void end) = struct
                               ^^^^^^
Error: This type signature for x is not a value type.
       x has layout void, which is not a sublayout of value.
|}];;
(* CR layouts v5: the test above should be made to work *)

module F2 (X : sig val f : void_record -> unit end) = struct
  let g z = X.f { vr_void = z; vr_int = 42 }
end;;
[%%expect{|
Line 2, characters 8-44:
2 |   let g z = X.f { vr_void = z; vr_int = 42 }
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}];;

(**************************************)
(* Test 3: basic annotated parameters *)
type ('a : immediate) imm_id = 'a

[%%expect{|
type ('a : immediate) imm_id = 'a
|}];;

type my_int = int imm_id
let plus_3 (x : my_int) = x + 3
let plus_3' (x : int imm_id) = x + 3;;

[%%expect{|
type my_int = int imm_id
val plus_3 : my_int -> int = <fun>
val plus_3' : int imm_id -> int = <fun>
|}];;

let string_id (x : string imm_id) = x;;
[%%expect{|
Line 1, characters 19-25:
1 | let string_id (x : string imm_id) = x;;
                       ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}];;

let id_for_imms (x : 'a imm_id) = x

let three = id_for_imms 3
let true_ = id_for_imms true;;
[%%expect{|
val id_for_imms : ('a : immediate). 'a imm_id -> 'a imm_id = <fun>
val three : int imm_id = 3
val true_ : bool imm_id = true
|}]

let not_helloworld = id_for_imms "hello world";;
[%%expect{|
Line 1, characters 33-46:
1 | let not_helloworld = id_for_imms "hello world";;
                                     ^^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         'a imm_id = ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}]

(************************************)
(* Test 4: parameters and recursion *)
type ('a : immediate) t4
and s4 = string t4;;

[%%expect{|
Line 2, characters 9-15:
2 | and s4 = string t4;;
             ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}];;

type s4 = string t4
and ('a : immediate) t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       string has layout value, which is not a sublayout of immediate.
|}]

type s4 = int t4
and ('a : immediate) t4;;

[%%expect{|
type s4 = int t4
and ('a : immediate) t4
|}]

type s4 = s5 t4
and ('a : immediate) t4
and s5 = int;;

[%%expect{|
type s4 = s5 t4
and ('a : immediate) t4
and s5 = int
|}]

type s4 = s5 t4
and ('a : immediate) t4
and s5 = string;;

[%%expect{|
Line 3, characters 0-15:
3 | and s5 = string;;
    ^^^^^^^^^^^^^^^
Error:
       s5 has layout value, which is not a sublayout of immediate.
|}]
(* CR layouts v2.9: improve error, which will require layout histories *)

type ('a : any) t4 = 'a
and s4 = string t4;;
[%%expect{|
type ('a : any) t4 = 'a
and s4 = string t4
|}];;

type s4 = string t4
and ('a : any) t4;;
[%%expect{|
type s4 = string t4
and ('a : any) t4
|}];;

(************************************************************)
(* Test 5: You can touch a void, but not return it directly *)

(* CR layouts v5: these tests should be updated to allow returning void, and
   moved to [basics_beta.ml]. *)

type ('a : void) void5 = Void5  of 'a
type ('a : any) any5 = Any5 of 'a

let id5 : 'a void5 -> 'a void5 = function
  | Void5 x -> Void5 x

(* CR layouts v2.8: At the moment, the code in the comment below does not work.
   Because we demand that constructor arguments have layout (Sort 'l), the type
   [any5] actually only works on values.

   In the future, we would like to allow constructors to take arguments of any
   layout and instead restrict how those arguments are used.  In that case, the
   below functions will work (though only on for ('a : void)).
*)
(* let f5 : 'a void5 -> 'a any5 = function
 *     Void5 x -> Any5 x
 *
 * let g5 : 'a any5 -> 'a void5 = function
 *   Any5 x -> Void5 x
 * ;; *)

[%%expect{|
type ('a : void) void5 = Void5 of 'a
type 'a any5 = Any5 of 'a
Lines 4-5, characters 33-22:
4 | .................................function
5 |   | Void5 x -> Void5 x
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       'a has layout void, which is not a sublayout of value.
|}];;

(* disallowed attempts to use f5 and Void5 on non-voids *)
let h5 (x : int void5) = f5 x
[%%expect{|
Line 1, characters 12-15:
1 | let h5 (x : int void5) = f5 x
                ^^^
Error: This type int should be an instance of type ('a : void)
       int has layout immediate, which is not a sublayout of void.
|}];;

let h5' (x : int any5) = Void5 x
[%%expect{|
Line 1, characters 31-32:
1 | let h5' (x : int any5) = Void5 x
                                   ^
Error: This expression has type int any5
       but an expression was expected of type ('a : void)
       int any5 has layout value, which is not a sublayout of void.
|}];;

(* disallowed - tries to return void *)
let g (x : 'a void5) =
  match x with
  | Void5 x -> x;;
[%%expect{|
Lines 1-3, characters 6-16:
1 | ......(x : 'a void5) =
2 |   match x with
3 |   | Void5 x -> x..
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       'a has layout void, which is not a sublayout of value.
|}]

(****************************************)
(* Test 6: explicitly polymorphic types *)
type ('a : immediate) t6_imm = T6imm of 'a
type ('a : value) t6_val = T6val of 'a;;
[%%expect{|
type ('a : immediate) t6_imm = T6imm of 'a
type 'a t6_val = T6val of 'a
|}];;

let ignore_val6 : 'a . 'a -> unit =
  fun a -> let _ = T6val a in ();;
[%%expect{|
val ignore_val6 : 'a -> unit = <fun>
|}];;

let ignore_imm6 : 'a . 'a -> unit =
  fun a -> let _ = T6imm a in ();;
[%%expect{|
Line 2, characters 2-32:
2 |   fun a -> let _ = T6imm a in ();;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> unit which is less general than
         'a. 'a -> unit
       'a has layout value, which is not a sublayout of immediate.
|}];;

let o6 = object
  method ignore_imm6 : 'a . 'a -> unit =
    fun a -> let _ = T6imm a in ()
end;;
[%%expect{|
Line 3, characters 4-34:
3 |     fun a -> let _ = T6imm a in ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This method has type 'b -> unit which is less general than
         'a. 'a -> unit
       'a has layout value, which is not a sublayout of immediate.
|}];;

(* CR layouts v1.5: add more tests here once you can annotate these types with
   layouts.
*)

(*****************************************)
(* Test 7: the layout check in unify_var *)

type ('a : immediate) t7 = Foo7 of 'a

type t7' = (int * int) t7;;
[%%expect{|
type ('a : immediate) t7 = Foo7 of 'a
Line 3, characters 12-21:
3 | type t7' = (int * int) t7;;
                ^^^^^^^^^
Error: This type int * int should be an instance of type ('a : immediate)
       int * int has layout value, which is not a sublayout of immediate.
|}]

(**********************************************************)
(* Test 8: Polymorphic variants take value args (for now) *)

(* CR layouts: we'll eventually allow non-value arguments to polymorphic
   variants *)
module M8_1 = struct
  type foo1 = [ `Foo1 of int | `Baz1 of t_void | `Bar1 of string ];;
end
[%%expect{|
Line 2, characters 40-46:
2 |   type foo1 = [ `Foo1 of int | `Baz1 of t_void | `Bar1 of string ];;
                                            ^^^^^^
Error: Polymorpic variant constructor argument types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

module M8_2 = struct
  type t = { v : t_void } [@@unboxed]
  type result = V of t | I of int

  let foo x =
    match x with
    | `Baz 42 -> I 53
    | `Bar v -> { v }
    | `Bas i -> I i
end;;
[%%expect {|
Line 8, characters 16-21:
8 |     | `Bar v -> { v }
                    ^^^^^
Error: This expression should not be a record, the expected type is result
|}, Principal{|
Line 8, characters 18-19:
8 |     | `Bar v -> { v }
                      ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

module M8_3 = struct
  type 'a t = [ `Foo of 'a | `Baz of int ]

  type bad = t_void t
end;;
[%%expect {|
Line 4, characters 13-19:
4 |   type bad = t_void t
                 ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

module M8_4 = struct
  type 'a t = [ `Foo of 'a | `Baz of int ] constraint 'a = void_unboxed_record
end;;
[%%expect {|
Line 2, characters 54-78:
2 |   type 'a t = [ `Foo of 'a | `Baz of int ] constraint 'a = void_unboxed_record
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type void_unboxed_record
       void_unboxed_record has layout void,
         which is not a sublayout of value.
|}];;

module type S8_5 = sig
  val x : [`A of t_void]
end;;
[%%expect{|
Line 2, characters 17-23:
2 |   val x : [`A of t_void]
                     ^^^^^^
Error: Polymorpic variant constructor argument types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}]

(************************************************)
(* Test 9: Tuples only work on values (for now) *)

(* CR layouts v5: these should work *)
module M9_1 = struct
  type foo1 = int * t_void * [ `Foo1 of int | `Bar1 of string ];;
end
[%%expect{|
Line 2, characters 20-26:
2 |   type foo1 = int * t_void * [ `Foo1 of int | `Bar1 of string ];;
                        ^^^^^^
Error: Tuple element types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

module M9_2 = struct
  type result = V of (string * void_unboxed_record) | I of int
end;;
[%%expect {|
Line 2, characters 31-50:
2 |   type result = V of (string * void_unboxed_record) | I of int
                                   ^^^^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
        void_unboxed_record has layout void,
          which is not a sublayout of value.
|}];;

module M9_3 = struct
  type s = V of void_unboxed_record | I of int

  let foo x =
    match x with
    | I _ -> assert false
    | V t -> t, 27
end;;
[%%expect {|
Line 7, characters 13-14:
7 |     | V t -> t, 27
                 ^
Error: This expression has type void_unboxed_record
       but an expression was expected of type ('a : value)
       void_unboxed_record has layout void,
         which is not a sublayout of value.
|}];;

module M9_4 = struct
  let foo x =
    match x with
    | ({vur_void = _},i) -> i
end;;
[%%expect {|
Line 4, characters 8-16:
4 |     | ({vur_void = _},i) -> i
            ^^^^^^^^
Error: The record field vur_void belongs to the type void_unboxed_record
       but is mixed here with fields of type ('a : value)
       void_unboxed_record has layout void,
         which is not a sublayout of value.
|}];;

module M9_5 = struct
  type 'a t = (int * 'a)

  type bad = t_void t
end;;
[%%expect {|
Line 4, characters 13-19:
4 |   type bad = t_void t
                 ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

module M9_6 = struct
  type 'a t = int * 'a constraint 'a = void_unboxed_record
end;;
[%%expect {|
Line 2, characters 34-58:
2 |   type 'a t = int * 'a constraint 'a = void_unboxed_record
                                      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type void_unboxed_record
       void_unboxed_record has layout void,
         which is not a sublayout of value.
|}];;

module type S9_7 = sig
  val x : int * t_void
end;;
[%%expect{|
Line 2, characters 16-22:
2 |   val x : int * t_void
                    ^^^^^^
Error: Tuple element types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

module M9_9 (X : sig
    val vr : void_record
  end) =
struct
  match 3, X.vr.vr_void with
  | _ -> 42
end;;
[%%expect {|
Line 5, characters 11-23:
5 |   match 3, X.vr.vr_void with
               ^^^^^^^^^^^^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

(*************************************************)
(* Test 10: layouts are checked by "more general" *)

(* This hits the first linktype in moregen (no expansion required to see it's a
   var) *)
module M10_1 : sig
  val x : string
end = struct
  type ('a : immediate) t = 'a

  let f : 'a t -> 'a = fun x -> x

  let x = f (assert false)
end;;
[%%expect {|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = 'a
5 |
6 |   let f : 'a t -> 'a = fun x -> x
7 |
8 |   let x = f (assert false)
9 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immediate) t = 'a
           val f : ('a : immediate). 'a t -> 'a
           val x : ('a : immediate). 'a
         end
       is not included in
         sig val x : string end
       Values do not match:
         val x : ('a : immediate). 'a
       is not included in
         val x : string
       The type string is not compatible with the type string
       string has layout value, which is not a sublayout of immediate.
|}];;

(* This hits the second linktype in moregen (requires expansion to see it's a
   var) *)
module M10_2 : sig
  val x : string
end = struct
  type ('a : immediate) t = 'a

  let f (x : 'a t) : 'a t = x

  let x = f (assert false)
end;;
[%%expect {|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = 'a
5 |
6 |   let f (x : 'a t) : 'a t = x
7 |
8 |   let x = f (assert false)
9 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immediate) t = 'a
           val f : ('a : immediate). 'a t -> 'a t
           val x : ('a : immediate). 'a t
         end
       is not included in
         sig val x : string end
       Values do not match:
         val x : ('a : immediate). 'a t
       is not included in
         val x : string
       The type string t = string is not compatible with the type string
       string has layout value, which is not a sublayout of immediate.
|}]

(**************************************************************)
(* Test 11: objects are values and methods take/return values *)
module M11_1 = struct
  type ('a : void) t = { x : int; v : 'a }

  let f t =
    t.v # baz11
end;;
[%%expect{|
Line 5, characters 4-7:
5 |     t.v # baz11
        ^^^
Error: Methods must have layout value.
       This expression has layout void, which does not overlap with value.
|}]

module M11_2 = struct
  let foo x = VV (x # getvoid)
end;;
[%%expect{|
Line 2, characters 17-30:
2 |   let foo x = VV (x # getvoid)
                     ^^^^^^^^^^^^^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

module M11_3 = struct
  type ('a : void) t = A of 'a

  let foo o (A x) = o # usevoid x
end;;
[%%expect{|
Line 4, characters 12-33:
4 |   let foo o (A x) = o # usevoid x
                ^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       'a has layout void, which is not a sublayout of value.
|}];;

module M11_4 = struct
  val x : < l : t_void >
end;;
[%%expect{|
Line 2, characters 12-22:
2 |   val x : < l : t_void >
                ^^^^^^^^^^
Error: Object field types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

module M11_5 = struct
  type 'a t = < l : 'a s >
  and ('a : void) s = 'a
end;;
[%%expect{|
Line 3, characters 2-24:
3 |   and ('a : void) s = 'a
      ^^^^^^^^^^^^^^^^^^^^^^
Error:
       'a s has layout void, which does not overlap with value.
|}];;

module M11_6 = struct
  type 'a t = < l : 'a > constraint 'a = t_void
end;;
[%%expect{|
Line 2, characters 36-47:
2 |   type 'a t = < l : 'a > constraint 'a = t_void
                                        ^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

(*******************************************************************)
(* Test 12: class parameters and bound vars must have layout value *)

(* Hits `Pcl_let` *)
module M12_1 = struct
  class foo12 v =
    let VV v = v in
    object
      val bar = VV v
    end;;
end
[%%expect{|
Line 3, characters 11-12:
3 |     let VV v = v in
               ^
Error: Variables bound in a class must have layout value.
       v has layout void, which is not a sublayout of value.
|}];;

(* Hits the Cfk_concrete case of Pcf_val *)
module M12_2 = struct
  class foo v =
    object
      val bar = v.vr_void
    end
end;;
[%%expect{|
Line 4, characters 10-13:
4 |       val bar = v.vr_void
              ^^^
Error: Variables bound in a class must have layout value.
       bar has layout void, which is not a sublayout of value.
|}];;

(* Hits the Cfk_virtual case of Pcf_val *)
module M12_3 = struct
  class virtual foo =
    object
      val virtual bar : t_void
    end
end;;
[%%expect{|
Line 4, characters 18-21:
4 |       val virtual bar : t_void
                      ^^^
Error: Variables bound in a class must have layout value.
       bar has layout void, which is not a sublayout of value.
|}];;

module M12_4 = struct
  type ('a : void) t

  class virtual ['a] foo =
    object
      val virtual baz : 'a t
    end
end
[%%expect{|
Line 6, characters 24-26:
6 |       val virtual baz : 'a t
                            ^^
Error: This type ('a : void) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with void.
|}];;

module M12_5 = struct
  type ('a : void) t = A of 'a

  class ['a] foo =
    object
      method void_id (A a) : 'a t = a
    end
end;;
[%%expect{|
Line 6, characters 29-31:
6 |       method void_id (A a) : 'a t = a
                                 ^^
Error: This type ('a : void) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with void.
|}];;

module type S12_6 = sig
  type ('a : void) t = A of 'a

  class ['a] foo :
    'a t ->
    object
      method baz : int
    end
end;;
[%%expect{|
Line 5, characters 4-6:
5 |     'a t ->
        ^^
Error: This type ('a : void) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with void.
|}];;

module type S12_7 = sig
  class foo :
    object
      val baz : t_void
    end
end;;
[%%expect{|
Line 4, characters 6-22:
4 |       val baz : t_void
          ^^^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       baz has layout void, which is not a sublayout of value.
|}];;

(***********************************************************)
(* Test 13: built-in type constructors work only on values *)

(* lazy *)
type t13 = t_void Lazy.t;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void Lazy.t;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 (VV v) = lazy v;;
[%%expect{|
Line 1, characters 22-23:
1 | let x13 (VV v) = lazy v;;
                          ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 v =
  match v with
  | lazy v -> VV v
[%%expect{|
Line 3, characters 17-18:
3 |   | lazy v -> VV v
                     ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

(* option *)
(* CR layouts v5: allow this *)
type t13 = t_void option;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void option;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 (VV v) = Some v;;
[%%expect{|
Line 1, characters 22-23:
1 | let x13 (VV v) = Some v;;
                          ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 v =
  match v with
  | Some v -> VV v
  | None -> assert false
[%%expect{|
Line 3, characters 17-18:
3 |   | Some v -> VV v
                     ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

(* list *)
(* CR layouts: should work after relaxing the mixed block restriction. *)
type t13 = t_void list;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void list;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 (VV v) = [v];;
[%%expect{|
Line 1, characters 18-19:
1 | let x13 (VV v) = [v];;
                      ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 v =
  match v with
  | [v] -> VV v
  | _ -> assert false
[%%expect{|
Line 3, characters 14-15:
3 |   | [v] -> VV v
                  ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

(* array *)
(* CR layouts v4: should work *)
type t13 = t_void array;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void array;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 (VV v) = [| v |];;
[%%expect{|
Line 1, characters 20-21:
1 | let x13 (VV v) = [| v |];;
                        ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}];;

let x13 v =
  match v with
  | [| v |] -> VV v
  | _ -> assert false
[%%expect{|
Line 3, characters 18-19:
3 |   | [| v |] -> VV v
                      ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_void
       t_void has layout void, which is not a sublayout of value.
|}];;

(****************************************************************************)
(* Test 14: Examples motivating the trick with the manifest in [enter_type] *)
type t14 = foo14 list
and foo14 = string;;
[%%expect{|
type t14 = foo14 list
and foo14 = string
|}];;

type t14 = foo14 list
and foo14 = t_void;;
[%%expect{|
Line 2, characters 0-18:
2 | and foo14 = t_void;;
    ^^^^^^^^^^^^^^^^^^
Error:
       foo14 has layout void, which is not a sublayout of value.
|}];;

(****************************************************)
(* Test 15: Type aliases need not have layout value *)
(* (In [transl_type_aux], this hits the layout given to the type variable in the
   Not_found case of the Ptyp_alias case. *)
type ('a : void) t15
type ('a, 'b) foo15 = ('a as 'b) t15 -> 'b t15;;
[%%expect{|
type ('a : void) t15
type ('a : void, 'b) foo15 = 'a t15 -> 'a t15 constraint 'b = 'a
|}]

(********************************************************)
(* Test 16: seperability: [msig_of_external_type] logic *)
type 'a t_void_16 : void

type t_16 = T_16 : 'a t_void_16 -> t_16 [@@unboxed];;
[%%expect{|
type 'a t_void_16 : void
type t_16 = T_16 : 'a t_void_16 -> t_16 [@@unboxed]
|}];;

(**************************************************************************)
(* Test 17: incremental layout checking of @@unboxed types - see comment on
   [constrain_type_layout]. *)

type 'a t17 = 'a list
type s17 = { lbl : s17 t17 } [@@unboxed];;

[%%expect{|
type 'a t17 = 'a list
type s17 = { lbl : s17 t17; } [@@unboxed]
|}];;

(*****************************************)
(* Test 18: expansion in [check_univars] *)
(* This test isn't really layouts-specific, but it checks that the layout checks
   we've added in [Typecore.check_univars] don't choke when expansion is needed
   to see a variable *)
type 'a t18 = 'a

let id18 (x : 'a t18) = x

let f18 : 'a . 'a -> 'a = fun x -> id18 x;;

[%%expect{|
type 'a t18 = 'a
val id18 : 'a t18 -> 'a t18 = <fun>
val f18 : 'a -> 'a = <fun>
|}];;

(********************************)
(* Test 19: non-value coercions *)
let f19 () =
  let x : t_void = assert false in
  let _y = (x :> t_void) in
  ();;
[%%expect{|
Line 3, characters 6-8:
3 |   let _y = (x :> t_void) in
          ^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}];;

(********************************************)
(* Test 20: Non-value bodies for let module *)
let f20 () =
  let x : t_void = assert false in
  let _y =
    let module M = struct end in
    x
  in
  ();;
[%%expect{|
Line 3, characters 6-8:
3 |   let _y =
          ^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}];;

(**********************************)
(* Test 21: Non-value unpack body *)
module type M21 = sig end

let f21 () =
  let x : t_void = assert false in
  let _y =
    let (module M) = (module struct end : M21) in
    x
  in
  ();;
[%%expect{|
module type M21 = sig end
Line 7, characters 4-5:
7 |     x
        ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}];;

(***************************************************************)
(* Test 22: approx_type catch-all can't be restricted to value *)
type t_void : void

type ('a : void) r = { x : int; y : 'a }

let f () =
  let rec g { x = x ; y = y } : _ r = g { x; y } in
  g (failwith "foo");;
[%%expect{|
type t_void : void
type ('a : void) r = { x : int; y : 'a; }
Lines 5-7, characters 6-20:
5 | ......() =
6 |   let rec g { x = x ; y = y } : _ r = g { x; y } in
7 |   g (failwith "foo")..
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       'a has layout void, which is not a sublayout of value.
|}];;

(********************************************************************)
(* Test 23: checking the error message from impossible GADT matches *)

type (_ : any, _ : any) eq = Refl : ('a, 'a) eq

module M : sig
  type t_void : void
  type t_imm : immediate
end = struct
  type t_void : void
  type t_imm : immediate
end
(* these are abstract, so the only trouble with unifying them in a GADT
   match is around their layouts *)

let f (x : (M.t_void, M.t_imm) eq) =
  match x with
  | Refl -> ()

[%%expect{|
type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq
module M : sig type t_void : void type t_imm : immediate end
Line 15, characters 4-8:
15 |   | Refl -> ()
         ^^^^
Error: This pattern matches values of type (M.t_void, M.t_void) eq
       but a pattern was expected which matches values of type
         (M.t_void, M.t_imm) eq
       M.t_void has layout void, which does not overlap with immediate.
|}]
(* CR layouts v2.9: error message is OK, but it could probably be better.
   But a similar case without layouts is already pretty bad, so try
   that before spending too much time here. *)

(*****************************************************)
(* Test 24: Polymorphic parameter with exotic layout *)

type 'a t2_void : void

let f (x : 'a. 'a t2_void) = x

[%%expect{|
type 'a t2_void : void
Line 3, characters 6-30:
3 | let f (x : 'a. 'a t2_void) = x
          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       'a. 'a t2_void.
       Please report this error to the Jane Street compilers team.
|}]

(**************************************************)
(* Test 25: Optional parameter with exotic layout *)

let f (x : t_void) =
  let g ?(x2 = x) () = () in
  ()

[%%expect{|
Line 2, characters 15-16:
2 |   let g ?(x2 = x) () = () in
                   ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

(*********************************************************)
(* Test 26: Inferring an application to an exotic layout *)

let g f (x : t_void) : t_void = f x

[%%expect{|
Line 1, characters 8-35:
1 | let g f (x : t_void) : t_void = f x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(******************************************)
(* Test 27: Exotic layouts in approx_type *)

let rec f : _ -> _ = fun (x : t_void) -> x

[%%expect{|
Line 1, characters 21-42:
1 | let rec f : _ -> _ = fun (x : t_void) -> x
                         ^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(**********************************************)
(* Test 28: Exotic layouts in letop and andop *)

(* CR layouts: this must be [let rec] and [and] so that we can test the
   type-checker, as opposed to the value-kind check. After we have proper
   support for a non-value argument type, remove the [rec], throughout
   this test.
*)
let rec ( let* ) (x : t_void) f = ()

and q () =
  let* x = assert false in
  ()

[%%expect{|
Line 1, characters 17-36:
1 | let rec ( let* ) (x : t_void) f = ()
                     ^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x (f : t_void -> _) = ()

and q () =
  let* x = assert false in
  ()

[%%expect{|
Lines 4-5, characters 2-4:
4 | ..let* x = assert false in
5 |   ()
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x (f : _ -> t_void) = ()

and q () =
  let* x = assert false in
  assert false

[%%expect{|
Line 5, characters 2-14:
5 |   assert false
      ^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x f : t_void = assert false

and q () =
  let* x = 5 in
  ()

[%%expect{|
Line 1, characters 19-44:
1 | let rec ( let* ) x f : t_void = assert false
                       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x f = ()
and ( and* ) x1 (x2 : t_void) = ()
and q () =
    let* x = 5
    and* y = assert false
    in
    ()

[%%expect{|
Line 2, characters 16-34:
2 | and ( and* ) x1 (x2 : t_void) = ()
                    ^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x f = ()
and ( and* ) (x1 : t_void) x2 = ()
and q () =
    let* x = assert false
    and* y = 5
    in
    ()

[%%expect{|
Line 2, characters 13-34:
2 | and ( and* ) (x1 : t_void) x2 = ()
                 ^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let rec ( let* ) x f = ()
and ( and* ) x1 x2 : t_void = assert false
and q () =
    let* x = 5
    and* y = 5
    in
    ()

[%%expect{|
Line 1, characters 17-25:
1 | let rec ( let* ) x f = ()
                     ^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(* CR layouts v5: when we allow non-values in tuples, this next one should
   type-check *)
let rec ( let* ) x f = ()
and ( and* ) x1 x2 = assert false
and q () =
    let* x : t_void = assert false
    and* y = 5
    in
    ()

[%%expect{|
Line 4, characters 9-19:
4 |     let* x : t_void = assert false
             ^^^^^^^^^^
Error: This pattern matches values of type t_void
       but a pattern was expected which matches values of type ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

(*******************************************)
(* Test 29: [external]s default to [value] *)

(* CR layouts: this must be done in a module so that we can test the
   type-checker, as opposed to the value-kind check. After we have proper
   support for a non-value argument type, remove the module wrapper.
*)
module _ = struct
  external eq : 'a -> 'a -> bool = "%equal"
  let mk_void () : t_void = assert false
  let x () = eq (mk_void ()) (mk_void ())
end

[%%expect{|
Line 4, characters 16-28:
4 |   let x () = eq (mk_void ()) (mk_void ())
                    ^^^^^^^^^^^^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

(**************************************)
(* Test 30: [val]s default to [value] *)

(* CR layouts: this must be done in a module so that we can test the
   type-checker, as opposed to the value-kind check. After we have proper
   support for a non-value argument type, remove the module wrapper.
*)
module _ = struct
  module M : sig
    val f : 'a -> 'a
  end = struct
    let f x = x
  end

  let g (x : t_void) = M.f x
end

[%%expect{|
Line 8, characters 27-28:
8 |   let g (x : t_void) = M.f x
                               ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

(**************************************************)
(* Test 31: checking that #poly_var patterns work *)

type ('a : void) poly_var = [`A of int * 'a | `B]

let f #poly_var = "hello"

[%%expect{|
Line 1, characters 41-43:
1 | type ('a : void) poly_var = [`A of int * 'a | `B]
                                             ^^
Error: This type ('a : value) should be an instance of type ('a0 : void)
       'a has layout void, which does not overlap with value.
|}]

(* CR layouts bug: this should be accepted (or maybe we should reject
   the type definition if we're not allowing `void` things in structures).
   This bug is a goof at the top of Typecore.build_or_pat;
   there is another CR layouts there. *)

(*********************************************************)
(* Test 32: Polymorphic variant constructors take values *)

let f _ = `Mk (assert false : t_void)

[%%expect{|
Line 1, characters 14-37:
1 | let f _ = `Mk (assert false : t_void)
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

(******************************************************)
(* Test 33: Externals must have representable types *)
external foo33 : t_any = "foo33";;

[%%expect{|
Line 1, characters 17-22:
1 | external foo33 : t_any = "foo33";;
                     ^^^^^
Error: This type signature for foo33 is not a value type.
       foo33 has layout any, which is not a sublayout of value.
|}]


(****************************************************)
(* Test 34: Layout clash in polymorphic record type *)

(* tested elsewhere *)
