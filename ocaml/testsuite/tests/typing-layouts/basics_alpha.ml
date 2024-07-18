(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t_any   : any
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_void  : void
type t_any_non_null : any_non_null;;
type t_value_or_null : value_or_null;;

type void_variant = VV of t_void
type void_record = {vr_void : t_void; vr_int : int}
type void_unboxed_record = { vur_void : t_void } [@@unboxed];;

[%%expect{|
type t_any : any
type t_value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_void : void
type t_any_non_null : any_non_null
type t_value_or_null : value_or_null
type void_variant = VV of t_void
type void_record = { vr_void : t_void; vr_int : int; }
type void_unboxed_record = { vur_void : t_void; } [@@unboxed]
|}];;

(******************************************************************)
(* Test 1: Allow non-representable function args/returns in types *)
module type S1 = sig
  val f : int -> t_any
end;;
[%%expect {|
module type S1 = sig val f : int -> t_any end
|}];;

module type S1 = sig
  val f : t_any -> int
end;;
[%%expect {|
module type S1 = sig val f : t_any -> int end
|}];;

module type S1 = sig
  type t : any

  type ('a : any) s = 'a -> int constraint 'a = t
end;;
[%%expect{|
module type S1 = sig type t : any type 'a s = 'a -> int constraint 'a = t end
|}]

module type S1 = sig
  type t : any

  type ('a : any) s = int -> 'a constraint 'a = t
end;;
[%%expect{|
module type S1 = sig type t : any type 'a s = int -> 'a constraint 'a = t end
|}]

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
       The layout of t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable
         because it instantiates an unannotated type parameter of s.
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
       The layout of t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable
         because it instantiates an unannotated type parameter of s.
|}]

let f1 () : t_any = assert false;;
[%%expect{|
Line 1, characters 20-32:
1 | let f1 () : t_any = assert false;;
                        ^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_3)
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of t_any must be representable
         because we must know concretely how to return a function result.
|}];;

let f1 (x : t_any) = ();;
[%%expect{|
Line 1, characters 7-18:
1 | let f1 (x : t_any) = ();;
           ^^^^^^^^^^^
Error: This pattern matches values of type t_any
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_4)
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of t_any must be representable
         because we must know concretely how to pass a function argument.
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
       The layout of type t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of type t_void must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;
(* CR layouts v5: the test above should be made to work *)

module F2 (X : sig val f : void_record -> unit end) = struct
  let g z = X.f { vr_void = z; vr_int = 42 }
end;;
[%%expect{|
Line 2, characters 16-44:
2 |   let g z = X.f { vr_void = z; vr_int = 42 }
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it has to be value for the V1 safety check.
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
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of imm_id at line 1, characters 0-33.
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
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of id_for_imms at line 1, characters 16-35.
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
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the annotation on 'a in the declaration of the type t4.
|}];;

type s4 = string t4
and ('a : immediate) t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the annotation on 'a in the declaration of the type t4.
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
       The kind of s5 is immutable_data
         because it is the primitive type string.
       But the kind of s5 must be a subkind of immediate
         because of the annotation on 'a in the declaration of the type t4.
|}]

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
Lines 3-4, characters 33-22:
3 | .................................function
4 |   | Void5 x -> Void5 x
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of 'a is void
         because of the definition of void5 at line 1, characters 0-37.
       But the layout of 'a must be a sublayout of value
         because it has to be value for the V1 safety check.
|}];;

(* disallowed attempts to use f5 and Void5 on non-voids *)
let h5 (x : int void5) = f5 x
[%%expect{|
Line 1, characters 12-15:
1 | let h5 (x : int void5) = f5 x
                ^^^
Error: This type int should be an instance of type ('a : void)
       The layout of int is value
         because it is the primitive immediate type int.
       But the layout of int must be a sublayout of void
         because of the definition of void5 at line 1, characters 0-37.
|}];;

let h5' (x : int) = Void5 x
[%%expect{|
Line 1, characters 26-27:
1 | let h5' (x : int) = Void5 x
                              ^
Error: This expression has type int but an expression was expected of type
         ('a : void)
       The layout of int is value
         because it is the primitive immediate type int.
       But the layout of int must be a sublayout of void
         because of the definition of void5 at line 1, characters 0-37.
|}];;

(* disallowed - tries to return void *)
let g (x : 'a void5) =
  match x with
  | Void5 x -> x;;
[%%expect{|
Lines 2-3, characters 2-16:
2 | ..match x with
3 |   | Void5 x -> x..
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       'a. Please report this error to the Jane Street compilers team.
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
       The kind of 'a is value
         because it is or unifies with an unannotated universal variable.
       But the kind of 'a must be a subkind of immediate
         because of the definition of t6_imm at line 1, characters 0-42.
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
       The kind of 'a is value
         because it is or unifies with an unannotated universal variable.
       But the kind of 'a must be a subkind of immediate
         because of the definition of t6_imm at line 1, characters 0-42.
|}];;

(* CR layouts v1.5: add more tests here once you can annotate these types with
   jkinds.
*)

(*****************************************)
(* Test 7: the jkind check in unify_var *)

type ('a : immediate) t7 = Foo7 of 'a

type t7' = (int * int) t7;;
[%%expect{|
type ('a : immediate) t7 = Foo7 of 'a
Line 3, characters 12-21:
3 | type t7' = (int * int) t7;;
                ^^^^^^^^^
Error: This type int * int should be an instance of type ('a : immediate)
       The kind of int * int is value
         because it's a tuple type.
       But the kind of int * int must be a subkind of immediate
         because of the definition of t7 at line 1, characters 0-37.
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
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because of the definition of t at line 2, characters 2-42.
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
       The layout of void_unboxed_record is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of void_unboxed_record must be a sublayout of value
         because it instantiates an unannotated type parameter of t,
         defaulted to layout value.
|}];;

module type S8_5 = sig
  val x : [`A of t_void]
end;;
[%%expect{|
Line 2, characters 17-23:
2 |   val x : [`A of t_void]
                     ^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of a tuple element.
|}];;

module M9_2 = struct
  type result = V of (string * void_unboxed_record) | I of int
end;;
[%%expect {|
Line 2, characters 31-50:
2 |   type result = V of (string * void_unboxed_record) | I of int
                                   ^^^^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of void_unboxed_record is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of void_unboxed_record must be a sublayout of value
         because it's the type of a tuple element.
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
       but an expression was expected of type ('a : value_or_null)
       The layout of void_unboxed_record is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of void_unboxed_record must be a sublayout of value
         because it's the type of a tuple element.
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
       The layout of void_unboxed_record is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of void_unboxed_record must be a sublayout of value
         because it's a boxed record type.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because of the definition of t at line 2, characters 2-24.
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
       The layout of void_unboxed_record is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of void_unboxed_record must be a sublayout of value
         because it instantiates an unannotated type parameter of t,
         defaulted to layout value.
|}];;

module type S9_7 = sig
  val x : int * t_void
end;;
[%%expect{|
Line 2, characters 16-22:
2 |   val x : int * t_void
                    ^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of a tuple element.
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
         ('a : value_or_null)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of a tuple element.
|}];;

(*************************************************)
(* Test 10: jkinds are checked by "more general" *)

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
       The type ('a : immediate) is not compatible with the type string
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of x at line 8, characters 10-26.
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
       The type 'a t = ('a : immediate) is not compatible with the type
         string
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of x at line 8, characters 10-26.
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
Error: Object types must have layout value.
       The layout of the type of this expression is void
         because of the definition of t at line 2, characters 2-42.
       But the layout of the type of this expression must overlap with value
         because it's the type of an object.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of an object field.
|}];;

module M11_3 = struct
  type ('a : void) t = A of 'a

  let foo o (A x) = o # usevoid x
end;;
[%%expect{|
Line 4, characters 32-33:
4 |   let foo o (A x) = o # usevoid x
                                    ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       'a. Please report this error to the Jane Street compilers team.
|}];;

module M11_4 = struct
  val x : < l : t_void >
end;;
[%%expect{|
Line 2, characters 12-22:
2 |   val x : < l : t_void >
                ^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of an object field.
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
       The layout of 'a s is void
         because of the annotation on 'a in the declaration of the type s.
       But the layout of 'a s must be a sublayout of value
         because it's the type of an object field.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of an object field.
|}];;

(*******************************************************************)
(* Test 12: class parameters and bound vars must have jkind value *)

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
Error: The types of variables bound by a 'let' in a class function
       must have layout value. Instead, v's type has layout void.
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
       The layout of bar is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of bar must be a sublayout of value
         because it's the type of a class field.
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
       The layout of bar is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of bar must be a sublayout of value
         because it's the type of a class field.
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
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with void
         because of the definition of t at line 2, characters 2-20.
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
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with void
         because of the definition of t at line 2, characters 2-30.
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
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with void
         because of the definition of t at line 2, characters 2-30.
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
       The layout of baz is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of baz must be a sublayout of value
         because it's the type of an instance variable.
|}];;

(*************************************************************************)
(* Test 13: built-in type constructors and support for non-value layouts *)

(* lazy *)
type t13 = t_void Lazy.t;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void Lazy.t;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of Lazy.t has this layout.
|}];;

let x13 (VV v) = lazy v;;
[%%expect{|
Line 1, characters 22-23:
1 | let x13 (VV v) = lazy v;;
                          ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of a lazy expression.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of a lazy expression.
|}];;

(* option *)
(* CR layouts v5: allow this *)
type t13 = t_void option;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void option;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of option has layout value.
|}];;

let x13 (VV v) = Some v;;
[%%expect{|
Line 1, characters 22-23:
1 | let x13 (VV v) = Some v;;
                          ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of option has layout value.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of option has layout value.
|}];;

(* list *)
(* CR layouts: should work after relaxing the mixed block restriction. *)
type t13 = t_void list;;
[%%expect{|
Line 1, characters 11-17:
1 | type t13 = t_void list;;
               ^^^^^^
Error: This type t_void should be an instance of type ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of list has layout value.
|}];;

let x13 (VV v) = [v];;
[%%expect{|
Line 1, characters 18-19:
1 | let x13 (VV v) = [v];;
                      ^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of list has layout value.
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
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* array *)
(* CR layouts v4: should work *)
type t13 = t_void array;;
[%%expect{|
type t13 = t_void array
|}];;

let x13 (VV v) = [| v |];;
[%%expect{|
Line 1, characters 17-24:
1 | let x13 (VV v) = [| v |];;
                     ^^^^^^^
Error: Layout void is not supported yet.
|}];;

let x13 v =
  match v with
  | [| v |] -> VV v
  | _ -> assert false
[%%expect{|
Lines 2-4, characters 2-21:
2 | ..match v with
3 |   | [| v |] -> VV v
4 |   | _ -> assert false
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it has to be value for the V1 safety check.
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
       The layout of foo14 is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of foo14 must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(****************************************************)
(* Test 15: Type aliases need not have jkind value *)
(* (In [transl_type_aux], this hits the jkind given to the type variable in the
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
(* Test 17: incremental jkind checking of @@unboxed types - see comment on
   [constrain_type_jkind]. *)

type 'a t17 = 'a list
type s17 = { lbl : s17 t17 } [@@unboxed];;

[%%expect{|
type 'a t17 = 'a list
type s17 = { lbl : s17 t17; } [@@unboxed]
|}];;

(*****************************************)
(* Test 18: expansion in [check_univars] *)
(* This test isn't really jkinds-specific, but it checks that the jkind checks
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
Lines 6-7, characters 2-20:
6 | ..let rec g { x = x ; y = y } : _ r = g { x; y } in
7 |   g (failwith "foo")..
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of 'a is void
         because of the definition of r at line 3, characters 0-40.
       But the layout of 'a must be a sublayout of value
         because it has to be value for the V1 safety check.
|}];;

(********************************************************************)
(* Test 23: checking the error message from impossible GADT matches *)

type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq

module M : sig
  type t_void : void
  type t_imm : immediate
end = struct
  type t_void : void
  type t_imm : immediate
end
(* these are abstract, so the only trouble with unifying them in a GADT
   match is around their jkinds *)

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
       The layout of M.t_void is void
         because of the definition of t_void at line 4, characters 2-20.
       But the layout of M.t_void must overlap with value
         because of the definition of t_imm at line 5, characters 2-24.
|}]

(*****************************************************)
(* Test 24: Polymorphic parameter with exotic layout *)

type 'a t2_void : void

let f (x : 'a. 'a t2_void) = x

[%%expect{|
type 'a t2_void : void
Line 3, characters 29-30:
3 | let f (x : 'a. 'a t2_void) = x
                                 ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       'a t2_void.
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
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because the type argument of option has layout value.
|}]

(*********************************************************)
(* Test 26: Inferring an application to an exotic layout *)

let g f (x : t_void) : t_void = f x

[%%expect{|
Line 1, characters 32-35:
1 | let g f (x : t_void) : t_void = f x
                                    ^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(******************************************)
(* Test 27: Exotic layouts in approx_type *)

let rec f : _ -> _ = fun (x : t_void) -> x

[%%expect{|
Line 1, characters 41-42:
1 | let rec f : _ -> _ = fun (x : t_void) -> x
                                             ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(**********************************************)
(* Test 28: Exotic layouts in letop and andop *)

(* CR layouts v5: the void parts of this test use [let rec] and [and] so that we
   can test the type-checker as opposed to the value-kind check.  Once void is
   properly supported, they don't need to be recursive anymore. *)

(* 28.1: non-value letop arg *)
let rec ( let* ) (x : t_void) f = ()

and q () =
  let* x = assert false in
  ()

[%%expect{|
Line 1, characters 17-29:
1 | let rec ( let* ) (x : t_void) f = ()
                     ^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) (x : t_float64) f = ()

let q () =
  let* x = assert false in
  ()

[%%expect{|
val ( let* ) : ('a : value_or_null). t_float64 -> 'a -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.2: non-value letop binder arg without and *)
let rec ( let* ) x (f : t_void -> _) = ()

and q () =
  let* x = assert false in
  ()

[%%expect{|
Line 4, characters 7-8:
4 |   let* x = assert false in
           ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) x (f : t_float64 -> _) = ()

let q () =
  let* x = assert false in
  ()

[%%expect{|
val ( let* ) :
  ('a : value_or_null) ('b : any). 'a -> (t_float64 -> 'b) -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.3: non-value letop binder result *)
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

let ( let* ) x (f : _ -> t_float64) = ()

let q () =
  let* x = assert false in
  assert false

[%%expect{|
val ( let* ) :
  ('a : value_or_null) ('b : any). 'a -> ('b -> t_float64) -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.4: non-value letop result *)
let rec ( let* ) x f : t_void = assert false

and q () =
  let* x = 5 in
  ()

[%%expect{|
Line 1, characters 32-44:
1 | let rec ( let* ) x f : t_void = assert false
                                    ^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) x f : t_float64 = assert false

let q () =
  let* x = 5 in
  ()

[%%expect{|
val ( let* ) :
  ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> t_float64 = <fun>
val q : unit -> t_float64 = <fun>
|}]

(* 28.5: non-value andop second arg *)
let rec ( let* ) x f = ()
and ( and* ) x1 (x2 : t_void) = ()
and q () =
    let* x = 5
    and* y = assert false
    in
    ()

[%%expect{|
Line 2, characters 16-29:
2 | and ( and* ) x1 (x2 : t_void) = ()
                    ^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) x f = ()
let ( and* ) x1 (x2 : t_float64) = ()
let q () =
    let* x = 5
    and* y = assert false
    in
    ()

[%%expect{|
val ( let* ) : ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> unit =
  <fun>
val ( and* ) : ('a : value_or_null). 'a -> t_float64 -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.5: non-value andop first arg *)
let rec ( let* ) x f = ()
and ( and* ) (x1 : t_void) x2 = ()
and q () =
    let* x = assert false
    and* y = 5
    in
    ()

[%%expect{|
Line 2, characters 13-26:
2 | and ( and* ) (x1 : t_void) x2 = ()
                 ^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) x f = ()
let ( and* ) (x1 : t_float64) x2 = ()
let q () =
    let* x = assert false
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> unit =
  <fun>
val ( and* ) : ('a : value_or_null). t_float64 -> 'a -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.6: non-value andop result *)
let rec ( let* ) x f = ()
and ( and* ) x1 x2 : t_void = assert false
and q () =
    let* x = 5
    and* y = 5
    in
    ()

[%%expect{|
Line 1, characters 17-18:
1 | let rec ( let* ) x f = ()
                     ^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

let ( let* ) (x : (_ : float64))  f = ()
let ( and* ) x1 x2 : t_float64 = assert false
let q () =
    let* x = 5
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : ('a : float64) ('b : value_or_null). 'a -> 'b -> unit = <fun>
val ( and* ) :
  ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> t_float64 = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.7: non-value letop binder arg with and *)
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
       but a pattern was expected which matches values of type
         ('a : value_or_null)
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because it's the type of a tuple element.
|}]

let ( let* ) x f = ()
let ( and* ) x1 x2 = assert false
let q () =
    let* x : t_float64 = assert false
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : ('a : value_or_null) ('b : value_or_null). 'a -> 'b -> unit =
  <fun>
val ( and* ) :
  ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a -> 'b -> 'c =
  <fun>
Line 4, characters 9-22:
4 |     let* x : t_float64 = assert false
             ^^^^^^^^^^^^^
Error: This pattern matches values of type t_float64
       but a pattern was expected which matches values of type
         ('a : value_or_null)
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the type of a tuple element.
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
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because of the definition of eq at line 2, characters 2-43.
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
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because of the definition of f at line 3, characters 4-20.
|}]

(**************************************************)
(* Test 31: checking that #poly_var patterns work *)

type ('a : void) poly_var = [`A of int * 'a | `B]

let f #poly_var = "hello"

[%%expect{|
Line 1, characters 41-43:
1 | type ('a : void) poly_var = [`A of int * 'a | `B]
                                             ^^
Error: This type ('a : value_or_null) should be an instance of type
         ('a0 : void)
       The layout of 'a is void
         because of the annotation on 'a in the declaration of the type
                                      poly_var.
       But the layout of 'a must overlap with value
         because it's the type of a tuple element.
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
         ('a : value_or_null)
       The layout of t_void is void
         because of the definition of t_void at line 1, characters 0-18.
       But the layout of t_void must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

(******************************************************)
(* Test 33: Externals must have representable types *)
external foo33 : t_any = "foo33";;

[%%expect{|
Line 1, characters 17-22:
1 | external foo33 : t_any = "foo33";;
                     ^^^^^
Error: This type signature for foo33 is not a value type.
       The layout of type t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of type t_any must be a sublayout of value
         because it's the type of something stored in a module structure.
|}]


(****************************************************)
(* Test 34: Layout clash in polymorphic record type *)

(* tested elsewhere *)

(****************************************************)
(* Test 35: check bad layout error in filter_arrow *)

type ('a : immediate) t35 = 'a
let f35 : 'a t35 = fun () -> ()

[%%expect {|
type ('a : immediate) t35 = 'a
Line 2, characters 19-31:
2 | let f35 : 'a t35 = fun () -> ()
                       ^^^^^^^^^^^^
Error:
       The kind of 'a -> 'b is value mod unique uncontended
         because it's a function type.
       But the kind of 'a -> 'b must be a subkind of immediate
         because of the definition of t35 at line 1, characters 0-30.
|}]

(**************************************************)
(* Test 36: Disallow non-representable statements *)

let () = (assert false : t_any); ()
[%%expect{|
Line 1, characters 9-31:
1 | let () = (assert false : t_any); ()
             ^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 1, characters 10-22:
1 | let () = (assert false : t_any); ()
              ^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_5)
       because it is in the left-hand side of a sequence
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of t_any must be representable
         because it's the type of a statement.
|}]

let () = while false do (assert false : t_any); done
[%%expect{|
Line 1, characters 24-46:
1 | let () = while false do (assert false : t_any); done
                            ^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 1, characters 25-37:
1 | let () = while false do (assert false : t_any); done
                             ^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_6)
       because it is in the body of a while-loop
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of t_any must be representable
         because it's the type of a statement.
|}]

let () = for i = 0 to 0 do (assert false : t_any); done
[%%expect{|
Line 1, characters 27-49:
1 | let () = for i = 0 to 0 do (assert false : t_any); done
                               ^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 1, characters 28-40:
1 | let () = for i = 0 to 0 do (assert false : t_any); done
                                ^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_7)
       because it is in the body of a for-loop
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of t_any must be representable
         because it's the type of a statement.
|}]

(******************************************************)
(* Test 37: Ensure signature inclusion checks layouts *)

(* Doesn't need layouts_alpha. *)


(*****************************************************)
(* Test 38: Ensure Univar unification checks layouts *)

(* Doesn't need layouts_alpha. *)


(*************************************************************)
(* Test 39: Inference of functions that don't bind arguments *)

(* Doesn't need layouts_alpha. *)

(****************************************************)
(* Test 40: unannotated type parameter defaults to layout value *)

(* Doesn't need layouts_alpha. *)

(**********************************************************************)
(* Test 41: constraints in manifests in mutually recursive typedecls. *)

(* Doesn't need layouts_alpha. *)
