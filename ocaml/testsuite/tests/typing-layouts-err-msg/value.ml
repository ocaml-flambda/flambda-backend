(* TEST
 flags = "-extension layouts_alpha -extension comprehensions";
 expect;
*)

(***********************)
(* Value layout errors *)

type t_any   : any
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_void  : void

type void_variant = VV of t_void
[%%expect{|
type t_any : any
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_void : void
type void_variant = VV of t_void
|}];;

(* Class_let_binding *)
let f (): t_float64 = assert false
class foo () =
  let v = f () in
  object end;;
[%%expect{|
val f : unit -> t_float64 = <fun>
Line 3, characters 6-7:
3 |   let v = f () in
          ^
Error: The types of variables bound by a 'let' in a class function
       must have layout value. Instead, v's type has layout float64.
|}];;

(* Tuple_element *)
type t = t_any * t_any
[%%expect{|
Line 1, characters 9-14:
1 | type t = t_any * t_any
             ^^^^^
Error: Tuple element types must have layout value.
       The layout of "t_any" is any
         because of the definition of t_any at line 1, characters 0-18.
       But the layout of "t_any" must be a sublayout of value
         because it's the type of a tuple element.
|}];;

(* Probe *)
(* See [probe.ml] *)

(* Object *)
let f: ('a : void) -> 'b = fun x -> x # baz
[%%expect{|
Line 1, characters 36-37:
1 | let f: ('a : void) -> 'b = fun x -> x # baz
                                        ^
Error: Object types must have layout value.
       The layout of the type of this expression is void
         because of the annotation on the type variable 'a.
       But the layout of the type of this expression must overlap with value
         because it's the type of an object.
|}];;

(* Instance_variable *)
module type S = sig
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

(* Object_field *)
let f x: t_void = x # baz
[%%expect{|
Line 1, characters 18-25:
1 | let f x: t_void = x # baz
                      ^^^^^^^
Error: This expression has type "('a : value)"
       but an expression was expected of type "t_void"
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because it's the type of an object field.
|}];;

(* Class_field *)
class foo () =
  object
    val bar: t_void = assert false
  end
[%%expect{|
Line 3, characters 8-11:
3 |     val bar: t_void = assert false
            ^^^
Error: Variables bound in a class must have layout value.
       The layout of bar is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of bar must be a sublayout of value
         because it's the type of a class field.
|}];;

(* Boxed_record *)
type r : void = {a:string}
[%%expect{|
Line 1, characters 0-26:
1 | type r : void = {a:string}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "r" is value
         because it's a boxed record type.
       But the layout of type "r" must be a sublayout of void
         because of the annotation on the declaration of the type r.
|}];;

(* Boxed_variant *)
type v : void = A of t_value
[%%expect{|
Line 1, characters 0-28:
1 | type v : void = A of t_value
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "v" is value
         because it's a boxed variant type.
       But the layout of type "v" must be a sublayout of void
         because of the annotation on the declaration of the type v.
|}];;

(* Extensible_variant *)
type attr : void = ..
[%%expect{|
Line 1, characters 0-21:
1 | type attr : void = ..
    ^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "attr" is value
         because it's an extensible variant type.
       But the layout of type "attr" must be a sublayout of void
         because of the annotation on the declaration of the type attr.
|}]

(* Primitive *)
let f : unit -> ('a : void) = fun () -> "abc"
[%%expect{|
Line 1, characters 40-45:
1 | let f : unit -> ('a : void) = fun () -> "abc"
                                            ^^^^^
Error: This expression has type "string" but an expression was expected of type
         "('a : void)"
       The layout of string is value
         because it is the primitive immutable_data type string.
       But the layout of string must be a sublayout of void
         because of the annotation on the type variable 'a.
|}];;

(* Type_argument *)
let f (x: t_void) = [x]
[%%expect{|
Line 1, characters 21-22:
1 | let f (x: t_void) = [x]
                         ^
Error: This expression has type "t_void" but an expression was expected of type
         "('a : value)"
       The layout of t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of t_void must be a sublayout of value
         because the type argument of list has layout value.
|}];;

(* Tuple *)
let f : unit -> ('a : void) = fun () -> (1,2)
[%%expect{|
Line 1, characters 40-45:
1 | let f : unit -> ('a : void) = fun () -> (1,2)
                                            ^^^^^
Error: This expression has type "'b * 'c"
       but an expression was expected of type "('a : void)"
       The layout of 'a * 'b is value
         because it's a tuple type.
       But the layout of 'a * 'b must be a sublayout of void
         because of the annotation on the type variable 'a.
|}];;

(* Row_variable *)
(* Unable to produce this error *)

(* Polymorphic_variant *)
type ('a: void) t = 'a
let f (x: [`A of int | `B]): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 36-37:
2 | let f (x: [`A of int | `B]): 'a t = x
                                        ^
Error: This expression has type "[ `A of int | `B ]"
       but an expression was expected of type "'a t" = "('a : void)"
       The layout of [ `A of int | `B ] is value
         because it's a polymorphic variant type.
       But the layout of [ `A of int | `B ] must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
|}]

(* Arrow *)
type ('a: void) t = 'a
let f (x : int -> int): 'a t = x
[%%expect{|
type ('a : void) t = 'a
Line 2, characters 31-32:
2 | let f (x : int -> int): 'a t = x
                                   ^
Error: This expression has type "int -> int"
       but an expression was expected of type "'a t" = "('a : void)"
       The layout of int -> int is value
         because it's a function type.
       But the layout of int -> int must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
|}]

(* Tfield *)
(* Unable to produce this error *)

(* Tnil *)
(* Unable to produce this error *)

(* First_class_module *)
type ('a: void) t = 'a
module type X_int = sig val x : int end;;
module Three : X_int = struct let x = 3 end;;
let f (): 'a t = (module Three : X_int)
[%%expect{|
type ('a : void) t = 'a
module type X_int = sig val x : int end
module Three : X_int
Line 4, characters 17-39:
4 | let f (): 'a t = (module Three : X_int)
                     ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(module X_int)"
       but an expression was expected of type "'a t" = "('a : void)"
       The layout of (module X_int) is value
         because it's a first-class module type.
       But the layout of (module X_int) must be a sublayout of void
         because of the definition of t at line 1, characters 0-22.
|}]

(* Separability_check *)
(* Only used within [Result.is_error] and not thrown as an exception *)

(* Univar *)
let f: 'a. 'a -> ('b : void) = fun x -> x
[%%expect{|
Line 1, characters 40-41:
1 | let f: 'a. 'a -> ('b : void) = fun x -> x
                                            ^
Error: This expression has type "('a : value)"
       but an expression was expected of type "('b : void)"
       The layout of 'b is void
         because of the annotation on the type variable 'b.
       But the layout of 'b must overlap with value
         because it is or unifies with an unannotated universal variable.
|}];;

(* Polymorphic_variant_field *)
let f (x : t_float64) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f (x : t_float64) = `A x;;
                               ^
Error: This expression has type "t_float64"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

(* Default_type_jkind *)
type ('a : immediate) t2 = {a: 'a} and t3 = t t2 and t
[%%expect{|
Line 1, characters 49-54:
1 | type ('a : immediate) t2 = {a: 'a} and t3 = t t2 and t
                                                     ^^^^^
Error:
       The kind of t is value
         because an abstract type has the value kind by default.
       But the kind of t must be a subkind of immediate
         because of the annotation on 'a in the declaration of the type t2.
|}];;

(* Float_record_field *)
(* The type is restricted to float which always has layout value.
   Can't generate an error *)

(* Existential_type_variable *)
(* See [gadt_existential.ml] *)

(* Array_comprehension_element *)
let f (x : t_float64) = [| x for i = 0 to 1 |]
[%%expect{|
Line 1, characters 27-28:
1 | let f (x : t_float64) = [| x for i = 0 to 1 |]
                               ^
Error: This expression has type "t_float64"
       but an expression was expected of type "('a : value)"
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the element type of array comprehension.
|}];;

(* Lazy_expression *)
let f (x : t_float64) = lazy x
[%%expect{|
Line 1, characters 29-30:
1 | let f (x : t_float64) = lazy x
                                 ^
Error: This expression has type "t_float64"
       but an expression was expected of type "('a : value)"
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the type of a lazy expression.
|}];;

(* Class_type_argument *)
module M = struct
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
Error: This type "('a : value)" should be an instance of type "('b : void)"
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with void
         because of the definition of t at line 2, characters 2-20.
|}];;

(* Class_term_argument *)
class foo (x : t_float64) =
  object end;;
[%%expect{|
Line 1, characters 10-25:
1 | class foo (x : t_float64) =
              ^^^^^^^^^^^^^^^
Error: This pattern matches values of type "t_float64"
       but a pattern was expected which matches values of type "('a : value)"
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the type of a term-level argument to a class constructor.
|}];;

(* Structure_element *)
module type S = sig val x : t_void end
[%%expect{|
Line 1, characters 28-34:
1 | module type S = sig val x : t_void end
                                ^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type t_void is void
         because of the definition of t_void at line 6, characters 0-19.
       But the layout of type t_void must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;

(* Debug_printer_argument *)
(* See [debug_printer.ml] *)

(* V1_safety_check *)
type t = {a: t_void; b: int}
let f (x: t) = match x with | {a; b} -> a
[%%expect {|
type t = { a : t_void; b : int; }
Line 2, characters 15-41:
2 | let f (x: t) = match x with | {a; b} -> a
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout void detected in [Typeopt.layout] as sort for type
       t_void. Please report this error to the Jane Street compilers team.
|}]

(* Captured_in_object *)
let f (m1 : t_float64) = object
  val f = fun () -> m1
end;;
[%%expect{|
Line 2, characters 20-22:
2 |   val f = fun () -> m1
                        ^^
Error: "m1" must have a type of layout value because it is captured by an object.
       The layout of t_float64 is float64
         because of the definition of t_float64 at line 5, characters 0-24.
       But the layout of t_float64 must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(* Unknown *)
(* Unable to produce this error *)
