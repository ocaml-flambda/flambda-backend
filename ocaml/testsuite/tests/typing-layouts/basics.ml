(* TEST
 include stdlib_upstream_compatible;
 {
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }
*)

type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64
type t_float64 : float64;;
type t_any   : any;;

[%%expect{|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_float64 : float64
type t_any : any
|}]

type t_void  : void;;
[%%expect{|
Line 1, characters 15-19:
1 | type t_void  : void;;
                   ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_non_null_value : non_null_value;;
[%%expect{|
Line 1, characters 24-38:
1 | type t_non_null_value : non_null_value;;
                            ^^^^^^^^^^^^^^
Error: Layout value is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

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

  type q = t s
end;;
[%%expect{|
module type S1 =
  sig type t : any type 'a s = 'a -> int constraint 'a = t type q = t s end
|}]

module type S1 = sig
  type t : any

  type 'a s = 'a -> int constraint 'a = t

  type q = t s
end;;
[%%expect{|
Line 4, characters 35-41:
4 |   type 'a s = 'a -> int constraint 'a = t
                                       ^^^^^^
Error: The type constraints are not consistent.
       Type ('a : '_representable_layout_1) is not compatible with type t
       The layout of t is any, because
         of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable, because
         it instantiates an unannotated type parameter of s.
|}]

module type S1 = sig
  type t : any

  type ('a : any) s = int -> 'a constraint 'a = t

  type q = t s
end;;
[%%expect{|
module type S1 =
  sig type t : any type 'a s = int -> 'a constraint 'a = t type q = t s end
|}]

module M1 = struct
  type t : any

  type ('a : any) s = { a: 'a -> 'a }

  type q = t s

  let f1 () : 'a s = { a = fun x -> Stdlib_upstream_compatible.Float_u.abs x }
  let f2 () : 'a s = { a = fun x -> x ^ "!" }
  let f3 () : 'a s = { a = fun x -> x + 1 }
end;;
[%%expect{|
module M1 :
  sig
    type t : any
    type ('a : any) s = { a : 'a -> 'a; }
    type q = t s
    val f1 : unit -> Stdlib_upstream_compatible.Float_u.t s
    val f2 : unit -> string s
    val f3 : unit -> int s
  end
|}]

module M1 = struct
  type t : any

  type ('a : any) s = A of ('a -> 'a)

  type q = t s

  let f1 () : 'a s = A (fun x -> Stdlib_upstream_compatible.Float_u.abs x)
  let f2 () : 'a s = A (fun x -> x ^ "!")
  let f3 () : 'a s = A (fun x -> x + 1)
end;;
[%%expect{|
module M1 :
  sig
    type t : any
    type ('a : any) s = A of ('a -> 'a)
    type q = t s
    val f1 : unit -> Stdlib_upstream_compatible.Float_u.t s
    val f2 : unit -> string s
    val f3 : unit -> int s
  end
|}]

module M1 = struct
  type t : any

  type ('a : any) s = A of { a: 'a -> 'a }

  type q = t s

  let f1 () : 'a s = A { a = fun x -> Stdlib_upstream_compatible.Float_u.abs x }
  let f2 () : 'a s = A { a = fun x -> x ^ "!" }
  let f3 () : 'a s = A { a = fun x -> x + 1 }
end;;
[%%expect{|
module M1 :
  sig
    type t : any
    type ('a : any) s = A of { a : 'a -> 'a; }
    type q = t s
    val f1 : unit -> Stdlib_upstream_compatible.Float_u.t s
    val f2 : unit -> string s
    val f3 : unit -> int s
  end
|}]

module M1 = struct
  type t : any

  type ('a : any) s = A : ('a : any) 'b. { a: 'a -> 'b -> 'a } -> 'a s

  type q = t s

  let f0 () = A {a = (fun x y -> x)}
  let f1 () = A {a = (fun x y -> x + 1)}
  let f2 () = A {a = (fun x y -> x ^ "!")}
  let f3 () = A {a = (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)}
  let f4 () = A {a = (fun x y -> x + y)}
  let f5 () = A {a = (fun x y -> x ^ y)}
end;;
[%%expect{|
module M1 :
  sig
    type t : any
    type ('a : any) s = A : ('a : any) 'b. { a : 'a -> 'b -> 'a; } -> 'a s
    type q = t s
    val f0 : unit -> 'a s
    val f1 : unit -> int s
    val f2 : unit -> string s
    val f3 : unit -> Stdlib_upstream_compatible.Float_u.t s
    val f4 : unit -> int s
    val f5 : unit -> string s
  end
|}]

module M1 = struct
  type t : any

  type ('a : any) s = A : ('a : any) 'b. ('a -> 'b -> 'a) -> 'a s

  type q = t s

  let f0 () = A (fun x y -> x)
  let f1 () = A (fun x y -> x + 1)
  let f2 () = A (fun x y -> x ^ "!")
  let f3 () = A (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)
  let f4 () = A (fun x y -> x + y)
  let f5 () = A (fun x y -> x ^ y)
end
[%%expect{|
module M1 :
  sig
    type t : any
    type ('a : any) s = A : ('a : any) 'b. ('a -> 'b -> 'a) -> 'a s
    type q = t s
    val f0 : unit -> 'a s
    val f1 : unit -> int s
    val f2 : unit -> string s
    val f3 : unit -> Stdlib_upstream_compatible.Float_u.t s
    val f4 : unit -> int s
    val f5 : unit -> string s
  end
|}]

module type S1 = sig
  type t : any

  type ('a : any) s = A : { a: 'a -> 'b -> 'a } -> 'a s

  type q = t s
end;;
[%%expect{|
module type S1 =
  sig
    type t : any
    type ('a : any) s = A : { a : 'a -> 'b -> 'a; } -> 'a s
    type q = t s
  end
|}]

module M1 = struct
  type ('a : any) s = A : { a: 'a -> 'b -> 'a } -> 'a s

  let f1 () = A {a = (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)}
end;;

[%%expect{|
Line 4, characters 72-73:
4 |   let f1 () = A {a = (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)}
                                                                            ^
Error: This expression has type ('a : value)
       but an expression was expected of type
         Stdlib_upstream_compatible.Float_u.t = float#
       The layout of Stdlib_upstream_compatible.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_upstream_compatible.Float_u.t must be a sublayout of value, because
         of the definition of s at line 2, characters 2-55.
|}]

module type S1 = sig
  type t : any

  type ('a : any) s = A : ('a -> 'b -> 'a) -> 'a s

  type q = t s
end;;
[%%expect{|
module type S1 =
  sig
    type t : any
    type ('a : any) s = A : ('a -> 'b -> 'a) -> 'a s
    type q = t s
  end
|}]

module M1 = struct
  type ('a : any) s = A : ('a -> 'b -> 'a) -> 'a s

  let f1 () = A (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)
end;;

[%%expect{|
Line 4, characters 67-68:
4 |   let f1 () = A (fun x y -> Stdlib_upstream_compatible.Float_u.abs x)
                                                                       ^
Error: This expression has type ('a : value)
       but an expression was expected of type
         Stdlib_upstream_compatible.Float_u.t = float#
       The layout of Stdlib_upstream_compatible.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_upstream_compatible.Float_u.t must be a sublayout of value, because
         of the definition of s at line 2, characters 2-50.
|}]

module M1 = struct
  type ('a : any) s = A : ('a : any) 'b. { a: 'a -> 'b -> 'a } -> 'a s

  let f6 () = A {a = (fun x y -> Stdlib_upstream_compatible.Float_u.add x y)}
end;;

[%%expect{|
Line 4, characters 74-75:
4 |   let f6 () = A {a = (fun x y -> Stdlib_upstream_compatible.Float_u.add x y)}
                                                                              ^
Error: This expression has type ('a : value)
       but an expression was expected of type
         Stdlib_upstream_compatible.Float_u.t = float#
       The layout of Stdlib_upstream_compatible.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_upstream_compatible.Float_u.t must be a sublayout of value, because
         of the definition of s at line 2, characters 2-70.
|}]

module M1 = struct
  type ('a : any) s = A : ('a : any) 'b. ('a -> 'b -> 'a) -> 'a s

  let f6 () = A (fun x y -> Stdlib_upstream_compatible.Float_u.add x y)
end;;

[%%expect{|
Line 4, characters 69-70:
4 |   let f6 () = A (fun x y -> Stdlib_upstream_compatible.Float_u.add x y)
                                                                         ^
Error: This expression has type ('a : value)
       but an expression was expected of type
         Stdlib_upstream_compatible.Float_u.t = float#
       The layout of Stdlib_upstream_compatible.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_upstream_compatible.Float_u.t must be a sublayout of value, because
         of the definition of s at line 2, characters 2-65.
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
       Type ('a : '_representable_layout_2) is not compatible with type t
       The layout of t is any, because
         of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable, because
         it instantiates an unannotated type parameter of s.
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
       Type ('a : '_representable_layout_3) is not compatible with type t
       The layout of t is any, because
         of the definition of t at line 2, characters 2-14.
       But the layout of t must be representable, because
         it instantiates an unannotated type parameter of s.
|}]

let f1 () : t_any = assert false;;
[%%expect{|
Line 1, characters 20-32:
1 | let f1 () : t_any = assert false;;
                        ^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : '_representable_layout_4)
       The layout of t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of t_any must be representable, because
         we must know concretely how to return a function result.
|}];;

let f1 (x : t_any) = ();;
[%%expect{|
Line 1, characters 7-18:
1 | let f1 (x : t_any) = ();;
           ^^^^^^^^^^^
Error: This pattern matches values of type t_any
       but a pattern was expected which matches values of type
         ('a : '_representable_layout_5)
       The layout of t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of t_any must be representable, because
         we must know concretely how to pass a function argument.
|}];;

(*****************************************************)
(* Test 2: Permit representable function arg/returns *)

(* CR layouts v5: the void bits of this test should be copied here from
   basics_alpha *)
module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|
module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

module type S2 = sig
  val g : float# -> int
end;;
[%%expect{|
module type S2 = sig val g : float# -> int end
|}];;

module type S2 = sig
  val g : int -> float#
end
[%%expect {|
module type S2 = sig val g : int -> float# end
|}];;

module type S2 = sig
  type t' : float64
  type s' = r' -> int
  and r' = t'
end;;
[%%expect{|
module type S2 = sig type t' : float64 type s' = r' -> int and r' = t' end
|}]

module type S2 = sig
  val f : int -> t_float64
end;;
[%%expect {|
module type S2 = sig val f : int -> t_float64 end
|}];;

module type S = sig
  type t' : float64
  type 'a s' = 'a -> int constraint 'a = t'
end;;
[%%expect{|
module type S =
  sig type t' : float64 type 'a s' = 'a -> int constraint 'a = t' end
|}]

module F2 (X : sig val x : t_float64 end) = struct
  let f () = X.x
end;;
[%%expect{|
Line 1, characters 27-36:
1 | module F2 (X : sig val x : t_float64 end) = struct
                               ^^^^^^^^^
Error: This type signature for x is not a value type.
       The layout of type t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of type t_float64 must be a sublayout of value, because
         it's the type of something stored in a module structure.
|}];;
(* CR layouts v5: the test above should be made to work *)

module F2 (X : sig val f : t_float64 -> unit end) = struct
  let g z = X.f z
end;;
[%%expect{|
module F2 :
  functor (X : sig val f : t_float64 -> unit end) ->
    sig val g : t_float64 -> unit end
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of imm_id at line 1, characters 0-33.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of id_for_imms at line 1, characters 16-35.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
|}];;

type s4 = string t4
and ('a : immediate) t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
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
       The layout of s5 is value, because
         it is the primitive value type string.
       But the layout of s5 must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
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

type ('a : void) void4 = Void4  of 'a;;
[%%expect{|
Line 1, characters 11-15:
1 | type ('a : void) void4 = Void4  of 'a;;
               ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : any) any4 = Any4 of 'a
[%%expect{|
Line 1, characters 23-33:
1 | type ('a : any) any4 = Any4 of 'a
                           ^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of 'a is any, because
         of the annotation on 'a in the declaration of the type any4.
       But the layout of 'a must be representable, because
         it's the type of a constructor field.
|}];;

(************************************************************)
(* Test 5: You can touch a void, but not return it directly *)

(* CR layouts v5: these tests moved to [basics_alpha.ml].  Bring them here.
   Also the tests will change because we'll allow returning void. *)

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
       The layout of 'a is value, because
         it is or unifies with an unannotated universal variable.
       But the layout of 'a must be a sublayout of immediate, because
         of the definition of t6_imm at line 1, characters 0-42.
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
       The layout of 'a is value, because
         it is or unifies with an unannotated universal variable.
       But the layout of 'a must be a sublayout of immediate, because
         of the definition of t6_imm at line 1, characters 0-42.
|}];;

(* CR layouts v1.5: add more tests here once you can annotate these types with
   jkinds. *)

(****************************************)
(* Test 7: the jkind check in unify_var *)

type ('a : immediate) t7 = Foo7 of 'a

type t7' = (int * int) t7;;
[%%expect{|
type ('a : immediate) t7 = Foo7 of 'a
Line 3, characters 12-21:
3 | type t7' = (int * int) t7;;
                ^^^^^^^^^
Error: This type int * int should be an instance of type ('a : immediate)
       The layout of int * int is value, because
         it's a tuple type.
       But the layout of int * int must be a sublayout of immediate, because
         of the definition of t7 at line 1, characters 0-37.
|}]

(**********************************************************)
(* Test 8: Polymorphic variants take value args (for now) *)

(* CR layouts v5: Bring over void versions of these tests. *)

module M8_1f = struct
  type foo1 = [ `Foo1 of int | `Baz1 of t_float64 | `Bar1 of string ];;
end
[%%expect{|
Line 2, characters 40-49:
2 |   type foo1 = [ `Foo1 of int | `Baz1 of t_float64 | `Bar1 of string ];;
                                            ^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

module M8_2f = struct
  let foo x =
    match x with
    | `Baz 42 -> Stdlib_upstream_compatible.Float_u.of_float 3.14
    | `Bar v -> v
    | `Bas i -> Stdlib_upstream_compatible.Float_u.of_float 3.14
end;;
[%%expect {|
Line 5, characters 16-17:
5 |     | `Bar v -> v
                    ^
Error: This expression has type ('a : value)
       but an expression was expected of type
         Stdlib_upstream_compatible.Float_u.t = float#
       The layout of Stdlib_upstream_compatible.Float_u.t is float64, because
         it is the primitive float64 type float#.
       But the layout of Stdlib_upstream_compatible.Float_u.t must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

module M8_3f = struct
  type 'a t = [ `Foo of 'a | `Baz of int ]

  type bad = t_float64 t
end;;
[%%expect {|
Line 4, characters 13-22:
4 |   type bad = t_float64 t
                 ^^^^^^^^^
Error: This type t_float64 should be an instance of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of t at line 2, characters 2-42.
|}];;

module M8_4f = struct
  type 'a t = [ `Foo of 'a | `Baz of int ] constraint 'a = t_float64
end;;
[%%expect {|
Line 2, characters 54-68:
2 |   type 'a t = [ `Foo of 'a | `Baz of int ] constraint 'a = t_float64
                                                          ^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

module type S8_5f = sig
  val x : [`A of t_float64]
end;;
[%%expect{|
Line 2, characters 17-26:
2 |   val x : [`A of t_float64]
                     ^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}]

(************************************************)
(* Test 9: Tuples only work on values (for now) *)

(* CR layouts v5: bring over void tests. *)
module M9_1f = struct
  type foo1 = int * t_float64 * [ `Foo1 of int | `Bar1 of string ];;
end
[%%expect{|
Line 2, characters 20-29:
2 |   type foo1 = int * t_float64 * [ `Foo1 of int | `Bar1 of string ];;
                        ^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

module M9_2f = struct
  type result = V of (string * t_float64) | I of int
end;;
[%%expect {|
Line 2, characters 31-40:
2 |   type result = V of (string * t_float64) | I of int
                                   ^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

module M9_4f = struct
  let f_id (x : float#) = x

  let foo x =
    match x with
    | (a, _) -> f_id a
end;;
[%%expect {|
Line 6, characters 21-22:
6 |     | (a, _) -> f_id a
                         ^
Error: This expression has type ('a : value)
       but an expression was expected of type float#
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

module M9_5f = struct
  type 'a t = (int * 'a)

  type bad = t_float64 t
end;;
[%%expect {|
Line 4, characters 13-22:
4 |   type bad = t_float64 t
                 ^^^^^^^^^
Error: This type t_float64 should be an instance of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of t at line 2, characters 2-24.
|}];;

module M9_6f = struct
  type 'a t = int * 'a constraint 'a = t_float64
end;;
[%%expect {|
Line 2, characters 34-48:
2 |   type 'a t = int * 'a constraint 'a = t_float64
                                      ^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

module type S9_7f = sig
  val x : int * t_float64
end;;
[%%expect{|
Line 2, characters 16-25:
2 |   val x : int * t_float64
                    ^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of x at line 8, characters 10-26.
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
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of x at line 8, characters 10-26.
|}]

(**********************************************************************)
(* Test 11: objects are values.  methods may take/return other sorts. *)

(* CR layouts v5: bring the void versions back here. *)
module M11_1 = struct
  type ('a : void) t = { x : int; v : 'a }

  let f t =
    t.v # baz11
end;;
[%%expect{|
Line 2, characters 13-17:
2 |   type ('a : void) t = { x : int; v : 'a }
                 ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

module M11_1f = struct
  type ('a : float64) t = 'a

  let f (x : 'a t) =
    x # baz11
end;;
[%%expect{|
Line 5, characters 4-5:
5 |     x # baz11
        ^
Error: Object types must have layout value.
       The layout of the type of this expression is float64, because
         of the definition of t at line 2, characters 2-28.
       But the layout of the type of this expression must overlap with value, because
         it's the type of an object.
|}]

module M11_2f = struct
  type ('a : float64) t = 'a
  let f_id (x : 'a t) = x
  let foo x = f_id (x # getfloat)
end;;
[%%expect{|
Line 4, characters 19-33:
4 |   let foo x = f_id (x # getfloat)
                       ^^^^^^^^^^^^^^
Error: This expression has type ('a : value)
       but an expression was expected of type 'b t = ('b : float64)
       The layout of 'a t is float64, because
         of the definition of f_id at line 3, characters 11-25.
       But the layout of 'a t must overlap with value, because
         it's the type of an object field.
|}];;

module M11_3f = struct
  type ('a : float64) t = 'a

  let foo o (x : 'a t) = o # usefloat x
end;;
[%%expect{|
module M11_3f :
  sig
    type ('a : float64) t = 'a
    val foo : ('a : float64) 'b. < usefloat : 'a t -> 'b; .. > -> 'a t -> 'b
  end
|}];;

module M11_4f = struct
  val x : < l : t_float64 >
end;;
[%%expect{|
Line 2, characters 12-25:
2 |   val x : < l : t_float64 >
                ^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of an object field.
|}];;

module M11_5f = struct
  type 'a t = < l : 'a s >
  and ('a : float64) s = 'a
end;;
[%%expect{|
Line 3, characters 2-27:
3 |   and ('a : float64) s = 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'a s is float64, because
         of the annotation on 'a in the declaration of the type s.
       But the layout of 'a s must be a sublayout of value, because
         it's the type of an object field.
|}];;

module M11_6f = struct
  type 'a t = < l : 'a > constraint 'a = t_float64
end;;
[%%expect{|
Line 2, characters 36-50:
2 |   type 'a t = < l : 'a > constraint 'a = t_float64
                                        ^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type ('a : value) is not compatible with type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of an object field.
|}];;

(*******************************************************************)
(* Test 12: class parameters and bound vars must have jkind value *)

(* CR layouts v5: Bring the void versions back here. *)

(* Hits `Pcl_let` *)
module M12_1f = struct
  let f : ('a : float64) . unit -> 'a = fun () -> assert false
  class foo12 u =
    let d = f u in
    object
      val bar = ()
    end;;
end
[%%expect{|
Line 4, characters 8-9:
4 |     let d = f u in
            ^
Error: The types of variables bound by a 'let' in a class function
       must have layout value. Instead, d's type has layout float64.
|}];;

(* Hits the Cfk_concrete case of Pcf_val *)
module M12_2f = struct
  let f : ('a : float64) . unit -> 'a = fun () -> assert false
  class foo u =
    object
      val bar = f u
    end
end;;
[%%expect{|
Line 5, characters 10-13:
5 |       val bar = f u
              ^^^
Error: Variables bound in a class must have layout value.
       The layout of bar is float64, because
         of the definition of f at line 2, characters 6-7.
       But the layout of bar must overlap with value, because
         it's the type of a class field.
|}];;

(* Hits the Cfk_virtual case of Pcf_val *)
module M12_3f = struct
  class virtual foo =
    object
      val virtual bar : t_float64
    end
end;;
[%%expect{|
Line 4, characters 18-21:
4 |       val virtual bar : t_float64
                      ^^^
Error: Variables bound in a class must have layout value.
       The layout of bar is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of bar must be a sublayout of value, because
         it's the type of a class field.
|}];;

module M12_4f = struct
  type ('a : float64) t

  class virtual ['a] foo =
    object
      val virtual baz : 'a t
    end
end
[%%expect{|
Line 6, characters 24-26:
6 |       val virtual baz : 'a t
                            ^^
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value, because
         it's a type argument to a class constructor.
       But the layout of 'a must overlap with float64, because
         of the definition of t at line 2, characters 2-23.
|}];;

module M12_5f = struct
  type ('a : float64) t = 'a

  class ['a] foo =
    object
      method void_id (a : 'a t) : 'a t = a
    end
end;;
[%%expect{|
Line 6, characters 26-28:
6 |       method void_id (a : 'a t) : 'a t = a
                              ^^
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value, because
         it's a type argument to a class constructor.
       But the layout of 'a must overlap with float64, because
         of the definition of t at line 2, characters 2-28.
|}];;

module type S12_6f = sig
  type ('a : float64) t = 'a

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
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value, because
         it's a type argument to a class constructor.
       But the layout of 'a must overlap with float64, because
         of the definition of t at line 2, characters 2-28.
|}];;

module type S12_7f = sig
  class foo :
    object
      val baz : t_float64
    end
end;;
[%%expect{|
Line 4, characters 6-25:
4 |       val baz : t_float64
          ^^^^^^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       The layout of baz is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of baz must be a sublayout of value, because
         it's the type of an instance variable.
|}];;

(*************************************************************************)
(* Test 13: built-in type constructors and support for non-value layouts *)

(* CR layouts v5: Bring the void versions over from basics_alpha *)

(* lazy *)
type t13f = t_float64 Lazy.t;;
[%%expect{|
Line 1, characters 12-21:
1 | type t13f = t_float64 Lazy.t;;
                ^^^^^^^^^
Error: This type t_float64 should be an instance of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of Lazy.t has this layout.
|}];;

let x13f (v : t_float64) = lazy v;;
[%%expect{|
Line 1, characters 32-33:
1 | let x13f (v : t_float64) = lazy v;;
                                    ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a lazy expression.
|}];;

let f_id (x : t_float64) = x
let x13f v =
  match v with
  | lazy v -> f_id v
[%%expect{|
val f_id : t_float64 -> t_float64 = <fun>
Line 4, characters 19-20:
4 |   | lazy v -> f_id v
                       ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a lazy expression.
|}];;

(* option *)
(* CR layouts v5: allow this *)
type t13f = t_float64 option;;
[%%expect{|
Line 1, characters 12-21:
1 | type t13f = t_float64 option;;
                ^^^^^^^^^
Error: This type t_float64 should be an instance of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of option has layout value.
|}];;

let x13f (v : t_float64) = Some v;;
[%%expect{|
Line 1, characters 32-33:
1 | let x13f (v : t_float64) = Some v;;
                                    ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of option has layout value.
|}];;

let x13f v =
  match v with
  | Some v -> f_id v
  | None -> assert false
[%%expect{|
Line 3, characters 19-20:
3 |   | Some v -> f_id v
                       ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of option has layout value.
|}];;

(* list *)
type t13f = t_float64 list;;
[%%expect{|
Line 1, characters 12-21:
1 | type t13f = t_float64 list;;
                ^^^^^^^^^
Error: This type t_float64 should be an instance of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

let x13 (v : t_float64) = [v];;
[%%expect{|
Line 1, characters 27-28:
1 | let x13 (v : t_float64) = [v];;
                               ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

let x13 v =
  match v with
  | [v] -> f_id v
  | _ -> assert false
[%%expect{|
Line 3, characters 16-17:
3 |   | [v] -> f_id v
                    ^
Error: This expression has type ('a : value)
       but an expression was expected of type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

(* array *)
type t13f = t_float64 array;;
[%%expect{|
type t13f = t_float64 array
|}];;

let x13f (v : t_float64) = [| v |];;
[%%expect{|
val x13f : t_float64 -> t_float64 array = <fun>
|}];;

let x13f v =
  match v with
  | [| v |] -> f_id v
  | _ -> assert false
[%%expect{|
val x13f : t_float64 array -> t_float64 = <fun>
|}];;

(****************************************************************************)
(* Test 14: Examples motivating the trick with the manifest in [enter_type] *)

type t14 = foo14 list
and foo14 = string;;
[%%expect{|
type t14 = foo14 list
and foo14 = string
|}];;

(* CR layouts v5: Bring back void version from basics_alpha. *)

type t14 = foo14 list
and foo14 = t_float64;;
[%%expect{|
Line 2, characters 0-21:
2 | and foo14 = t_float64;;
    ^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of foo14 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of foo14 must be a sublayout of value, because
         the type argument of list has layout value.
|}];;

(****************************************************)
(* Test 15: Type aliases need not have jkind value *)

(* CR layouts v5: Bring back void version from basics_alpha. *)

type ('a : float64) t15
type ('a, 'b) foo15 = ('a as 'b) t15 -> 'b t15;;
[%%expect{|
type ('a : float64) t15
type ('a : float64, 'b) foo15 = 'a t15 -> 'a t15 constraint 'b = 'a
|}]


(********************************************************)
(* Test 16: seperability: [msig_of_external_type] logic *)

(* CR layouts v5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort in a variant.  Bring back here when we have one. *)

type 'a t_void_16 : void;;
[%%expect{|
Line 1, characters 20-24:
1 | type 'a t_void_16 : void;;
                        ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
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

(* CR layouts v5: bring void version here from layouts_alpha *)

let f19f () =
  let x : t_float64 = assert false in
  let _y = (x :> t_float64) in
  ();;
[%%expect{|
val f19f : unit -> unit = <fun>
|}];;

(********************************************)
(* Test 20: Non-value bodies for let module *)

(* CR layouts v5: bring void version here from layouts_alpha *)

let f20f () =
  let x : t_float64 = assert false in
  let _y =
    let module M = struct end in
    x
  in
  ();;
[%%expect{|
val f20f : unit -> unit = <fun>
|}];;

(**********************************)
(* Test 21: Non-value unpack body *)
module type M21 = sig end

(* CR layouts v5: bring void version here from layouts_alpha *)

let f21f () =
  let x : t_float64 = assert false in
  let _y =
    let (module M) = (module struct end : M21) in
    x
  in
  ();;
[%%expect{|
module type M21 = sig end
val f21f : unit -> unit = <fun>
|}];;

(***************************************************************)
(* Test 22: approx_type catch-all can't be restricted to value *)

(* CR layouts v5: bring void version here from layouts_alpha *)

type ('a : float64) t22f = 'a

let f () =
  let rec g x : _ t22f = g x in
  g (assert false);;
[%%expect{|
type ('a : float64) t22f = 'a
val f : ('a : float64). unit -> 'a t22f t22f = <fun>
|}];;


(********************************************************************)
(* Test 23: checking the error message from impossible GADT matches *)

(* CR layouts v5: bring void version here from layouts_alpha *)

type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq

module Mf : sig
  type t_float64 : float64
  type t_imm : immediate
end = struct
  type t_float64 : float64
  type t_imm : immediate
end
(* these are abstract, so the only trouble with unifying them in a GADT
   match is around their layouts *)

let f (x : (Mf.t_float64, Mf.t_imm) eq) =
  match x with
  | Refl -> ()

[%%expect{|
type (_ : any, _ : any) eq = Refl : ('a : any). ('a, 'a) eq
module Mf : sig type t_float64 : float64 type t_imm : immediate end
Line 15, characters 4-8:
15 |   | Refl -> ()
         ^^^^
Error: This pattern matches values of type (Mf.t_float64, Mf.t_float64) eq
       but a pattern was expected which matches values of type
         (Mf.t_float64, Mf.t_imm) eq
       The layout of Mf.t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 2-26.
       But the layout of Mf.t_float64 must overlap with immediate, because
         of the definition of t_imm at line 5, characters 2-24.
|}]

(*****************************************************)
(* Test 24: Polymorphic parameter with exotic layout *)

(* CR layouts v5: bring void version here from layouts_alpha *)
type 'a t2_float : float64

let f (x : 'a. 'a t2_float) = x

[%%expect{|
type 'a t2_float : float64
val f : ('a. 'a t2_float) -> 'b t2_float = <fun>
|}]

(**************************************************)
(* Test 25: Optional parameter with exotic layout *)

(* CR layouts v5: bring void version here from layouts_alpha *)

let f (x : t_float64) =
  let g ?(x2 = x) () = () in
  ()

[%%expect{|
Line 2, characters 15-16:
2 |   let g ?(x2 = x) () = () in
                   ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         the type argument of option has layout value.
|}]

(*********************************************************)
(* Test 26: Inferring an application to an exotic layout *)

(* CR layouts v5: bring void version here from layouts_alpha *)

let g f (x : t_float64) : t_float64 = f x

[%%expect{|
val g : (t_float64 -> t_float64) -> t_float64 -> t_float64 = <fun>
|}]

(******************************************)
(* Test 27: Exotic layouts in approx_type *)

(* CR layouts v5: bring void version here from layouts_alpha *)

let rec f : _ -> _ = fun (x : t_float64) -> x

[%%expect{|
val f : t_float64 -> t_float64 = <fun>
|}]

(************************************)
(* Test 28: Exotic layouts in letop *)

(* CR layouts v5: bring void version here from layouts_alpha *)

(* 28.1: non-value letop arg *)
let ( let* ) (x : t_float64) f = ()

let q () =
  let* x = assert false in
  ()

[%%expect{|
val ( let* ) : t_float64 -> 'a -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.2: non-value letop binder arg without and *)
let ( let* ) x (f : t_float64 -> _) = ()

let q () =
  let* x = assert false in
  ()

[%%expect{|
val ( let* ) : 'a ('b : any). 'a -> (t_float64 -> 'b) -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.3: non-value letop binder result *)
let ( let* ) x (f : _ -> t_float64) = ()

let q () =
  let* x = assert false in
  assert false

[%%expect{|
val ( let* ) : 'a ('b : any). 'a -> ('b -> t_float64) -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.4: non-value letop result *)
let ( let* ) x f : t_float64 = assert false

let q () =
  let* x = 5 in
  ()

[%%expect{|
val ( let* ) : 'a -> 'b -> t_float64 = <fun>
val q : unit -> t_float64 = <fun>
|}]

(* 28.5: non-value andop second arg *)
let ( let* ) x f = ()
let ( and* ) x1 (x2 : t_float64) = ()
let q () =
    let* x = 5
    and* y = assert false
    in
    ()

[%%expect{|
val ( let* ) : 'a -> 'b -> unit = <fun>
val ( and* ) : 'a -> t_float64 -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.6: non-value andop first arg *)
let ( let* ) x f = ()
let ( and* ) (x1 : t_float64) x2 = ()
let q () =
    let* x = assert false
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : 'a -> 'b -> unit = <fun>
val ( and* ) : t_float64 -> 'a -> unit = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.7: non-value andop result *)
let ( let* ) (x : (_ : float64))  f = ()
let ( and* ) x1 x2 : t_float64 = assert false
let q () =
    let* x = 5
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : ('a : float64) 'b. 'a -> 'b -> unit = <fun>
val ( and* ) : 'a -> 'b -> t_float64 = <fun>
val q : unit -> unit = <fun>
|}]

(* 28.8: non-value letop binder arg with and *)
let ( let* ) x f = ()
let ( and* ) x1 x2 = assert false
let q () =
    let* x : t_float64 = assert false
    and* y = 5
    in
    ()

[%%expect{|
val ( let* ) : 'a -> 'b -> unit = <fun>
val ( and* ) : 'a -> 'b -> 'c = <fun>
Line 4, characters 9-22:
4 |     let* x : t_float64 = assert false
             ^^^^^^^^^^^^^
Error: This pattern matches values of type t_float64
       but a pattern was expected which matches values of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}]

(*******************************************)
(* Test 29: [external]s default to [value] *)

(* CR layouts v5: bring void version here from layouts_alpha *)

external eq : 'a -> 'a -> bool = "%equal"
let mk_float64 () : t_float64 = assert false
let x () = eq (mk_float64 ()) (mk_float64 ())

[%%expect{|
external eq : 'a -> 'a -> bool = "%equal"
val mk_float64 : unit -> t_float64 = <fun>
Line 3, characters 14-29:
3 | let x () = eq (mk_float64 ()) (mk_float64 ())
                  ^^^^^^^^^^^^^^^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of eq at line 1, characters 0-41.
|}]

(**************************************)
(* Test 30: [val]s default to [value] *)

(* CR layouts v5: bring void version here from layouts_alpha *)

module M : sig
  val f : 'a -> 'a
end = struct
  let f x = x
end

let g (x : t_float64) = M.f x

[%%expect{|
module M : sig val f : 'a -> 'a end
Line 7, characters 28-29:
7 | let g (x : t_float64) = M.f x
                                ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of f at line 2, characters 2-18.
|}]

(**************************************************)
(* Test 31: checking that #poly_var patterns work *)

(* CR layouts v5: bring void version here from layouts_alpha *)

type ('a : float64) poly_var = [`A of int * 'a | `B]

let f #poly_var = "hello"

[%%expect{|
Line 1, characters 44-46:
1 | type ('a : float64) poly_var = [`A of int * 'a | `B]
                                                ^^
Error: This type ('a : value) should be an instance of type ('a0 : float64)
       The layout of 'a is float64, because
         of the annotation on 'a in the declaration of the type poly_var.
       But the layout of 'a must overlap with value, because
         it's the type of a tuple element.
|}]

(*********************************************************)
(* Test 32: Polymorphic variant constructors take values *)

(* CR layouts v5: bring void version here from layouts_alpha *)

let f _ = `Mk (assert false : t_float64)

[%%expect{|
Line 1, characters 14-40:
1 | let f _ = `Mk (assert false : t_float64)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}]

(******************************************************)
(* Test 33: Externals must have representable types *)

external foo33 : t_any = "foo33";;

[%%expect{|
Line 1, characters 17-22:
1 | external foo33 : t_any = "foo33";;
                     ^^^^^
Error: This type signature for foo33 is not a value type.
       The layout of type t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of type t_any must be a sublayout of value, because
         it's the type of something stored in a module structure.
|}]

external foo44 : ('a : any). 'a -> unit = "foo44";;

[%%expect{|
Line 1, characters 29-31:
1 | external foo44 : ('a : any). 'a -> unit = "foo44";;
                                 ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be representable, because
         it's the type of an argument in an external declaration.
|}]

external foo55 : ('a : any). unit -> 'a = "foo55";;

[%%expect{|
Line 1, characters 37-39:
1 | external foo55 : ('a : any). unit -> 'a = "foo55";;
                                         ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be representable, because
         it's the type of the result of an external declaration.
|}]

(****************************************************)
(* Test 34: Layout clash in polymorphic record type *)

type ('a : immediate) t2_imm

type s = { f : ('a : value) . 'a -> 'a u }
and 'a u = 'a t2_imm

[%%expect {|
type ('a : immediate) t2_imm
Line 3, characters 15-40:
3 | type s = { f : ('a : value) . 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is value, because
           of the annotation on the universal variable 'a.
         But the layout of 'a must be a sublayout of immediate, because
           of the definition of t2_imm at line 1, characters 0-28.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

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
       The layout of 'a -> 'b is value, because
         it's a function type.
       But the layout of 'a -> 'b must be a sublayout of immediate, because
         of the definition of t35 at line 1, characters 0-30.
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
         ('a : '_representable_layout_6)
       because it is in the left-hand side of a sequence
       The layout of t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of t_any must be representable, because
         it's the type of a statement.
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
         ('a : '_representable_layout_7)
       because it is in the body of a while-loop
       The layout of t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of t_any must be representable, because
         it's the type of a statement.
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
         ('a : '_representable_layout_8)
       because it is in the body of a for-loop
       The layout of t_any is any, because
         of the definition of t_any at line 5, characters 0-18.
       But the layout of t_any must be representable, because
         it's the type of a statement.
|}]

(******************************************************)
(* Test 37: Ensure signature inclusion checks layouts *)

module M1 : sig
  val f : ('a : any). 'a -> 'a
end = struct
  let f x = x
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : any). 'a -> 'a end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : any). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The layout of 'a is any, because
         of the definition of f at line 2, characters 2-30.
       But the layout of 'a must be representable, because
         of the definition of f at line 4, characters 8-13.
|}]

module M1 : sig
  val f : unit -> 'a -> 'a
end = struct
  let rec f : type (a : any). unit -> a -> a = fun _ -> f ()
end

[%%expect{|
module M1 : sig val f : unit -> 'a -> 'a end
|}]

module type S_any = sig
  val f : ('a : any). 'a -> 'a
end

module type S_value = sig
  val f : 'a -> 'a
end

module type S_float64 = sig
  val f : ('a : float64). 'a -> 'a
end

[%%expect{|
module type S_any = sig val f : ('a : any). 'a -> 'a end
module type S_value = sig val f : 'a -> 'a end
module type S_float64 = sig val f : ('a : float64). 'a -> 'a end
|}]

module F (X : S_any) : S_value = X

[%%expect{|
module F : functor (X : S_any) -> S_value
|}]

module F (X : S_value) : S_any = X

[%%expect{|
Line 1, characters 33-34:
1 | module F (X : S_value) : S_any = X
                                     ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         S_any
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : any). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The layout of 'a is any, because
         of the definition of f at line 2, characters 2-30.
       But the layout of 'a must be a sublayout of value, because
         of the definition of f at line 6, characters 2-18.
|}]

module F (X : S_value) : S_float64 = X

[%%expect{|
Line 1, characters 37-38:
1 | module F (X : S_value) : S_float64 = X
                                         ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         S_float64
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : float64). 'a -> 'a
       The type 'a -> 'a is not compatible with the type 'b -> 'b
       The layout of 'a is float64, because
         of the definition of f at line 10, characters 2-34.
       But the layout of 'a must be a sublayout of value, because
         of the definition of f at line 6, characters 2-18.
|}]

module M2 : sig
  type ('a : any) t = 'a
end = struct
  type ('a : value) t = 'a
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type ('a : any) t = 'a end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type ('a : any) t = 'a
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}]

module M3 : sig
  type ('a : any) t = 'a -> 'a
end = struct
  type 'a t = 'a -> 'a
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = 'a -> 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a -> 'a end
       is not included in
         sig type ('a : any) t = 'a -> 'a end
       Type declarations do not match:
         type 'a t = 'a -> 'a
       is not included in
         type ('a : any) t = 'a -> 'a
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}]

module M4 : sig
  type t = { f : ('a : any). 'a -> 'a }
end = struct
  type t = { f : 'a. 'a -> 'a }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : 'a. 'a -> 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : 'a. 'a -> 'a; } end
       is not included in
         sig type t = { f : ('a : any). 'a -> 'a; } end
       Type declarations do not match:
         type t = { f : 'a. 'a -> 'a; }
       is not included in
         type t = { f : ('a : any). 'a -> 'a; }
       Fields do not match:
         f : 'a. 'a -> 'a;
       is not the same as:
         f : ('a : any). 'a -> 'a;
       The type 'a. 'a -> 'a is not equal to the type ('a : any). 'a -> 'a
       Type 'a is not equal to type 'a0
|}]

module M5 : sig
  type t = { f : 'a. 'a -> 'a }
end = struct
  type t = { f : ('a : any). 'a -> 'a }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : ('a : any). 'a -> 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : ('a : any). 'a -> 'a; } end
       is not included in
         sig type t = { f : 'a. 'a -> 'a; } end
       Type declarations do not match:
         type t = { f : ('a : any). 'a -> 'a; }
       is not included in
         type t = { f : 'a. 'a -> 'a; }
       Fields do not match:
         f : ('a : any). 'a -> 'a;
       is not the same as:
         f : 'a. 'a -> 'a;
       The type ('a : any). 'a -> 'a is not equal to the type 'a. 'a -> 'a
       Type 'a is not equal to type 'a0
|}]

module M6 : sig
  val f : ('a. 'a -> unit) -> unit
end = struct
  let f (g : ('a : any). 'a -> unit) =
    ignore (g (Stdlib_upstream_compatible.Float_u.of_float 3.14)); ignore (g "hello"); ignore (g 5); ()
end

[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let f (g : ('a : any). 'a -> unit) =
5 |     ignore (g (Stdlib_upstream_compatible.Float_u.of_float 3.14)); ignore (g "hello"); ignore (g 5); ()
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : (('a : any). 'a -> unit) -> unit end
       is not included in
         sig val f : ('a. 'a -> unit) -> unit end
       Values do not match:
         val f : (('a : any). 'a -> unit) -> unit
       is not included in
         val f : ('a. 'a -> unit) -> unit
       The type (('a : any). 'a -> unit) -> unit
       is not compatible with the type ('a. 'a -> unit) -> unit
       Type 'a is not compatible with type 'a0
|}]

module M7 : sig
  val f : (('a : any). 'a -> 'a) -> unit
end = struct
  let f (g : 'a. 'a -> 'a) =
    ignore (g "hello"); ()
end

[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let f (g : 'a. 'a -> 'a) =
5 |     ignore (g "hello"); ()
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a. 'a -> 'a) -> unit end
       is not included in
         sig val f : (('a : any). 'a -> 'a) -> unit end
       Values do not match:
         val f : ('a. 'a -> 'a) -> unit
       is not included in
         val f : (('a : any). 'a -> 'a) -> unit
       The type ('a. 'a -> 'a) -> unit is not compatible with the type
         (('a : any). 'a -> 'a) -> unit
       Type 'a is not compatible with type 'a0
|}]

module M8 : sig
  type ('a : any) t = K of ('a -> 'a)
end = struct
  type 'a t = K of ('a -> 'a)
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = K of ('a -> 'a)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = K of ('a -> 'a) end
       is not included in
         sig type ('a : any) t = K of ('a -> 'a) end
       Type declarations do not match:
         type 'a t = K of ('a -> 'a)
       is not included in
         type ('a : any) t = K of ('a -> 'a)
       Their parameters differ:
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}]

module M9 : sig
  type 'a t = K of ('a -> 'a)
end = struct
  type ('a : any) t = K of ('a -> 'a)
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : any) t = K of ('a -> 'a)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : any) t = K of ('a -> 'a) end
       is not included in
         sig type 'a t = K of ('a -> 'a) end
       Type declarations do not match:
         type ('a : any) t = K of ('a -> 'a)
       is not included in
         type 'a t = K of ('a -> 'a)
       Their parameters differ:
       The type ('a : any) is not equal to the type ('a0 : value)
       because their layouts are different.
|}]
(* CR layouts: This one should be fine to accept *)

(*****************************************************)
(* Test 38: Ensure Univar unification checks layouts *)

let poly : ('a. 'a -> 'a) -> int * bool =
  fun (id : ('a : immediate). 'a -> 'a) -> id 3, id true

[%%expect{|
Line 2, characters 7-38:
2 |   fun (id : ('a : immediate). 'a -> 'a) -> id 3, id true
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type ('a : immediate). 'a -> 'a
       but a pattern was expected which matches values of type 'a. 'a -> 'a
       Type 'a is not compatible with type 'a0
|}]
(* CR layouts: This one should be fine to accept *)

type ('a : any) foo = 'a
type ('a : any) bar

let f (x : < foo : ('a : float64) . 'a foo bar >)
  : < foo : 'a . 'a foo bar > = x

[%%expect{|
type ('a : any) foo = 'a
type ('a : any) bar
Line 5, characters 32-33:
5 |   : < foo : 'a . 'a foo bar > = x
                                    ^
Error: This expression has type < foo : ('a : float64). 'a foo bar >
       but an expression was expected of type < foo : 'a. 'a foo bar >
       Type 'a foo = 'a is not compatible with type 'a0 foo = 'a0
       Types for method foo are incompatible
|}]

(*************************************************************)
(* Test 39: Inference of functions that don't bind arguments *)

let rec f () : 'a -> 'a = f ()

[%%expect{|
val f : ('a : any). unit -> 'a -> 'a = <fun>
|}]

module M = struct
  let rec f () : 'a -> 'a = f ()
end

[%%expect{|
module M : sig val f : ('a : any). unit -> 'a -> 'a end
|}]

let rec f () : 'a -> 'a = f ()
let g : ('a : any). unit -> 'a -> 'a = f

[%%expect{|
val f : ('a : any). unit -> 'a -> 'a = <fun>
val g : ('a : any). unit -> 'a -> 'a = <fun>
|}]

module M : sig
  val f : ('a : any). unit -> 'a -> 'a
end = struct
  let rec f () : 'a -> 'a = f ()
end

[%%expect{|
module M : sig val f : ('a : any). unit -> 'a -> 'a end
|}]

let rec f : ('a : any). unit -> 'a -> 'a = fun () -> f ()

[%%expect{|
val f : ('a : any). unit -> 'a -> 'a = <fun>
|}]

(****************************************************************)
(* Test 40: unannotated type parameter defaults to layout value *)

type 'a t40 = 'a
let f40 (x: t_float64): 'a t40 = x

[%%expect{|
type 'a t40 = 'a
Line 2, characters 33-34:
2 | let f40 (x: t_float64): 'a t40 = x
                                     ^
Error: This expression has type t_float64
       but an expression was expected of type 'a t40 = ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 4, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of t40 at line 1, characters 0-16.
|}]

(**********************************************************************)
(* Test 41: constraints in manifests in mutually recursive typedecls. *)

(* This example must be rejected. *)
type t1 = string t2 as (_ : immediate)
and 'a t2 = 'a

[%%expect{|
Line 2, characters 0-14:
2 | and 'a t2 = 'a
    ^^^^^^^^^^^^^^
Error:
       The layout of 'a t2 is value, because
         it instantiates an unannotated type parameter of t2, defaulted to layout value.
       But the layout of 'a t2 must be a sublayout of immediate, because
         of the annotation on the wildcard _ at line 1, characters 28-37.
|}]

(* This example is unfortunately rejected as a consequence of the fix for the
   above in typedecl. If we ever change that so that the below starts working,
   make sure [t1]'s parameter is immediate! Previously this was allowed and t1's
   parameter was just value (a bug). *)
type 'a t1 = 'a t2 as (_ : immediate)
and 'a t2 = 'a

[%%expect{|
Line 2, characters 0-14:
2 | and 'a t2 = 'a
    ^^^^^^^^^^^^^^
Error:
       The layout of 'a t2 is '_representable_layout_9, because
         it instantiates an unannotated type parameter of t2.
       But the layout of 'a t2 must be a sublayout of immediate, because
         of the annotation on the wildcard _ at line 1, characters 27-36.
|}]

(* This one also unfortunately rejected for the same reason. *)
type t1 = int t2 as (_ : immediate)
and 'a t2 = 'a

[%%expect{|
Line 2, characters 0-14:
2 | and 'a t2 = 'a
    ^^^^^^^^^^^^^^
Error:
       The layout of 'a t2 is value, because
         it instantiates an unannotated type parameter of t2, defaulted to layout value.
       But the layout of 'a t2 must be a sublayout of immediate, because
         of the annotation on the wildcard _ at line 1, characters 25-34.
|}]

(**********************************************************************)
(* Test 42: Externals for built-in primitives have some safety checks *)

(* oops the argument/return got swapped *)
external f : float# -> float = "%unbox_float";;
[%%expect{|
Line 1, characters 13-28:
1 | external f : float# -> float = "%unbox_float";;
                 ^^^^^^^^^^^^^^^
Error: The primitive [%unbox_float] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* using the wrong primitive *)
external f : ('a : bits64). 'a -> int64 = "%box_int32";;
[%%expect{|
Line 1, characters 13-39:
1 | external f : ('a : bits64). 'a -> int64 = "%box_int32";;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%box_int32] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* can't use primitives for jkind conversions *)
external f : float# -> int32# = "%identity";;
[%%expect{|
Line 1, characters 13-29:
1 | external f : float# -> int32# = "%identity";;
                 ^^^^^^^^^^^^^^^^
Error: The primitive [%identity] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external f : float# -> int32# = "%opaque";;
[%%expect{|
Line 1, characters 13-29:
1 | external f : float# -> int32# = "%opaque";;
                 ^^^^^^^^^^^^^^^^
Error: The primitive [%opaque] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external f : float# -> int32# = "%obj_magic";;
[%%expect{|
Line 1, characters 13-29:
1 | external f : float# -> int32# = "%obj_magic";;
                 ^^^^^^^^^^^^^^^^
Error: The primitive [%obj_magic] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

(* not smart enough to stop this
   but the middle end should error in this case *)
external f : (float# -> int32#) -> int32# -> int32# = "%apply";;
[%%expect{|
external f : (float# -> int32#) -> int32# -> int32# = "%apply"
|}]

external f : float# -> int -> int = "%send";;
[%%expect{|
Line 1, characters 13-33:
1 | external f : float# -> int -> int = "%send";;
                 ^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%send] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external f : int -> int -> float# = "%sendself";;
[%%expect{|
Line 1, characters 13-33:
1 | external f : int -> int -> float# = "%sendself";;
                 ^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%sendself] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external f : int -> float# -> int -> int -> int = "%sendcache";;
[%%expect{|
Line 1, characters 13-47:
1 | external f : int -> float# -> int -> int -> int = "%sendcache";;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%sendcache] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
