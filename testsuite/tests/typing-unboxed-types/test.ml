(* TEST
 flags = "-extension layouts_beta";
 expect;
*)
(* CR layouts: Using [-extension layouts_beta] here is not backward-compatible.
   We can delete this when internal ticket 1110 is resolved.
*)

(* Check the unboxing *)

(* For concrete types *)
type t1 = A of string [@@ocaml.unboxed];;
[%%expect{|
type t1 = A of string [@@unboxed]
|}];;

let x = A "foo" in
Obj.repr x == Obj.repr (match x with A s -> s)
;;
[%%expect{|
- : bool = true
|}];;

(* For records *)
type t2 = { f : string } [@@ocaml.unboxed];;
[%%expect{|
type t2 = { f : string; } [@@unboxed]
|}];;

let x = { f = "foo" } in
Obj.repr x == Obj.repr x.f
;;
[%%expect{|
- : bool = true
|}];;

(* For unboxed records *)
type t2 = #{ f : string } ;;
[%%expect{|
type t2 = #{ f : string; }
|}];;

let x = #{ f = "foo" } in
Obj.repr x == Obj.repr x.#f
;;
[%%expect{|
- : bool = true
|}];;

(* For inline records *)
type t3 = B of { g : string } [@@ocaml.unboxed];;
[%%expect{|
type t3 = B of { g : string; } [@@unboxed]
|}];;

let x = B { g = "foo" } in
Obj.repr x == Obj.repr (match x with B {g} -> g)
;;
[%%expect{|
- : bool = true
|}];;

(* Check unboxable types *)
type t4 = C [@@ocaml.unboxed];;  (* no argument *)
[%%expect{|
Line 1, characters 0-29:
1 | type t4 = C [@@ocaml.unboxed];;  (* no argument *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because its constructor has no argument.
|}];;
type t5 = D of int * string [@@ocaml.unboxed];; (* more than one argument *)
[%%expect{|
Line 1, characters 0-45:
1 | type t5 = D of int * string [@@ocaml.unboxed];; (* more than one argument *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       its constructor has more than one argument.
|}];;
type t5 = E | F [@@ocaml.unboxed];;          (* more than one constructor *)
[%%expect{|
Line 1, characters 0-33:
1 | type t5 = E | F [@@ocaml.unboxed];;          (* more than one constructor *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t6 = G of int | H [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-40:
1 | type t6 = G of int | H [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t7 = I of string | J of bool [@@ocaml.unboxed];;

type t8 = { h : bool; i : int } [@@ocaml.unboxed];;  (* more than one field *)
[%%expect{|
Line 1, characters 0-51:
1 | type t7 = I of string | J of bool [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because it has more than one constructor.
|}];;
type t9 = K of { j : string; l : int } [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-56:
1 | type t9 = K of { j : string; l : int } [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       its constructor has more than one field.
|}];;

(* This test was made to error by disallowing singleton recursive unboxed types.
   We keep it in case these are re-allowed, in which case it should error with:
   [This kind of expression is not allowed as right-hand side of "let rec"] *)
type t10 : value = A of t10 [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-45:
1 | type t10 : value = A of t10 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t10" is recursive without boxing:
         "t10" contains "t10"
|}];;
let rec x = A x;;
[%%expect{|
Line 1, characters 14-15:
1 | let rec x = A x;;
                  ^
Error: This expression has type "t1" but an expression was expected of type
         "string"
|}];;

(* Representation mismatch between module and signature must be rejected *)
module M : sig
  type t = A of string
end = struct
  type t = A of string [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of string [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of string [@@unboxed] end
       is not included in
         sig type t = A of string end
       Type declarations do not match:
         type t = A of string [@@unboxed]
       is not included in
         type t = A of string
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module M' : sig
  type t = A of string [@ocaml.unboxed]
end = struct
  type t = A of string [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of string [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of string [@@unboxed] end
       is not included in
         sig type t = A of string end
       Type declarations do not match:
         type t = A of string [@@unboxed]
       is not included in
         type t = A of string
       Their internal representations differ:
       the first declaration uses unboxed representation.
       Hint: the second declaration has [@unboxed]. Did you mean [@@unboxed]?
|}];;

module N : sig
  type t = A of string [@@ocaml.unboxed]
end = struct
  type t = A of string
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of string
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of string end
       is not included in
         sig type t = A of string [@@unboxed] end
       Type declarations do not match:
         type t = A of string
       is not included in
         type t = A of string [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;

module O : sig
  type t = { f : string }
end = struct
  type t = { f : string } [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : string } [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : string; } [@@unboxed] end
       is not included in
         sig type t = { f : string; } end
       Type declarations do not match:
         type t = { f : string; } [@@unboxed]
       is not included in
         type t = { f : string; }
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module P : sig
  type t = { f : string } [@@ocaml.unboxed]
end = struct
  type t = { f : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { f : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { f : string; } end
       is not included in
         sig type t = { f : string; } [@@unboxed] end
       Type declarations do not match:
         type t = { f : string; }
       is not included in
         type t = { f : string; } [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;

module Q : sig
  type t = A of { f : string }
end = struct
  type t = A of { f : string } [@@ocaml.unboxed]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of { f : string } [@@ocaml.unboxed]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of { f : string; } [@@unboxed] end
       is not included in
         sig type t = A of { f : string; } end
       Type declarations do not match:
         type t = A of { f : string; } [@@unboxed]
       is not included in
         type t = A of { f : string; }
       Their internal representations differ:
       the first declaration uses unboxed representation.
|}];;

module R : sig
  type t = A of { f : string } [@@ocaml.unboxed]
end = struct
  type t = A of { f : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of { f : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of { f : string; } end
       is not included in
         sig type t = A of { f : string; } [@@unboxed] end
       Type declarations do not match:
         type t = A of { f : string; }
       is not included in
         type t = A of { f : string; } [@@unboxed]
       Their internal representations differ:
       the second declaration uses unboxed representation.
|}];;

module M : sig
  type t = { a : string }
end = struct
  type t = #{ a : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ a : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ a : string; } end
       is not included in
         sig type t = { a : string; } end
       Type declarations do not match:
         type t = #{ a : string; }
       is not included in
         type t = { a : string; }
       The first is an unboxed record, but the second is a record.
|}];;

module M : sig
  type t = #{ a : string }
end = struct
  type t = { a : string }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { a : string }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : string; } end
       is not included in
         sig type t = #{ a : string; } end
       Type declarations do not match:
         type t = { a : string; }
       is not included in
         type t = #{ a : string; }
       The first is a record, but the second is an unboxed record.
|}]


(* Check interference with representation of float arrays. *)
type t11 = L of float [@@ocaml.unboxed];;
[%%expect{|
type t11 = L of float [@@unboxed]
|}];;
let x = Array.make 10 (L 3.14)   (* represented as a flat array *)
and f (a : t11 array) = a.(0)    (* might wrongly assume an array of pointers *)
in assert (f x = L 3.14);;
[%%expect{|
- : unit = ()
|}];;

type t11 = #{ f : float };;
[%%expect{|
type t11 = #{ f : float; }
|}];;
let x = Array.make 10 #{ f = 3.14 }   (* represented as a flat array *)
and f (a : t11 array) = a.(0)    (* might wrongly assume an array of pointers *)
in assert (f x = #{ f = 3.14});;
[%%expect{|
- : unit = ()
|}];;

(* Check for a potential infinite loop in the typing algorithm.
   (This test was made to error upon disallowing singleton recursive [@@unboxed]
   types. We keep it around in case these are re-allowed.) *)
type 'a t12 = M of 'a t12 [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 0-43:
1 | type 'a t12 = M of 'a t12 [@@ocaml.unboxed];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t12" is recursive without boxing:
         "'a t12" contains "'a t12"
|}];;
let f (a : int t12 array) = a.(0);;
[%%expect{|
Line 1, characters 15-18:
1 | let f (a : int t12 array) = a.(0);;
                   ^^^
Error: Unbound type constructor "t12"
Hint: Did you mean "t1", "t11" or "t2"?
|}];;

type 'a t12 : value = #{ a : 'a t12 };;
[%%expect{|
Line 1, characters 0-37:
1 | type 'a t12 : value = #{ a : 'a t12 };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t12" is recursive without boxing:
         "'a t12" contains "'a t12"
|}];;
let f (a : int t12 array) = a.(0);;
[%%expect{|
Line 1, characters 15-18:
1 | let f (a : int t12 array) = a.(0);;
                   ^^^
Error: Unbound type constructor "t12"
Hint: Did you mean "t1", "t11" or "t2"?
|}];;

(* Check for another possible loop *)
type t13 = A : _ t12 -> t13 [@@ocaml.unboxed];;
[%%expect{|
Line 1, characters 17-20:
1 | type t13 = A : _ t12 -> t13 [@@ocaml.unboxed];;
                     ^^^
Error: Unbound type constructor "t12"
Hint: Did you mean "t1", "t11", "t13" or "t2"?
|}];;


(* should work *)
type t14;;
type t15 = A of t14 [@@ocaml.unboxed];;
[%%expect{|
type t14
type t15 = A of t14 [@@unboxed]
|}];;
type t14;;
type t15 = #{ a : t14 };;
[%%expect{|
type t14
type t15 = #{ a : t14; }
|}];;

(* should fail because the compiler knows that t is actually float and
   optimizes the record's representation *)
module S : sig
  type t
  type u = { f1 : t; f2 : t }
end = struct
  type t = A of float [@@ocaml.unboxed]
  type u = { f1 : t; f2 : t }
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = A of float [@@ocaml.unboxed]
6 |   type u = { f1 : t; f2 : t }
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of float [@@unboxed] type u = { f1 : t; f2 : t; } end
       is not included in
         sig type t type u = { f1 : t; f2 : t; } end
       Type declarations do not match:
         type u = { f1 : t; f2 : t; }
       is not included in
         type u = { f1 : t; f2 : t; }
       Their internal representations differ:
       the first declaration uses unboxed float representation.
|}];;

module S : sig
  type t
  type u = { f1 : t; f2 : t }
end = struct
  type t = #{ a : float }
  type u = { f1 : t; f2 : t }
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t = #{ a : float }
6 |   type u = { f1 : t; f2 : t }
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ a : float; } type u = { f1 : t; f2 : t; } end
       is not included in
         sig type t type u = { f1 : t; f2 : t; } end
       Type declarations do not match:
         type u = { f1 : t; f2 : t; }
       is not included in
         type u = { f1 : t; f2 : t; }
       Their internal representations differ:
       the first declaration uses unboxed float representation.
|}];;

(* implementing [@@immediate] with [@@ocaml.unboxed]: this works because the
   representation of [t] is [int]
 *)
module T : sig
  type t [@@immediate]
end = struct
  type t = A of int [@@ocaml.unboxed]
end;;
[%%expect{|
module T : sig type t : immediate end
|}];;

module T : sig
  type t [@@immediate]
end = struct
  type t = #{ i : int }
end;;
[%%expect{|
module T : sig type t : immediate end
|}];;

(* Another corner case *)
type 'a s
type ('a, 'p) t = private 'a s
type 'a packed = T : ('a, _) t -> 'a packed [@@unboxed]
;;
[%%expect{|
type 'a s
type ('a, 'p) t = private 'a s
type 'a packed = T : ('a, 'b) t -> 'a packed [@@unboxed]
|}];;

(* MPR#7682 *)
type f = {field: 'a. 'a list} [@@unboxed];;
let g = Array.make 10 { field=[] };;
let h = g.(5);;
[%%expect{|
type f = { field : 'a. 'a list; } [@@unboxed]
val g : f array =
  [|{field = []}; {field = []}; {field = []}; {field = []}; {field = []};
    {field = []}; {field = []}; {field = []}; {field = []}; {field = []}|]
val h : f = {field = []}
|}];;

type f = #{field: 'a. 'a list} ;;
let g = Array.make 10 #{ field=[] };;
let h = g.(5);;
[%%expect{|
type f = #{ field : 'a. 'a list; }
val g : f array =
  [|#{field = []}; #{field = []}; #{field = []}; #{field = []};
    #{field = []}; #{field = []}; #{field = []}; #{field = []};
    #{field = []}; #{field = []}|]
val h : f = #{field = []}
|}];;

(* Using [@@immediate] information (GPR#1469) *)
type 'a t [@@immediate];;
type u = U : 'a t -> u [@@unboxed];;
[%%expect{|
type 'a t : immediate
type u = U : 'a t -> u [@@unboxed]
|}];;

(* This could not be accepted without using a fixpoint to check unboxed declarations
   (GPR#2188) *)
type ('a, 'b) t = K : 'c -> (bool, 'c) t [@@unboxed]
and t1 = T1 : (bool, int) t -> t1 [@@unboxed]
[%%expect{|
type ('a, 'b) t = K : 'c -> (bool, 'c) t [@@unboxed]
and t1 = T1 : (bool, int) t -> t1 [@@unboxed]
|}];;

(* This real-world example of recursive declaration comes from Markus Mottl
   -- see MPR#7361 *)
type ('a, 'kind) tree =
  | Root : { mutable value : 'a; mutable rank : int } -> ('a, [ `root ]) tree
  | Inner : { mutable parent : 'a node } -> ('a, [ `inner ]) tree
and 'a node = Node : ('a, _) tree -> 'a node [@@ocaml.unboxed]
[%%expect{|
type ('a, 'kind) tree =
    Root : { mutable value : 'a; mutable rank : int;
    } -> ('a, [ `root ]) tree
  | Inner : { mutable parent : 'a node; } -> ('a, [ `inner ]) tree
and 'a node = Node : ('a, 'b) tree -> 'a node [@@unboxed]
|}];;

(*******************************)
(* [@@unboxed] non-value GADTs *)

module Result = struct
  type ('a, 'b) t = Ok of 'a | Error of 'b
end
[%%expect{|
module Result : sig type ('a, 'b) t = Ok of 'a | Error of 'b end
|}]

module Result_u : sig
  type ('a, 'b) t : immediate & value

  val to_result : ('a, 'b) t -> ('a, 'b) Result.t
  val of_result : ('a, 'b) Result.t -> ('a, 'b) t
end = struct
  type ('a, 'b, 'c) tag =
    | Ok : ('a, 'b, 'a) tag
    | Error : ('a, 'b, 'b) tag

  type ('a, 'b) t =
    | T : #(('a, 'b, 'c) tag * 'c) -> ('a, 'b) t [@@unboxed]

  let to_result (type a) (type b) (T #(tag, x) : (a, b) t) : (a, b) Result.t =
    match tag with
    | Ok -> Ok x
    | Error -> Error x

  let of_result = function
    | Result.Ok x -> T #(Ok, x)
    | Result.Error x -> T #(Error, x)
end
[%%expect{|
module Result_u :
  sig
    type ('a, 'b) t : value & value
    val to_result : ('a, 'b) t -> ('a, 'b) Result.t
    val of_result : ('a, 'b) Result.t -> ('a, 'b) t
  end
|}]

module Result_u_VV : sig
  type ('a : value & value, 'b : value & value) t : immediate & (value & value)
  val ok_exn : ('a, 'b) t -> 'a
  val error_exn : ('a, 'b) t -> 'b
  val ok : 'a -> ('a, _) t
  val error : 'b -> (_, 'b) t
end = struct
  type ('a : (value & value), 'b : (value & value), 'c : (value & value)) tag =
    | Ok : ('a, 'b, 'a) tag
    | Error : ('a, 'b, 'b) tag

  type ('a : value & value, 'b : value & value) t =
    | T : #(('a, 'b, 'c) tag * 'c) -> ('a, 'b) t [@@unboxed]

  let ok_exn (type a : value & value) (type b : value & value)
        (T #(tag, x) : (a, b) t) : a =
    match tag with
    | Ok -> x
    | Error -> assert false

  let error_exn (type a : value & value) (type b : value & value)
        (T #(tag, x) : (a, b) t) : b =
    match tag with
    | Ok -> assert false
    | Error -> x

  let ok x = T #(Ok, x)
  let error x = T #(Error, x)
end
[%%expect{|
module Result_u_VV :
  sig
    type ('a : value & value, 'b : value & value) t : value & (value & value)
    val ok_exn : ('a : value & value) ('b : value & value). ('a, 'b) t -> 'a
    val error_exn :
      ('a : value & value) ('b : value & value). ('a, 'b) t -> 'b
    val ok : ('a : value & value) ('b : value & value). 'a -> ('a, 'b) t
    val error : ('b : value & value) ('a : value & value). 'b -> ('a, 'b) t
  end
|}]

(*******************************************************************)
(* Cases where GADT type equalities imply that kinds doesn't match *)

(* 'c cannot be both [value & value] and [float64] *)
type ('a : value & value, 'b : float64, 'c : float64) tag =
  | Ok : ('a, 'b, 'a) tag
  | Error : ('a, 'b, 'b) tag
[%%expect{|
Line 2, characters 18-20:
2 |   | Ok : ('a, 'b, 'a) tag
                      ^^
Error: This type "('a : value & value)" should be an instance of type
         "('b : float64)"
       The layout of 'a is value & value
         because of the annotation on 'a in the declaration of the type tag.
       But the layout of 'a must overlap with float64
         because of the annotation on 'c in the declaration of the type tag.
|}]

(* 'c defaults to [value], 'a and 'b require that it's [value & value] *)
type ('a : value & value, 'b : value & value, 'c) tag =
  | Ok : ('a, 'b, 'a) tag
  | Error : ('a, 'b, 'b) tag
[%%expect{|
Line 2, characters 9-25:
2 |   | Ok : ('a, 'b, 'a) tag
             ^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is value
           because it instantiates an unannotated type parameter of tag,
           chosen to have layout value.
         But the layout of 'a must overlap with value & value
           because of the annotation on 'a in the declaration of the type tag.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

(* Good case, where kinds line up *)
type ('a : value & value, 'b : value & value, 'c : value & value) tag =
  | Ok : ('a, 'b, 'a) tag
  | Error : ('a, 'b, 'b) tag
[%%expect{|
type ('a : value & value, 'b : value & value, 'c : value & value) tag =
    Ok : ('a : value & value) ('b : value & value). ('a, 'b, 'a) tag
  | Error : ('a : value & value) ('b : value & value). ('a, 'b, 'b) tag
|}]
