(* TEST
 flags = "-extension-universe alpha";
 include stdlib_alpha;
 expect;
*)

module type Or_null = sig
  type ('a : non_null_value) t = 'a or_null =
    | Null
    | This of 'a

  (* CR layouts v3.0: implement those functions. *)

  (* val none : 'a or_null
  val some : 'a -> 'a or_null
  val value : 'a or_null -> default:'a -> 'a
  val get : 'a or_null -> 'a
  val bind : 'a or_null -> ('a -> 'b or_null) -> 'b or_null
  (* unlike [option] we cannot have [join] *)
  val map : ('a -> 'b) -> 'a or_null -> 'b or_null
  val fold : none:'a -> some:('b -> 'a) -> 'b or_null -> 'a
  val iter : ('a -> unit) -> 'a or_null -> unit

  val is_none : 'a or_null -> bool
  val is_some : 'a or_null -> bool
  val equal : ('a -> 'a -> bool) -> 'a or_null -> 'a or_null -> bool
  val compare : ('a -> 'a -> int) -> 'a or_null -> 'a or_null -> int

  val to_result : none:'e -> 'a or_null -> ('a, 'e) result
  val to_list : 'a or_null -> 'a list
  val to_seq : 'a or_null -> 'a Seq.t

  val to_option : 'a or_null -> 'a option
  val of_option : 'a option -> 'a or_null *)
end

module Or_null : Or_null = Stdlib_alpha.Or_null

(* CR layouts (v3): check output to see how bad the pretty-printing is.
   In particular, it would be nice to suppress layout annotations that
   are implied by the rest of the signature, but this may be hard. *)
[%%expect {|
module type Or_null =
  sig type ('a : non_null_value) t = 'a or_null = Null | This of 'a end
module Or_null : Or_null
|}]

(* CR layouts v3.0: ensure that immediacy "looks through" or_null.
   Currently, [immediate] is always non-null, so we can't test this. *)
type t1 : immediate = int or_null
type t2 : immediate = bool or_null

[%%expect {|
Line 1, characters 0-33:
1 | type t1 : immediate = int or_null
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type int or_null is value, because
         it is the primitive value type or_null.
       But the layout of type int or_null must be a sublayout of immediate, because
         of the definition of t1 at line 1, characters 0-33.
|}]

type t : immediate = string or_null

[%%expect {|
Line 1, characters 0-35:
1 | type t : immediate = string or_null
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string or_null is value, because
         it is the primitive value type or_null.
       But the layout of type string or_null must be a sublayout of immediate, because
         of the definition of t at line 1, characters 0-35.
|}]

type t : value = string or_null

[%%expect {|
type t = string or_null
|}]

(* ensure that or_null can't be repeated *)
type 'a t = 'a or_null or_null

[%%expect {|
Line 1, characters 12-22:
1 | type 'a t = 'a or_null or_null
                ^^^^^^^^^^
Error: This type 'a or_null should be an instance of type
         ('b : non_null_value)
       The layout of 'a or_null is value, because
         it is the primitive value type or_null.
       But the layout of 'a or_null must be a sublayout of non_null_value, because
         the type argument of option has layout non_null_value.
|}]

(* check inference around or_null *)
type 'a t = 'a or_null
type ('a : immediate) t = 'a or_null

[%%expect {|
type ('a : non_null_value) t = 'a or_null
type ('a : immediate) t = 'a or_null
|}]

(* more jkind checking *)
type t : non_null_value = string or_null

[%%expect {|
Line 1, characters 0-40:
1 | type t : non_null_value = string or_null
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type string or_null is value, because
         it is the primitive value type or_null.
       But the layout of type string or_null must be a sublayout of non_null_value, because
         of the definition of t at line 1, characters 0-40.
|}]

(* CR layouts v3.0: implement [immediate_or_null] *)

type t1 : non_null_value = string
type t2 : non_null_value = int
type t3 : immediate = int
type t4 : immediate_or_null = int or_null

[%%expect {|
type t1 = string
type t2 = int
type t3 = int
Line 4, characters 10-27:
4 | type t4 : immediate_or_null = int or_null
              ^^^^^^^^^^^^^^^^^
Error: Unknown layout immediate_or_null
|}]

(* magic looking-through of [or_null] can't be abstracted over *)
type 'a t = 'a or_null
type q1 : value = string t
type q2 : immediate_or_null = int t  (* but t isn't abstract, so this is OK *)

[%%expect {|
type ('a : non_null_value) t = 'a or_null
type q1 = string t
Line 3, characters 10-27:
3 | type q2 : immediate_or_null = int t  (* but t isn't abstract, so this is OK *)
              ^^^^^^^^^^^^^^^^^
Error: Unknown layout immediate_or_null
|}]

type q = string t t

[%%expect {|
Line 1, characters 9-17:
1 | type q = string t t
             ^^^^^^^^
Error: This type string t = string or_null should be an instance of type
         ('a : non_null_value)
       The layout of string t is value, because
         it is the primitive value type or_null.
       But the layout of string t must be a sublayout of non_null_value, because
         of the definition of t at line 1, characters 0-22.
|}]

type q = int t t

[%%expect {|
Line 1, characters 9-14:
1 | type q = int t t
             ^^^^^
Error: This type int t = int or_null should be an instance of type
         ('a : non_null_value)
       The layout of int t is value, because
         it is the primitive value type or_null.
       But the layout of int t must be a sublayout of non_null_value, because
         of the definition of t at line 1, characters 0-22.
|}]

(* CR layouts v2.8: Make [or_null] kind polymorphic, so this is accepted. *)

type 'a q1 = 'a t
type ('a : immediate) q2 : immediate_or_null = 'a t

[%%expect {|
type ('a : non_null_value) q1 = 'a t
Line 2, characters 27-44:
2 | type ('a : immediate) q2 : immediate_or_null = 'a t
                               ^^^^^^^^^^^^^^^^^
Error: Unknown layout immediate_or_null
|}]

(* CR layouts v3.0: default to [non_null_value] for abstract types *)
module type T = sig
  type t
end

[%%expect {|
module type T = sig type t end
|}]

(* this should be rejected, because the default for [t] is [non_null_value] *)
module M : T = struct
  type t = string or_null
end

[%%expect {|
module M : T
|}]

module M : T = struct
  type t = int or_null
end

[%%expect {|
module M : T
|}]

module M : sig
  type 'a t
end = struct
  type 'a t = 'a or_null
end

(* CR layouts (v3): This error message had better be excellent, because the
   solution -- to add a [: value] annotation -- will be unusual. Normally,
   people think of [value] as the default! *)
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = 'a or_null
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : non_null_value) t = 'a or_null end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type ('a : non_null_value) t = 'a or_null
       is not included in
         type 'a t
       Their parameters differ:
       The type ('a : non_null_value) is not equal to the type ('a0 : value)
       because their layouts are different.
|}]

(* CR layouts v3.0: ['a] in signature should default to [non_null_value] *)

module M : sig
  type 'a t : value
end = struct
  type 'a t = 'a or_null
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = 'a or_null
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : non_null_value) t = 'a or_null end
       is not included in
         sig type 'a t : value end
       Type declarations do not match:
         type ('a : non_null_value) t = 'a or_null
       is not included in
         type 'a t : value
       Their parameters differ:
       The type ('a : non_null_value) is not equal to the type ('a0 : value)
       because their layouts are different.
|}]

module M : sig
  type ('a : non_null_value) t : value
end = struct
  type 'a t = 'a or_null
end

[%%expect {|
module M : sig type ('a : non_null_value) t : value end
|}]

type t = string M.t

[%%expect {|
type t = string M.t
|}]

type t = int M.t

[%%expect {|
type t = int M.t
|}]

type ('a : immediate) id_imm = 'a

type t = (int M.t) id_imm  (* this is the one that requires "looking through" *)

[%%expect {|
type ('a : immediate) id_imm = 'a
Line 3, characters 10-17:
3 | type t = (int M.t) id_imm  (* this is the one that requires "looking through" *)
              ^^^^^^^
Error: This type int M.t should be an instance of type ('a : immediate)
       The layout of int M.t is value, because
         of the definition of t at line 2, characters 2-38.
       But the layout of int M.t must be a sublayout of immediate, because
         of the definition of id_imm at line 1, characters 0-33.
|}]

(* CR layouts v3: [float or_null] should compile: *)

type t = float or_null
;;

[%%expect {|
Line 1, characters 9-14:
1 | type t = float or_null
             ^^^^^
Error: This type float should be an instance of type ('a : non_null_value)
       The layout of float is value, because
         it is the primitive value type float.
       But the layout of float must be a sublayout of non_null_value, because
         the type argument of option has layout non_null_value.
|}]

(* CR layouts v3: [float or_null array] should not compile,
   but for a different reason: *)

type t = float or_null array
;;

[%%expect {|
Line 1, characters 9-14:
1 | type t = float or_null array
             ^^^^^
Error: This type float should be an instance of type ('a : non_null_value)
       The layout of float is value, because
         it is the primitive value type float.
       But the layout of float must be a sublayout of non_null_value, because
         the type argument of option has layout non_null_value.
|}]

(* CR layouts v3.0: implement features below. *)

(*

(* tests that or_null actually works at runtime *)

let x = match Or_null.some 5 with
  | None -> 6
  | Some n -> n

let x = match Or_null.Some 5 with
  | None -> 6
  | Some n -> n

let x = match Or_null.some "hello" with
  | None -> "bad"
  | Some s -> s

let x = match Or_null.Some "hello" with
  | None -> "bad"
  | Some s -> s

let x = match Or_null.none with
  | None -> 6
  | Some s -> s

let x = match Or_null.None with
  | None -> 6
  | Some s -> s

let x = match Or_null.none with
  | None -> "good"
  | Some s -> s

let x = match Or_null.None with
  | None -> "good"
  | Some s -> s

[%%expect {|
5
5
"hello"
"hello"
6
6
"good"
"good"
|}]

let b = Or_null.some 0 = Obj.magic 0

(* this should work because they're immediate, though it's technically unspecified *)
let b = Or_null.some 0 == Obj.magic 0

let b = (Or_null.none : int or_null) = Obj.magic 0

let b = (Or_null.none : string or_null) = Obj.magic 0

let b = (Or_null.none : int or_null) = Obj.magic (Or_null.none : string or_null)

[%%expect {|
true
true
false
false
true
|}]

(* CR layouts (v3): make other reference-implementation tests for the
   [Or_null] interface once we have the quickcheck-like architecture
   (TANDC-1809). *)

(* check allocation behavior *)

let measure_alloc f =
  (* NB: right-to-left evaluation order gets this right *)
  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let result = (f[@inlined never]) () in
  let after = Gc.allocated_bytes () in
  (after -. before) -. baseline_allocation, result

[%%expect {|
success
|}]

let alloc = measure_alloc (fun () -> let x = Or_null.some 5 in ())
let alloc = measure_alloc (fun () -> let x = Or_null.Some 5 in ())
let alloc =
  measure_alloc (fun () ->
    (* this should infer f to be local, and thus the closures at usage
       sites won't allocate *)
    let bind opt f = Or_null.(match opt with
      None -> None
      Some x -> f x
    ) in
    let x = Or_null.some 5 in
    let y = Or_null.some 6 in
    let f a b = bind x (fun x -> bind y Or_null.(fun y -> some (x + y))) in
    f x y)

[%%expect {|
0
0
0
|}]

(* sub-typing *)

let f x = (x : int :> int or_null)
let f x = (x : string :> string or_null)
let f x = (x : int list :> int or_null list)
let f x = (x : string list :> string or_null list)
let f x = (x : int list :> int list or_null)
let f x = (x : string list :> string list or_null)

[%%expect {|
success
|}]

let f x = (x : int or_null :> int)

[%%expect {|
error
|}]

let f x = (x : string or_null :> string)

[%%expect {|
error
|}]

let f x = (x : int :> int or_null or_null)

[%%expect {|
error
|}]

let f x = (x : int :> string or_null)

[%%expect {|
error
|}]

*)
