(* TEST
   * expect *)

(* CR-soon rgodse: We expect this output to change soon, but for now it shows
   that parsing works for multicase patterns. *)

(* Test basic usage of pattern guards. *)
let basic_usage ~f ~default x =
  match x with
    | Some x when f x match Some y -> y
    | _ -> default
;;
[%%expect{|
val basic_usage : f:('a -> 'b option) -> default:'b -> 'a option -> 'b =
  <fun>
|}];;

let f x = if x > 0 then Some (x - 1) else None;;
[%%expect{|
val f : int -> int option = <fun>
|}];;

basic_usage ~f ~default:~-1 (Some 5);;
[%%expect{|
- : int = 4
|}];;
basic_usage ~f ~default:~-1 (Some 0);;
[%%expect{|
- : int = -1
|}];;
basic_usage ~f ~default:~-1 (Some ~-5);;
[%%expect{|
- : int = -1
|}];;
basic_usage ~f ~default:~-1 None;;
[%%expect{|
- : int = -1
|}];;

(* Demonstrate parsing of sequences in boolean predicates. *)
let seq_predicate x ~f ~g ~default =
  match x with
    | Some x when f x; g x -> x
    | _ -> default
;;
[%%expect{|
val seq_predicate :
  'a option -> f:('a -> 'b) -> g:('a -> bool) -> default:'a -> 'a = <fun>
|}];;

(* Demonstrate semantics of sequences in pattern guard scrutinees. *)
let seq_pattern x ~f ~g ~default =
  match x with
    | Some x when (f x; g x) match Some y -> y
    | _ -> default
;;
[%%expect{|
val seq_pattern :
  'a option -> f:('a -> 'b) -> g:('a -> 'c option) -> default:'c -> 'c =
  <fun>
|}];;

let counter = ref 0;;
[%%expect{|
val counter : int ref = {contents = 0}
|}];;
let f () = incr counter;;
[%%expect{|
val f : unit -> unit = <fun>
|}];;
let g () = if !counter > 1 then Some (!counter - 1) else None;;
[%%expect{|
val g : unit -> int option = <fun>
|}];;

seq_pattern (Some ()) ~f ~g ~default:0;;
[%%expect{|
- : int = 0
|}];;
seq_pattern (Some ()) ~f ~g ~default:0;;
[%%expect{|
- : int = 1
|}];;
seq_pattern None ~f ~g ~default:0;;
[%%expect{|
- : int = 0
|}];;

let complex_types (x : int list option) : bool =
  let strs_opt = match x with
    | Some [ y ] when y + 1 > 5 -> Some ("foo", "bar")
    | Some ys when List.length ys match 0 -> Some ("baz", "qux")
    | Some _ | None -> None
  in
  match strs_opt with
    | Some strs when strs match ("foo", s2) -> String.equal s2 "bar"
    | Some _ | None -> false
;;
[%%expect {|
val complex_types : int list option -> bool = <fun>
|}];;

(* Check typing of pattern guard patterns. *)
let ill_typed_pattern (x : int list option) : bool =
  match x with
    | Some [ y ] when y match None -> true
    | _ -> false
[%%expect{|
Line 3, characters 30-34:
3 |     | Some [ y ] when y match None -> true
                                  ^^^^
Error: This pattern matches values of type 'a option
       but a pattern was expected which matches values of type int
|}]

let typing_no_value_clauses f x =
  match x with
    | Some x when f x match exception e -> Error e
    | Some x -> Ok (f x)
    | None -> Error (Failure "x is None")
;;

let f x = 100 / x;;
[%%expect{|
val typing_no_value_clauses : ('a -> 'b) -> 'a option -> ('b, exn) result =
  <fun>
val f : int -> int = <fun>
|}];;

typing_no_value_clauses f None;;
[%%expect{|
- : (int, exn) result = Error (Failure "x is None")
|}];;
typing_no_value_clauses f (Some 0);;
[%%expect{|
- : (int, exn) result = Error Division_by_zero
|}];;
typing_no_value_clauses f (Some 5);;
[%%expect{|
- : (int, exn) result = Ok 20
|}];;

(* Check typing of pattern guards with no value cases. *)
let typing_no_value_clauses f x =
  match x with
    | Some x when f x match exception e -> Error e
    | Some x -> Ok (f x)
    | None -> Error (Failure "x is None")
;;

let f x = 100 / x;;
[%%expect{|
val typing_no_value_clauses : ('a -> 'b) -> 'a option -> ('b, exn) result =
  <fun>
val f : int -> int = <fun>
|}];;

typing_no_value_clauses f None;;
[%%expect{|
- : (int, exn) result = Error (Failure "x is None")
|}];;
typing_no_value_clauses f (Some 0);;
[%%expect{|
- : (int, exn) result = Error Division_by_zero
|}];;
typing_no_value_clauses f (Some 5);;
[%%expect{|
- : (int, exn) result = Ok 20
|}];;

(* Check typing of vars bound in pattern guard patterns. *)
let ill_typed_pattern_var (x : int list option) : bool =
  match x with
    | Some xs when xs match [ y ] -> String.equal y "foo"
    | _ -> false
[%%expect{|
Line 3, characters 50-51:
3 |     | Some xs when xs match [ y ] -> String.equal y "foo"
                                                      ^
Error: This expression has type int but an expression was expected of type
         String.t = string
|}];;

(* Check that pattern guards can shadow and use outer variables correctly. *)
let shadow_outer_variables (n : int option) (strs : string list) =
  match n with
  | Some n when List.nth_opt strs n match Some s -> s
  | _ -> "Not found"
;;
[%%expect{|
val shadow_outer_variables : int option -> string list -> string = <fun>
|}];;

shadow_outer_variables (Some 0) [ "foo"; "bar" ];;
[%%expect{|
- : string = "foo"
|}];;
shadow_outer_variables (Some 1) [ "foo"; "bar" ];;
[%%expect{|
- : string = "bar"
|}];;
shadow_outer_variables (Some 2) [ "foo"; "bar" ];;
[%%expect{|
- : string = "Not found"
|}]

(* Test inward propagation of GADT type information. *)

type ('a, 'b) eq = Eq : ('a, 'a) eq
[%%expect{|
type ('a, 'b) eq = Eq : ('a, 'a) eq
|}];;

let in_pattern_guard (type a b) (eq : (a, b) eq) (compare : a -> a -> int)
                     (x : a) (y : b) =
  match eq with
    | Eq when compare x y match 0 -> true
    | _ -> false
[%%expect{|
val in_pattern_guard : ('a, 'b) eq -> ('a -> 'a -> int) -> 'a -> 'b -> bool =
  <fun>
|}]

let from_pattern_guard (type a b) (eqs : (a, b) eq option list)
                       (compare : a -> a -> int) (x : a) (y : b) =
  match eqs with
    | eq_opt :: _ when eq_opt match Some Eq -> compare x y
    | _ -> 0
[%%expect{|
val from_pattern_guard :
  ('a, 'b) eq option list -> ('a -> 'a -> int) -> 'a -> 'b -> int = <fun>
|}]

type void = |
[%%expect{|
type void = |
|}];;

(* Ensure that warning 73 is appropriately issued. *)
let exhaustive_pattern_guards (x : (unit, void option) Either.t) : int =
  match x with
    | Left u when u match () -> 0
    | Right v when v match None -> 1
    | _ -> 2
;;
[%%expect{|
Line 3, characters 13-33:
3 |     | Left u when u match () -> 0
                 ^^^^^^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
Line 4, characters 14-36:
4 |     | Right v when v match None -> 1
                  ^^^^^^^^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
val exhaustive_pattern_guards : (unit, void option) Either.t -> int = <fun>
|}];;

exhaustive_pattern_guards (Left ());;
[%%expect{|
- : int = 0
|}];;
exhaustive_pattern_guards (Right None);;
[%%expect{|
- : int = 1
|}];;

let prove_false () : void = failwith "qed";;

let guard_matching_empty_variant = function
  | None when prove_false () match exception (Failure str) -> "failed: " ^ str
  | None -> "proved false!"
  | Some x -> x
;;
[%%expect{|
val prove_false : unit -> void = <fun>
val guard_matching_empty_variant : string option -> string = <fun>
|}];;

guard_matching_empty_variant None;;
[%%expect{|
- : string = "failed: qed"
|}];;
guard_matching_empty_variant (Some "foo");;
[%%expect{|
- : string = "foo"
|}]

(* Test rejection of pattern guards on mixed exception/value or-patterns *)
let reject_guarded_val_exn_orp k =
  match k () with
  | Some s | exception Failure s when s match "foo" -> s
  | _ -> "Not foo"
;;
[%%expect{|
Line 3, characters 4-32:
3 |   | Some s | exception Failure s when s match "foo" -> s
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Mixing value and exception patterns under when-guards is not supported.
|}];;

(* Test rejection of pattern guards on mixed exception/value or-patterns *)
let reject_guarded_val_exn_orp k =
  match k () with
  | Some s | exception Failure s when s match "foo" -> s
  | _ -> "Not foo"
;;
[%%expect{|
Line 3, characters 4-32:
3 |   | Some s | exception Failure s when s match "foo" -> s
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Mixing value and exception patterns under when-guards is not supported.
|}];;

module M : sig
  type 'a t

  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
  val find_opt : int -> 'a t -> 'a option
end = Map.Make (Int);;
[%%expect{|
module M :
  sig
    type 'a t
    val empty : 'a t
    val add : int -> 'a -> 'a t -> 'a t
    val find : int -> 'a t -> 'a
    val find_opt : int -> 'a t -> 'a option
  end
|}];;

(* Correctness test for pattern guard matching and failure. *)
let say_hello (id : int option) (name_map : string M.t) =
  match id with
    | Some id when M.find_opt id name_map match Some name -> "Hello, " ^ name
    | None | Some _ -> "Hello, stranger"
;;
[%%expect{|
val say_hello : int option -> string M.t -> string = <fun>
|}];;

let name_map = M.empty |> M.add 0 "Fred" |> M.add 4 "Barney";;
[%%expect{|
val name_map : string M.t = <abstr>
|}];;

say_hello (Some 0) name_map;;
[%%expect{|
- : string = "Hello, Fred"
|}];;
say_hello (Some 2) name_map;;
[%%expect{|
- : string = "Hello, stranger"
|}];;
say_hello (Some 4) name_map;;
[%%expect{|
- : string = "Hello, Barney"
|}];;
say_hello None name_map;;
[%%expect{|
- : string = "Hello, stranger"
|}]

(* Correctness test for pattern guards with mixed value and exception
   patterns. *)
let say_hello_catching_exns id name_map =
  match id with
    | Some id when M.find id name_map match "Barney" | exception _ ->
        "Hello, Barney"
    | None | Some _ -> "Hello, Fred"
;;
[%%expect{|
val say_hello_catching_exns : int option -> string M.t -> string = <fun>
|}];;

say_hello_catching_exns (Some 0) name_map;;
[%%expect{|
- : string = "Hello, Fred"
|}];;
say_hello_catching_exns (Some 2) name_map;;
[%%expect{|
- : string = "Hello, Barney"
|}];;
say_hello_catching_exns (Some 4) name_map;;
[%%expect{|
- : string = "Hello, Barney"
|}];;
say_hello_catching_exns None name_map;;
[%%expect{|
- : string = "Hello, Fred"
|}]

(* Ensure that Match_failure is raised when all cases, including pattern-guarded
   ones, fail to match. *)
let patch_to_match_failure f default x =
  match x with
  | None -> default
  | Some x when f x match Some y -> y
;;
[%%expect{|
Lines 2-4, characters 2-37:
2 | ..match x with
3 |   | None -> default
4 |   | Some x when f x match Some y -> y
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some _
(However, some guarded clause may match this value.)
val patch_to_match_failure : ('a -> 'b option) -> 'b -> 'a option -> 'b =
  <fun>
|}];;

let exact_half n = if n mod 2 = 0 then Some (n / 2) else None;;
[%%expect{|
val exact_half : int -> int option = <fun>
|}];;

patch_to_match_failure exact_half ~-1 None;;
[%%expect{|
- : int = -1
|}];;
patch_to_match_failure exact_half ~-1 (Some 42);;
[%%expect{|
- : int = 21
|}];;
patch_to_match_failure exact_half ~-1 (Some 41);;
[%%expect{|
Exception: Match_failure ("", 2, 2).
|}];;

(* Ensure that warning 8 is issued appropriately in the presence of pattern
   guards. *)
let warn_partial = function
  | [] -> 0
  | xs when List.hd xs match Some y -> y
;;
[%%expect{|
Lines 1-3, characters 19-40:
1 | ...................function
2 |   | [] -> 0
3 |   | xs when List.hd xs match Some y -> y
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
(However, some guarded clause may match this value.)
val warn_partial : int option list -> int = <fun>
|}];;

(* Ensure that warning 57 is appropriately issued for pattern guards. *)
let warn_ambiguous = function
  | ([ x ], _) | (_, [ x ]) when (let one = 1 in Int.abs x + one) match 2 -> 1
  | _ -> 0
;;
[%%expect{|
Line 2, characters 4-27:
2 |   | ([ x ], _) | (_, [ x ]) when (let one = 1 in Int.abs x + one) match 2 -> 1
        ^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 11.5)
val warn_ambiguous : int list * int list -> int = <fun>
|}];;

(* Ensure that warning 57 is not extraneously issued for pattern guards. *)
let dont_warn_ambiguous = function
  | ([ x ], _) | (_, [ x ]) when (let one = 1 in one + one) match 2 -> x
  | _ -> 0
;;
[%%expect{|
val dont_warn_ambiguous : int list * int list -> int = <fun>
|}]

let single_bar_syntax x =
  match x with
  | Some x when x match | Some y -> y
  | _ -> 0
;;
[%%expect{|
val single_bar_syntax : int option option -> int = <fun>
|}];;

single_bar_syntax (Some (Some 5));;
[%%expect{|
- : int = 5
|}];;
single_bar_syntax (Some None);;
[%%expect{|
- : int = 0
|}];;
single_bar_syntax None;;
[%%expect{|
- : int = 0
|}];;

single_bar_syntax (Some (Some 5));;
[%%expect{|
- : int = 5
|}];;
single_bar_syntax (Some None);;
[%%expect{|
- : int = 0
|}];;
single_bar_syntax None;;
[%%expect{|
- : int = 0
|}];;

let nested_singleway f g h ~default = function
  | Some x
      when f x match Some y
      when g y match Some z
      when h z match Some a ->
      a
  | _ -> default
;;

let collatz = function
  | 1 -> None
  | n -> if n mod 2 = 0 then Some (n / 2) else Some (3 * n + 1)
;;

[%%expect{|
val nested_singleway :
  ('a -> 'b option) ->
  ('b -> 'c option) -> ('c -> 'd option) -> default:'d -> 'a option -> 'd =
  <fun>
val collatz : int -> int option = <fun>
|}];;

nested_singleway collatz collatz collatz ~default:~-1 None;;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 1);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 2);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 3);;
[%%expect{|
- : int = 16
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 4);;
[%%expect{|
- : int = -1
|}];;
nested_singleway collatz collatz collatz ~default:~-1 (Some 8);;
[%%expect{|
- : int = 1
|}];;

let find_multiway ~eq ~flag ~finish ~default = function
  | x :: xs when List.find_opt (fun y -> eq flag y || eq x y) xs match (
      | Some y when eq flag y -> finish flag
      | Some y -> finish y
    )
  | _ -> default
;;
[%%expect{|
val find_multiway :
  eq:('a -> 'a -> bool) ->
  flag:'a -> finish:('a -> 'b) -> default:'b -> 'a list -> 'b = <fun>
|}];;

let eq n m = (n - m) mod 100 = 0;;
let flag = 0;;
let finish n = Int.to_string n;;
let default = "No match found";;
[%%expect{|
val eq : int -> int -> bool = <fun>
val flag : int = 0
val finish : int -> string = <fun>
val default : string = "No match found"
|}];;

find_multiway ~eq ~flag ~finish ~default [ 10; 20; 110; 100 ];;
[%%expect{|
- : string = "110"
|}];;
find_multiway ~eq ~flag ~finish ~default [ 10; 20; 100; 110 ];;
[%%expect{|
- : string = "0"
|}];;
find_multiway ~eq ~flag ~finish ~default [ 10; 20; 30; 40 ];;
[%%expect{|
- : string = "No match found"
|}];;
find_multiway ~eq ~flag ~finish ~default [ 0; 100 ];;
[%%expect{|
- : string = "0"
|}];;

let nested_multiway f g h = function
  | Some x when f x match (
      | "foo" when h x -> "foo1"
      | "bar" when g x match (
          | [] -> "bar empty"
          | [ y ] -> "bar singleton " ^ y
        )
    )
  | _ -> "not found"
;;
[%%expect{|
val nested_multiway :
  ('a -> string) ->
  ('a -> string list) -> ('a -> bool) -> 'a option -> string = <fun>
|}];;

let f = function
  | 0 | 1 -> "foo"
  | 10 | 100 | 1000 -> "bar"
  | _ -> "neither"
;;

let g = function
  | 10 -> []
  | 100 -> [ "one" ]
  | _ -> [ "more"; "than"; "one" ]
;;

let h x = x = 1;;
[%%expect{|
val f : int -> string = <fun>
val g : int -> string list = <fun>
val h : int -> bool = <fun>
|}];;

nested_multiway f g h None;;
[%%expect{|
- : string = "not found"
|}];;
nested_multiway f g h (Some 0);;
[%%expect{|
- : string = "not found"
|}];;
nested_multiway f g h (Some 1);;
[%%expect{|
- : string = "foo1"
|}];;
nested_multiway f g h (Some 10);;
[%%expect{|
- : string = "bar empty"
|}];;
nested_multiway f g h (Some 100);;
[%%expect{|
- : string = "bar singleton one"
|}];;
nested_multiway f g h (Some 1000);;
[%%expect{|
- : string = "not found"
|}];;

(* Checks that optional arguments with defaults are correclty bound in the
   presence of pattern guards. *)
let check_push_defaults g ?(s="hello") = function
  | x when g s match Some t -> t ^ ", " ^ x
  | x -> x
;;
[%%expect{|
val check_push_defaults :
  (string -> string option) -> ?s:string -> string -> string = <fun>
|}];;

let g = function
  | "hello" -> Some "jello"
  | "ha" -> Some "ja"
  | _ -> None
;;
[%%expect{|
val g : string -> string option = <fun>
|}];;

check_push_defaults g "name";;
[%%expect{|
- : string = "jello, name"
|}];;
check_push_defaults g ~s:"ha" "name";;
[%%expect{|
- : string = "ja, name"
|}];;
check_push_defaults g ~s:"hmm" "name";;
[%%expect{|
- : string = "name"
|}];;
