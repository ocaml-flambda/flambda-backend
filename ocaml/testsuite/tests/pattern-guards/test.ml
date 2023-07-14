(* TEST
   * expect *)

(* CR-soon rgodse: We expect this output to change soon, but for now it shows
   that parsing and typechecking work (as the compiler fails at translation). *)

let basic_usage ~f ~default x =
  match x with 
    | Some x when f x match Some y -> y
    | _ -> default
;;

let f x = if x > 0 then Some (x - 1) else None;;

basic_usage ~f ~default:~-1 (Some 5);;
basic_usage ~f ~default:~-1 (Some 0);;
basic_usage ~f ~default:~-1 (Some ~-5);;
basic_usage ~f ~default:~-1 None;;
[%%expect{|
val basic_usage : f:('a -> 'b option) -> default:'b -> 'a option -> 'b =
  <fun>
val f : int -> int option = <fun>
- : int = 4
- : int = -1
- : int = -1
- : int = -1
|}]
;;

let seq_predicate x ~f ~g ~default =
  match x with
    | Some x when f x; g x -> x
    | _ -> default
;;
[%%expect{|
val seq_predicate :
  'a option -> f:('a -> 'b) -> g:('a -> bool) -> default:'a -> 'a = <fun>
|}]

let seq_pattern x ~f ~g ~default =
  match x with
    | Some x when (f x; g x) match Some y -> y
    | _ -> default

let counter = ref 0;;
let f () = incr counter;;
let g () = if !counter > 1 then Some (!counter - 1) else None;;

seq_pattern (Some ()) ~f ~g ~default:0;;
seq_pattern (Some ()) ~f ~g ~default:0;;
seq_pattern None ~f ~g ~default:0;;
[%%expect{|
val seq_pattern :
  'a option -> f:('a -> 'b) -> g:('a -> 'c option) -> default:'c -> 'c =
  <fun>
val counter : int ref = {contents = 0}
val f : unit -> unit = <fun>
val g : unit -> int option = <fun>
- : int = 0
- : int = 1
- : int = 0
|}]

let complex_types (x : int list option) : bool =
  let strs_opt = match x with
    | Some [ y ] when y + 1 > 5 -> Some ("foo", "bar")
    | Some ys when List.length ys match 0 -> Some ("baz", "qux")
    | Some _ | None -> None
  in
  match strs_opt with
    | Some strs when strs match ("foo", s2) -> String.equal s2 "bar"
    | Some _ | None -> false
[%%expect {|
val complex_types : int list option -> bool = <fun>
|}]

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
|}]

let shadow_outer_variables (n : int option) (strs : string list) =
  match n with
  | Some n when List.nth_opt strs n match Some s -> s
  | _ -> "Not found"
;;

shadow_outer_variables (Some 0) [ "foo"; "bar" ];;
shadow_outer_variables (Some 1) [ "foo"; "bar" ];;
shadow_outer_variables (Some 2) [ "foo"; "bar" ];;
[%%expect{|
val shadow_outer_variables : int option -> string list -> string = <fun>
- : string = "foo"
- : string = "bar"
- : string = "Not found"
|}]

type ('a, 'b) eq = Eq : ('a, 'a) eq

let in_pattern_guard (type a b) (eq : (a, b) eq) (compare : a -> a -> int)
                     (x : a) (y : b) =
  match eq with
    | Eq when compare x y match 0 -> true
    | _ -> false
[%%expect{|
type ('a, 'b) eq = Eq : ('a, 'a) eq
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

let exhaustive_pattern_guards (x : (unit, void option) Either.t) : int =
  match x with
    | Left u when u match () -> 0
    | Right v when v match None -> 1
    | _ -> 2
;;

exhaustive_pattern_guards (Left ());;
exhaustive_pattern_guards (Right None);;
[%%expect{|
type void = |
Line 144, characters 13-28:
144 |     | Left u when u match () -> 0
                   ^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
Line 145, characters 14-31:
145 |     | Right v when v match None -> 1
                    ^^^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
val exhaustive_pattern_guards : (unit, void option) Either.t -> int = <fun>
- : int = 0
- : int = 1
|}]

module M = Map.Make (Int);;

let say_hello (id : int option) (name_map : string M.t) =
  match id with
    | Some id when M.find_opt id name_map match Some name -> "Hello, " ^ name
    | None | Some _ -> "Hello, stranger"
;;

let name_map = M.empty |> M.add 0 "Fred" |> M.add 4 "Barney";;
say_hello (Some 0) name_map;;
say_hello (Some 2) name_map;;
say_hello (Some 4) name_map;;
say_hello None name_map;;
[%%expect{|
module M :
  sig
    type key = Int.t
    type 'a t = 'a Map.Make(Int).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
val say_hello : int option -> string M.t -> string = <fun>
val name_map : string M.t = <abstr>
- : string = "Hello, Fred"
- : string = "Hello, stranger"
- : string = "Hello, Barney"
- : string = "Hello, stranger"
|}]

let say_hello_catching_exns id name_map =
  match id with
    | Some id when M.find id name_map match "Barney" | exception _ ->
        "Hello, Barney"
    | None | Some _ -> "Hello, Fred"
;;
say_hello_catching_exns (Some 0) name_map;;
say_hello_catching_exns (Some 2) name_map;;
say_hello_catching_exns (Some 4) name_map;;
say_hello_catching_exns None name_map;;
[%%expect{|
val say_hello_catching_exns : M.key option -> string M.t -> string = <fun>
- : string = "Hello, Fred"
- : string = "Hello, Barney"
- : string = "Hello, Barney"
- : string = "Hello, Fred"
|}]
