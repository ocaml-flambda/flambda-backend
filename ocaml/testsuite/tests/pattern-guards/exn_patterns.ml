(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Test interaction of pattern guards with exception patterns. *)

(* Test behavior of pattern guards that only match exception patterns. *)

let no_value_clauses f x =
  match x with
  | Some x when f x match exception e -> Error e
  | Some x -> Ok (f x)
  | None -> Error (Failure "x is None")
;;
[%%expect{|
val no_value_clauses : ('a -> 'b) -> 'a option -> ('b, exn) result = <fun>
|}];;

let f x = 100 / x;;
[%%expect{|
val f : int -> int = <fun>
|}];;

no_value_clauses f None;;
[%%expect{|
- : (int, exn) result = Error (Failure "x is None")
|}];;
no_value_clauses f (Some 0);;
[%%expect{|
- : (int, exn) result = Error Division_by_zero
|}];;
no_value_clauses f (Some 5);;
[%%expect{|
- : (int, exn) result = Ok 20
|}];;

(* Test behavior of pattern guards matching empty variants with exception
   patterns. *)

type void = |;;
[%%expect{|
type void = |
|}];;

let prove_false () : void = failwith "qed";;
[%%expect{|
val prove_false : unit -> void = <fun>
|}];;

let guard_matching_empty_variant = function
  | None when prove_false () match exception (Failure str) -> "failed: " ^ str
  | None -> "proved false!"
  | Some x -> x
;;
[%%expect{|
val guard_matching_empty_variant : string option -> string = <fun>
|}];;

guard_matching_empty_variant None;;
[%%expect{|
- : string = "failed: qed"
|}];;
guard_matching_empty_variant (Some "foo");;
[%%expect{|
- : string = "foo"
|}];;

(* Correctness test for pattern guards with mixed value and exception
   patterns. *)

module M : sig
  type 'a t

  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
end = Map.Make (Int);;
[%%expect{|
module M :
  sig
    type 'a t
    val empty : 'a t
    val add : int -> 'a -> 'a t -> 'a t
    val find : int -> 'a t -> 'a
  end
|}];;

let name_map = M.empty |> M.add 0 "Fred" |> M.add 4 "Barney";;
[%%expect{|
val name_map : string M.t = <abstr>
|}];;


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
|}];;

