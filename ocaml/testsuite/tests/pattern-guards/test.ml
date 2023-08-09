(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Typical usage of pattern guards *)

(* Test basic usage of single-case pattern guards. *)

let basic_usage ~f ~default x =
  match x with
    | Some x when f x match
      | Some y -> y
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

(* Correctness test for pattern guard with interesting scrutinee. *)

module M : sig
  type 'a t

  val empty : 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find_opt : int -> 'a t -> 'a option
end = Map.Make (Int);;
[%%expect{|
module M :
  sig
    type 'a t
    val empty : 'a t
    val add : int -> 'a -> 'a t -> 'a t
    val find_opt : int -> 'a t -> 'a option
  end
|}];;

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

(* Test behavior of typical multiway pattern guard. *)
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
[%%expect{|
val eq : int -> int -> bool = <fun>
|}];;
let flag = 0;;
[%%expect{|
val flag : int = 0
|}];;
let finish n = Int.to_string n;;
[%%expect{|
val finish : int -> string = <fun>
|}];;
let default = "No match found";;
[%%expect{|
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

(* Checks that optional arguments with defaults are correctly bound in the
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

(* Test that type information introduced in a guard is available in the rhs. *)

module Foo = struct
  type t = A | B

  let is_a = function
    | A -> true
    | B -> false
end
[%%expect{|
module Foo : sig type t = A | B val is_a : t -> bool end
|}];;

let disambiguation_impossible = function
  | None -> "None"
  | Some y ->
      match y with
      | A -> "A"
      | B -> "B"
;;
[%%expect{|
Line 5, characters 8-9:
5 |       | A -> "A"
            ^
Error: Unbound constructor A
|}];;

let disambiguation_from_guard = function
  | None -> "None"
  | Some y when Foo.is_a y ->
      (match y with
       | A -> "A"
       | B -> "B")
  | _ -> "Not A"
;;
[%%expect{|
val disambiguation_from_guard : Foo.t option -> string = <fun>
|}, Principal{|
Line 5, characters 9-10:
5 |        | A -> "A"
             ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
Line 6, characters 9-10:
6 |        | B -> "B")
             ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val disambiguation_from_guard : Foo.t option -> string = <fun>
|}];;

(* Test approximate typechecking of guards under function. *)

let rec f = function
  | x :: xs when x match Some y -> y + f xs
  | _ :: xs -> f xs
  | [] -> 0
;;
[%%expect{|
val f : int option list -> int = <fun>
|}];;
