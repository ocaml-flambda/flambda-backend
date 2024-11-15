(* TEST
   flags += "-extension unique ";
   expect;
*)

type aliased = { a : int @@ aliased many }
type record = { x : string; y : string @@ many aliased }
type 'a or_null = Null | This of 'a
let unique_id : 'a @ unique -> 'a @ unique = fun x -> x
let aliased_id : 'a @ aliased -> 'a @ aliased = fun x -> x
[%%expect{|
type aliased = { a : int @@ many aliased; }
type record = { x : string; y : string @@ many aliased; }
type 'a or_null = Null | This of 'a
val unique_id : 'a @ unique -> 'a @ unique = <fun>
val aliased_id : 'a -> 'a = <fun>
|}]

let pat_match xs opt =
  match xs, opt with
  | { a = x } :: ({ a = 3 } :: _) as xs, Some y -> (x, aliased_id xs, y)
  | { a = x } :: (_ :: _) as xs, y -> (x, aliased_id xs, 4)
  | { a = x } :: _ as xs, Some y -> (x, unique_id xs, y)
  | _, _ -> (42, [], 4)
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val pat_match :
  aliased list @ unique -> int option -> int * aliased list * int = <fun>
|}]

let pat_match a b =
  match a, b with
  | a, ({ a = 3 }) -> { a = 4 }, (a, b)
  | (a, b) as t -> a, t
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val pat_match : aliased -> aliased -> aliased * (aliased * aliased) = <fun>
|}]

let pat_match a b =
  match a, b with
  | a, b -> a, b
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val pat_match : 'a -> 'b -> 'a * 'b = <fun>
|}]

let pat_match a b =
  match a, b with
  | a, b as t -> a, t
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val pat_match : 'a -> 'b -> 'a * ('a * 'b) = <fun>
|}]

let compare cmp t0 t1 =
  match (t0, t1) with
  | Null, Null -> 0
  | Null, This _ -> -1
  | This _, Null -> 1
  | This v0, This v1 -> cmp v0 v1
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val compare : ('a -> 'b -> int) -> 'a or_null -> 'b or_null -> int = <fun>
|}]

let match_mini_anf_aliased r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = aliased_id r in
  (r, y)
[%%expect{|
val match_mini_anf_aliased : record -> record * string = <fun>
|}]

let match_mini_anf_unique r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = unique_id r in
  (r, y)
[%%expect{|
val match_mini_anf_unique : record @ unique -> record * string = <fun>
|}]

let match_anf_aliased r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = aliased_id r in
  (r, y)
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val match_anf_aliased : record -> record * string = <fun>
|}]

let match_anf_unique r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = unique_id r in
  (r, y)
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val match_anf_unique : record @ unique -> record * string = <fun>
|}]

let x f =
  match f #4L with
  | `Four -> ()
  | _ -> assert false
[%%expect{|
File "_none_", line 1:
Alert internal: Unique barrier was not resolved

val x : (int64# -> [> `Four ]) -> unit = <fun>
|}]

(* See [Matching.assign_pat] *)
let tuple_match foo z =
  let (x, y) = if foo then z else (1,2) in
  (y, x)
[%%expect{|
File "_none_", line 1:
Alert internal: A discarded barrier was resolved

val tuple_match : bool -> int * int -> int * int = <fun>
|}]

let tuple_match foo z =
  let (x, y) = if foo then (3,4) else (1,2) in
  (y, x)
[%%expect{|
val tuple_match : bool -> 'a -> int * int = <fun>
|}]

let tuple_match_nested foo z =
  let ((x, y), y') = if foo then (z, 3) else ((1,2), 3) in
  ((y, x), y')
[%%expect{|
File "_none_", line 1:
Alert internal: A discarded barrier was resolved

val tuple_match_nested : bool -> int * int -> (int * int) * int = <fun>
|}]

let tuple_match_nested foo z =
  let ((x, y), y') = if foo then ((3,4), 5) else ((1,2), 3) in
  ((y, x), y')
[%%expect{|
val tuple_match_nested : bool -> 'a -> (int * int) * int = <fun>
|}]
