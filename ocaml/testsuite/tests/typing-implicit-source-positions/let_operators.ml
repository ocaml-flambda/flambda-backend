<<<<<<< HEAD
(* TEST
   * expect
*)

let ( let+ ) ~(src_pos : [%src_pos]) a f = f (src_pos, a);;
[%%expect{|
val ( let+ ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
|}]

(* Would be nice to add support for implicit position parameters and (also maybe optional
   arguments) for let operators. *)
let _ = 
  let+ (src_pos, a) = 1 in
  src_pos

[%%expect{|
Line 2, characters 2-6:
2 |   let+ (src_pos, a) = 1 in
      ^^^^
Error: The operator let+ has type
         src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b
       but it was expected to have type 'c -> ('d -> 'e) -> 'f
|}]

let ( let* ) ?(src_pos = 1) a g = g (src_pos, a);; 

let _ =
  let* (src_pos, a) = 1 in
  src_pos

[%%expect{|
val ( let* ) : ?src_pos:int -> 'a -> (int * 'a -> 'b) -> 'b = <fun>
Line 4, characters 2-6:
4 |   let* (src_pos, a) = 1 in
      ^^^^
Error: The operator let* has type
         ?src_pos:int -> 'a -> (int * 'a -> 'b) -> 'b
       but it was expected to have type 'c -> ('d -> 'e) -> 'f
|}]

(* Infix operators work! *)
let ( >>| ) ~(src_pos : [%src_pos]) x f = f (src_pos, x)
let _ =
  1 >>| fun (src_pos, a) -> src_pos

[%%expect{|
val ( >>| ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
- : lexing_position =
{pos_fname = ""; pos_lnum = 3; pos_bol = 1108; pos_cnum = 1110}
|}]
||||||| parent of 6b207b13 (Implicit Source Positions Conflict resolution)
=======
(* TEST
   * expect
*)

let ( let+ ) ~(src_pos : [%src_pos]) a f = f (src_pos, a);;
[%%expect{|
val ( let+ ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
|}]

(* CR src_pos: Add support for implicit position parameters and (also maybe optional
   arguments) for let operators. *)
let _ = 
  let+ (src_pos, a) = 1 in
  src_pos

[%%expect{|
Line 2, characters 2-6:
2 |   let+ (src_pos, a) = 1 in
      ^^^^
Error: The operator let+ has type
         src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b
       but it was expected to have type 'c -> ('d -> 'e) -> 'f
|}]

let ( let* ) ?(src_pos = 1) a g = g (src_pos, a);; 

let _ =
  let* (src_pos, a) = 1 in
  src_pos

[%%expect{|
val ( let* ) : ?src_pos:int -> 'a -> (int * 'a -> 'b) -> 'b = <fun>
Line 4, characters 2-6:
4 |   let* (src_pos, a) = 1 in
      ^^^^
Error: The operator let* has type
         ?src_pos:int -> 'a -> (int * 'a -> 'b) -> 'b
       but it was expected to have type 'c -> ('d -> 'e) -> 'f
|}]

(* Infix operators work! *)
let ( >>| ) ~(src_pos : [%src_pos]) x f = f (src_pos, x)
let _ =
  1 >>| fun (src_pos, a) -> src_pos

[%%expect{|
val ( >>| ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
- : lexing_position =
{pos_fname = ""; pos_lnum = 3; pos_bol = 1103; pos_cnum = 1105}
|}]
>>>>>>> 6b207b13 (Implicit Source Positions Conflict resolution)
