(* TEST
   * expect
*)

let ( let+ ) ~(src_pos : [%src_pos]) a f = f (src_pos, a);;
[%%expect{|
val ( let+ ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
|}]


(* XXX jrodri: I _think_ it would be cool if this was allowed, although I do not understand
   enough to know for sure whether this is unsound... *)
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

(* XXX jrodri: Optional parameters also do not work, which makes sense, I think this situation
   is more silly for optional parameters, but I could the "[%src_pos]"
   parameter being useful for let* / let+ in scenarios where ppx_let does not
   exist, making it ~never be sent in.

   Is this something we want to support/should it be a CR src_pos?
   *)
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


(* XXX jrodri: Ok, this working makes me think that making let+ and let* working is not _totally_
   unsound... *)
let ( >>| ) ~(src_pos : [%src_pos]) x f = f (src_pos, x)
let _ =
  1 >>| fun (src_pos, a) -> src_pos

[%%expect{|
val ( >>| ) : src_pos:[%src_pos] -> 'a -> (lexing_position * 'a -> 'b) -> 'b =
  <fun>
- : lexing_position =
{pos_fname = ""; pos_lnum = 3; pos_bol = 1577; pos_cnum = 1579}
|}]
