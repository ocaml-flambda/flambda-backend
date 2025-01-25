(* Labeled tuples usage *)

(* 1 expression *)
let labeled_1 = ~x:1, ~y:2, 3

(* 1 type, 1 expression *)
let (labeled_2 : x:int * int * y:int) = ~x:4, 5, ~y:6

(* 1 pattern *)
let ~x, ~y, _ = labeled_1

(* Unlabeled tuples usage (should not count) *)

let (normal_1 : int * int * int) = 1, 2, 3

let a, b, c = 4, 5, 6

let _, b, _ = 7, 8, 9
