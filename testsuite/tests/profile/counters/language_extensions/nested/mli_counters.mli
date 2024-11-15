(* Nested language extensions *)

(* 2 labeled tuples *)
val nested_labeled_tuple : z:(x:int * int * y:int) * int

(* 1 labeled tuple, 1 immutable array *)
val nested_labeled_tuple_iarray : (x:int * int * y:int) iarray
