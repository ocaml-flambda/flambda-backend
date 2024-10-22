(* Nested language extensions *)

(* 2 comprehensions *)
[[x + y for y = 1 to 5] for x = 1 to 5];;

(* 1 comprehension, 1 labeled tuple *)
[(~y:5, ~z:2, x) for x = 1 to 5];;
