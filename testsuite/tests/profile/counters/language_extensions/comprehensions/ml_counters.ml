(* List comprehensions *)

[x for x = 1 to 5];;

[x * 2 for x = 1 to 5];;

[x * 2 for x = 1 to 5 when x mod 2 <> 1];;

[((x + y) * (x + y + 1)) / 2 + 1 for x = 1 to 5 for y = 1 to 5];;

(* Array comprehensions *)

[|x for x = 1 to 5|];;

[|x * 2 for x = 1 to 5|];;

[|x * 2 for x = 1 to 5 when x mod 2 <> 1|];;

[|((x + y) * (x + y + 1)) / 2 + 1 for x = 1 to 5 for y = 1 to 5|];;

(* Normal lists and arrays (should not be counted) *)

[1; 2; 3; 4; 5];;
[|1; 2; 3; 4; 5|];;

module type M = sig
  val x : 'a list

  val y : 'a array
end
