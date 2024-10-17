(* Immutable array comprehensions *)

[:x for x = 1 to 5:];;

[:x * 2 for x = 1 to 5:];;

[:x * 2 for x = 1 to 5 when x mod 2 <> 1:];;

[:((x + y) * (x + y + 1)) / 2 + 1 for x = 1 to 5 for y = 1 to 5:];;
