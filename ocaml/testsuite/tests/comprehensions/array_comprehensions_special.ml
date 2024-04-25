(* TEST
 flags = "-extension-universe beta";
 expect;
*)

(* Tests for special array comprehension behavior that don't have a direct
   analog for lists.  For array comprehensions, we pre-allocate the resulting
   array if it has a statically-knowable size, which we can see by including 0
   in something that would otherwise run for close enough to forever.  However,
   even if one of the sequences being iterated through is empty, these optimized
   comprehensions will still fail if any individual [for]-[to] or [for]-[downto]
   iterator would overflow an [int]. *)

(* Zeros, even though we'd normally expect this to run for ~forever *)

[|i,j,k for i = 1 to 0
        and j = 0 to Int.max_int / 256
        and k = 1 to 2 |];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int / 256
        and j = 1 to 0
        and k = 1 to 2 |];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int / 256
        and j = 1 to 2
        and k = 1 to 0|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

(* Zeros, even though we'd normally expect the array size to overflow *)

[|i,j,k for i = 1 to 0
        and j = 0 to Int.max_int - 1
        and k = 0 downto Int.min_int + 2|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int - 1
        and j = 1 to 0
        and k = 0 downto Int.min_int + 2|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int - 1
        and j = 0 downto Int.min_int + 2
        and k = 1 to 0|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

(* Zeros, even though we'd normally expect the individual iterator sizes to
   overflow *)

[|i,j,k for i = 1 to 0
        and j = 0 to Int.max_int
        and k = 0 downto Int.min_int|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int
        and j = 1 to 0
        and k = 0 downto Int.min_int|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

[|i,j,k for i = 0 to Int.max_int
        and j = 0 downto Int.min_int
        and k = 1 to 0|];;
[%%expect{|
- : (int * int * int) array = [||]
|}];;

(* Various kind of overflow *)

[|i for i = 0 to Int.max_int |];;
[%%expect{|
Exception:
Invalid_argument
 "integer overflow when precomputing the size of an array comprehension".
|}];;

[|i for i = 0 downto Int.min_int |];;
[%%expect{|
Exception:
Invalid_argument
 "integer overflow when precomputing the size of an array comprehension".
|}];;

[|i for i = 0 downto Int.min_int + 1 |];;
[%%expect{|
Exception:
Invalid_argument
 "integer overflow when precomputing the size of an array comprehension".
|}];;

[|i,j for i = 0 to Int.max_int / 256
      and j = 0 to 1023 |];;
[%%expect{|
Exception:
Invalid_argument
 "integer overflow when precomputing the size of an array comprehension".
|}];;

[|i,j for i = 0 to 1023
      and j = 0 to Int.max_int / 256 |];;
[%%expect{|
Exception:
Invalid_argument
 "integer overflow when precomputing the size of an array comprehension".
|}];;

(* One case that works, whose output should be unremarkable *)

[|i,j,k for i = 0 to 3 and j = 0 to 3 and k = 0 to 3|];;
[%%expect{|
- : (int * int * int) array =
[|(0, 0, 0); (0, 0, 1); (0, 0, 2); (0, 0, 3); (0, 1, 0); (0, 1, 1);
  (0, 1, 2); (0, 1, 3); (0, 2, 0); (0, 2, 1); (0, 2, 2); (0, 2, 3);
  (0, 3, 0); (0, 3, 1); (0, 3, 2); (0, 3, 3); (1, 0, 0); (1, 0, 1);
  (1, 0, 2); (1, 0, 3); (1, 1, 0); (1, 1, 1); (1, 1, 2); (1, 1, 3);
  (1, 2, 0); (1, 2, 1); (1, 2, 2); (1, 2, 3); (1, 3, 0); (1, 3, 1);
  (1, 3, 2); (1, 3, 3); (2, 0, 0); (2, 0, 1); (2, 0, 2); (2, 0, 3);
  (2, 1, 0); (2, 1, 1); (2, 1, 2); (2, 1, 3); (2, 2, 0); (2, 2, 1);
  (2, 2, 2); (2, 2, 3); (2, 3, 0); (2, 3, 1); (2, 3, 2); (2, 3, 3);
  (3, 0, 0); (3, 0, 1); (3, 0, 2); (3, 0, 3); (3, 1, 0); (3, 1, 1);
  (3, 1, 2); (3, 1, 3); (3, 2, 0); (3, 2, 1); (3, 2, 2); (3, 2, 3);
  (3, 3, 0); (3, 3, 1); (3, 3, 2); (3, 3, 3)|]
|}];;
