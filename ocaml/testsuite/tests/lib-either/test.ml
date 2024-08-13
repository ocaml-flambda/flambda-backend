(* TEST
 expect;
*)

open Either;;

[left 1; right true];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

List.map is_left [left 1; right true];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

List.map is_right [left 1; right true];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[find_left (Left 1); find_left (Right 1)];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[find_right (Left 1); find_right (Right 1)];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[map_left succ (Left 1); map_left succ (Right true)];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[map_right succ (Left ()); map_right succ (Right 2)];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[map ~left:succ ~right:not (Left 1);
 map ~left:succ ~right:not (Right true)];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

[fold ~left:succ ~right:int_of_string (Left 1);
 fold ~left:succ ~right:int_of_string (Right "2")];;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

let li = ref [] in
let add to_str x = li := to_str x :: !li in
iter ~left:(add Fun.id) ~right:(add string_of_int) (Left "foo");
iter ~left:(add Fun.id) ~right:(add string_of_int) (Right 2);
List.rev !li;;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

(
  for_all ~left:((=) 1) ~right:((=) "foo") (Left 1),
  for_all ~left:((=) 1) ~right:((=) "foo") (Right "foo"),
  for_all ~left:((=) 1) ~right:((=) "foo") (Left 2),
  for_all ~left:((=) 1) ~right:((=) "foo") (Right "bar")
);;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

equal ~left:(=) ~right:(=) (Left 1) (Left 1),
equal ~left:(=) ~right:(=) (Right true) (Right true);;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

(equal ~left:(=) ~right:(=) (Left 1) (Left 2),
 equal ~left:(=) ~right:(=) (Right true) (Right false),
 equal ~left:(=) ~right:(=) (Left 1) (Right true),
 equal ~left:(=) ~right:(=) (Right 1) (Left true));;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

equal ~left:(fun _ _ -> false) ~right:(=) (Left 1) (Left 1),
equal ~left:(=) ~right:(fun _ _ -> false) (Right true) (Right true);;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;

let cmp = Stdlib.compare in
(
 (compare ~left:cmp ~right:cmp (Left 0) (Left 1),
  compare ~left:cmp ~right:cmp (Left 1) (Left 1),
  compare ~left:cmp ~right:cmp (Left 1) (Left 0)),

 (compare ~left:cmp ~right:cmp (Right 0) (Right 1),
  compare ~left:cmp ~right:cmp (Right 1) (Right 1),
  compare ~left:cmp ~right:cmp (Right 1) (Right 0)),

 (compare ~left:cmp ~right:cmp (Left 1) (Right true),
  compare ~left:cmp ~right:cmp (Right 1) (Left true))
);;
[%%expect {|
Line 1:
Error: Reference to undefined compilation unit "Stdlib__Either"
Hint: This means that the interface of a module is loaded, but its implementation is not.
      Did you mean to load a compiled implementation of the module
      using "#load" or by passing it as an argument to the toplevel?
|}];;
