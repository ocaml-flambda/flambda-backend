(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Array type arguments are [any_non_null]: *)

type t_any : any

type should_fail = t_any array

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any array
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : any_non_null)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null array

[%%expect{|
type t_value_or_null : value_or_null
Line 3, characters 19-34:
3 | type should_fail = t_value_or_null array
                       ^^^^^^^^^^^^^^^
Error: This type "t_value_or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_any : any

type should_fail = t_any iarray

[%%expect{|
type t_any : any
Line 3, characters 19-24:
3 | type should_fail = t_any iarray
                       ^^^^^
Error: This type "t_any" should be an instance of type "('a : any_non_null)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null iarray

[%%expect{|
type t_value_or_null : value_or_null
Line 3, characters 19-34:
3 | type should_fail = t_value_or_null iarray
                       ^^^^^^^^^^^^^^^
Error: This type "t_value_or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of t_value_or_null is value_or_null
         because of the definition of t_value_or_null at line 1, characters 0-36.
       But the kind of t_value_or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]
