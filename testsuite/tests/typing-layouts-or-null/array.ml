(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* CR layouts v3.0: array type arguments should be [any_non_null]: *)

type t_any : any

type should_fail = t_any array

[%%expect{|
type t_any : any
type should_fail = t_any array
|}]

type t_value_or_null : value_or_null

type should_fail = t_value_or_null array

[%%expect{|
type t_value_or_null : value_or_null
type should_fail = t_value_or_null array
|}]
