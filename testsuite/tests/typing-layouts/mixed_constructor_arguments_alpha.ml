(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Void is not allowed in mixed constructors, for now *)

type t_void : void

type t_void_in_value_prefix = A of t_void * string * float#
[%%expect {|
type t_void : void
type t_void_in_value_prefix = A of t_void * string * float#
|}]

type t_void_in_flat_suffix = A of int * float# * t_void
[%%expect {|
type t_void_in_flat_suffix = A of int * float# * t_void
|}]

type t_void_at_border_of_prefix_and_suffix = A of string * t_void * float#
[%%expect {|
type t_void_at_border_of_prefix_and_suffix = A of string * t_void * float#
|}]
