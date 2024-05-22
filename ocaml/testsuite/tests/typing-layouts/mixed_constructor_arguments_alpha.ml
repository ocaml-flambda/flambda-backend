(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Void is not allowed in mixed constructors, for now *)

type t_void : void

type t_void_in_value_prefix = A of t_void * string * float#
[%%expect {|
type t_void : void
Line 3, characters 30-59:
3 | type t_void_in_value_prefix = A of t_void * string * float#
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]

type t_void_in_flat_suffix = A of int * float# * t_void
[%%expect {|
Line 1, characters 29-55:
1 | type t_void_in_flat_suffix = A of int * float# * t_void
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]

type t_void_at_border_of_prefix_and_suffix = A of string * t_void * float#
[%%expect {|
Line 1, characters 45-74:
1 | type t_void_at_border_of_prefix_and_suffix = A of string * t_void * float#
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]
