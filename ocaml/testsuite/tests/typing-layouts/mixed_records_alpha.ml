(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Void is not allowed in mixed records, for now *)

type t_void : void

type t_void_in_value_prefix = { x : t_void; y : string; z : float# }
[%%expect {|
type t_void : void
Line 3, characters 0-68:
3 | type t_void_in_value_prefix = { x : t_void; y : string; z : float# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]

type t_void_in_flat_suffix = { x : int; y : float#; z : t_void }
[%%expect {|
Line 1, characters 0-64:
1 | type t_void_in_flat_suffix = { x : int; y : float#; z : t_void }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]

type t_void_at_border_of_prefix_and_suffix =
  { x : string; y : t_void; z : float# }
[%%expect {|
Lines 1-2, characters 0-40:
1 | type t_void_at_border_of_prefix_and_suffix =
2 |   { x : string; y : t_void; z : float# }
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]

type t_void_in_all_float_mixed_record = { x : t_void; y : float#; z : float }
[%%expect {|
Line 1, characters 0-77:
1 | type t_void_in_all_float_mixed_record = { x : t_void; y : float#; z : float }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t_void has layout void.
       Structures with non-value elements may not yet contain types of this layout.
|}]
