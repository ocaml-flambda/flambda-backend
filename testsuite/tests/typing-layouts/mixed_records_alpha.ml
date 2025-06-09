(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Void is not allowed in mixed records, for now *)

type t_void : void

type t_void_in_value_prefix = { x : t_void; y : string; z : float# }
[%%expect {|
type t_void : void
type t_void_in_value_prefix = { x : t_void; y : string; z : float#; }
|}]

type t_void_in_flat_suffix = { x : int; y : float#; z : t_void }
[%%expect {|
type t_void_in_flat_suffix = { x : int; y : float#; z : t_void; }
|}]

type t_void_at_border_of_prefix_and_suffix =
  { x : string; y : t_void; z : float# }
[%%expect {|
type t_void_at_border_of_prefix_and_suffix = {
  x : string;
  y : t_void;
  z : float#;
}
|}]

type t_void_in_all_float_mixed_record = { x : t_void; y : float#; z : float }
[%%expect {|
type t_void_in_all_float_mixed_record = {
  x : t_void;
  y : float#;
  z : float;
}
|}]
