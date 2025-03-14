(* TEST
 include stdlib_upstream_compatible;
 expect;
*)

(* Basic typing tests for the array reinterpret primitives. The dynamic tests
   are over in [array-reinterpret/] *)

(* The return can be layout polymorphic. *)
external[@layout_poly] reinterp_get_int64 :
  ('a : any). int64# array -> int -> 'a = "%obj_reinterp_array_unsafe_get"

external[@layout_poly] reinterp_set_int64 :
  ('a : any). int64# array -> int -> 'a -> unit = "%obj_reinterp_array_unsafe_set"

external[@layout_poly] reinterp_get_value :
  ('a : any). string array -> int -> 'a = "%obj_reinterp_array_unsafe_get"

external[@layout_poly] reinterp_set_value :
  ('a : any). string array -> int -> 'a -> unit = "%obj_reinterp_array_unsafe_set"
[%%expect{|
external reinterp_get_int64 : ('a : any). int64# array -> int -> 'a
  = "%obj_reinterp_array_unsafe_get" [@@layout_poly]
external reinterp_set_int64 : ('a : any). int64# array -> int -> 'a -> unit
  = "%obj_reinterp_array_unsafe_set" [@@layout_poly]
external reinterp_get_value : ('a : any). string array -> int -> 'a
  = "%obj_reinterp_array_unsafe_get" [@@layout_poly]
external reinterp_set_value : ('a : any). string array -> int -> 'a -> unit
  = "%obj_reinterp_array_unsafe_set" [@@layout_poly]
|}]

(* Or it can be any concrete layout, including products *)
external reinterp_get_int64_one :
  int64# array -> int -> int64# = "%obj_reinterp_array_unsafe_get"

external reinterp_get_int64_many :
  int64# array -> int -> #(int64# * int64# * int64#)
  = "%obj_reinterp_array_unsafe_get"

external reinterp_set_int64_one :
  int64# array -> int -> int64# -> unit = "%obj_reinterp_array_unsafe_set"

external reinterp_set_int64_many :
  int64# array -> int -> #(int64# * int64# * int64#) -> unit
  = "%obj_reinterp_array_unsafe_set"

external reinterp_get_value_one :
  string array -> int -> string = "%obj_reinterp_array_unsafe_get"

external reinterp_get_value_many :
  string array -> int -> #(string * bool * int option)
  = "%obj_reinterp_array_unsafe_get"

external reinterp_set_value_one :
  string array -> int -> string -> unit = "%obj_reinterp_array_unsafe_set"

external reinterp_set_value_many :
  string array -> int -> #(string * bool * int option) -> unit
  = "%obj_reinterp_array_unsafe_set"
[%%expect{|
external reinterp_get_int64_one : int64# array -> int -> int64#
  = "%obj_reinterp_array_unsafe_get"
external reinterp_get_int64_many :
  int64# array -> int -> #(int64# * int64# * int64#)
  = "%obj_reinterp_array_unsafe_get"
external reinterp_set_int64_one : int64# array -> int -> int64# -> unit
  = "%obj_reinterp_array_unsafe_set"
external reinterp_set_int64_many :
  int64# array -> int -> #(int64# * int64# * int64#) -> unit
  = "%obj_reinterp_array_unsafe_set"
external reinterp_get_value_one : string array -> int -> string
  = "%obj_reinterp_array_unsafe_get"
external reinterp_get_value_many :
  string array -> int -> #(string * bool * int option)
  = "%obj_reinterp_array_unsafe_get"
external reinterp_set_value_one : string array -> int -> string -> unit
  = "%obj_reinterp_array_unsafe_set"
external reinterp_set_value_many :
  string array -> int -> #(string * bool * int option) -> unit
  = "%obj_reinterp_array_unsafe_set"
|}]

(* There is no checking of the relationship between the array parameter layout
   and the element layout (that would be nice, though) *)
external reinterp_get_int64_mismatch :
  int64# array -> int -> string = "%obj_reinterp_array_unsafe_get"

external reinterp_set_int64_mismatch :
  int64# array -> int -> #(string * bool * int option) -> unit
  = "%obj_reinterp_array_unsafe_set"

external reinterp_get_value_mismatch :
  string array -> int -> #(int64 * int64 * int64) = "%obj_reinterp_array_unsafe_get"

external reinterp_set_value_mismatch :
  string array -> int -> int64# -> unit = "%obj_reinterp_array_unsafe_set"
[%%expect{|
external reinterp_get_int64_mismatch : int64# array -> int -> string
  = "%obj_reinterp_array_unsafe_get"
external reinterp_set_int64_mismatch :
  int64# array -> int -> #(string * bool * int option) -> unit
  = "%obj_reinterp_array_unsafe_set"
external reinterp_get_value_mismatch :
  string array -> int -> #(int64 * int64 * int64)
  = "%obj_reinterp_array_unsafe_get"
external reinterp_set_value_mismatch : string array -> int -> int64# -> unit
  = "%obj_reinterp_array_unsafe_set"
|}]

(* The arity is checked *)
external reinterp_get_too_few_args :
  int64# array -> int = "%obj_reinterp_array_unsafe_get"
[%%expect{|
Lines 1-2, characters 0-56:
1 | external reinterp_get_too_few_args :
2 |   int64# array -> int = "%obj_reinterp_array_unsafe_get"
Error: Wrong arity for builtin primitive "%obj_reinterp_array_unsafe_get"
|}]

external reinterp_get_too_many_args :
  int64# array -> int -> int64# -> int64# = "%obj_reinterp_array_unsafe_get"
[%%expect{|
Lines 1-2, characters 0-76:
1 | external reinterp_get_too_many_args :
2 |   int64# array -> int -> int64# -> int64# = "%obj_reinterp_array_unsafe_get"
Error: Wrong arity for builtin primitive "%obj_reinterp_array_unsafe_get"
|}]

external reinterp_set_too_few_args :
  int64# array -> int -> #(string * bool * int option)
  = "%obj_reinterp_array_unsafe_set"
[%%expect{|
Lines 1-3, characters 0-36:
1 | external reinterp_set_too_few_args :
2 |   int64# array -> int -> #(string * bool * int option)
3 |   = "%obj_reinterp_array_unsafe_set"
Error: Wrong arity for builtin primitive "%obj_reinterp_array_unsafe_set"
|}]

external reinterp_set_too_few_args :
  int64# array -> int -> #(string * bool * int option) -> unit -> unit
  = "%obj_reinterp_array_unsafe_set"
[%%expect{|
Lines 1-3, characters 0-36:
1 | external reinterp_set_too_few_args :
2 |   int64# array -> int -> #(string * bool * int option) -> unit -> unit
3 |   = "%obj_reinterp_array_unsafe_set"
Error: Wrong arity for builtin primitive "%obj_reinterp_array_unsafe_set"
|}]

(* Only the "element" position can be non-value *)
external reinterp_get_bad_layout_1 :
  int64# -> int -> int64# = "%obj_reinterp_array_unsafe_get"
[%%expect{|
Line 2, characters 2-25:
2 |   int64# -> int -> int64# = "%obj_reinterp_array_unsafe_get"
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_reinterp_array_unsafe_get] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external reinterp_get_bad_layout_2 :
  int64# array -> int64# -> int64# = "%obj_reinterp_array_unsafe_get"
[%%expect{|
Line 2, characters 2-34:
2 |   int64# array -> int64# -> int64# = "%obj_reinterp_array_unsafe_get"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_reinterp_array_unsafe_get] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external reinterp_set_bad_layout_1 :
  int64# -> int -> int64# -> unit = "%obj_reinterp_array_unsafe_set"
[%%expect{|
Line 2, characters 2-33:
2 |   int64# -> int -> int64# -> unit = "%obj_reinterp_array_unsafe_set"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_reinterp_array_unsafe_set] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external reinterp_set_bad_layout_2 :
  int64# array -> int64# -> int64# -> unit = "%obj_reinterp_array_unsafe_set"
[%%expect{|
Line 2, characters 2-42:
2 |   int64# array -> int64# -> int64# -> unit = "%obj_reinterp_array_unsafe_set"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_reinterp_array_unsafe_set] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external reinterp_set_bad_layout_3 :
  int64# array -> int -> int64# -> int64# = "%obj_reinterp_array_unsafe_set"
[%%expect{|
Line 2, characters 2-41:
2 |   int64# array -> int -> int64# -> int64# = "%obj_reinterp_array_unsafe_set"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%obj_reinterp_array_unsafe_set] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
