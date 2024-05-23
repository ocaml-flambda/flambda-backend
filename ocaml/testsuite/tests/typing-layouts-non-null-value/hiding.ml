(* TEST
 flags = "-extension-universe stable";
 expect;
*)

(* Test that [non_null_value] layouts are hidden from users. *)

(* Despite [string]'s true layout being [non_null_value],
   the error displays [value]. *)

type ('a : bits64) id_bits64 = 'a

type fail = string id_bits64
;;

[%%expect{|
type ('a : bits64) id_bits64 = 'a
Line 3, characters 12-18:
3 | type fail = string id_bits64
                ^^^^^^
Error: This type string should be an instance of type ('a : bits64)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of bits64, because
         of the definition of id_bits64 at line 1, characters 0-33.
|}]

(* [non_null_value] is still displayed on annotation errors. *)

type t_non_null_value : non_null_value
;;

[%%expect{|
Line 1, characters 24-38:
1 | type t_non_null_value : non_null_value
                            ^^^^^^^^^^^^^^
Error: Layout non_null_value is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]
