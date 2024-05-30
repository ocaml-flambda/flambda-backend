(* TEST
 flags = "-extension-universe stable";
 include stdlib_alpha;
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

(* It should normally be impossible to get a sublayout error
   between [value] and [non_null_value] without enabling [-extension-universe alpha].
   We trigger it here by using [Stdlib_alpha.Or_null].

   In this case, [non_null_value] is still displayed. *)

type t_value : value
type fail = t_value Stdlib_alpha.Or_null.t
;;

[%%expect{|
type t_value : value
Line 2, characters 12-19:
2 | type fail = t_value Stdlib_alpha.Or_null.t
                ^^^^^^^
Error: This type t_value should be an instance of type ('a : value)
       The layout of t_value is value, because
         of the definition of t_value at line 1, characters 0-20.
       But the layout of t_value must be a sublayout of non_null_value, because
         the type argument of Stdlib_alpha.Or_null.t has this layout.
|}]
