(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

type 'a my_list : immutable_data with 'a = Nil | Cons of 'a * 'a my_list
[%%expect {|
type 'a my_list = Nil | Cons of 'a * 'a my_list
|}]

type 'a my_list : immutable_data with 'a = 'a list = [] | ( :: ) of 'a * 'a my_list
[%%expect {|
type 'a my_list = 'a list = [] | (::) of 'a * 'a my_list
|}]

type 'a my_list : immutable_data with 'a = Nil | Cons of 'a * 'a foo
and 'a foo = 'a my_list
[%%expect {|
type 'a my_list = Nil | Cons of 'a * 'a foo
and 'a foo = 'a my_list
|}]

(* CR layouts v2.8: this should be accepted *)
type 'a my_list : immutable_data with 'a =
  | Nil
  | Cons of 'a * 'a my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list
[%%expect {|
Line 3, characters 17-115:
3 |   | Cons of 'a * 'a my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list my_list
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a my_list my_list my_list my_list my_list my_list
                       my_list my_list my_list my_list my_list is any
           because the .cmi file for my_list is missing.
         But the layout of 'a my_list my_list my_list my_list my_list my_list
                           my_list my_list my_list my_list my_list must be a sublayout of value
           because it instantiates an unannotated type parameter of my_list,
           chosen to have layout value.
         No .cmi file found containing my_list.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

type 'a mutable_list : mutable_data with 'a = Nil | Cons of 'a ref * 'a mutable_list
[%%expect {|
type 'a mutable_list = Nil | Cons of 'a ref * 'a mutable_list
|}]

type ('a : immutable_data) immutable_list : immutable_data = Nil | Cons of 'a * 'a immutable_list
[%%expect {|
type ('a : immutable_data) immutable_list =
    Nil
  | Cons of 'a * 'a immutable_list
|}]

(* CR layouts v2.8: this should be accepted *)
(* CR layouts v2.8: this error message is bad *)
type 'a degenerate : immutable_data with 'a = Leaf of 'a | Branch of ('a * 'a) degenerate
[%%expect {|
Line 1, characters 0-89:
1 | type 'a degenerate : immutable_data with 'a = Leaf of 'a | Branch of ('a * 'a) degenerate
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "degenerate" is value with 'a with 'a * 'a
         because it's a boxed variant type.
       But the kind of type "degenerate" must be a subkind of immutable_data
         with 'a
         because of the annotation on the declaration of the type degenerate.
|}]

type ('a, 'b) zipped_list : immutable_data with 'a with 'b = Nil | Cons of 'a * 'b * ('a, 'b) zipped_list
[%%expect {|
type ('a, 'b) zipped_list = Nil | Cons of 'a * 'b * ('a, 'b) zipped_list
|}]
