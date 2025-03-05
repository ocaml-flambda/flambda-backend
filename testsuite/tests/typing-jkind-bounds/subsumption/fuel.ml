(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

type 'a my_list = 'a list = [] | ( :: ) of 'a * 'a my_list

(* At the time this test is written, "fuel" is used to deal with recursive types. Two
   types that are equal can get different jkinds due to fuel running out at different
   places. The below types are chosen so that they are equal, but fuel runs out in
   different places. If fuel is changed, they should be modified so that this continues
   to be true, and all instances of them in this file should also be updated. *)
type t : immutable_data = int list my_list list my_list list
[%%expect {|
type 'a my_list = 'a list = [] | (::) of 'a * 'a my_list
type t = int list my_list list my_list list
|}]
type t : immutable_data = int list list list list list
(* CR layouts v2.8: The "because of the definition of t at line 1" part of the message
   is not great. It should say something about the kind annotation on t. *)
[%%expect {|
Line 1, characters 0-54:
1 | type t : immutable_data = int list list list list list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int list list list list list" is immutable_data
         with int list
         because it's a boxed variant type.
       But the kind of type "int list list list list list" must be a subkind of
         immutable_data
         because of the definition of t at line 1, characters 0-54.
|}]

(* Differences in fuel consumption do not cause errors for module inclusion check
   when both types have manifests *)
module M : sig
  type t = int list my_list list my_list list
end = struct
  type t = int list list list list list
end
[%%expect{|
module M : sig type t = int list my_list list my_list list end
|}]

(* Differences in fuel consumption do not cause errors for module inclusion check
   when both types have kinds *)
module M : sig
  type t = Foo of int list my_list list my_list list
end = struct
  type t = Foo of int list list list list list
end
[%%expect{|
module M : sig type t = Foo of int list my_list list my_list list end
|}]

(* Differences in fuel consumption do not cause errors for module inclusion check
   when both types have kinds and manifests *)
type foo = Foo of int list my_list list my_list list
module M : sig
  type t = foo = Foo of int list my_list list my_list list
end = struct
  type t = foo = Foo of int list list list list list
end
[%%expect{|
type foo = Foo of int list my_list list my_list list
module M : sig type t = foo = Foo of int list my_list list my_list list end
|}]

type foo = Foo of int list list list list list
module M : sig
  type t = foo = Foo of int list my_list list my_list list
end = struct
  type t = foo = Foo of int list list list list list
end
[%%expect{|
type foo = Foo of int list list list list list
module M : sig type t = foo = Foo of int list my_list list my_list list end
|}]

(* Differences in fuel consumption do not cause errors when both a type has both kind and
   manifest *)
type foo1 = Foo of int list my_list list my_list list
type foo2 = foo1 = Foo of int list list list list list
[%%expect{|
type foo1 = Foo of int list my_list list my_list list
type foo2 = foo1 = Foo of int list list list list list
|}]

type foo1 = Foo of int list list list list list
type foo2 = foo1 = Foo of int list my_list list my_list list
[%%expect{|
type foo1 = Foo of int list list list list list
type foo2 = foo1 = Foo of int list my_list list my_list list
|}]

(* Differences in fuel consumption do not cause errors when satisfying a functor
   constraint *)
module F (M : sig
  type t = Foo of int list my_list list my_list list
end) = struct end
module M = F (struct
  type t = Foo of int list list list list list
end)
[%%expect {|
module F :
  functor (M : sig type t = Foo of int list my_list list my_list list end) ->
    sig end
module M : sig end
|}]

module F (M : sig
  type t = int list my_list list my_list list
end) = struct end
module M = F (struct
  type t = int list list list list list
end)
[%%expect {|
module F :
  functor (M : sig type t = int list my_list list my_list list end) ->
    sig end
module M : sig end
|}]
