(* TEST
    flags = "-extension layouts_alpha -infer-with-bounds";
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
(* CR layouts v2.8: consider making the error message say something about running out of fuel *)
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

type 'a mutable_list : mutable_data with 'a = Nil | Cons of { mutable hd : 'a; tl : 'a mutable_list }
[%%expect {|
type 'a mutable_list =
    Nil
  | Cons of { mutable hd : 'a; tl : 'a mutable_list; }
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
Error: The kind of type "degenerate" is immutable_data
         with 'a @@ global aliased with 'a * 'a @@ global aliased
         with ('a * 'a) * ('a * 'a) @@ global aliased
         with (('a * 'a) * ('a * 'a)) * (('a * 'a) * ('a * 'a)) @@ global aliased

         with (((('a * 'a) * ('a * 'a)) * (('a * 'a) * ('a * 'a))) *
 ((('a * 'a) * ('a * 'a)) * (('a * 'a) * ('a * 'a))))
degenerate @@ global aliased
         because it's a boxed variant type.
       But the kind of type "degenerate" must be a subkind of immutable_data
         with 'a
         because of the annotation on the declaration of the type degenerate.
|}]

type ('a, 'b) zipped_list : immutable_data with 'a with 'b = Nil | Cons of 'a * 'b * ('a, 'b) zipped_list
[%%expect {|
type ('a, 'b) zipped_list = Nil | Cons of 'a * 'b * ('a, 'b) zipped_list
|}]

module rec My_list : sig
  type 'a t : immutable_data with 'a = Nil | Cons of 'a * 'a My_list.t
end = My_list
(* CR layouts v2.8: fix this *)
[%%expect {|
>> Fatal error: I do not yet know how to deal with [with]-types (such as
                'a)
                in recursive modules. Please contact the Jane Street OCaml Language
                team for help if you see this.
Uncaught exception: Misc.Fatal_error

|}]

module rec My_list : sig
  type 'a t = Nil | Cons of 'a * 'a My_list.t
end = My_list
module My_list = struct
  type 'a t : immutable_data with 'a = 'a My_list.t
end
[%%expect {|
module rec My_list : sig type 'a t = Nil | Cons of 'a * 'a My_list.t end
module My_list : sig type 'a t = 'a My_list.t end
|}]

module rec My_int_list : sig
  type t : immutable_data = Nil | Cons of int * My_int_list.t
end = My_int_list
[%%expect {|
module rec My_int_list : sig type t = Nil | Cons of int * My_int_list.t end
|}]

module rec My_list : sig
  type 'a t = Nil | Cons of 'a * 'a Foo.t
end = My_list

and Foo : sig
  type 'a t = 'a My_list.t
end = Foo

module My_list = struct
  type 'a t : immutable_data with 'a = 'a My_list.t
end

module Foo = struct
  type 'a t : immutable_data with 'a = 'a Foo.t
end
[%%expect {|
module rec My_list : sig type 'a t = Nil | Cons of 'a * 'a Foo.t end
and Foo : sig type 'a t = 'a My_list.t end
module My_list : sig type 'a t = 'a My_list.t end
module Foo : sig type 'a t = 'a Foo.t end
|}]

type 'a my_list : immutable_data with 'a = Nil | Cons of 'a * 'a foo
and 'a foo = 'a my_list
[%%expect {|
type 'a my_list = Nil | Cons of 'a * 'a foo
and 'a foo = 'a my_list
|}]

(* This test was failing at one point due to insufficient fuel and an unnecessary
   subsumption check when there is both a manifest and a kind. *)
type info

module Types = struct
   module rec Ivar : sig
     type 'a t
   end =
     Ivar

   and Forwarding : sig
     type t =
       | Parent of Monitor.t
   end =
     Forwarding

   and Monitor : sig
     type t =
       { name : info
       ; mutable next_error : exn Ivar.t
       ; mutable tails_for_all_errors : exn Tail.t
       ; mutable forwarding : Forwarding.t
       }
   end =
     Monitor

   and Tail : sig
     type 'a t = {  next : 'a Ivar.t }
   end =
     Tail

   type 'a ivar
   and forwarding = | Parent of monitor
   and 'a tail = { next : 'a ivar }
   and monitor = { name : info
   ; mutable next_error : exn ivar
   ; mutable tails_for_all_errors : exn tail
   ; mutable forwarding : forwarding
   }
end

type t =
  { name : info
  ; mutable next_error : exn Types.Ivar.t
  ; mutable tails_for_all_errors : exn Types.Tail.t
  ; mutable forwarding : Types.Forwarding.t
  }
[%%expect {|
type info
module Types :
  sig
    module rec Ivar : sig type 'a t end
    and Forwarding : sig type t = Parent of Monitor.t end
    and Monitor :
      sig
        type t = {
          name : info;
          mutable next_error : exn Ivar.t;
          mutable tails_for_all_errors : exn Tail.t;
          mutable forwarding : Forwarding.t;
        }
      end
    and Tail : sig type 'a t = { next : 'a Ivar.t; } end
    type 'a ivar
    and forwarding = Parent of monitor
    and 'a tail = { next : 'a ivar; }
    and monitor = {
      name : info;
      mutable next_error : exn ivar;
      mutable tails_for_all_errors : exn tail;
      mutable forwarding : forwarding;
    }
  end
type t = {
  name : info;
  mutable next_error : exn Types.Ivar.t;
  mutable tails_for_all_errors : exn Types.Tail.t;
  mutable forwarding : Types.Forwarding.t;
}
|}]

type 'a t : immutable_data = Leaf | Node of int * 'a t
[%%expect {|
type 'a t = Leaf | Node of int * 'a t
|}]

type 'a mutable_list : mutable_data  = Nil | Cons of int ref * 'a mutable_list
[%%expect {|
type 'a mutable_list = Nil | Cons of int ref * 'a mutable_list
|}]

type t1
type 'a t2 : immutable_data with 'a with t1 = Leaf of 'a | Node of 'a * t1 t2
(* CR layouts v2.8: this should be accepted *)
[%%expect {|
type t1
Line 2, characters 0-77:
2 | type 'a t2 : immutable_data with 'a with t1 = Leaf of 'a | Node of 'a * t1 t2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data with 'a @@ global aliased
         with t1 @@ global aliased with t1 t2 @@ global aliased
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immutable_data with 'a
         with t1
         because of the annotation on the declaration of the type t2.
|}]
