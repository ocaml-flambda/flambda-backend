(* TEST
 * expect
*)

(* You may constrain abstract types in packages. *)
module type S = sig
  type t
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
|}];;

(* You may use variables in the current environment in the new definitions. *)
module type S = sig
  type t
end

type 'a m = (module S with type t = 'a);;
[%%expect{|
module type S = sig type t end
type 'a m = (module S with type t = 'a)
|}];;

(* It works with non-trivial paths. *)
module type S = sig
  module M : sig
    type t
  end
end

type m = (module S with type M.t = int)
[%%expect{|
module type S = sig module M : sig type t end end
type m = (module S with type M.t = int)
|}];;

(* It should respect immediacy - [m1] should typecheck but not [m2]. *)
module type S = sig
  type t [@@immediate]
end

type m1 = (module S with type t = int)
type m2 = (module S with type t = string);;
[%%expect{|
module type S = sig type t : immediate end
type m1 = (module S with type t = int)
Line 6, characters 10-41:
6 | type m2 = (module S with type t = string);;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       The layout of the first is value, because
         it is the primitive value type string.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-22.
|}];;

(* You may not constrain types with a manifest in a package *)
module type S = sig
  type t = int
end

type m = (module S with type t = string);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-40:
5 | type m = (module S with type t = string);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be int.
       Package `with' constraints may only be used on abstract types.
|}];;

(* Even if your constraint would be satisfied. *)
(* It would be nice if this worked. *)
module type S = sig
  type t = int
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-37:
5 | type m = (module S with type t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be int.
       Package `with' constraints may only be used on abstract types.
|}];;

(* And even if the manifest is not obvious in the original definition. *)
module M = struct
  type t
end

module type S = sig
  module P = M
end

type m = (module S with type P.t = int);;
[%%expect{|
module M : sig type t end
module type S = sig module P = M end
Line 9, characters 9-39:
9 | type m = (module S with type P.t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type P.t is defined to be M.t.
       Package `with' constraints may only be used on abstract types.
|}];;

(* If writing a package constraint in a mutually recursive group of type decls,
   checking that the constraint's immediacy may rely on the definitions of other
   elements of the mutually recursive group. *)
module type S = sig
  type t [@@immediate]
end

type t1 = int
and t2 = (module S with type t = t1);;
[%%expect{|
module type S = sig type t : immediate end
type t1 = int
and t2 = (module S with type t = t1)
|}];;

module type S = sig
  type t [@@immediate]
end

type t1 = (module S with type t = t2)
and t2 = string;;
[%%expect{|
module type S = sig type t : immediate end
Line 6, characters 0-15:
6 | and t2 = string;;
    ^^^^^^^^^^^^^^^
Error:
       The layout of t2 is value, because
         it is the primitive value type string.
       But the layout of t2 must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-22.
|}];;

(* Though this sometimes fails if the check would require particularly clever
   inference around typedecl parameters. See also Test 41 in
   [tests/typing-layouts/basics.ml]. *)
(* It would be nice if this worked. *)
module type S = sig
  type t [@@immediate]
end

type t1 = (module S with type t = int t2)
and 'a t2 = 'a;;
[%%expect{|
module type S = sig type t : immediate end
Line 6, characters 0-14:
6 | and 'a t2 = 'a;;
    ^^^^^^^^^^^^^^
Error: Layout mismatch in checking consistency of mutually recursive groups.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a t2 is value, because
           it instantiates an unannotated type parameter of t2, defaulted to layout value.
         But the layout of 'a t2 must be a sublayout of immediate, because
           of the definition of t at line 2, characters 2-22.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

(* If the previous example ever starts checking, this one still should not. *)
module type S = sig
  type t [@@immediate]
end

type t1 = (module S with type t = string t2)
and 'a t2 = 'a;;
[%%expect{|
module type S = sig type t : immediate end
Line 6, characters 0-14:
6 | and 'a t2 = 'a;;
    ^^^^^^^^^^^^^^
Error: Layout mismatch in checking consistency of mutually recursive groups.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a t2 is value, because
           it instantiates an unannotated type parameter of t2, defaulted to layout value.
         But the layout of 'a t2 must be a sublayout of immediate, because
           of the definition of t at line 2, characters 2-22.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;


(* When using a package with constraint to give an abstract type a definition
   that is immediate, that immediacy information should be usable after
   unpacking. *)
module type S = sig
  type t
end

type m = (module S with type t = int)

module F (X : sig val x : m end) = struct
  module M = (val X.x)
  type t = M.t [@@immediate]
end;;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
module F :
  functor (X : sig val x : m end) ->
    sig module M : sig type t = int end type t = M.t end
|}];;
