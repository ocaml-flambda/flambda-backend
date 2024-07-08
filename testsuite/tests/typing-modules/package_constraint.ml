<<<<<<< HEAD
(* TEST
 expect;
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

(* Checking such a constraint may require expanding definitions from the module
   being updated. *)
module type S = sig
  module type S1 = sig
    type t
  end
  module M : S1
end

type t = (module S with type M.t = int)
[%%expect{|
module type S = sig module type S1 = sig type t end module M : S1 end
type t = (module S with type M.t = int)
|}];;

(* Ghosts haunted type definitions *)
module type Private_row = sig
  type a
  and t = private [< `A | `B ]
  and b
  and d = private [< `C ]
end

(* This is fine, the ghost type `t#row` is removed silently *)
module type Test = Private_row with type t = [ `A ]

(* This fails currently.  If we ever allow it, make sure the ghost type is
   removed as above. *)
type fail = (module Private_row with type t = [ `A ] )

[%%expect{|
module type Private_row =
  sig type a and t = private [< `A | `B ] and b and d = private [< `C ] end
module type Test =
  sig type a and t = [ `A ] and b and d = private [< `C ] end
Line 13, characters 12-54:
13 | type fail = (module Private_row with type t = [ `A ] )
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be [< `A | `B ].
       Package `with' constraints may only be used on abstract types.
|}]

(* More row type examples to consider, if we ever start allowing package type
   constraints to replace compatible manifests. *)
module type Private_row = sig
  type t = private [< `A ]
end

type t1 = (module Private_row with type t = [ `A ])
[%%expect{|
module type Private_row = sig type t = private [< `A ] end
Line 5, characters 10-51:
5 | type t1 = (module Private_row with type t = [ `A ])
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be [< `A ].
       Package `with' constraints may only be used on abstract types.
|}]

type t2 = (module Private_row with type t = [< `A ])
[%%expect{|
Line 1, characters 10-52:
1 | type t2 = (module Private_row with type t = [< `A ])
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be [< `A ].
       Package `with' constraints may only be used on abstract types.
|}]

type 'a t3 = (module Private_row with type t = [< `A ]) as 'a
[%%expect{|
Line 1, characters 13-55:
1 | type 'a t3 = (module Private_row with type t = [< `A ]) as 'a
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be [< `A ].
       Package `with' constraints may only be used on abstract types.
|}]

type 'a t4 = (module Private_row with type t = [< `A ] as 'a)
[%%expect{|
Line 1, characters 13-61:
1 | type 'a t4 = (module Private_row with type t = [< `A ] as 'a)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type t is defined to be [< `A ].
       Package `with' constraints may only be used on abstract types.
|}]

(* In cases where the package constraint involves a type variable from the
   current ty var env, it can affect inference.  t1 below should check but not
   t2. *)
module type S = sig
  type t : immediate
end

type 'a t = (module S with type t = 'a)

type t1 = int t
type t2 = string t
[%%expect{|
module type S = sig type t : immediate end
type ('a : immediate) t = (module S with type t = 'a)
type t1 = int t
Line 8, characters 10-16:
8 | type t2 = string t
              ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of t at line 5, characters 0-39.
|}];;
||||||| 121bedcfd2
=======
(* TEST
 expect;
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
module type S = sig type t [@@immediate] end
type m1 = (module S with type t = int)
Line 6, characters 10-41:
6 | type m2 = (module S with type t = string);;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
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
Error: In the constrained signature, type "t" is defined to be "int".
       Package "with" constraints may only be used on abstract types.
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
Error: In the constrained signature, type "t" is defined to be "int".
       Package "with" constraints may only be used on abstract types.
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
Error: In the constrained signature, type "P.t" is defined to be "M.t".
       Package "with" constraints may only be used on abstract types.
|}];;

(* If writing a package constraint in a mutually recursive group of type decls,
   checking that the constraint's immediacy may not rely on the definitions of
   other elements of the mutually recursive group. *)
(* It would be nice if this worked. *)
module type S = sig
  type t [@@immediate]
end

type t1 = int
and t2 = (module S with type t = t1);;
[%%expect{|
module type S = sig type t [@@immediate] end
Line 6, characters 9-36:
6 | and t2 = (module S with type t = t1);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = t1
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
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
    sig module M : sig type t = int end type t = M.t [@@immediate] end
|}];;

(* Checking such a constraint may require expanding definitions from the module
   being updated. *)
module type S = sig
  module type S1 = sig
    type t
  end
  module M : S1
end

type t = (module S with type M.t = int)
[%%expect{|
module type S = sig module type S1 = sig type t end module M : S1 end
type t = (module S with type M.t = int)
|}];;

(* Ghosts haunted type definitions *)
module type Private_row = sig
  type a
  and t = private [< `A | `B ]
  and b
  and d = private [< `C ]
end

(* This is fine, the ghost type `t#row` is removed silently *)
module type Test = Private_row with type t = [ `A ]

(* This fails currently.  If we ever allow it, make sure the ghost type is
   removed as above. *)
type fail = (module Private_row with type t = [ `A ] )

[%%expect{|
module type Private_row =
  sig type a and t = private [< `A | `B ] and b and d = private [< `C ] end
module type Test =
  sig type a and t = [ `A ] and b and d = private [< `C ] end
Line 13, characters 12-54:
13 | type fail = (module Private_row with type t = [ `A ] )
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type "t" is defined to be "[< `A | `B ]".
       Package "with" constraints may only be used on abstract types.
|}]

(* More row type examples to consider, if we ever start allowing package type
   constraints to replace compatible manifests. *)
module type Private_row = sig
  type t = private [< `A ]
end

type t1 = (module Private_row with type t = [ `A ])
[%%expect{|
module type Private_row = sig type t = private [< `A ] end
Line 5, characters 10-51:
5 | type t1 = (module Private_row with type t = [ `A ])
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type "t" is defined to be "[< `A ]".
       Package "with" constraints may only be used on abstract types.
|}]

type t2 = (module Private_row with type t = [< `A ])
[%%expect{|
Line 1, characters 10-52:
1 | type t2 = (module Private_row with type t = [< `A ])
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type "t" is defined to be "[< `A ]".
       Package "with" constraints may only be used on abstract types.
|}]

type 'a t3 = (module Private_row with type t = [< `A ]) as 'a
[%%expect{|
Line 1, characters 13-55:
1 | type 'a t3 = (module Private_row with type t = [< `A ]) as 'a
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type "t" is defined to be "[< `A ]".
       Package "with" constraints may only be used on abstract types.
|}]

type 'a t4 = (module Private_row with type t = [< `A ] as 'a)
[%%expect{|
Line 1, characters 13-61:
1 | type 'a t4 = (module Private_row with type t = [< `A ] as 'a)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the constrained signature, type "t" is defined to be "[< `A ]".
       Package "with" constraints may only be used on abstract types.
|}]
>>>>>>> 5.2.0
