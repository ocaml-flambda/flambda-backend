(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* CR rtjoa: Do all these tests with type parameters *)

(*************)
(* Shadowing *)

module type S = sig
  type t = { i : int }
end
[%%expect{|
module type S = sig type t = { i : int; } end
|}]

(* Shadowing a boxed record type also shadows the ghost unboxed record type *)
module type Shadowed = sig
  include S
  type t
end
[%%expect {|
module type Shadowed = sig type t end
|}]

(* ...so we can't reference the derived type... *)
module M(Shadowed : Shadowed) = struct
  type t = Shadowed.t#
end
[%%expect{|
Line 2, characters 11-22:
2 |   type t = Shadowed.t#
               ^^^^^^^^^^^
Error: "Shadowed.t" has no unboxed version.
|}]

(* ...or its labels. *)
module M(Shadowed : Shadowed) = struct
  let x = #{ i = 1 }
end
[%%expect{|
Line 2, characters 13-14:
2 |   let x = #{ i = 1 }
                 ^
Error: Unbound unboxed record field "i"
|}]

(*************************)
(* With type constraints *)

module type S = sig
  type t = { i : int }
  val dummy : int
  val f : t# -> t#
end

type t' = { i : int }
[%%expect{|
module type S =
  sig type t = { i : int; } val dummy : int val f : t# -> t# end
type t' = { i : int; }
|}]

module type S' = S with type t = t'
[%%expect{|
module type S' =
  sig type t = t' = { i : int; } val dummy : int val f : t# -> t# end
|}]

type t = { i : int ; j : int }


(* Check that the constraint was added (we can't check this by printing because
   t# is hidden) *)
module Check_constraints
  (M: sig
        type t = { i : int ; j : int }
      end with type t = t) =
struct
  let f : t -> M.t = fun x -> x
  let f : t# -> M.t# = fun x -> x
end
[%%expect{|
type t = { i : int; j : int; }
module Check_constraints :
  functor (M : sig type t = t = { i : int; j : int; } end) ->
    sig val f : t# -> M.t# end
|}]

(* alias gets hash type *)
module type S = sig
  type u = t
  and t
end with type t = float

module F(M : S) = struct
  type u = M.u#
end
[%%expect{|
module type S = sig type u = t and t = float end
module F : functor (M : S) -> sig type u = M.u# end
|}]


module type S = sig
  type u = t
  and t

  module M : sig
    type t = u
  end
end with type t = float
module F(M : S) = struct
  type u = M.u#
end
[%%expect{|
module type S =
  sig type u = t and t = float module M : sig type t = u end end
module F : functor (M : S) -> sig type u = M.u# end
|}]

(*******************************)
(* With type subst constraints *)

(* Unboxed number #-type *)

type t = float
module type S = sig
  type t_to_replace = float
  type th = t_to_replace#
end with type t_to_replace := t
[%%expect{|
type t = float
module type S = sig type th = t# end
|}]

module F (M : S) = struct
  let f : M.th -> float# = fun x -> x
  let g : M.th -> t# = fun x -> x
end
[%%expect{|
module F :
  functor (M : S) -> sig val f : M.th -> float# val g : M.th -> t# end
|}]

module Bad (M : S) = struct
  type bad = M.t_to_replace#
end
[%%expect{|
Line 2, characters 13-28:
2 |   type bad = M.t_to_replace#
                 ^^^^^^^^^^^^^^^
Error: Unbound type constructor "M.t_to_replace"
|}]

module type Bad = sig
  type t = float
end with type t := float#
[%%expect{|
Lines 1-3, characters 18-25:
1 | ..................sig
2 |   type t = float
3 | end with type t := float#
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = float#
       is not included in
         type t = float
       The type "float#" is not equal to the type "float"
|}]

module type S = sig
  type t : float64
  type fu = t
end with type t := float#
[%%expect{|
module type S = sig type fu = float# end
|}]

module type Bad = sig
  type t
end with type t := float#
[%%expect{|
Line 3, characters 9-25:
3 | end with type t := float#
             ^^^^^^^^^^^^^^^^
Error: The layout of type "float#" is float64
         because it is unboxed version of the primitive type float.
       But the layout of type "float#" must be a sublayout of value
         because of the definition of t at line 2, characters 2-8.
|}]

module type Bad = sig
  type t = int32#
end with type t := float#
[%%expect{|
Lines 1-3, characters 18-25:
1 | ..................sig
2 |   type t = int32#
3 | end with type t := float#
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = float#
       is not included in
         type t = int32#
       The type "float#" is not equal to the type "int32#"
|}]

module F (M : S) = struct
  let f : M.th -> float# = fun x -> x
  let g : M.th -> t# = fun x -> x
end
[%%expect{|
Line 2, characters 10-14:
2 |   let f : M.th -> float# = fun x -> x
              ^^^^
Error: Unbound type constructor "M.th"
|}]

module Bad (M : S) = struct
  type bad = M.t_to_replace#
end
[%%expect{|
Line 2, characters 13-28:
2 |   type bad = M.t_to_replace#
                 ^^^^^^^^^^^^^^^
Error: Unbound type constructor "M.t_to_replace"
|}]


module type S = sig
  type t = float
  type u = t#
end with type t := float
[%%expect{|
module type S = sig type u = float# end
|}]

module S : sig
  type t = float
  type u = t#
  val f : u -> t#
end with type u := float# = struct
  type t = float
  let f x = x
end
[%%expect{|
module S : sig type t = float val f : float# -> t# end
|}]

(* Implicit unboxed record #-type *)

type t = { i : int ; j : string }

module type S = sig
  type t_to_replace = { i : int ; j : string }

  type r = t_to_replace
  type ru = t_to_replace#
end with type t_to_replace := t
[%%expect{|
type t = { i : int; j : string; }
module type S = sig type r = t type ru = t# end
|}]

module CheckSubsted(M : S) = struct
  let f : t -> M.r = fun x -> x
  let f : t# -> M.ru = fun x -> x
end
[%%expect{|
module CheckSubsted : functor (M : S) -> sig val f : t# -> M.ru end
|}]

module Bad(M : S) = struct
  type bad = t_to_replace#
end
[%%expect{|
Line 2, characters 13-26:
2 |   type bad = t_to_replace#
                 ^^^^^^^^^^^^^
Error: Unbound type constructor "t_to_replace"
|}]

(* Check that order stays the same after replacing *)

type s' = { s : string }
type i' = { i : int }

module type s = sig
  type top
  and s = { s : string }
  and mid
  and i = { i : int }
  and bot
end with type s = s'
    with type i = i'
[%%expect {|
type s' = { s : string; }
type i' = { i : int; }
module type s =
  sig
    type top
    and s = s' = { s : string; }
    and mid
    and i = i' = { i : int; }
    and bot
  end
|}]

(**********************************************)
(* Shadowing behavior with predefined #-types *)

type orig_float = float
[%%expect{|
type orig_float = float
|}]

(* float# starts out as an unboxed float *)
type float64 : float64 = float#
[%%expect{|
type float64 = float#
|}]

(* it doesn't normally get shadowed by "float" *)
type float = int
type float64 : float64 = float#
[%%expect{|
type float = int
Line 2, characters 25-31:
2 | type float64 : float64 = float#
                             ^^^^^^
Error: "float" has no unboxed version.
|}]

(* but it gets shadowed by a "float" boxed record! *)
type float = { i : int ; j : int }
type ij : value & value = float#

(* and when we shadow "float", the predefined "float#" becomes visible again *)
type float = int
type float64 : float64 = float#
[%%expect{|
type float = { i : int; j : int; }
type ij = float#
type float = int
Line 6, characters 25-31:
6 | type float64 : float64 = float#
                             ^^^^^^
Error: "float" has no unboxed version.
|}]

(********************************************)
(* Illegal shadowing with predefined #-types*)

(* CR layouts v7.2: "float/2#" makes sense with the view of [#] as an operator,
   but we may aesthetically prefer "float#/2". *)
include struct type float = { a : int } end
type float = string
let x = #{ a = 1 }
[%%expect{|
type float = { a : int; }
type float = string
val x : float/2# = #{a = 1}
|}]

include struct type floot = { a : int } end
type floot = string
let x = #{ a = 1 }
[%%expect{|
type floot = { a : int; }
type floot = string
val x : floot/2# = #{a = 1}
|}]

(* Restore [float] for other tests *)
type float = orig_float
[%%expect{|
type float = orig_float
|}]

(*****************************************************)
(* Propagating constraints from recursive type decls *)

type ('a : value & float64) t
[%%expect{|
type ('a : value & float64) t
|}]

type s = r# t
and r = {x:int; y:float#}
[%%expect{|
type s = r# t
and r = { x : int; y : float#; }
|}]

type s = q t
and r = {x:int; y:float#}
and q = r#
[%%expect{|
type s = q t
and r = { x : int; y : float#; }
and q = r#
|}]

type s_bad = r# t
and r = {x:int; y:bool}
[%%expect{|
Line 2, characters 0-23:
2 | and r = {x:int; y:bool}
    ^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of r# is any & any
         because it is an unboxed record.
       But the layout of r# must be a sublayout of value & float64
         because of the definition of t at line 1, characters 0-29.
|}]

type s_bad = q t
and r = {x:int; y:bool}
and q = r#
[%%expect{|
Line 3, characters 0-10:
3 | and q = r#
    ^^^^^^^^^^
Error:
       The layout of q is any & any
         because it is an unboxed record.
       But the layout of q must be a sublayout of value & float64
         because of the definition of t at line 1, characters 0-29.
|}]

(****************************)
(* Module type substitution *)

(* With type constraint *)

(* Unboxed number *)
module type S = sig
  module type x
  module M:x
end
with module type x = sig type t = float end
module F (M : S) = struct
  let id : M.M.t# -> float# = fun x -> x
end
[%%expect{|
module type S = sig module type x = sig type t = float end module M : x end
module F : functor (M : S) -> sig val id : M.M.t# -> float# end
|}]

(* Unboxed record *)
type r = { i : int }
module type S = sig
  module type x
  module M:x
end
with module type x = sig type t = r end
module F (M : S) = struct
  let id : M.M.t# -> r# = fun x -> x
end
[%%expect{|
type r = { i : int; }
module type S = sig module type x = sig type t = r end module M : x end
module F : functor (M : S) -> sig val id : M.M.t# -> r# end
|}]

(* No unboxed version *)
module type S = sig
  module type x
  module M:x
end
with module type x = sig type t = int end
module Bad (M : S) = struct
  let id : M.M.t# -> r# = fun x -> x
end
[%%expect{|
module type S = sig module type x = sig type t = int end module M : x end
Line 7, characters 11-17:
7 |   let id : M.M.t# -> r# = fun x -> x
               ^^^^^^
Error: "M.M.t" has no unboxed version.
|}]

(* Destructive substition *)

(* Unboxed number *)
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = float end
module F (M : S) = struct
  let id : M.M.t# -> float# = fun x -> x
  type u : float64 = M.M.t#
end
[%%expect{|
module type S = sig module M : sig type t = float end end
module F :
  functor (M : S) -> sig val id : M.M.t# -> float# type u = M.M.t# end
|}]

(* Unboxed record *)
type r = { i : int ; j : int }
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = r end
module F (M : S) = struct
  let id : M.M.t# -> r# = fun x -> x
  type u : immediate & immediate = M.M.t#
end
[%%expect{|
type r = { i : int; j : int; }
module type S = sig module M : sig type t = r end end
module F : functor (M : S) -> sig val id : M.M.t# -> r# type u = M.M.t# end
|}]

(* No unboxed version *)
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = int end
module Bad (M : S) = struct
  type u = M.M.t#
end
[%%expect{|
module type S = sig module M : sig type t = int end end
Line 7, characters 11-17:
7 |   type u = M.M.t#
               ^^^^^^
Error: "M.M.t" has no unboxed version.
|}]

(***********************)
(* Package constraints *)

(* Unboxed number *)
module type S = sig type t end
type m = (module S with type t = float)
module F (X : sig val x : m end) = struct
  module M = (val X.x)
  let id : M.t# -> float# = fun x -> x
  type u : float64 = M.t#
end
[%%expect{|
module type S = sig type t end
type m = (module S with type t = float)
module F :
  functor (X : sig val x : m end) ->
    sig
      module M : sig type t = float end
      val id : M.t# -> float#
      type u = M.t#
    end
|}]

(* Unboxed record *)
module type S = sig type t end
type r = { i : int; j : int }
type m = (module S with type t = r)
module F (X : sig val x : m end) = struct
  module M = (val X.x)
  let id : M.t# -> r# = fun x -> x
  type u : immediate & immediate = M.t#
end
[%%expect{|
module type S = sig type t end
type r = { i : int; j : int; }
type m = (module S with type t = r)
module F :
  functor (X : sig val x : m end) ->
    sig module M : sig type t = r end val id : M.t# -> r# type u = M.t# end
|}]


(* No unboxed version *)
module type S = sig type t end
type m = (module S with type t = int)
module Bad (X : sig val x : m end) = struct
  module M = (val X.x)
  type u = M.t#
end
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
Line 5, characters 11-15:
5 |   type u = M.t#
               ^^^^
Error: "M.t" has no unboxed version.
|}]
