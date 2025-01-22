(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(*************)
(* Shadowing *)

module type S = sig
  type t = { i : int }
end
[%%expect{|
module type S = sig type t = { i : int; } end
|}]

(* Shadowing a boxed record type also shadows the ghost unboxed record type... *)
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

(* Check that the constraint was added (we can't check this by printing because
   t# is hidden) *)
type t = { i : int ; j : int }

module type S = sig
  type t = { i : int ; j : int }
end
with type t = t

(* This compiles if the constraints were added *)
module Check_constraints(M:S) = struct
  let f : t -> M.t = fun x -> x
  let f : t# -> M.t# = fun x -> x
end
[%%expect{|
type t = { i : int; j : int; }
module type S = sig type t = t = { i : int; j : int; } end
module Check_constraints : functor (M : S) -> sig val f : t# -> M.t# end
|}]

(*******************************)
(* With type subst constraints *)

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

module CheckCantAccessOldGhost(M : S) = struct
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


(*****************************************************)
(* Propagating constraints from recursive type decls *)

type ('a : value & float64) t
[%%expect{|
type ('a : value & float64) t
|}]

type s = r# t
and r = {x:int; y:float#}
[%%expect{|
Line 2, characters 18-24:
2 | and r = {x:int; y:float#}
                      ^^^^^^
Error: "float" has no unboxed version.
|}]

type s = q t
and r = {x:int; y:float#}
and q = r#
[%%expect{|
Line 2, characters 18-24:
2 | and r = {x:int; y:float#}
                      ^^^^^^
Error: "float" has no unboxed version.
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
