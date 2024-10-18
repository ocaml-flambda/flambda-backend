(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Richard Eisenberg, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Note [Allowance]
   ~~~~~~~~~~~~~~~~

   Each variable mode has its allowance encoded in its type as [l * r], where
   [l] and [r] could be [allowed] or [disallowed]. The typing of [submode a b]
   requires [a] to have [l = allowed] and [b] to have [r = allowed]. This
   encoding is useful for two reasons.

   The first is to represent the availability of adjoints. Applying morphism [f]
   to variable mode [m] gives you a variable mode [f m]. If [f] doesn't have
   a right adjoint, the solver will not be able to handle [submode] request
   where [f m] is on the LHS. To statically prevent such invocation of
   [submode], we should type [f m] as [l=disallowed]. Therefore, we extend the
   allowance encoding to morphisms to indicate the existence of left/right
   adjoint. The [f] in the example will have [l=disallowed], and via [apply]
   will make [f m] to be [l=disallowed] as well.

   In addition, recall that a "lower" mode is stronger than a "higher" mode.
   Therefore, the user of the solver typically calls [submode a b] where [a] is
   the _actual_ mode of a value, and [b] the _expected_ mode of the value. The
   caller can encode the distinction by typing [a] as [left_only] and [b] as
   [right_only], preventing misuse statically. They might use [Allow_disallow]
   below to explicitly weaken variable modes for safety.
*)

type allowed = private Allowed

type disallowed = private Disallowed

type left_only = allowed * disallowed

type right_only = disallowed * allowed

type both = allowed * allowed

module type Allow_disallow = sig
  type ('a, 'b, 'd) sided constraint 'd = 'l * 'r

  (** Disallows on the right.  *)
  val disallow_right :
    ('a, 'b, 'l * 'r) sided -> ('a, 'b, 'l * disallowed) sided

  (** Disallows a the left.  *)
  val disallow_left : ('a, 'b, 'l * 'r) sided -> ('a, 'b, disallowed * 'r) sided

  (** Generalizes a right-hand-side [allowed] to be any allowance.  *)
  val allow_right : ('a, 'b, 'l * allowed) sided -> ('a, 'b, 'l * 'r) sided

  (** Generalizes a left-hand-side [allowed] to be any allowance.  *)
  val allow_left : ('a, 'b, allowed * 'r) sided -> ('a, 'b, 'l * 'r) sided
end

(** Takes a slow but type-correct [Allow_disallow] module and returns the
    magic version, which is faster.
    NOTE: for this to be sound, the functions in the original module must be
    identity functions (up to runtime representation). *)
module Magic_allow_disallow (X : Allow_disallow) :
  Allow_disallow with type ('a, 'b, 'd) sided = ('a, 'b, 'd) X.sided
