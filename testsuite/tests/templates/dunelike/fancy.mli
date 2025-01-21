open! Fancy__
open! No_direct_access_to_fancy

module Flourish = Flourish
module Ornament = Ornament

(* Re-export this alias so we can test it from a non-parameterised module *)
module PI = PI

(* Re-export this alias so we can test it from a less-parameterised module.
   Notably, [Util] only takes [P], not [Q], but this won't be checked here
   since this is an alias with [-no-alias-deps] on. *)
module Util = Util

type t

val create : P.t -> Flourish.t -> Ornament.t -> t
val basic : t -> Basic.t
val to_string : t -> string
