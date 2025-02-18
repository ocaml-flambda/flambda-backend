(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things.

    {b Warning:} this module is unstable and part of
    {{!Compiler_libs}compiler-libs}. *)

open Container_types_intf

module type Thing_no_hash = Thing_no_hash

module type Thing = Thing

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type Set = Set

module Make_set (T : Thing_no_hash) : Set with type elt = T.t

module type Map = Map

module type S = S

module type S_plus_stdlib = S_plus_stdlib

module type Map_plus_iterator = Map_plus_iterator

module type S_plus_iterator = S_plus_iterator

module Make (T : Thing) : S_plus_stdlib with type t := T.t

module Make_pair (T1 : S) (T2 : S) : sig
  include S with type t := T1.t * T2.t

  val create_from_cross_product : T1.Set.t -> T2.Set.t -> Set.t
end
