(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2023 OCamlPro SAS                                    *)
(*   Copyright 2014--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Arities are used to describe the layouts of things like function and
    continuation parameter lists.

    In Flambda 2, variables are always assigned kinds, which are at most
    register width (presently machine word width, but in the future of SIMD
    widths too).  Variables from Lambda which cannot be accommodated in one
    register, for example if they are of an unboxed product layout, are split
    by a process called unarization.

    Despite this, the arities preserve the information about any unboxed
    products, for later use (e.g. during Cmm translation to optimize
    caml_apply).
*)

type t

module Component_for_creation : sig
  type t =
    | Singleton of Flambda_kind.With_subkind.t
      (* The nullary unboxed product is called "void". It is important to
         propagate information about void layouts, even though the corresponding
         variables have no runtime representation, as they interact with
         currying. *)
    | Unboxed_product of t list

  val from_lambda : Lambda.layout -> t
end

(** One component per function or continuation parameter, for example. Each
    component may in turn have an arity describing an unboxed product. *)
val create : Component_for_creation.t list -> t

val create_singletons : Flambda_kind.With_subkind.t list -> t

(** "No parameters".  (Not e.g. "one parameter of type void".) *)
val nullary : t

val print : Format.formatter -> t -> unit

val equal_ignoring_subkinds : t -> t -> bool

(* It's usually a mistake to use this function, but it's needed for
   [Compare]. *)
val equal_exact : t -> t -> bool

val is_one_param_of_kind_value : t -> bool

val must_be_one_param : t -> Flambda_kind.With_subkind.t option

module Component : sig
  type t = private
    | Singleton of Flambda_kind.With_subkind.t
    | Unboxed_product of t list
end

(** Converts, in a left-to-right depth-first order, an arity into a flattened
    list of kinds for all parameters. *)
val unarize : t -> Flambda_kind.With_subkind.t list

(** The length of the list returned by [unarize]. *)
val cardinal_unarized : t -> int
