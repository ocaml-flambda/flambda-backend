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
    register width (e.g. 64-bit integer machine word width, or 128-bit SIMD
    machine word width).  Variables from Lambda which cannot be accommodated in
    one register, for example if they are of an unboxed product layout, are
    split by a process called unarization.

    Despite this, [`Complex] arities preserve the information about any unboxed
    products, for later use (e.g. during Cmm translation to optimize
    caml_apply).
*)

type _ t

(** Print an arity to a formatter. *)
val print : Format.formatter -> _ t -> unit

module Component_for_creation : sig
  type _ t =
    | Singleton : Flambda_kind.With_subkind.t -> [> ] t
      (* The nullary unboxed product is called "void". It is important to
         propagate information about void layouts, even though the corresponding
         variables have no runtime representation, as they interact with
         currying. (For example void parameters cannot just be erased.) *)
    | Unboxed_product : _ t list -> [`Complex] t

  (** Conversion from a Lambda layout, which might involve unboxed products. *)
  val from_lambda : Lambda.layout -> [`Complex] t
end

(** One component per function or continuation parameter, for example. Each
    component may in turn have an arity describing an unboxed product. *)
val create : 'uc Component_for_creation.t list -> 'uc t

(** A shorthand for creation of arities where no parameter has an unboxed
    product layout. *)
val create_singletons : Flambda_kind.With_subkind.t list -> [> ] t

(** "No parameters".  (Not e.g. "one parameter of type void".) *)
val nullary : [> `Unarized] t

(** How many parameters, potentially of unboxed product layout, the given
    arity describes. *)
val num_params : _ t -> int

(** Comparison of arities, but ignoring subkind information. *)
val equal_ignoring_subkinds : 'a t -> 'a t -> bool

(** It's usually a mistake to use this function, but it's needed for
    [Compare].  This is like [equal_ignoring_subkinds] but also insists on
    equality of subkind information. *)
val equal_exact : 'a t -> 'a t -> bool

(** Returns [true] iff the given arity describes one parameter of kind [Value].
    (Note that this function does not look inside unboxed products, not even
    ones with a single component.) *)
val is_one_param_of_kind_value : _ t -> bool

(** Convert, in a left-to-right depth-first order, an arity into a flattened
    list of kinds for all parameters. *)
val unarize : [`Complex] t -> Flambda_kind.With_subkind.t list

(** Like [unarize] but works on arities that are known not to contain any
    unboxed products. *)
val unarized_components : [`Unarized] t -> Flambda_kind.With_subkind.t list

(** Like [unarize] but returns one list per parameter. *)
val unarize_per_parameter :
  [`Complex] t -> Flambda_kind.With_subkind.t list list

(** Like [unarize] but returns a value of type [t]. *)
val unarize_t : [`Complex] t -> [`Unarized] t

(** Given an arity and an identifier, produce a list of identifiers (with
    corresponding kinds) whose length matches [unarize t], with names derived
    from the given identifier. *)
val fresh_idents_unarized :
  _ t -> id:Ident.t -> (Ident.t * Flambda_kind.With_subkind.t) list

(** The length of the list returned by [unarize]. *)
val cardinal_unarized : _ t -> int

(** Take a list of Lambda layouts, one per parameter, and form the
    corresponding arity. *)
val from_lambda_list : Lambda.layout list -> [`Complex] t

(** Remove the first portion of an arity to correspond to a partial
    application (or other similar situation). *)
val partially_apply :
  [`Complex] t -> num_non_unarized_params_provided:int -> [`Complex] t

(** Concatenation of two arities, usually used to add information about more
    parameters to an existing arity. *)
val concat : 'a t -> 'a t -> 'a t
