(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 New parameters for lifted continuations}

    While lifting continuations, new parameters have to be introduced to
    replace variables that are in the lexical scope at the original location of
    the continuation being lifted, but are not at its destination.

    The tricky part with these parameters is to compute the adequate new
    arguments to apply at each call site. In simple situations, these new
    params are simply renamed versions of the original variables, and the
    arguments are the original variables. However, that stops being true if a
    call site is in fact in another continuation that is also being lifted, in
    which case the arg to provide is actually the new name of the new param in
    that last continuation.

    This is reasonably solved by mapping all new params to the original
    variable (i.e. the one that was in the lexical scope), and storing that
    mapping for each continuation. Then, for a given call site, for each new
    parameter, we can lookup the original variable that introduced it, and then
    look into the stack of parent continuations to see which one first defines
    a new (lifting) param that maps back to the same original variable. To avoid
    confusing the original variables and the renamed parameters, we instead use
    unique ids (which are in fact integers). *)

(** This type represents all of the new params for one lifted continuation.
    These added parameters are internally indexed by a unique identifier: when an individual
    new param is first created/added to a value of type [t], it is given a fresh identifier.
*)
type t

(** Print function. *)
val print : Format.formatter -> t -> unit

(** The empty set of lifted cont params. *)
val empty : t

(** Is the set of new params empty ? *)
val is_empty : t -> bool

(** Number of new params. *)
val length : t -> int

(** Add a new parameter *)
val new_param : t -> Bound_parameter.t -> t

(** Rename all new parameters, and returns the corresponding renaming. *)
val rename : t -> t * Renaming.t

(** This function computes all adequate args for
    a given callsite.  This takes the lifted cont params of the continuation
    being called, as well as the stack of the lifted params for all parent
    continuations at the callsite. *)
val args :
  callee_lifted_params:t -> caller_stack_lifted_params:t list -> Simple.t list

(** Returns the bound parameters for a given set of new params.
    The parameters returned by this function are in the same order as the
    arguments returned by the {args} function. *)
val bound_parameters : t -> Bound_parameters.t

(** Fold over all of the new parameters. The order in which the parameters are
    iterated through is **not** specified. *)
val fold : init:'a -> f:(Bound_parameter.t -> 'a -> 'a) -> t -> 'a
