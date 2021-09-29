(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Simple := Reg_width_things.Simple

(* CR mshinwell: Rename to [Parameter] *)

(** A parameter (to a function, continuation, etc.) together with its kind. *)
type t

include Bindable.S with type t := t

(** Create a kinded parameter. *)
val create : Variable.t -> Flambda_kind.With_subkind.t -> t

(** The underlying variable. *)
val var : t -> Variable.t

val name : t -> Name.t

(** As for [var], but returns a [Simple.t] describing the variable. *)
val simple : t -> Simple.t

(** The kind of the given parameter. *)
val kind : t -> Flambda_kind.With_subkind.t

(** Replace the kind of the given parameter. *)
val with_kind : t -> Flambda_kind.With_subkind.t -> t

(* CR mshinwell: We should use [Name.t] underneath *)

(** Returns [true] iff the provided kinded parameters have the same kind and
    subkind. *)
val equal_kinds : t -> t -> bool

val rename : t -> t

module List : sig
  type nonrec t = t list

  include Contains_names.S with type t := t

  val create : (Variable.t * Flambda_kind.With_subkind.t) list -> t

  (** As for [Variable.List.vars]. *)
  val vars : t -> Variable.t list

  (** As for [vars] but returns a list of [Simple.t] values describing the
      variables. *)
  val simples : t -> Simple.t list

  (** As for [vars] but returns a set. *)
  val var_set : t -> Variable.Set.t

  (** As for [var_set] but returns a set of [Name]s. *)
  val name_set : t -> Name.Set.t

  val equal_vars : t -> Variable.t list -> bool

  val rename : t -> t

  val arity : t -> Flambda_arity.t

  val arity_with_subkinds : t -> Flambda_arity.With_subkinds.t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end
