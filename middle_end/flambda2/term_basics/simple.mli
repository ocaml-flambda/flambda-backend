(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A value that is known to fit into a register (of the appropriate kind) on
    the target machine. We do not require such values to be [Let]-bound. *)

include module type of struct
  include Int_ids.Simple
end

include Contains_names.S with type t := t

val has_coercion : t -> bool

val apply_coercion : t -> Coercion.t -> t option

val apply_coercion_exn : t -> Coercion.t -> t

val without_coercion : t -> t

val must_be_var : t -> (Variable.t * Coercion.t) option

val must_be_symbol : t -> (Symbol.t * Coercion.t) option

val must_be_name : t -> (Name.t * Coercion.t) option

(** The constant representating the given number of type "int". *)
val const_int : Targetint_31_63.t -> t

(** The constant representating the given boolean value. *)
val const_bool : bool -> t

(** The naked immediate constant representating the given boolean value. *)
val untagged_const_bool : bool -> t

(** The constant representating boolean true. *)
val const_true : t

val untagged_const_true : t

(** The constant representating boolean false. *)
val const_false : t

val untagged_const_false : t

(** The constant representating the number zero of type "int". *)
val const_zero : t

val untagged_const_zero : t

val untagged_const_int : Targetint_31_63.t -> t

val const_one : t

(** The constant representing the unit value. *)
val const_unit : t

val const_from_descr : Reg_width_const.Descr.t -> t

val dummy_const : Flambda_kind.t -> t

val is_const : t -> bool

val is_symbol : t -> bool

val is_var : t -> bool

val is_imported_or_constant : t -> bool

val free_names_in_types : t -> Name_occurrences.t

val pattern_match' :
  t ->
  var:(Variable.t -> coercion:Coercion.t -> 'a) ->
  symbol:(Symbol.t -> coercion:Coercion.t -> 'a) ->
  const:(Reg_width_const.t -> 'a) ->
  'a

module List : sig
  type nonrec t = t list

  include Contains_names.S with type t := t

  include Container_types.S with type t := t
end

module With_kind : sig
  type nonrec t = t * Flambda_kind.t

  include Contains_names.S with type t := t

  include Container_types.S with type t := t
end
