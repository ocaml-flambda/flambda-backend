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

(** A parameter (to a function, continuation, etc.) together with its kind. *)
type t

(** Create a kinded parameter. *)
val create :
  Variable.t -> Flambda_kind.With_subkind.t -> Flambda_debug_uid.t -> t

(** The underlying variable. *)
val var : t -> Variable.t

val var_and_uid : t -> Variable.t * Flambda_debug_uid.t

val name : t -> Name.t

(** As for [var], but returns a [Simple.t] describing the variable. *)
val simple : t -> Int_ids.Simple.t

(** The kind of the given parameter. *)
val kind : t -> Flambda_kind.With_subkind.t

(** Replace the kind of the given parameter. *)
val with_kind : t -> Flambda_kind.With_subkind.t -> t

val rename : t -> t

val is_renamed_version_of : t -> t -> bool

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
