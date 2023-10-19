(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
type t =
  | Symbol of Symbol.t  (** The address of the given symbol. *)
  | Tagged_immediate of Targetint_31_63.t  (** The given tagged immediate. *)
  | Dynamically_computed of Variable.t * Debuginfo.t
      (** The value of the given variable. This has [Debuginfo.t] for the same
          reason as the [Or_variable.Var] constructor does. *)

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val tagged_immediate : Targetint_31_63.t -> t
