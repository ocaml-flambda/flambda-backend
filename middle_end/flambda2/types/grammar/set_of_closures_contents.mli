(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Descriptions of the entities inside sets of closures: - closures; - closure
    variables. These descriptions do not necessarily describe the entire
    contents of any particular set of closures. *)

type t

val create : Function_slot.Set.t -> Value_slot.Set.t -> t

include Container_types.S with type t := t

val subset : t -> t -> bool

val inter : t -> t -> t

val union : t -> t -> t

val closures : t -> Function_slot.Set.t

val value_slots : t -> Value_slot.Set.t

include Contains_names.S with type t := t

val remove_unused_value_slots : t -> used_value_slots:Value_slot.Set.t -> t

module With_function_slot : sig
  type nonrec t = Function_slot.t * t

  include Container_types.S with type t := t
end

module With_function_slot_or_unknown : sig
  type nonrec t = Function_slot.t Or_unknown.t * t

  include Container_types.S with type t := t
end
