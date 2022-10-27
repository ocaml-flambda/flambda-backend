(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Projection : sig
  type t = private
    | Block_load of { index : Targetint_31_63.t }
    | Project_value_slot of
        { project_from : Function_slot.t;
          value_slot : Value_slot.t;
          kind : Flambda_kind.With_subkind.t
        }

  val block_load : index:Targetint_31_63.t -> t

  val project_value_slot :
    Function_slot.t -> Value_slot.t -> Flambda_kind.With_subkind.t -> t
end

type t

val print : Format.formatter -> t -> unit

val create : Symbol.t -> Projection.t -> t

val symbol : t -> Symbol.t

val projection : t -> Projection.t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
