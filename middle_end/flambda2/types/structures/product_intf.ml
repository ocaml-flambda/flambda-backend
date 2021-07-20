(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S_base = sig
  type t

  type flambda_type
  type typing_env
  type meet_env
  type typing_env_extension

  module Index : Container_types.S

  val create_top : Flambda_kind.t -> t

  val width : t -> Targetint_31_63.Imm.t

  val components : t -> flambda_type list

  val map_types
     : t
    -> f:(flambda_type -> flambda_type Or_bottom.t)
    -> t Or_bottom.t

  val project : t -> Index.t -> flambda_type Or_unknown.t

  include Type_structure_intf.S
    with type t := t
    with type flambda_type := flambda_type
    with type typing_env := typing_env
    with type meet_env := meet_env
    with type typing_env_extension := typing_env_extension
end

module type S = sig
  include S_base

  (** Create a product value given the indexes with associated components. *)
  val create : Flambda_kind.t -> flambda_type Index.Map.t -> t

  val to_map : t -> flambda_type Index.Map.t
end

module type Index = sig
  include Container_types.S

  val remove_on_import : t -> Renaming.t -> bool
end
