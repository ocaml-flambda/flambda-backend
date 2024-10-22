(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Partition : sig
  type t

  module Set : Stdlib.Set.S with type elt = t

  val to_cu : t -> Compilation_unit.t

  val to_string : t -> string

  val name : t -> string
end

(** Generate generic functions *)
module Tbl : sig
  type t

  val make : unit -> t

  val add :
    imports:Partition.Set.t -> t -> Cmx_format.generic_fns -> Partition.Set.t

  val of_fns : Cmx_format.generic_fns -> t

  val entries : t -> Cmx_format.generic_fns
end

module Cache : sig
  type send =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Cmx_format.alloc_mode

  type apply =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Cmx_format.alloc_mode

  type curry =
    Lambda.function_kind
    * Cmm.machtype_component array list
    * Cmm.machtype_component array

  val mem_send : send -> bool

  val mem_apply : apply -> bool

  val mem_curry : curry -> bool

  val all : unit -> (Partition.t, Tbl.t) Hashtbl.t
end

val compile : shared:bool -> Tbl.t -> Cmm.phrase list

val imported_units : Partition.Set.t -> Compilation_unit.t list
