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

(** Generate generic functions *)
module Tbl : sig
  type t

  val make : unit -> t

  val add : t -> Cmx_format.generic_fns -> unit

  val of_fns : Cmx_format.generic_fns -> t

  val entries : t -> Cmx_format.generic_fns
end

module Cache : sig
  type send =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Lambda.locality_mode

  type apply =
    Cmm.machtype_component array list
    * Cmm.machtype_component array
    * Lambda.locality_mode

  type curry =
    Lambda.function_kind
    * Cmm.machtype_component array list
    * Cmm.machtype_component array

  val mem_send : send -> bool

  val mem_apply : apply -> bool

  val mem_curry : curry -> bool

  val partition_send : send -> string

  val partition_apply : apply -> string

  val partition_curry : curry -> string

  val all : unit -> (string, Tbl.t) Hashtbl.t
end

val compile : shared:bool -> Tbl.t -> Cmm.phrase list
