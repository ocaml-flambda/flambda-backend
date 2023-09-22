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

open Cmm_helpers

(** Generate generic functions *)
module Tbl : sig
  type t

  val make : unit -> t

  val add : t -> Cmx_format.generic_fns -> unit

  val of_fns : Cmx_format.generic_fns -> t

  val entries : t -> Cmx_format.generic_fns
end

module Cache : sig
  val all : unit -> (string, Tbl.t) Hashtbl.t
end

val compile : shared:bool -> Tbl.t -> Cmm.phrase list
