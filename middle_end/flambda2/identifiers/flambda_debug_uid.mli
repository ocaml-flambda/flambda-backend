(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Augmented version of [Lambda.debug_uid] that can track variables forming parts
    of unboxed products. *)

type t = private
  | Uid of Lambda.debug_uid
  | Proj of Lambda.debug_uid * int

val none : t

val of_lambda_debug_uid : Lambda.debug_uid -> t

val of_lambda_debug_uid_proj : Lambda.debug_uid -> field:int -> t

include Identifiable.S with type t := t
