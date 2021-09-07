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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Code_id of Code_id.t
  | Symbol of Symbol.t

include Container_types.S with type t := t

val compilation_unit : t -> Compilation_unit.t

val set_of_code_id_set : Code_id.Set.t -> Set.t

val set_of_symbol_set : Symbol.Set.t -> Set.t
