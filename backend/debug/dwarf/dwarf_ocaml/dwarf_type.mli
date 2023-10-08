(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Tomasz Nowak and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Conversion of type shape information into DWARF types. *)

open! Dwarf_low
open! Dwarf_high
module Uid = Flambda2_identifiers.Flambda_uid

val variable_to_die :
  Dwarf_state.t -> Uid.t -> parent_proto_die:Proto_die.t -> Proto_die.reference
