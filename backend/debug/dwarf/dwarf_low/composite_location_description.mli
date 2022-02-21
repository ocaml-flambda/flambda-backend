(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Dwarf_emittable.S with type t := t

(** Create a composite DW_OP_piece location description from a list of simple
    location descriptions along with the sizes of the described objects measured
    in bytes. *)
val pieces_of_simple_location_descriptions :
  (Simple_location_description.t * Targetint.t) list -> t
