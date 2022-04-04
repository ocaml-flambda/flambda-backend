(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Asm_targets

include Location_or_range_list_entry.Make (struct
  module Payload = struct
    type t = unit

    let size () = Dwarf_int.zero ()

    let emit ~asm_directives:_ () = ()
  end

  let code_for_entry_kind (entry : _ Location_or_range_list_entry.entry) =
    match entry with
    (* DWARF-5 spec page 240. *)
    | End_of_list -> 0x00
    | Base_addressx _ -> 0x01
    | Startx_endx _ -> 0x02
    | Startx_length _ -> 0x03
    | Offset_pair _ -> 0x04
    | Base_address _ -> 0x05
    | Start_end _ -> 0x06
    | Start_length _ -> 0x07

  let section : Asm_section.dwarf_section = Debug_rnglists
end)
