(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** file sections cache *)

val add_unit :
  Compilation_unit.t -> int array -> in_channel -> first_section_offset:int -> unit

val read_section_from_file : unit:Compilation_unit.t -> index:int -> Obj.t

val read_all_sections : unit:Compilation_unit.t -> Obj.t array

val close_all : unit -> unit

val serialize : Obj.t array -> string array * int array * int
