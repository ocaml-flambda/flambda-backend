(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_width_things.Symbol

let rename t =
  create (compilation_unit t) (Linkage_name.rename (linkage_name t))

let import_for_pack _ ~pack:_ = Misc.fatal_error "Not yet implemented"

let in_compilation_unit t comp_unit =
  Compilation_unit.equal (compilation_unit t) comp_unit

let is_predefined_exception t =
  Compilation_unit.is_predefined_exception (compilation_unit t)
