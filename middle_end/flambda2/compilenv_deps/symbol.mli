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

(** A symbol identifies a constant provided by either:
    - another compilation unit; or
    - a top-level module.

    The linkage name must be globally unique: two compilation units linked in
    the same program must not share a linkage name. *)

include module type of struct include Reg_width_things.Symbol end

val import_for_pack : t -> pack:Compilation_unit.t -> t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val is_predefined_exception : t -> bool

val rename : t -> t
