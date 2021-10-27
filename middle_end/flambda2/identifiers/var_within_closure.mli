(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** An identifier, unique across the whole program, that identifies a particular
    variable within a particular closure. Only [Project_var], and not [Var],
    nodes are tagged with these identifiers. *)

include Container_types.S

val wrap : Compilation_unit.t -> Variable.t -> t

val unwrap : t -> Variable.t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val is_imported : t -> bool

val get_compilation_unit : t -> Compilation_unit.t

val to_string : t -> string

val rename : t -> t
