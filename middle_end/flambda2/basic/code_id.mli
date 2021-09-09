(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Container_types.S

type exported

module Lmap : Lmap.S with type key = t

val initialise : unit -> unit

val create : name:string -> Compilation_unit.t -> t

val get_compilation_unit : t -> Compilation_unit.t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val name : t -> string

(* The [rename] function, in addition to changing the stamp of the code ID,
   changes the compilation unit to the current one. *)
val rename : t -> t

(** [Code_id]s uniquely determine function symbols. *)
val code_symbol : t -> Symbol.t

val invert_map : t Map.t -> t Map.t

val export : t -> exported

val import : exported -> t

val map_compilation_unit :
  (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported
