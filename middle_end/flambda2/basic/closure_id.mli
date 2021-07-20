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

(* CR lwhite: "Closure_id" is quite a generic name.  I wonder
   whether something like "Closure_label" would better capture that it is
   the label of a projection. *)

(* CR mshinwell: update comment *)
(** An identifier, unique across the whole program (not just one compilation
    unit), that identifies a closure within a particular set of closures
    (viz. [Project_closure]). *)

include Container_types.S

module Lmap : Lmap.S with type key = t

val wrap : Compilation_unit.t -> Variable.t -> t

val unwrap : t -> Variable.t

val in_compilation_unit : t -> Compilation_unit.t -> bool
val get_compilation_unit : t -> Compilation_unit.t

val to_string : t -> string

val name : t -> string

val rename : t -> t
