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

[@@@ocaml.warning "+a-30-40-41-42"]

(** A label, unique across the whole program, that identifies a function within
    a set of closures. In essence these function slots describe the semantics of
    a particular piece of code when it is executed in the context of a
    particular closure.

    Function slots are assigned integer offsets inside [Closure_tag] blocks,
    where the relevant information (code pointers, arity, etc.) will be stored
    at runtime, by the [Slot_offsets] module. *)

include Container_types.S

module Lmap : Lmap.S with type key = t

val wrap : Compilation_unit.t -> Variable.t -> t

val unwrap : t -> Variable.t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val get_compilation_unit : t -> Compilation_unit.t

val to_string : t -> string

val name : t -> string

val rename : t -> t
