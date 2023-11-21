(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Symbols that identify statically-allocated code or data. *)

type t

(* For predefined exception identifiers. *)
val for_predef_ident : Ident.t -> t

(** It is assumed that the provided [Ident.t] is in the current unit. *)
val for_local_ident : Ident.t -> t

(** To be avoided if possible. Linkage names are intended to be generated
    by this module. *)
val unsafe_create : Compilation_unit.t -> Linkage_name.t -> t

val for_name : Compilation_unit.t -> string -> t
val for_compilation_unit : Compilation_unit.t -> t
val for_current_unit : unit -> t
val for_new_const_in_current_unit : unit -> t

val compilation_unit : t -> Compilation_unit.t

val linkage_name : t -> Linkage_name.t

(** Linkage names displayed in ocamlobjinfo are formatted differently. *)
val linkage_name_for_ocamlobjinfo : t -> string

include Identifiable.S with type t := t

val is_predef_exn : t -> bool

(* Temporary means by which to force symbol names to use __.  Only for use
   for flambda2 flexpect tests. *)
val force_runtime4_symbols : unit -> unit
