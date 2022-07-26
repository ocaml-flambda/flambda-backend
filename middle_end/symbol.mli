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

val for_predef_ident : Ident.t -> t

(* CR mshinwell: Insist on -for-pack for .mli files; then this function
   will not need to take a pack prefix. *)
val for_global_or_predef_ident : Compilation_unit.Prefix.t -> Ident.t -> t

(** It is assumed that the provided [Ident.t] is in the current unit. *)
val for_local_ident : Ident.t -> t

val for_name : Compilation_unit.t -> string -> t
val for_compilation_unit : Compilation_unit.t -> t
val for_current_unit : unit -> t
val for_new_const_in_current_unit : unit -> t

module Flambda : sig
  val for_variable : Variable.t -> t
  val for_closure : Closure_id.t -> t
  val for_code_of_closure : Closure_id.t -> t

  val import_for_pack : t -> pack:Compilation_unit.Prefix.t -> t
end

val compilation_unit : t -> Compilation_unit.t

val linkage_name : t -> string

(** Linkage names displayed in ocamlobjinfo are formatted differently. *)
val linkage_name_for_ocamlobjinfo : t -> string

include Identifiable.S with type t := t

val is_predef_exn : t -> bool
