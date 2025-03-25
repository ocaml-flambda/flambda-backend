(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

(* Convert environment summaries to environments. The Boolean [allow_missing_modules]
   controls whether missing modules are allowed. If it set to [true], missing modules
   will not be opened into the environment such that their contents will be missing from
   the resulting environment. If it set to [false], an exception is raised when missing
   modules are encountered. *)

val env_from_summary : allow_missing_modules:bool -> Env.summary -> Subst.t -> Env.t

(* Empty the environment caches. To be called when load_path changes. *)

val reset_cache: preserve_persistent_env:bool -> unit

(* Reconstructs an environment from a summary. The Boolean [allow_missing_modules]
   controls whether missing modules are allowed. If it set to [true], missing modules
   will not be opened into the environment such that their contents will be missing from
   the resulting environment. If it set to [false], an exception is raised when missing
   modules are encountered. The default is [false]. *)
val env_of_only_summary : ?allow_missing_modules:bool -> Env.t -> Env.t

(* Error report *)

type error =
    Module_not_found of Path.t

exception Error of error

val report_error: formatter -> error -> unit
