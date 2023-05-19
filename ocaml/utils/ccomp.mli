(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compiling C files and building C libraries

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val command: string -> int
val run_command: string -> unit
val compile_file:
  ?output:string -> ?opt:string -> ?stable_name:string -> string -> int
val create_archive: string -> string list -> int
val quote_files: string list -> string
val quote_optfile: string option -> string
(*val make_link_options: string list -> string*)

type link_mode =
  | Exe
  | Dll
  | MainDll
  | Partial

(* If the ~native_toplevel flag is true, we don't pass any `-L` flags to gcc.
   In some cases we observed so many flags being passed that gcc would crash,
   but they should all be unnecessary as we're compiling with `-shared` in that
   case. *)
(* CR-someday ccasinghino: the argument above equally applies to all cases when
   `link_mode` is `Dll`, but that didn't seem to work.  Understand why. *)
val call_linker:
  ?native_toplevel:bool -> link_mode -> string -> string list -> string -> int

val linker_is_flexlink : bool
