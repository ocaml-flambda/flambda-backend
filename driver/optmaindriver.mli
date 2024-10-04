(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* [main argv ppf] runs the compiler with arguments [argv], printing any
   errors encountered to [ppf], and returns the exit code.

   NB: Due to internal state in the compiler, calling [main] twice during
   the same process is unsupported. *)
val main
   : (module Compiler_owee.Unix_intf.S)
  -> string array
  -> Format.formatter
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Cmm.phrase list)
  -> int
