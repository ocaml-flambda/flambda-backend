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
   : string array
  -> Format.formatter
  -> flambda2_backend:(module Flambda2__Flambda_backend_intf.S)
  -> flambda2_to_cmm:(
        Flambda2__Flambda_middle_end.middle_end_result
     -> Cmm.phrase list)
  -> int
