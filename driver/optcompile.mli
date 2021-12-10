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

(** Native compilation for .ml and .mli files. *)

val interface: source_file:string -> output_prefix:string -> unit

val implementation
   : backend:(module Backend_intf.S)
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    filename:string ->
    module_ident:Ident.t ->
    module_block_size_in_words:int ->
    module_initializer:Lambda.lambda ->
    keep_symbol_tables:bool ->
    Cmm.phrase list)
  -> start_from:Clflags.Compiler_pass.t
  -> source_file:string -> output_prefix:string -> unit

(** {2 Internal functions} **)

val clambda :
  Compile_common.info ->
  (module Backend_intf.S) ->
  Typedtree.structure * Typedtree.module_coercion -> unit
(** [clambda info typed] applies the regular compilation pipeline to the
    given typechecked implementation and outputs the resulting files.
*)

val flambda :
  Compile_common.info ->
  (module Backend_intf.S) ->
  Typedtree.structure * Typedtree.module_coercion -> unit
(** [flambda info backend typed] applies the Flambda compilation pipeline to the
    given typechecked implementation and outputs the resulting files.
*)
