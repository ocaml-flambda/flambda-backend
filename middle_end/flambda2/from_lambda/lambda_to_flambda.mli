(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Conversion from Lambda to Flambda. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val lambda_to_flambda :
  symbol_for_global:(?comp_unit:Compilation_unit.t -> Ident.t -> Symbol.t) ->
  big_endian:bool ->
  module_ident:Ident.t ->
  module_block_size_in_words:int ->
  Lambda.lambda ->
  Flambda_unit.t * Exported_code.t * Exported_offsets.t Or_unknown.t
