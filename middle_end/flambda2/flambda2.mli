(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translate Lambda code to Cmm using Flambda 2. *)

[@@@ocaml.warning "+a-30-40-41-42"]

(** This function is not currently re-entrant. *)
val lambda_to_cmm :
  ppf_dump:Format.formatter ->
  prefixname:string ->
  filename:string ->
  module_ident:Ident.t ->
  module_block_size_in_words:int ->
  module_initializer:Lambda.lambda ->
  Cmm.phrase list
