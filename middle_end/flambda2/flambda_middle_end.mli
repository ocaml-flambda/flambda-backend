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

(** Translate Lambda code to Flambda 2.0 code and then optimize it. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type middle_end_result = private {
  cmx : Flambda_cmx_format.t option;
  unit : Flambda_unit.t;
  all_code : Exported_code.t;
}

(** This function is not currently re-entrant. *)
val middle_end
   : ppf_dump:Format.formatter
  -> prefixname:string
  -> backend:(module Flambda_backend_intf.S)
  -> filename:string
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> module_initializer:Lambda.lambda
  -> middle_end_result
