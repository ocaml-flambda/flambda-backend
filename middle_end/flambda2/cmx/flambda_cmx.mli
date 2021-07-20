(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Dumping and restoring of simplification environment information to and
    from .cmx files. *)

val load_cmx_file_contents
   : (module Flambda_backend_intf.S)
  -> Compilation_unit.t
  -> imported_units:Flambda_type.Typing_env.t option Compilation_unit.Map.t ref
  -> imported_names:Name.Set.t ref
  -> imported_code:Exported_code.t ref
  -> Flambda_type.Typing_env.t option

val prepare_cmx_file_contents
   : return_cont_env:Continuation_uses_env.t
  -> return_continuation:Continuation.t
  -> module_symbol:Symbol.t
  -> used_closure_vars:Var_within_closure.Set.t
  -> Exported_code.t
  -> Flambda_cmx_format.t option
