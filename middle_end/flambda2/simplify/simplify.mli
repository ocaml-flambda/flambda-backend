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

(** Simplification of Flambda programs: inlining, specialisation, unboxing and
    so forth.

    Readers interested in the function inlining strategy should read the
    [Inlining_decision] module first. *)

type simplify_result = private
  { free_names : Name_occurrences.t;
    final_typing_env : Typing_env.t option;
    all_code : Exported_code.t;
    slot_offsets : Slot_offsets.t;
    unit : Flambda_unit.t
  }

val run :
  cmx_loader:Flambda_cmx.loader ->
  round:int ->
  code_slot_offsets:Slot_offsets.t Code_id.Map.t ->
  Flambda_unit.t ->
  simplify_result
