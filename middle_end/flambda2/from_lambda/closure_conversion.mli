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

(** Introduce closures into CPS code, producing Flambda. *)

module IR = Closure_conversion_aux.IR
module Env = Closure_conversion_aux.Env
module Acc = Closure_conversion_aux.Acc
module Function_decl = Closure_conversion_aux.Function_decls.Function_decl
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val close_let :
  Acc.t ->
  Env.t ->
  (Ident.t * Flambda_kind.With_subkind.t) list ->
  IR.user_visible ->
  IR.named ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  Expr_with_acc.t

val close_let_rec :
  Acc.t ->
  Env.t ->
  function_declarations:Function_decl.t list ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  current_region:Ident.t ->
  Expr_with_acc.t

val close_let_cont :
  Acc.t ->
  Env.t ->
  name:Continuation.t ->
  is_exn_handler:bool ->
  params:(Ident.t * IR.user_visible * Flambda_kind.With_subkind.t) list ->
  recursive:Asttypes.rec_flag ->
  handler:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  body:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  Expr_with_acc.t

val close_apply : Acc.t -> Env.t -> IR.apply -> Expr_with_acc.t

val close_apply_cont :
  Acc.t ->
  Env.t ->
  dbg:Debuginfo.t ->
  Continuation.t ->
  IR.trap_action option ->
  IR.simple list ->
  Expr_with_acc.t

val close_switch :
  Acc.t ->
  Env.t ->
  condition_dbg:Debuginfo.t ->
  Ident.t ->
  IR.switch ->
  Expr_with_acc.t

val close_raise :
  Acc.t ->
  Env.t ->
  raise_kind:Lambda.raise_kind ->
  arg:IR.simple ->
  dbg:Debuginfo.t ->
  IR.exn_continuation ->
  Expr_with_acc.t

type 'a close_program_metadata =
  | Normal : [`Normal] close_program_metadata
  | Classic :
      (Exported_code.t
      * Name_occurrences.t
      * Flambda_cmx_format.t option
      * Exported_offsets.t)
      -> [`Classic] close_program_metadata

type 'a close_program_result =
  { unit : Flambda_unit.t;
    metadata : 'a close_program_metadata;
    code_slot_offsets : Slot_offsets.t Code_id.Map.t
  }

val close_program :
  mode:'mode Flambda_features.mode ->
  big_endian:bool ->
  cmx_loader:Flambda_cmx.loader ->
  compilation_unit:Compilation_unit.t ->
  module_block_size_in_words:int ->
  program:(Acc.t -> Env.t -> Expr_with_acc.t) ->
  prog_return_cont:Continuation.t ->
  exn_continuation:Continuation.t ->
  toplevel_my_region:Ident.t ->
  'mode close_program_result
