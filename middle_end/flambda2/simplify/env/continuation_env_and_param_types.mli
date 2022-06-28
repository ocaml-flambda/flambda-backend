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

type arg_at_use =
  { arg_type : Flambda2_types.t;
    typing_env : Flambda2_types.Typing_env.t
  }

(** The type of an argument at each use site. The moral equivalent of an SSA
    phi-node. *)
type arg_types_by_use_id = arg_at_use Apply_cont_rewrite_id.Map.t

type t =
  | No_uses
  | Uses of
      { handler_env : Downwards_env.t;
        arg_types_by_use_id : arg_types_by_use_id list;
        extra_params_and_args : Continuation_extra_params_and_args.t;
        is_single_inlinable_use : bool;
        escapes : bool
      }
