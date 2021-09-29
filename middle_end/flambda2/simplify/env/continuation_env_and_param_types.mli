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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type arg_at_use =
  { arg_type : Flambda_type.t;
    typing_env : Flambda_type.Typing_env.t
  }

type t =
  | No_uses
  | Uses of
      { handler_env : Downwards_env.t;
        arg_types_by_use_id : arg_at_use Apply_cont_rewrite_id.Map.t list;
        extra_params_and_args : Continuation_extra_params_and_args.t;
        is_single_inlinable_use : bool;
        escapes : bool
      }
