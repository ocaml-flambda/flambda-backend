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

open! Simplify_import

let simplify_get_cached_method dacc ~original_prim
    ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~arg3:_ ~arg3_ty:_ ~arg4:_ ~arg4_ty:_
    ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_quaternary_primitive dacc original_prim (prim : P.quaternary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3 ~arg3_ty ~arg4 ~arg4_ty dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Get_cached_method ->
      simplify_get_cached_method ~original_prim
  in
  simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~arg3
    ~arg3_ty ~arg4 ~arg4_ty ~result_var
