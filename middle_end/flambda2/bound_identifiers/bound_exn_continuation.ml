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

include Exn_continuation

let add_to_name_permutation t ~guaranteed_fresh perm =
  Renaming.add_fresh_continuation perm (exn_handler t)
    ~guaranteed_fresh:(exn_handler guaranteed_fresh)

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty

let rename t =
  let exn_handler = Continuation.rename (exn_handler t) in
  create ~exn_handler ~extra_args:(extra_args t)
