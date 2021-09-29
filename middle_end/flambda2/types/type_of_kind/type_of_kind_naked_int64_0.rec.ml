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

[@@@ocaml.warning "+a-30-40-41-42"]

module Int64 = Numeric_types.Int64
module TEE = Typing_env_extension

type t = Int64.Set.t

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[(Naked_int64s@ (%a))@]" Int64.Set.print t

let apply_renaming t _renaming = t

let free_names _t = Name_occurrences.empty

let all_ids_for_export _t = Ids_for_export.empty

let apply_coercion t coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok t else Bottom

let eviscerate _ : _ Or_unknown.t = Unknown

let meet _env t1 t2 : _ Or_bottom.t =
  let t = Int64.Set.inter t1 t2 in
  if Int64.Set.is_empty t then Bottom else Ok (t, TEE.empty ())

let join _env t1 t2 : _ Or_unknown.t = Known (Int64.Set.union t1 t2)
