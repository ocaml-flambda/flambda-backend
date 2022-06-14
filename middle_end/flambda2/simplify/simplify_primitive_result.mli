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

open! Flambda.Import

type t = private
  { simplified_named : Simplified_named.t Or_invalid.t;
    try_reify : bool;
    dacc : Downwards_acc.t
  }

val create : Named.t -> try_reify:bool -> Downwards_acc.t -> t

val create_simplified :
  Simplified_named.t -> try_reify:bool -> Downwards_acc.t -> t

val create_invalid : Downwards_acc.t -> t

val with_dacc : t -> Downwards_acc.t -> t
