(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A type language [Variable] equipped with operations that mean it can be
    used in binding position within a [Name_abstraction] value. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Variable.t

include Bindable.S
  with type t := t
  with module Set = Variable.Set
  with module Map = Variable.Map
