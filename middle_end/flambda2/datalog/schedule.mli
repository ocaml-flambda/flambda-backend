(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type builder

val create_builder : unit -> builder

type 'a rule_fn

val call_rule_fn : 'a rule_fn -> 'a Heterogenous_list.Constant.hlist -> unit

val add_rule : builder -> ('t, 'k, unit) Table.Id.t -> 'k rule_fn

type stats

val create_stats : unit -> stats

val print_stats : Format.formatter -> stats -> unit

type rule

val build : builder -> 'a Cursor.t -> rule

type t

val saturate : rule list -> t

val fixpoint : t list -> t

val run : ?stats:stats -> t -> Table.Map.t -> Table.Map.t
