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

type stats

val create_stats : unit -> stats

val print_stats : Format.formatter -> stats -> unit

type rule

type deduction =
  [ `Atom of Datalog.atom
  | `And of deduction list ]

val deduce : deduction -> (Heterogenous_list.nil, rule) Datalog.program

type t

val saturate : rule list -> t

val fixpoint : t list -> t

val run : ?stats:stats -> t -> Table.Map.t -> Table.Map.t
