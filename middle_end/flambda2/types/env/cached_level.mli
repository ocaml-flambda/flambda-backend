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

type t

val print_name_modes :
  restrict_to:Name.Set.t ->
  min_binding_time:Binding_time.t ->
  Format.formatter ->
  t ->
  unit

val empty : t

val names_to_types :
  t -> (Type_grammar.t * Binding_time.With_name_mode.t) Name.Map.t

val aliases : t -> Aliases.t

val add_or_replace_binding :
  t -> Name.t -> Type_grammar.t -> Binding_time.t -> Name_mode.t -> t

val replace_variable_binding : t -> Variable.t -> Type_grammar.t -> t

val with_aliases : t -> aliases:Aliases.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val clean_for_export : t -> reachable_names:Name_occurrences.t -> t

val apply_renaming : t -> Renaming.t -> t

val merge : t -> t -> t
