(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The sum type holding a [Variable] or a [Symbol]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

include module type of struct include Reg_width_things.Name end

val map_var : t -> f:(Variable.t -> Variable.t) -> t

val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

val to_var : t -> Variable.t option

val print_sexp : Format.formatter -> t -> unit

val variables_only : Set.t -> Set.t

val symbols_only_map : 'a Map.t -> 'a Map.t

val set_of_var_set : Variable.Set.t -> Set.t

val set_of_symbol_set : Symbol.Set.t -> Set.t

val set_to_var_set : Set.t -> Variable.Set.t

val set_to_symbol_set : Set.t -> Symbol.Set.t

val is_predefined_exception : t -> bool

val is_var : t -> bool

val is_symbol : t -> bool

val must_be_symbol : t -> Symbol.t

val compilation_unit : t -> Compilation_unit.t

val is_imported : t -> bool

val must_be_var_opt : t -> Variable.t option

val must_be_symbol_opt : t -> Symbol.t option

val rename : t -> t

module Pair : sig
  type nonrec t = t * t

  include Container_types.S with type t := t
end

