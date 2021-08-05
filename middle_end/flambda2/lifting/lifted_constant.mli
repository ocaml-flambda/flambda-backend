(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: The name of this module is a bit misleading *)

(** Description of a group of statically-allocated constants discovered
    during simplification. *)

module Definition : sig
  type descr = private
    | Code of Code_id.t
    | Set_of_closures of {
        denv : Downwards_env.t;
        closure_symbols_with_types
          : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t;
        symbol_projections : Symbol_projection.t Variable.Map.t;
      }
    | Block_like of {
        symbol : Symbol.t;
        denv : Downwards_env.t;
        ty : Flambda_type.t;
        symbol_projections : Symbol_projection.t Variable.Map.t;
      }

  type t

  val print : Format.formatter -> t -> unit

  val descr : t -> descr

  val defining_expr : t -> Rebuilt_static_const.t

  val denv : t -> Downwards_env.t option

  val code : Code_id.t -> Rebuilt_static_const.t -> t

  val set_of_closures
     : Downwards_env.t
    -> closure_symbols_with_types
         : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t
    -> symbol_projections:Symbol_projection.t Variable.Map.t
    -> Rebuilt_static_const.t
    -> t

  val block_like
     : Downwards_env.t
    -> Symbol.t
    -> Flambda_type.t
    -> symbol_projections:Symbol_projection.t Variable.Map.t
    -> Rebuilt_static_const.t
    -> t

  val bound_symbols : t -> Bound_symbols.t

  val free_names : t -> Name_occurrences.t

  val symbol_projections : t -> Symbol_projection.t Variable.Map.t
end

type t

val print : Format.formatter -> t -> unit

(** The creation functions take the types of symbols to avoid re-inferring
    them. *)
val create_block_like
   : Symbol.t
  -> symbol_projections:Symbol_projection.t Variable.Map.t
  -> Rebuilt_static_const.t
  -> Downwards_env.t
  -> Flambda_type.t
  -> t

val create_set_of_closures
   : Downwards_env.t
  -> closure_symbols_with_types:(Symbol.t * Flambda_type.t) Closure_id.Lmap.t
  -> symbol_projections:Symbol_projection.t Variable.Map.t
  -> Rebuilt_static_const.t
  -> t

val create_code
   : Code_id.t
  -> Rebuilt_static_const.t
  -> t

val definitions : t -> Definition.t list
val bound_symbols : t -> Bound_symbols.t
val defining_exprs : t -> Rebuilt_static_const.Group.t
val types_of_symbols : t -> (Downwards_env.t * Flambda_type.t) Symbol.Map.t
val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val concat : t list -> t

val is_fully_static : t -> bool

val all_defined_symbols : t -> Symbol.Set.t

val free_names_of_defining_exprs : t -> Name_occurrences.t

val apply_projection : t -> Symbol_projection.t -> Simple.t option
