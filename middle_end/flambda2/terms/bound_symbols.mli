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

module Pattern : sig
  type t = private
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Closure_id.Lmap.t
    | Block_like of Symbol.t

  val code : Code_id.t -> t

  val set_of_closures : Symbol.t Closure_id.Lmap.t -> t

  val block_like : Symbol.t -> t

  val print : Format.formatter -> t -> unit
end

type t

val empty : t

val create : Pattern.t list -> t

val singleton : Pattern.t -> t

val to_list : t -> Pattern.t list

val being_defined : t -> Symbol.Set.t

val code_being_defined : t -> Code_id.Set.t

val binds_code : t -> bool

val binds_symbols : t -> bool

val non_closure_symbols_being_defined : t -> Symbol.Set.t

val closure_symbols_being_defined : t -> Symbol.Set.t

val everything_being_defined : t -> Code_id_or_symbol.Set.t

val for_all_everything_being_defined
   : t
  -> f:(Code_id_or_symbol.t -> bool)
  -> bool

val concat : t -> t -> t

val gc_roots : t -> Symbol.t list

include Expr_std.S with type t := t
include Contains_ids.S with type t := t
