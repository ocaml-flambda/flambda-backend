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

(** The left-hand sides of [Let]-expressions that bind statically-allocated
    constants and pieces of code. Used via [Bound_pattern] in the term
    language. *)

module Pattern : sig
  type t = private
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Function_slot.Lmap.t
    | Block_like of Symbol.t

  val code : Code_id.t -> t

  val set_of_closures : Symbol.t Function_slot.Lmap.t -> t

  val block_like : Symbol.t -> t

  val print : Format.formatter -> t -> unit
end

type t

val empty : t

(* CR vlaviron: I believe that the property we want is that all recursive cycles
   go through at least a code ID. So we could check that we're not creating
   bound patterns with more than one element, unless one of these elements is a
   Code pattern. I'm not completely sure that we can enforce this invariant on
   the result of From_lambda without going through the code for the classic mode
   though. *)

(** All recursive cycles between the names bound by the provided pattern(s) must
    go through at least one code ID. (So for example the declaration of just a
    block that points to itself is forbidden.) *)
val create : Pattern.t list -> t

val singleton : Pattern.t -> t

val to_list : t -> Pattern.t list

val binds_code : t -> bool

val binds_symbols : t -> bool

val symbols_being_defined : t -> Symbol.Set.t

val code_being_defined : t -> Code_id.Set.t

val everything_being_defined : t -> Code_id_or_symbol.Set.t

val concat : t -> t -> t

val gc_roots : t -> Symbol.t list

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t
