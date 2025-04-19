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

(** The names of continuations. *)

type t = private Table_by_int_id.Id.t

type exported

include Container_types.S with type t := t

module Lmap : Lmap.S with type key := t

module Sort : sig
  type t =
    | Normal_or_exn
    | Return
    | Define_root_symbol
    | Toplevel_return

  val equal : t -> t -> bool
end

val create : ?sort:Sort.t -> ?name:string -> unit -> t

val rename : t -> t

val name : t -> string

val sort : t -> Sort.t

val export : t -> exported

val import : exported -> t

val initialise : unit -> unit

val reset : unit -> unit
