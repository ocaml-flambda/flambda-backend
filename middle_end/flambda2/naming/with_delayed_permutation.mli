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

(** Management of delayed permutations and cached free names. *)

module Make (Descr : sig
  include Contains_names.S

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  val print : Format.formatter -> t -> unit
end) : sig
  type t

  val create : Descr.t -> t

  val descr : t -> Descr.t

  (** [peek_descr] allows access to the underlying description without
      the current permutation being applied.  This should only be used when
      it is certain and obvious that the subsequent operations on the
      returned description do not look at any part of the description that
      involves names.  This is a performance optimisation. *)
  val peek_descr : t -> Descr.t

  include Contains_names.S with type t := t

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  val print : Format.formatter -> t -> unit
end
