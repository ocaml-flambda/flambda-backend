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

(** The signature of "name-like things" that may occur in binding position
    inside [Name_abstraction] constructs. *)

module type S = sig
  type t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  (** Freshen the given name. *)
  val rename : t -> t

  (** Equivalence relation on renamed variables.

      [is_renamed_version_of x y] is [true] if there exists a bindable [z]
      such that [x] and [y] are renamed versions of [z].

      Note: this function can return [true] in other cases (if there are some name collisions
      for instance), this is (at least currently) only used for a sanity check, so
      users should not rely too much on its expected semantics.
      *)
  val is_renamed_version_of : t -> t -> bool

  (** [renaming stale ~guaranteed_fresh:fresh] is to create a renaming that
      turns all occurrences of the name [stale] into [fresh] (in a
      capture-avoiding manner, but that is inherent in [Renaming]). *)
  val renaming : t -> guaranteed_fresh:t -> Renaming.t
end
