(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(** Annotations on function declaration (not call sites) *)
module Property : sig
  type t = Zero_alloc
end

type t =
  | Default_check
  | Ignore_assert_all of Property.t
  | Assume of
      { property : Property.t;
        strict : bool;
        never_returns_normally : bool;
        loc : Location.t
      }
  | Check of
      { property : Property.t;
        strict : bool;
        loc : Location.t
      }

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val from_lambda : Lambda.check_attribute -> Location.t -> t

