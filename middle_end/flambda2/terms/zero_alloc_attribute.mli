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
(** [@zero_alloc ...] annotations on function declaration (not call sites) *)
type t =
  | Default_check
  | Assume of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check of
      { strict : bool;
        loc : Location.t
      }

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val is_default : t -> bool

val from_lambda : Lambda.zero_alloc_attribute -> t
