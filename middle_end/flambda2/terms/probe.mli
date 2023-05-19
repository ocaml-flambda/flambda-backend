(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(** Annotation on function call that represents a probe. *)

type desc =
  { name : string;
    enabled_at_init : bool
  }

type t = desc option

val print : Format.formatter -> t -> unit

val from_lambda : Lambda.probe -> t
