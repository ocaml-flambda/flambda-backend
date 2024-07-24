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
(** [@tail] and [@nontail] annotations on a function call. *)

type t =
  | Explicit_tail (* [@tail] *)
  | Hint_tail (* [@tail hint] *)
  | Explicit_non_tail (* [@nontail] *)
  | Default_tail (* No [@tail] or [@nontail] attribute *)

val from_lambda : Lambda.tail_attribute -> t

val to_lambda : t -> Lambda.tail_attribute

val print : Format.formatter -> t -> unit
