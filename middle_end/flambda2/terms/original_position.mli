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
(** The original source position and the [@tail] and
    [@nontail] annotations for a function application. *)

type tail_attribute = Lambda.tail_attribute

type t = Lambda.position_and_tail_attribute

val print : Format.formatter -> t -> unit
