(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Symbols in the assembly stream.  Unlike labels, symbols are named entities
    that are potentially accessible from outside an object file.

    These symbols are defined within sections and tied to a particular
    compilation unit.  They may point anywhere, unlike [Backend_sym]s, where
    those of [Data] kind must point at correctly-structured OCaml values.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Identifiable.S with type t := t