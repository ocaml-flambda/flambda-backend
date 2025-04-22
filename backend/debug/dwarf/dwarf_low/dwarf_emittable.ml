(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Asm_targets

module type S = sig
  type t

  (** Measure the size in bytes of the given entity. *)
  val size : t -> Dwarf_int.t

  (** Emit assembler directives to describe the given entity. *)
  val emit : asm_directives:Asm_directives_dwarf.t -> t -> unit
end

module type S1_ignore = sig
  type 'a t

  (** Measure the size in bytes of the given entity. *)
  val size : _ t -> Dwarf_int.t

  (** Emit assembler directives to describe the given entity. *)
  val emit : asm_directives:Asm_directives_dwarf.t -> _ t -> unit
end
