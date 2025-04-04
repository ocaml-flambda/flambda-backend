(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Interface to be satisfied by target-specific code, for common subexpression
    elimination. *)

(** Classification of operations *)

type op_class =
  | Op_pure (** pure arithmetic, produce one or several result *)
  | Op_load of Simple_operation.mutable_flag  (** memory load *)
  | Op_store of bool  (** memory store, false = init, true = assign *)
  | Op_other  (** anything else that does not allocate nor store in memory *)

type class_of_operation_result =
  | Class of op_class
  | Use_default

type is_cheap_operation_result =
  | Cheap of bool
  | Use_default

module type S = sig
  (** The following methods can be overridden to handle processor-specific
       operations. *)
  val class_of_operation : Operation.t -> class_of_operation_result

  (** Operations that are so cheap that it isn't worth factoring them. *)
  val is_cheap_operation : Operation.t -> is_cheap_operation_result
end
