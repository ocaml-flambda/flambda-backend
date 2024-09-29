(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Thomas Del Vecchio, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Modes are an experimental compiler feature, supported in the compiler branch found at:
    https://github.com/ocaml-flambda/ocaml-jst

    This module provides types that wrap a value in a different mode from its context. In
    the standard OCaml compiler, these types are all no-op wrappers. *)

module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
  (** Wraps values in the [global] mode, even in a [local] context. *)
end

module Portable : sig
  type 'a t : value mod portable = { portable : 'a @@ portable } [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
  (** Wraps values in the [portable] mode, even in a [nonportable] context.
      This additionally allows users to restrict a type that does not normally cross
      portability to only portable values so that the resulting type does cross
      portability. *)
end

module Contended : sig
  type 'a t : value mod contended = { contended : 'a @@ contended } [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
  (** Wraps values in the [contended] mode, even in an [uncontended] context. *)
end

module Portended : sig
  type 'a t : value mod portable contended = { portended : 'a @@ portable contended }
  [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
  (** Wraps values in the [portable contended] mode, even in a [nonportable uncontended]
      context. A ['a Portended.t] is equivalent to a ['a Portable.t Contended.t] and a
      ['a Contended.t Portable.t], but much more ergonomic to work with. *)
end
