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

(** This module provides types that wrap a value in a different mode from its
    context. In the standard OCaml compiler, these types are all no-op
    wrappers. *)

module Global : sig
  type 'a t = { global : 'a @@ global } [@@unboxed]
  (** Wraps values in the [global] mode, even in a [local] context. *)
end

module Portable : sig
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended : sig
  type 'a t = { contended : 'a @@ contended } [@@unboxed]
  (** Wraps values in the [contended] mode, even in an [uncontended] context. *)
end

module Portended : sig
  type 'a t = { portended : 'a @@ portable contended } [@@unboxed]
  (** Wraps values in the [portable contended] mode, even in a [nonportable uncontended]
      context. A ['a Portended.t] is equivalent to a ['a Portable.t Contended.t] and a
      ['a Contended.t Portable.t], but much more ergonomic to work with. *)
end

module Aliased : sig
  type 'a t = { aliased : 'a @@ aliased } [@@unboxed]
end

module Shared : sig
  type 'a t = { shared : 'a @@ shared } [@@unboxed]
end
