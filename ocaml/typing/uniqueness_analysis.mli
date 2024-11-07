(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_exp : expression -> unit

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_value_bindings : value_binding list -> unit

(* These definitions are just to allow printing in the debugger *)
module type P := sig
  type t

  val print : Format.formatter -> t -> unit
end

module Occurrence : P

module Maybe_unique : P

module Maybe_aliased : P

module Aliased : P

module Usage : P

module Tag : P

module Learned_tags : P

module Overwrites : P

module Projection : P

module Paths : P

module UF : P

module Value : P

module Ienv : sig
  include P

  module Extension : P
end
