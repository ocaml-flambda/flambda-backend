(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type modifiers =
  { modal_upper_bounds : Mode.Alloc.Const.Option.t;
    externality_upper_bound : Jkind_types.Externality.t option;
    nullability_upper_bound : Jkind_types.Nullability.t option
  }

(** Interpret a list of modes *)
val transl_mode_annots :
  ?required_mode_maturity:Language_extension.maturity ->
  Parsetree.modes ->
  Mode.Alloc.Const.Option.t

(** Interpret a list of modifiers.
    A "modifier" is any keyword coming after a `mod` in a jkind *)
val transl_modifier_annots : Parsetree.modes -> modifiers
