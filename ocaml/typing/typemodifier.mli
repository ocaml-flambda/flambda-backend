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
type alloc_list =
  { areality : Mode.Locality.Const.t list;
    uniqueness : Mode.Uniqueness.Const.t list;
    linearity : Mode.Linearity.Const.t list;
    portability : Mode.Portability.Const.t list;
    contention : Mode.Contention.Const.t list
  }

type modifiers =
  { modes : Mode.Alloc.Const.Option.t;
    externality : Jkind_types.Externality.t option;
    nullability : Jkind_types.Nullability.t option
  }

(** Interpret a list of modes *)
val transl_mode_annots :
  ?required_mode_maturity:Language_extension.maturity ->
  Parsetree.modes ->
  Mode.Alloc.Const.Option.t

(** Interpret a list of modalities into a set of modes *)
val transl_modality_annots :
  ?required_mode_maturity:Language_extension.maturity ->
  Parsetree.modalities ->
  alloc_list

(** Interpret a list of modifiers.
    A "modifier" is any keyword coming after a `mod` in a jkind *)
val transl_modifier_annots : Parsetree.modes -> modifiers
