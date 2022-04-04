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

(** Varieties of debugging information entries (DIEs), known as "tags". These
    are held within abbreviation table entries rather than within the DIE
    structures themselves. *)

type user = private Numbers.Int16.t

type dwarf_4 =
  | GNU_call_site
  | GNU_call_site_parameter

(** We omit the "DW_TAG_" prefix. *)
type t =
  | Array_type
  | Class_type
  | Entry_point
  | Enumeration_type
  | Formal_parameter
  | Imported_declaration
  | Label
  | Lexical_block
  | Member
  | Pointer_type
  | Reference_type
  | Compile_unit
  | String_type
  | Structure_type
  | Subroutine_type
  | Typedef
  | Union_type
  | Unspecified_parameters
  | Variant
  | Common_block
  | Common_inclusion
  | Inheritance
  | Inlined_subroutine
  | Module
  | Ptr_to_member_type
  | Set_type
  | Subrange_type
  | With_stmt
  | Access_declaration
  | Base_type
  | Catch_block
  | Const_type
  | Constant
  | Enumerator
  | File_type
  | Friend
  | Namelist
  | Namelist_item
  | Packed_type
  | Subprogram
  | Template_type_parameter
  | Template_value_parameter
  | Thrown_type
  | Try_block
  | Variant_part
  | Variable
  | Volatile_type
  | Dwarf_procedure
  | Restrict_type
  | Interface_type
  | Namespace
  | Imported_module
  | Unspecified_type
  | Partial_unit
  | Imported_unit
  | Condition
  | Shared_type
  | Type_unit
  | Rvalue_reference_type
  | Template_alias
  | Coarray_type
  | Generic_subrange
  | Dynamic_type
  | Atomic_type
  | Call_site
  | Call_site_parameter
  | Skeleton_unit
  | Immutable_type
  | Dwarf_4 of dwarf_4
  | User of user

include Dwarf_emittable.S with type t := t

val tag_name : t -> string

val compare : t -> t -> int
