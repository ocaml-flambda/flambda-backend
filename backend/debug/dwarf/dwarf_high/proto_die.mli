(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asm_targets
open Dwarf_low

(** A "proto-DIE" contains similar information to a DWARF debugging information
    entry (DIE), but contains the actual attributes whose values are being
    specified, rather than linking to them via an abbrevation code. The
    abbreviation codes are assigned later, when the proto-DIEs are turned into
    DIEs.

    Proto-DIEs may be formed into the required tree structures using a simple
    parenting relationship. The complexities of flattening the structure to the
    DWARF representation (Section 2.3, DWARF-4 spec) are hidden. *)

type t

(* CR-someday mshinwell: remove type equality *)

(** For creation of proto-DIEs in a group, with references between them. *)
type reference = Asm_label.t

val create_reference : unit -> reference

(* It is an error for [parent] to be [None] unless the [tag] is that for a
   compilation unit (which is a top-level entity). *)
val create :
  ?reference:reference ->
  ?sort_priority:int ->
  ?location_list_in_debug_loc_table:Dwarf_4_location_list.t ->
  parent:t option ->
  tag:Dwarf_tag.t ->
  attribute_values:Dwarf_attribute_values.Attribute_value.t list ->
  unit ->
  t

val create_ignore :
  ?reference:reference ->
  ?sort_priority:int ->
  ?location_list_in_debug_loc_table:Dwarf_4_location_list.t ->
  parent:t option ->
  tag:Dwarf_tag.t ->
  attribute_values:Dwarf_attribute_values.Attribute_value.t list ->
  unit ->
  unit

val add_or_replace_attribute_value :
  t -> Dwarf_attribute_values.Attribute_value.t -> unit

val replace_all_attribute_values :
  t -> Dwarf_attribute_values.Attribute_value.t list -> t

(* CR-someday mshinwell: add a [name] argument to the creation functions *)
val set_name : t -> Asm_symbol.t -> unit

(* [reference t] returns a label that may be used when constructing other
   attribute values. *)
(* CR-someday mshinwell: ideally, attribute values could accept proto-DIE values
   directly, but there is a circularity. *)
val reference : t -> Asm_label.t

type fold_arg = private
  | DIE of
      { tag : Dwarf_tag.t;
        has_children : Child_determination.t;
        attribute_values :
          Dwarf_attribute_values.Attribute_value.t
          Dwarf_attributes.Attribute_specification.Sealed.Map.t;
        label : Asm_label.t;
        name : Asm_symbol.t option;
        location_list_in_debug_loc_table :
          (* optional name *)
          Dwarf_4_location_list.t option
      }
  | End_of_siblings

(* [depth_first_fold] traverses a proto-DIE tree in a depth-first order
   convenient for DWARF information emission. (Section 2.3, DWARF-4 spec.)
   [`End_of_siblings] indicates that a chain of siblings---that is to say,
   proto-DIEs with the same parent and at the same tree depth as each other---
   has finished. This should correspond exactly to the points at which a "chain
   of sibling entries [must be] terminated by a null entry" specified in the
   DWARF-4 spec. *)
val depth_first_fold : t -> init:'a -> f:('a -> fold_arg -> 'a) -> 'a

(** If this proto-DIE has been marked as referencing a DWARF-4 location list,
    return which list it is. We do not currently need to reference more than one
    list in the [attribute_values] of any given proto-DIE. This information
    (which could theoretically be deduced directly from [attribute_values]
    rather than relying on the caller---though relying on the caller is easier)
    enables us to emit the .debug_loc location lists in the same order as they
    are encountered during a top-down traversal as per [depth_first_fold]. This
    suppresses a complaint from objdump "Location lists in .debug_loc start at
    ...". *)
val location_list_in_debug_loc_table : t -> Dwarf_4_location_list.t option

val equal : t -> t -> bool
