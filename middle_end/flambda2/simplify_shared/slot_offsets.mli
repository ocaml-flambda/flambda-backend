(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compute offsets for function and value slots in Closure_tag blocks. *)

(** {2 Computing offsets} *)

(** The type of state used to accumulate constraints on offsets. *)
type t

(** The slot information required from the caller of this module. *)
type used_slots =
  { function_slots_in_normal_projections : Function_slot.Set.t;
        (** Function slots that appear in projections with normal name mode *)
    all_function_slots : Function_slot.Set.t;
        (** All function slots, both in projections and declarations,
            irrespective of name mode *)
    value_slots_in_normal_projections : Value_slot.Set.t;
        (** Closure vars that appear in projections with normal name mode *)
    all_value_slots : Value_slot.Set.t
        (** All value slots, both in projections and declarations, irrespective
            of name mode *)
  }

(** The result of the slot offset computation *)
type result =
  { exported_offsets : Exported_offsets.t;
        (** This includes all offsets for function/value slots that occur in the
            current compilation unit. This includes offsets for slots from other
            compilation units that appear in the current compilation unit.

            Some slots can be marked as dead, because they are unused but
            simplify could not remove them. For instance, this occurs for unused
            value slots of non-liftable functions (.e.g in functors). *)
    used_value_slots : Value_slot.Set.t Or_unknown.t
        (** If known, this is the set of values slots that appears after
            simplify, minus the slots that are marked as dead in
            [exported_offsets]. *)
  }

(** Printing function. *)
val print : Format.formatter -> t -> unit

(** Create an empty set of constraints. *)
val empty : t

(** Add a set of closure to the set of constraints. *)
val add_set_of_closures : t -> is_phantom:bool -> Set_of_closures.t -> t

(** Aggregate sets of closures from two contexts *)
val add_offsets_from_function : t -> from_function:t -> t

(** Compute offsets for all function and value slots that occur in the current
    compilation unit, taking into account the constraints introduced by the
    potential sharing of slots across multiple sets of closures (see .ml file
    for more details). *)
val finalize_offsets :
  get_code_metadata:(Code_id.t -> Code_metadata.t) ->
  used_slots:used_slots Or_unknown.t ->
  t ->
  result

(** {2 Helper functions} *)

(** Returns the function symbol for a function slot. *)
val function_slot_symbol : Function_slot.t -> string

(** Turn a function slot symbol (as returned by [function_slot_symbol]) into the
    symbol for the corresponding piece of code. *)
val code_symbol : function_slot_symbol:string -> string

(** {2 Offsets & Layouts} *)

(** Layout slots, aka what might be found in a block at a given offset. A layout
    slot can take up more than one word of memory (this is the case for
    closures, which can take either 2 or 3 words depending on arity). *)
type layout_slot = private
  | Value_slot of Value_slot.t
  | Infix_header
  | Function_slot of Function_slot.t
(**)

(** Alias for complete layouts. The list is sorted according to offsets (in
    increasing order). *)
type layout = private
  { startenv : int;
    empty_env : bool;
    slots : (int * layout_slot) list
  }

(** Order the given function slots and env vars into a list of layout slots
    together with their respective offset. Note that there may be holes between
    the offsets. *)
val layout :
  Exported_offsets.t -> _ Function_slot.Lmap.t -> _ Value_slot.Map.t -> layout

(** Printing function for layouts. *)
val print_layout : Format.formatter -> layout -> unit

(** Printing functions for layout slots. *)
val print_layout_slot : Format.formatter -> layout_slot -> unit
