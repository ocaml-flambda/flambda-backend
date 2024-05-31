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
    used_value_slots : Value_slot.Set.t
        (** This is the set of values slots that appears after simplify, minus
            the slots that are marked as dead in [exported_offsets]. *)
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
  used_slots:used_slots ->
  t ->
  result

type words = int

(** {2 Offsets & Layouts} *)
module Layout : sig
  (**)

  (** Layout slots, aka what might be found in a block at a given offset. A
      layout slot can take up more than one word of memory (this is the case for
      closures, which can take either 2 or 3 words depending on arity). *)
  type slot = private
    | Value_slot of
        { size : words;
          is_scanned : bool;
          value_slot : Value_slot.t
        }
    | Infix_header
    | Function_slot of
        { size : words;
          function_slot : Function_slot.t;
          last_function_slot : bool
        }
    | Dummy_function_slot of { last_function_slot : bool }
  (**)

  (** Alias for complete layouts. The list is sorted according to offsets (in
      increasing order). *)
  type t = private
    { startenv : words;
      empty_env : bool;
      slots : (words * slot) list
    }

  (** Order the given function slots and env vars into a list of layout slots
      together with their respective offset. Note that there may be holes
      between the offsets. *)
  val make :
    Exported_offsets.t -> _ Function_slot.Lmap.t -> _ Value_slot.Map.t -> t

  (** Printing function for layouts. *)
  val print : Format.formatter -> t -> unit

  (** Printing functions for layout slots. *)
  val print_slot : Format.formatter -> slot -> unit
end
