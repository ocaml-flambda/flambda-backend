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

(* Compute offsets for elements in sets of closures *)

(** {2 Computing offsets} *)

(** The type of state used to accumulate constraints on offsets. *)
type t

(** The type of names that are used (and pertinent for offset computing) *)
type used_names =
  { closure_ids_normal : Closure_id.Set.t;
        (** Closure ids that appear in projections with normal name mode *)
    closure_ids_in_types : Closure_id.Set.t;
        (** Closure ids that appear in types (and thus can be eventually used in
            normal name mode later) *)
    closure_vars_normal : Var_within_closure.Set.t;
        (** Closure vars that appear in projections with normal name mode *)
    closure_vars_in_types : Var_within_closure.Set.t
        (** Closure vars that appear in types (and thus can be eventually used
            in normal name mode later) *)
  }

(** Printing function. *)
val print : Format.formatter -> t -> unit

(** Create an empty set of constraints. *)
val create : unit -> t

(** Add a set of closure to the set of constraints. *)
val add_set_of_closures :
  t ->
  is_phantom:bool ->
  all_code:Code.t Code_id.Map.t ->
  Set_of_closures.t ->
  t

(** Compute offsets for all closure_ids and env_vars that occur in the current
    compilation unit, taking into account the constraints introduced by the
    sharing of closure_id/env_var across multiple sets of closures. *)
val finalize_offsets :
  used_names:used_names Or_unknown.t ->
  t ->
  Var_within_closure.Set.t Or_unknown.t * Exported_offsets.t

(** {2 Helper functions} *)

(** Returns a cmm name for a closure id. *)
val closure_name : Closure_id.t -> string

(** Returns the address for a function code from the global name of a closure. *)
val closure_code : string -> string

(** Ensure the offsets for the given closure ids are in the given exported
    offsets. *)
val reexport_closure_ids :
  Closure_id.Set.t -> Exported_offsets.t -> Exported_offsets.t

(** Ensure the offsets for the given closure ids are in the given exported
    offsets. *)
val reexport_closure_vars :
  Var_within_closure.Set.t -> Exported_offsets.t -> Exported_offsets.t

(** {2 Offsets & Layouts} *)

(** Layout slots, aka what might be found in a block at a given offset. A layout
    slot can take up more than one word of memory (this is the case for
    closures, which can take either 2 or 3 words depending on arity). *)
type layout_slot =
  | Env_var of Var_within_closure.t
  | Infix_header
  | Closure of Closure_id.t
(**)

(** Alias for complete layouts. The list is sorted according to offsets (in
    increasing order). *)
type layout =
  { startenv : int;
    empty_env : bool;
    slots : (int * layout_slot) list
  }

(** Order the given closure ids and env vars into a list of layout slots
    together with their respective offset. Note that there may be holes between
    the offsets. *)
val layout :
  Exported_offsets.t -> Closure_id.t list -> Var_within_closure.t list -> layout

(** Printing function for layouts. *)
val print_layout : Format.formatter -> layout -> unit

(** Printing functions for layout slots. *)
val print_layout_slot : Format.formatter -> layout_slot -> unit
