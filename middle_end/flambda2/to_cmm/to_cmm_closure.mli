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

(** Compute offsets for a whole compilation unit. Takes the offsets from all cmx
    files read as input. *)
val compute_offsets :
  Exported_offsets.t -> Exported_code.t -> Flambda_unit.t -> Exported_offsets.t

(** Returns a cmm name for a closure id. *)
val closure_name : Closure_id.t -> string

(** Returns the address for a function code from the global name of a closure. *)
val closure_code : string -> string

(** Returns the assignments of closure variables to [Simple]s from the given set
    of closures, but ignoring any closure variable that does not occur in
    [used_closure_vars], so long as [used_closure_vars] is [Known]. If
    [used_closure_vars] is [Unknown] then assignments for all closure variables
    are returned. *)
val filter_closure_vars :
  Flambda.Set_of_closures.t ->
  used_closure_vars:Var_within_closure.Set.t Or_unknown.t ->
  Simple.t Var_within_closure.Map.t

(* val map_on_function_decl :
 *   (string -> Closure_id.t -> Flambda.Code_id.t -> 'a) ->
 *   Flambda_unit.t -> 'a Closure_id.Map.t
 * (\** Map a function on each function body exactly once, and return the
 *     resulting mapping. *\) *)

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
    slots : (int * layout_slot) list
  }

(** Order the given closure ids and env vars into a list of layout slots
    together with their respective offset. Note that there may be holes between
    the offsets. *)
val layout :
  Exported_offsets.t -> Closure_id.t list -> Var_within_closure.t list -> layout

val print_layout : Format.formatter -> layout -> unit

(** Printing functions for layout slots and layouts. *)
val print_layout_slot : Format.formatter -> layout_slot -> unit
