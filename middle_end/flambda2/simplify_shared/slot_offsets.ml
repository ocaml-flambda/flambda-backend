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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR chambart/gbury: This module could be renamed into [set_of_closures_layout]
   or similar. *)

open! Flambda.Import
module EO = Exported_offsets

type words = int

type result =
  { exported_offsets : EO.t;
    used_value_slots : Value_slot.Set.t
  }

type used_slots =
  { function_slots_in_normal_projections : Function_slot.Set.t;
    all_function_slots : Function_slot.Set.t;
    value_slots_in_normal_projections : Value_slot.Set.t;
    all_value_slots : Value_slot.Set.t
  }

let[@inline] value_slot_is_used ~used_value_slots v =
  if Compilation_unit.is_current (Value_slot.get_compilation_unit v)
  then Value_slot.Set.mem v used_value_slots
  else true

(* Compute offsets of the runtime memory layout of sets of closures. These
   offsets are computed in words, not in bytes.

   The layout for a block describing a set of closures is the following: *)
(*
 * |----------------------|
 * | Closure_tag header   |
 * |----------------------| offset 0
 * | function slot 0      | <- pointer to the block (and pointer to function slot 0)
 * | (pos 0, size 2 or 3) |
 * |----------------------|
 * | Infix_tag header     | (size=1)
 * |----------------------|
 * | function slot 1      | <- pointer to function slot 1
 * | (pos x, size 2 or 3) |  (x=1+size_of_function_slot_0)
 * |----------------------|
 * | Infix_tag header     |
 * |----------------------|
 * .                      .
 * .                      .
 * .                      .
 * |----------------------|
 * | Infix header         |
 * |----------------------|
 * | last function slot   |
 * |                      |
 * |----------------------|
 * | value slot 0  size=1 | <- start of the environment part of the block
 * |----------------------|
 * | value slot 1         |
 * |----------------------|
 * .                      .
 * .                      .
 * |----------------------|
 * | last value slot      |
 * |----------------------|
 *)
(* However, that ideal layout may not be possible in certain circumstances, as
   there may be arbitrary holes between slots (i.e. unused words in the block).

   All function slots must occur before all value slots, since the offset to the
   start of the environment is recorded in the arity field of each function
   slot. *)

module Layout = struct
  type slot =
    | Value_slot of Value_slot.t
    | Infix_header
    | Function_slot of
        { size : words;
          function_slot : Function_slot.t
        }

  type t =
    { startenv : words;
      empty_env : bool;
      slots : (words * slot) list
    }

  let order_function_slots env l acc =
    Function_slot.Lmap.fold
      (fun function_slot _ acc ->
        match EO.function_slot_offset env function_slot with
        | Some Dead_function_slot -> acc
        | Some (Live_function_slot { size; offset }) ->
          Numeric_types.Int.Map.add offset
            (Function_slot { size; function_slot })
            acc
        | None ->
          Misc.fatal_errorf "No function_slot offset for %a" Function_slot.print
            function_slot)
      l acc

  let order_value_slots env l acc =
    Value_slot.Map.fold
      (fun value_slot _ acc ->
        match EO.value_slot_offset env value_slot with
        | Some Dead_value_slot -> acc
        | Some (Live_value_slot { offset }) ->
          Numeric_types.Int.Map.add offset (Value_slot value_slot) acc
        | None ->
          Misc.fatal_errorf "No value slot offset for %a" Value_slot.print
            value_slot)
      l acc

  let layout_aux offset slot (startenv, acc_slots) =
    match slot with
    (* Starting from OCaml 4.12, all function slots *must* precede all value
       slots. The algorithms in this file should thus only generate slot
       assignments that respect this invariant. If that is not the case, this is
       a fatal error given that the start of the environment (i.e. the offset of
       the first value slot, with the added property that all slots after that
       are value slots (or at least scannable by the GC)), is needed by the GC
       when scanning the block. Thus, if we see a function slot, we check that
       then the environment has not started yet (i.e. we have not seen any value
       slots). *)
    | Function_slot _ when offset = 0 ->
      assert (acc_slots = []);
      assert (startenv = None);
      (* see comment above *)
      let acc_slots = [0, slot] in
      startenv, acc_slots
    | Function_slot _ ->
      assert (startenv = None);
      (* see comment above *)
      let acc_slots =
        (offset, slot) :: (offset - 1, Infix_header) :: acc_slots
      in
      startenv, acc_slots
    | Value_slot _ ->
      let startenv =
        match startenv with
        | Some i ->
          assert (i < offset);
          startenv
        | None -> Some offset
      in
      let acc_slots = (offset, slot) :: acc_slots in
      startenv, acc_slots
    | Infix_header ->
      (* Internal invariant: such layout slots are not generated by the {order}
         function, so they should not appear. *)
      assert false

  let make env function_slots value_slots =
    let map =
      Numeric_types.Int.Map.empty
      |> order_value_slots env value_slots
      |> order_function_slots env function_slots
    in
    let startenv_opt, rev_slots =
      Numeric_types.Int.Map.fold layout_aux map (None, [])
    in
    let startenv, empty_env =
      (* If there are no value slots, the start of env is considered to be the
         (non-existing) slot after the last slot used, and if the set is empty,
         the value does not matter. *)
      match startenv_opt, rev_slots with
      | Some i, _ -> i, false
      | None, [] -> 0, true (* will raise a fatal_error later *)
      | None, (offset, Function_slot { size; _ }) :: _ -> offset + size, true
      | None, (_, Infix_header) :: _ ->
        (* Cannot happen because a infix header is *always* preceded by a
           function slot (because the slot list is reversed) *)
        assert false
      | None, (_, Value_slot _) :: _ ->
        (* Cannot happen because if there is a value slot in the acc, then
           startenv_opt should be Some _ *)
        assert false
    in
    let slots = List.rev rev_slots in
    (* The Gc assumes that a Closure_tag block actually starts with a function
       slot at offset 0. Or more precisely, the GC unconditionally reads the
       second field of a Closure_tag block to find out the start of environment.
       Thus we add a check here to ensure that the slots start with a function
       slot at offset 0. *)
    match slots with
    | (0, Function_slot _) :: _ -> { startenv; slots; empty_env }
    | _ ->
      Misc.fatal_error
        "Sets of closures must start with a function slot at offset 0"

  let print_slot fmt = function
    | Value_slot v -> Format.fprintf fmt "value_slot %a" Value_slot.print v
    | Infix_header -> Format.fprintf fmt "infix_header"
    | Function_slot { size; function_slot } ->
      Format.fprintf fmt "function_slot(%d) %a" size Function_slot.print
        function_slot

  let print fmt l =
    Format.fprintf fmt "@[<v>startenv: %d;@ " l.startenv;
    List.iter
      (fun (i, slot) -> Format.fprintf fmt "@[<h>%d %a@]@," i print_slot slot)
      l.slots;
    Format.fprintf fmt "@]"
end

(* Greedy algorithm *)

module Greedy : sig
  type state

  val print : Format.formatter -> state -> unit

  val create_initial_state : unit -> state

  val create_slots_for_set :
    state ->
    get_code_metadata:(Code_id.t -> Code_metadata.t) ->
    Set_of_closures.t ->
    unit

  val finalize : used_slots:used_slots -> state -> result
end = struct
  (* Greedy algorithm for assigning offsets (in terms of words) to slots.

     The goal is to assign an offset to each slot (function slot or value slot).
     As input we have: - a size for each slot (currently between 1 and 3) - for
     each slot, a set of sets of closures in which it appears - some slots have
     already fixed offsets (i.e. slots from other compilation units)

     The constraints we want to satisfy are: - each slot has a unique offset -
     for each set of closures, the slots that appear in it should not overlap.
     Two slots s and s' overlap s.offset + s.size <= s'.offset (assuming
     s.offset <= s'.offset) - for each set of closures, the offset for all
     function slots is smaller than the offset of all value slots

     Some inputs can be unsatisfiable. This can only occur because of the fixed
     offsets imposed by previous compilation units. However, the current
     flambda2 does not generate such unsatisfiable situations.

     Slots are assigned using a "first comes, first served" basis, filling
     upwards from 0. This is complete (for satisfiable cases), but may return a
     non-optimal assignement of offsets (in the sens that there may be more
     unused fields in sets of closures than necessary), however that case should
     be fairly rare given the current behaviour of Simplify. *)

  (* Internal types *)

  type slot_desc =
    | Function_slot of Function_slot.t
    | Value_slot of Value_slot.t

  type slot_pos =
    | Assigned of words
    | Unassigned
    | Removed

  type set_of_closures =
    { id : int;
      (* Info about start of environment *)
      mutable first_slot_used_by_value_slots : words;
      mutable first_slot_after_function_slots : words;
      (* invariant : first_slot_after_function_slots <=
         first_slot_used_by_calue_slots *)
      mutable allocated_slots : slot Numeric_types.Int.Map.t
    }

  and slot =
    { desc : slot_desc;
      mutable pos : slot_pos;
      mutable size : int;
      mutable sets : set_of_closures list;
      mutable priority : int
          (* guides the order of traversal when assigning offsets. smaller
             priorities are assigned first. *)
    }

  (** Intermediate state to store offsets for function and value slots before
      computing the actual offsets of these elements within a block. *)
  type state =
    { mutable used_offsets : EO.t;
      mutable function_slots : slot Function_slot.Map.t;
      mutable value_slots : slot Value_slot.Map.t;
      mutable sets_of_closures : set_of_closures list;
      mutable function_slots_to_assign : slot list;
      mutable value_slots_to_assign : slot list
    }

  (* Create structures *)

  (* create a fresh slot (with no position allocated yet) *)
  let create_slot ~size desc pos =
    { desc; size; pos; sets = []; priority = max_int }

  let create_set =
    let c = ref 0 in
    fun () ->
      incr c;
      { id = !c;
        first_slot_after_function_slots = 0;
        first_slot_used_by_value_slots = max_int;
        allocated_slots = Numeric_types.Int.Map.empty
      }

  let create_initial_state () =
    { used_offsets = EO.empty;
      function_slots = Function_slot.Map.empty;
      value_slots = Value_slot.Map.empty;
      sets_of_closures = [];
      function_slots_to_assign = [];
      value_slots_to_assign = []
    }

  (* debug printing *)
  let print_set_id fmt s = Format.fprintf fmt "%d" s.id

  let print_set_ids fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_set_id s) l

  let print_desc fmt = function
    | Function_slot c -> Format.fprintf fmt "%a" Function_slot.print c
    | Value_slot v -> Format.fprintf fmt "%a" Value_slot.print v

  let print_slot_pos fmt = function
    | Assigned i -> Format.fprintf fmt "%d" i
    | Unassigned -> Format.fprintf fmt "?"
    | Removed -> Format.fprintf fmt "x"

  let print_slot fmt { pos; size; sets; desc; priority } =
    Format.fprintf fmt
      "@[<h>[pos: %a;@ size: %d;@ sets: %a;@ desc: %a;@ priority: %d]@]@,"
      print_slot_pos pos size print_set_ids sets print_desc desc priority

  let print_slot_map fmt map =
    Numeric_types.Int.Map.iter (fun _ slot -> print_slot fmt slot) map

  let print_slot_list fmt list =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.pp_print_list ~pp_sep print_slot fmt list

  let[@ocamlformat "disable"] print_set fmt s =
    Format.fprintf fmt
      "@[<v 2>%d:@ \
         @[<v>first_slot_after_function_slots: %d;@ \
              first_slot_used_by_value_slots: %d;@ \
              allocated: @[<v>%a@]\
          @]\
        @]"
      s.id
      s.first_slot_after_function_slots
      s.first_slot_used_by_value_slots
      print_slot_map s.allocated_slots

  let print_sets fmt l =
    List.iter (function s -> Format.fprintf fmt "%a@ " print_set s) l

  let [@ocamlformat "disable"] print fmt {
      used_offsets = _; function_slots = _; value_slots = _;
      sets_of_closures; function_slots_to_assign; value_slots_to_assign; } =
    Format.fprintf fmt
      "@[<v 2>\
          Sets of closures:@ %a@ \
          Function slots to assign:@ %a@ \
          Value slots to assign:@ %a\
       @]"
      print_sets sets_of_closures
      print_slot_list function_slots_to_assign
      print_slot_list value_slots_to_assign
  [@@warning "-32"]

  (* Keep the value slots offsets in sets up-to-date *)

  let update_set_for_slot slot set =
    (match slot.pos with
    | Unassigned | Removed -> ()
    | Assigned i -> (
      match slot.desc with
      | Value_slot _ ->
        set.first_slot_used_by_value_slots
          <- min set.first_slot_used_by_value_slots i
      | Function_slot _ ->
        set.first_slot_after_function_slots
          <- max set.first_slot_after_function_slots (i + slot.size)));
    if set.first_slot_used_by_value_slots < set.first_slot_after_function_slots
    then
      Misc.fatal_errorf
        "Set of closures invariant (all function slots before all value slots) \
         is broken"

  (* Slots *)

  let is_function_slot slot =
    match slot.desc with
    | Function_slot _ ->
      assert (slot.size = 2 || slot.size = 3);
      true
    | Value_slot _ ->
      assert (slot.size = 1);
      false

  let add_slot_offset_to_set offset slot set =
    update_set_for_slot slot set;
    let map = set.allocated_slots in
    assert (not (Numeric_types.Int.Map.mem offset map));
    let map = Numeric_types.Int.Map.add offset slot map in
    set.allocated_slots <- map

  let add_slot_offset state slot offset =
    assert (slot.pos = Unassigned);
    slot.pos <- Assigned offset;
    List.iter (add_slot_offset_to_set offset slot) slot.sets;
    match slot.desc with
    | Function_slot c ->
      let (info : EO.function_slot_info) =
        EO.Live_function_slot { offset; size = slot.size }
      in
      state.used_offsets
        <- EO.add_function_slot_offset state.used_offsets c info
    | Value_slot v ->
      let (info : EO.value_slot_info) = EO.Live_value_slot { offset } in
      state.used_offsets <- EO.add_value_slot_offset state.used_offsets v info

  let mark_slot_as_removed state slot =
    match slot.pos with
    | Removed -> Misc.fatal_error "Slot already marked as removed"
    | Unassigned -> (
      slot.pos <- Removed;
      match slot.desc with
      | Function_slot function_slot ->
        let (info : EO.function_slot_info) = EO.Dead_function_slot in
        state.used_offsets
          <- EO.add_function_slot_offset state.used_offsets function_slot info
      | Value_slot v ->
        let (info : EO.value_slot_info) = EO.Dead_value_slot in
        state.used_offsets <- EO.add_value_slot_offset state.used_offsets v info
      )
    | Assigned _ ->
      Misc.fatal_error "Cannot remove slot which is already assigned"

  (* Blocks with tag Closure_tag *)

  let add_unallocated_slot_to_set state slot set =
    slot.sets <- set :: slot.sets;
    update_set_for_slot slot set;
    match slot.desc with
    | Function_slot _ ->
      state.function_slots_to_assign <- slot :: state.function_slots_to_assign
    | Value_slot _ ->
      state.value_slots_to_assign <- slot :: state.value_slots_to_assign

  let add_allocated_slot_to_set slot offset set =
    slot.sets <- set :: slot.sets;
    add_slot_offset_to_set offset slot set

  (* Accumulator state *)

  let use_function_slot_info state c info =
    state.used_offsets <- EO.add_function_slot_offset state.used_offsets c info

  let add_function_slot state function_slot slot =
    state.function_slots
      <- Function_slot.Map.add function_slot slot state.function_slots

  let use_value_slot_info state var info =
    state.used_offsets <- EO.add_value_slot_offset state.used_offsets var info

  let add_value_slot state var slot =
    state.value_slots <- Value_slot.Map.add var slot state.value_slots

  let find_function_slot state closure =
    Function_slot.Map.find_opt closure state.function_slots

  let find_value_slot state var = Value_slot.Map.find_opt var state.value_slots

  (* Create slots (and create the cross-referencing). *)

  let create_function_slot set state get_code_metadata function_slot code_id =
    if Compilation_unit.is_current
         (Function_slot.get_compilation_unit function_slot)
    then (
      let size =
        let code_metadata = get_code_metadata code_id in
        let module CM = Code_metadata in
        let is_tupled = CM.is_tupled code_metadata in
        let params_arity = CM.params_arity code_metadata in
        let arity = Flambda_arity.With_subkinds.cardinal params_arity in
        if arity = 1 && not is_tupled then 2 else 3
      in
      let s = create_slot ~size (Function_slot function_slot) Unassigned in
      add_function_slot state function_slot s;
      add_unallocated_slot_to_set state s set)
    else
      (* We should be guaranteed that the corresponding compilation unit's cmx
         file has been read during the downward traversal. *)
      let imported_offsets = EO.imported_offsets () in
      match EO.function_slot_offset imported_offsets function_slot with
      | None ->
        (* This means that there is no cmx for the given function slot (either
           because of opaque, (or missing cmx ?), or that the offset is missing
           from the cmx. In any case, this is a hard error: the function slot
           must have been given an offset by its own compilation unit, and we
           must know it to avoid choosing a different one. *)
        Misc.fatal_errorf
          "Could not find the offset for function slot %a from another \
           compilation unit (because of -opaque, or missing cmx)."
          Function_slot.print function_slot
      | Some Dead_function_slot ->
        Misc.fatal_errorf
          "The function slot %a is dead in its original compilation unit, it \
           should not occur in a set of closures in this compilation unit."
          Function_slot.print function_slot
      | Some (Live_function_slot { offset; size } as info) ->
        let s =
          create_slot ~size (Function_slot function_slot) (Assigned offset)
        in
        use_function_slot_info state function_slot info;
        add_function_slot state function_slot s;
        add_allocated_slot_to_set s offset set

  let create_value_slot set state value_slot =
    if Compilation_unit.is_current (Value_slot.get_compilation_unit value_slot)
    then (
      let s = create_slot ~size:1 (Value_slot value_slot) Unassigned in
      add_value_slot state value_slot s;
      add_unallocated_slot_to_set state s set)
    else
      (* Same as the comments for the function_slots *)
      let imported_offsets = EO.imported_offsets () in
      match EO.value_slot_offset imported_offsets value_slot with
      | None ->
        (* See comment for the function_slot *)
        Misc.fatal_errorf
          "Could not find the offset for value slot %a from another \
           compilation unit (because of -opaque, or missing cmx)."
          Value_slot.print value_slot
      | Some Dead_value_slot ->
        Misc.fatal_errorf
          "The value slot %a has been removed by its original compilation \
           unit, it should not occur in a set of closures in this compilation \
           unit."
          Value_slot.print value_slot
      | Some (Live_value_slot { offset } as info) ->
        let s = create_slot ~size:1 (Value_slot value_slot) (Assigned offset) in
        use_value_slot_info state value_slot info;
        add_value_slot state value_slot s;
        add_allocated_slot_to_set s offset set

  let create_slots_for_set state ~get_code_metadata set_of_closures =
    let set = create_set () in
    state.sets_of_closures <- set :: state.sets_of_closures;
    (* Fill closure slots *)
    let closure_map =
      let function_decls = Set_of_closures.function_decls set_of_closures in
      Function_declarations.funs function_decls
    in
    Function_slot.Map.iter
      (fun function_slot code_id ->
        if not (Function_slot.Map.mem function_slot state.function_slots)
        then
          create_function_slot set state get_code_metadata function_slot code_id)
      closure_map;
    (* Fill value slot slots *)
    let env_map = Set_of_closures.value_slots set_of_closures in
    Value_slot.Map.iter
      (fun value_slot _ ->
        if not (Value_slot.Map.mem value_slot state.value_slots)
        then create_value_slot set state value_slot)
      env_map

  (* Find the first space available to fit a given slot.

     This function returns the first free offset with enough space to fit the
     slot (potential header included), but points at the start of the free space
     (so the header word for function slots). Function {assign_offset} is here
     to compute the actual offset/position from this free space start position.

     This function is a bit more complicated than necessary because each slot's
     size does not include the headers for function slots. There are two reasons
     for that choice:

     - the function slot at offset 0 does not need a header since it uses the
     header of the whole block, so the necessity of a header is actually
     dependant on the position of the function slot.

     - that way, the offset/position of a slot corresponds to the actual ocaml
     pointer (which points at the first field of a block rather than the
     header). *)

  let first_free_offset slot set start =
    let map = set.allocated_slots in
    (* space needed to fit a slot at the current offset. *)
    let needed_space curr =
      if is_function_slot slot && curr <> 0 then slot.size + 1 else slot.size
    in
    (* first offset used by a slot *)
    let first_used_by s =
      match s.pos with
      | Unassigned | Removed -> assert false
      | Assigned pos -> if is_function_slot s && pos <> 0 then pos - 1 else pos
    in
    (* first potentially free offset after a slot *)
    let first_free_after slot =
      match slot.pos with
      | Unassigned | Removed -> assert false
      | Assigned i -> i + slot.size
    in
    (* Adjust a starting position to not point in the middle of a block.
       Additionally, ensure the value slot slots are put after the function
       slots. *)
    let adjust (curr : int) =
      let curr =
        if is_function_slot slot
        then curr
        else max curr set.first_slot_after_function_slots
      in
      match Numeric_types.Int.Map.find_last (fun i -> i <= curr) map with
      | exception Not_found -> curr
      | j, s ->
        assert (Assigned j = s.pos);
        max curr (first_free_after s)
    in
    (* find the first available space for the slot. *)
    let rec loop curr =
      match Numeric_types.Int.Map.find_first (fun i -> i >= curr) map with
      | exception Not_found -> curr
      | _, next_slot ->
        let available_space = first_used_by next_slot - curr in
        assert (available_space >= 0);
        if available_space >= needed_space curr
        then curr
        else loop (first_free_after next_slot)
    in
    loop (adjust start)

  (** Assign an offset using the current offset, assuming there is enough space *)
  let assign_offset slot offset =
    if not (is_function_slot slot)
    then
      offset
      (* Function slots need a header (Infix_tag) before them, except for the
         first one (which uses the Closure_tag header of the block itself). *)
    else if offset = 0
    then offset
    else offset + 1

  (* Loop to find the first free offset available for a slot given the set of
     sets in which it appears. *)
  let rec first_available_offset slot start first_set other_sets =
    let aux ((_, offset) as acc) s =
      let new_offset = first_free_offset slot s offset in
      assert (new_offset >= offset);
      if new_offset = offset then acc else true, new_offset
    in
    let start = first_free_offset slot first_set start in
    let changed, offset = List.fold_left aux (false, start) other_sets in
    if not changed
    then assign_offset slot offset
    else first_available_offset slot offset first_set other_sets

  let first_available_offset slot start = function
    | s :: r -> first_available_offset slot start s r
    | [] ->
      (* Internal invariant: a slot cannot have an empty list of sets it belongs
         to (at least not slots for which we need to assign an offset), thus
         this case cannot happen. *)
      assert false

  (* Assign offsets to function slots *)

  let assign_slot_offset state slot =
    match slot.pos with
    | Unassigned ->
      let offset = first_available_offset slot 0 slot.sets in
      add_slot_offset state slot offset
    | Assigned _pos -> Misc.fatal_error "Slot has already been assigned"
    | Removed ->
      Misc.fatal_error
        "Slot has been explicitly removed, it cannot be assigned anymore"

  let assign_function_slot_offsets state =
    let function_slots_to_assign = state.function_slots_to_assign in
    state.function_slots_to_assign <- [];
    (* TODO: sort the work queue *)
    List.iter (assign_slot_offset state) function_slots_to_assign

  let assign_value_slot_offsets ~used_value_slots state =
    let value_slots_to_assign = state.value_slots_to_assign in
    state.value_slots_to_assign <- [];
    (* TODO: sort the work queue *)
    List.iter
      (function
        | { desc = Value_slot v; _ } as slot ->
          if value_slot_is_used ~used_value_slots v
          then assign_slot_offset state slot
          else mark_slot_as_removed state slot
        | { desc = Function_slot _; _ } ->
          Misc.fatal_error "function slot in queue of value slot to assign")
      value_slots_to_assign

  (* Ensure function slots/value slots that appear in the code for the current
     compilation unit are present in the offsets returned by finalize *)
  let add_used_imported_offsets ~used_slots state =
    let { function_slots_in_normal_projections;
          all_function_slots;
          value_slots_in_normal_projections;
          all_value_slots
        } =
      used_slots
    in
    state.used_offsets
      <- state.used_offsets
         |> EO.reexport_function_slots function_slots_in_normal_projections
         |> EO.reexport_function_slots all_function_slots
         |> EO.reexport_value_slots value_slots_in_normal_projections
         |> EO.reexport_value_slots all_value_slots

  (* We only want to keep value slots that appear in the creation of a set of
     closures, *and* appear as projection (at normal name mode). And we need to
     mark value_slots/ids that are not live, as dead in the exported_offsets, so
     that later compilation unit do not mistake that for a missing offset info
     on a value_slot/id. *)
  let live_value_slots state
      { value_slots_in_normal_projections;
        function_slots_in_normal_projections;
        _
      } =
    Function_slot.Set.iter
      (fun function_slot ->
        if Compilation_unit.is_current
             (Function_slot.get_compilation_unit function_slot)
        then
          match find_function_slot state function_slot with
          | Some _ -> ()
          | None ->
            state.used_offsets
              <- EO.add_function_slot_offset state.used_offsets function_slot
                   Dead_function_slot)
      function_slots_in_normal_projections;
    Value_slot.Set.filter
      (fun value_slot ->
        if Compilation_unit.is_current
             (Value_slot.get_compilation_unit value_slot)
        then (
          (* a value slot appears in a set of closures iff it has a slot *)
          match find_value_slot state value_slot with
          | Some _ -> true
          | None ->
            state.used_offsets
              <- EO.add_value_slot_offset state.used_offsets value_slot
                   Dead_value_slot;
            false)
        else true)
      value_slots_in_normal_projections

  (* Transform an internal accumulator state for slots into an actual mapping
     that assigns offsets. *)
  let finalize ~used_slots state =
    add_used_imported_offsets ~used_slots state;
    let used_value_slots = live_value_slots state used_slots in
    assign_function_slot_offsets state;
    assign_value_slot_offsets ~used_value_slots state;
    { used_value_slots; exported_offsets = state.used_offsets }
end

type t = Set_of_closures.t list

let print fmt l =
  Format.fprintf fmt "@[<hv>%a@]" (Format.pp_print_list Set_of_closures.print) l

let empty = []

let add_set_of_closures l ~is_phantom set_of_closures =
  if is_phantom then l else set_of_closures :: l

let add_offsets_from_function l1 ~from_function:l2 =
  (* Order is irrelevant *)
  List.rev_append l2 l1

let finalize_offsets ~get_code_metadata ~used_slots l =
  let state = ref (Greedy.create_initial_state ()) in
  Misc.try_finally
    (fun () ->
      List.iter (Greedy.create_slots_for_set !state ~get_code_metadata) l;
      Greedy.finalize ~used_slots !state)
    ~always:(fun () ->
      if Flambda_features.dump_slot_offsets ()
      then Format.eprintf "%a@." Greedy.print !state)
