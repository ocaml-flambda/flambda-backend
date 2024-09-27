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

let[@inline] function_slot_is_used ~used_function_slots v =
  if Compilation_unit.is_current (Function_slot.get_compilation_unit v)
  then Function_slot.Set.mem v used_function_slots
  else true

let[@inline] unboxed_slot_is_used ~used_unboxed_slots v =
  if Compilation_unit.is_current (Value_slot.get_compilation_unit v)
  then Value_slot.Set.mem v used_unboxed_slots
  else true

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
 * | unboxed slot 0       |
 * |----------------------|
 * | unboxed slot 1       |
 * |----------------------|
 * .                      .
 * .                      .
 * |----------------------|
 * | last unboxed slot    |
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

   Due to the representation above, all function slots must occur before all
   unboxed slots, which themselves must be before all value slots. *)

module Layout = struct
  type slot =
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

  type t =
    { startenv : words;
      empty_env : bool;
      slots : (words * slot) list
    }

  let print_slot fmt = function
    | Value_slot { size; is_scanned; value_slot } ->
      Format.fprintf fmt "value_slot(%d,%b) %a" size is_scanned Value_slot.print
        value_slot
    | Infix_header -> Format.fprintf fmt "infix_header"
    | Function_slot { size; function_slot; last_function_slot } ->
      Format.fprintf fmt "function_slot%s(%d) %a"
        (if last_function_slot then "[last]" else "")
        size Function_slot.print function_slot

  let print fmt l =
    Format.fprintf fmt "@[<v>startenv: %d;@ " l.startenv;
    List.iter
      (fun (i, slot) -> Format.fprintf fmt "@[<h>%d %a@]@," i print_slot slot)
      l.slots;
    Format.fprintf fmt "@]"

  let order_function_slots env l acc =
    Function_slot.Lmap.fold
      (fun function_slot _ acc ->
        match EO.function_slot_offset env function_slot with
        | Some Dead_function_slot -> acc
        | Some (Live_function_slot { size; offset }) ->
          Numeric_types.Int.Map.add offset
            (Function_slot { size; function_slot; last_function_slot = false })
            acc
        | None ->
          Misc.fatal_errorf "No function_slot offset for %a" Function_slot.print
            function_slot)
      l acc

  let mark_last_function_slot map =
    match Numeric_types.Int.Map.max_binding map with
    | offset, Function_slot slot ->
      Numeric_types.Int.Map.add offset
        (Function_slot { slot with last_function_slot = true })
        map
    | _, (Value_slot _ | Infix_header) ->
      Misc.fatal_errorf
        "Slot_offsets: function slots should be added before any other so that \
         the last function slot can be computed correctly"
    | exception Not_found ->
      Misc.fatal_errorf
        "Slot_offsets: set of closures msut have at least one function slot"

  let order_value_slots env l acc =
    Value_slot.Map.fold
      (fun value_slot _ acc ->
        match EO.value_slot_offset env value_slot with
        | Some Dead_value_slot -> acc
        | Some (Live_value_slot { offset; is_scanned; size }) ->
          Numeric_types.Int.Map.add offset
            (Value_slot { value_slot; is_scanned; size })
            acc
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
      assert (match acc_slots with [] -> true | _ :: _ -> false);
      assert (Option.is_none startenv);
      (* see comment above *)
      let acc_slots = [0, slot] in
      startenv, acc_slots
    | Function_slot _ ->
      assert (Option.is_none startenv);
      (* see comment above *)
      let acc_slots =
        (offset, slot) :: (offset - 1, Infix_header) :: acc_slots
      in
      startenv, acc_slots
    | Value_slot { is_scanned; _ } ->
      let startenv =
        match startenv with
        | Some i ->
          assert is_scanned;
          assert (i < offset);
          startenv
        | None -> if is_scanned then Some offset else None
      in
      let acc_slots = (offset, slot) :: acc_slots in
      startenv, acc_slots
    | Infix_header ->
      (* Internal invariant: such layout slots are not generated by the {order}
         function, so they should not appear. *)
      assert false

  let make env function_slots value_slots =
    (* Function slots must be added first to the map so that we can then
       identify the last function slot *)
    let map =
      Numeric_types.Int.Map.empty
      |> order_function_slots env function_slots
      |> mark_last_function_slot
      |> order_value_slots env value_slots
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
      | None, (offset, Value_slot { is_scanned = false; size; _ }) :: _ ->
        offset + size, false
      | None, (_, Infix_header) :: _ ->
        (* Cannot happen because a infix header is *always* preceded by a
           function slot (because the slot list is reversed) *)
        assert false
      | None, (_, Value_slot { is_scanned = true; _ }) :: _ ->
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
    let res = { startenv; slots; empty_env } in
    match slots with
    | (0, Function_slot _) :: _ -> res
    | [] | (_, (Function_slot _ | Infix_header | Value_slot _)) :: _ ->
      Misc.fatal_errorf
        "Sets of closures must start with a function slot at offset 0:@\n%a"
        print res
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

  (* *)

  type value_slot = Value

  type unboxed_slot = Unboxed

  type function_slot = Function

  (* silence warning 37 (unused constructor) *)
  let _ = Value, Unboxed, Function

  type _ slot_desc =
    | Function_slot : Function_slot.t -> function_slot slot_desc
    | Unboxed_slot : Value_slot.t -> unboxed_slot slot_desc
    | Scannable_value_slot : Value_slot.t -> value_slot slot_desc

  (* This module helps to distinguish between the two different notions of
     offsets that are used for function slots:

     - the exported offset (i.e. the one used with Exported_offsets.* , and that
     is used by to_cmm), it the pointer to the first word of the closure, i.e it
     points **after** the header for the closure (whether it is the Closure
     header or the Infix header)

     - when computing offsets we instead need to use the first offset in the
     block actually used by a slot, which includes the Infix header (but not the
     Closure header). *)
  module Exported_offset : sig
    type t

    val print : Format.formatter -> t -> unit

    val from_exported_offset : int -> t

    val mk : _ slot_desc -> first_offset_used_including_header:int -> t

    val range_used_by : _ slot_desc -> t -> slot_size:int -> int * int

    val add_slot_to_exported_offsets :
      EO.t -> _ slot_desc -> t -> slot_size:int -> EO.t
  end = struct
    type t = Offset of words
    (* This is the offset as exported, i.e. for function slots it points
       **after** the header word. *)
    [@@unboxed]

    let print fmt (Offset pos) = Format.fprintf fmt "%d" pos

    let from_exported_offset pos = Offset pos

    let mk (type a) (slot : a slot_desc) ~first_offset_used_including_header =
      let offset =
        match slot with
        | Function_slot _ -> first_offset_used_including_header + 1
        | Unboxed_slot _ | Scannable_value_slot _ ->
          first_offset_used_including_header
      in
      Offset offset

    let range_used_by (type a) (slot : a slot_desc) (Offset pos) ~slot_size =
      match slot with
      | Function_slot _ -> pos - 1, pos + slot_size
      | Unboxed_slot _ | Scannable_value_slot _ -> pos, pos + slot_size

    let add_slot_to_exported_offsets (type a) offsets (slot : a slot_desc)
        (Offset pos) ~slot_size =
      match slot with
      | Function_slot function_slot ->
        let (info : EO.function_slot_info) =
          EO.Live_function_slot { offset = pos; size = slot_size }
        in
        EO.add_function_slot_offset offsets function_slot info
      | Unboxed_slot unboxed_slot ->
        let (info : EO.value_slot_info) =
          EO.Live_value_slot
            { offset = pos; is_scanned = false; size = slot_size }
        in
        EO.add_value_slot_offset offsets unboxed_slot info
      | Scannable_value_slot value_slot ->
        let (info : EO.value_slot_info) =
          EO.Live_value_slot
            { offset = pos; is_scanned = true; size = slot_size }
        in
        EO.add_value_slot_offset offsets value_slot info
  end

  (* Internal types *)

  type slot_pos =
    | Assigned of Exported_offset.t
    | Unassigned
    | Removed

  type set_of_closures =
    { id : int;
      (* metadata used for priorities *)
      num_value_slots : int;
      num_function_slots : int;
      (* Info about transitions between different types of slots *)
      mutable first_slot_after_function_slots : words;
      mutable first_slot_used_by_unboxed_slots : words;
      mutable first_slot_after_unboxed_slots : words;
      mutable first_slot_used_by_value_slots : words;
      (* invariants :
       * first_slot_after_function_slots <= first_slot_used_by_unboxed_slots
       * first_slot_after_function_slots <= first_slot_after_unboxed_slots
       * first_slot_after_unboxed_slots <= first_slot_used_by_value_slots
       * first_slot_after_function_slots <= first_slot_used_by_value_slots
       *)
      mutable allocated_slots : any_slot Numeric_types.Int.Map.t
          (* map indexed by the offset of the first word used by a slot
             (including its infix header if it exists). *)
    }

  and 'a slot =
    { desc : 'a slot_desc;
      mutable pos : slot_pos;
      mutable size : int;
      mutable sets : set_of_closures list;
      (* metadata used for priorities *)
      mutable occurrences : int;
      mutable lowest_num_slots_in_sets : int
    }

  and any_slot = Slot : _ slot -> any_slot [@@unboxed]

  (** Intermediate state to store offsets for function and value slots before
      computing the actual offsets of these elements within a block. *)
  type state =
    { mutable used_offsets : EO.t;
      mutable function_slots : function_slot slot Function_slot.Map.t;
      mutable unboxed_slots : unboxed_slot slot Value_slot.Map.t;
      mutable value_slots : value_slot slot Value_slot.Map.t;
      mutable sets_of_closures : set_of_closures list;
      mutable function_slots_to_assign : function_slot slot list;
      mutable unboxed_slots_to_assign : unboxed_slot slot list;
      mutable value_slots_to_assign : value_slot slot list
    }

  (* Create structures *)

  (* create a fresh slot (with no position allocated yet) *)
  let create_slot ~size desc pos =
    { desc;
      size;
      pos;
      sets = [];
      occurrences = 0;
      lowest_num_slots_in_sets = max_int
    }

  let create_set =
    let c = ref 0 in
    fun ~num_value_slots ~num_function_slots ->
      incr c;
      { id = !c;
        num_value_slots;
        num_function_slots;
        first_slot_after_function_slots = 0;
        first_slot_used_by_unboxed_slots = max_int;
        first_slot_after_unboxed_slots = 0;
        first_slot_used_by_value_slots = max_int;
        allocated_slots = Numeric_types.Int.Map.empty
      }

  let create_initial_state () =
    { used_offsets = EO.empty;
      function_slots = Function_slot.Map.empty;
      unboxed_slots = Value_slot.Map.empty;
      value_slots = Value_slot.Map.empty;
      sets_of_closures = [];
      function_slots_to_assign = [];
      unboxed_slots_to_assign = [];
      value_slots_to_assign = []
    }

  (* debug printing *)
  let print_set_id fmt s = Format.fprintf fmt "%d" s.id

  let print_set_ids fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_set_id s) l

  let print_desc (type a) fmt (slot_desc : a slot_desc) =
    match slot_desc with
    | Function_slot c -> Format.fprintf fmt "%a" Function_slot.print c
    | Unboxed_slot v | Scannable_value_slot v ->
      Format.fprintf fmt "%a" Value_slot.print v

  let print_slot_pos fmt = function
    | Assigned offset -> Format.fprintf fmt "%a" Exported_offset.print offset
    | Unassigned -> Format.fprintf fmt "?"
    | Removed -> Format.fprintf fmt "x"

  let print_slot fmt
      { pos; size; sets; desc; occurrences; lowest_num_slots_in_sets } =
    Format.fprintf fmt
      "@[<h>[pos: %a;@ size: %d;@ sets: %a;@ desc: %a;@ occurrences: %d;@ \
       lowest_num_slots_in_sets: %d]@]@,"
      print_slot_pos pos size print_set_ids sets print_desc desc occurrences
      lowest_num_slots_in_sets

  let print_any_slot fmt (Slot slot) = print_slot fmt slot

  let print_any_slot_map fmt map =
    Numeric_types.Int.Map.iter (fun _ slot -> print_any_slot fmt slot) map

  let print_slot_list fmt list =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.pp_print_list ~pp_sep print_slot fmt list

  let[@ocamlformat "disable"] print_set fmt s =
    Format.fprintf fmt
      "@[<v 2>%d:@ \
         @[<v>first_slot_after_function_slots: %d;@ \
              first_slot_used_by_unboxed_slots: %d;@ \
              first_slot_after_unboxed_slots: %d;@ \
              first_slot_used_by_value_slots: %d;@ \
              allocated: @[<v>%a@]\
          @]\
        @]"
      s.id
      s.first_slot_after_function_slots
      s.first_slot_used_by_unboxed_slots
      s.first_slot_after_unboxed_slots
      s.first_slot_used_by_value_slots
      print_any_slot_map s.allocated_slots

  let print_sets fmt l =
    List.iter (function s -> Format.fprintf fmt "%a@ " print_set s) l

  let [@ocamlformat "disable"] print fmt {
      used_offsets = _; function_slots = _; unboxed_slots = _; value_slots = _;
      sets_of_closures; function_slots_to_assign; unboxed_slots_to_assign; value_slots_to_assign; } =
    Format.fprintf fmt
      "@[<hov 1>(@,\
          (function slots to assign@ @[<hov>%a@])@ \
          (unboxed slots to assign@ @[<hov>%a@])@ \
          (value slots to assign@ @[<hov>%a@])\
          (sets of closures@ @[<hov>%a@])@,\
       )@]"
      print_slot_list function_slots_to_assign
      print_slot_list unboxed_slots_to_assign
      print_slot_list value_slots_to_assign
      print_sets sets_of_closures
  [@@warning "-32"]

  (* Keep the value slots offsets in sets up-to-date *)

  let update_set_for_slot (type a) (slot : a slot) set =
    (match slot.pos with
    | Unassigned | Removed -> ()
    | Assigned offset -> (
      match slot.desc with
      | Scannable_value_slot _ ->
        if slot.size <> 1
        then
          Misc.fatal_errorf "Value slot has size %d, which is not 1." slot.size;
        let start, _ =
          Exported_offset.range_used_by slot.desc offset ~slot_size:1
        in
        set.first_slot_used_by_value_slots
          <- min set.first_slot_used_by_value_slots start;
        set.first_slot_used_by_unboxed_slots
          <- min set.first_slot_used_by_unboxed_slots start
      | Unboxed_slot _ ->
        let start, last =
          Exported_offset.range_used_by slot.desc offset ~slot_size:slot.size
        in
        set.first_slot_used_by_unboxed_slots
          <- min set.first_slot_used_by_unboxed_slots start;
        set.first_slot_after_unboxed_slots
          <- max set.first_slot_after_unboxed_slots last
      | Function_slot _ ->
        let _, last =
          Exported_offset.range_used_by slot.desc offset ~slot_size:slot.size
        in
        set.first_slot_after_function_slots
          <- max set.first_slot_after_function_slots last;
        set.first_slot_after_unboxed_slots
          <- max set.first_slot_after_unboxed_slots last));
    if set.first_slot_used_by_value_slots < set.first_slot_after_unboxed_slots
       || set.first_slot_used_by_unboxed_slots
          < set.first_slot_after_function_slots
    then Misc.fatal_errorf "Set of closures invariant (slot ordering) is broken"

  (* Slots *)

  let range_used_by slot =
    match slot.pos with
    | Unassigned | Removed -> assert false
    | Assigned offset ->
      Exported_offset.range_used_by slot.desc offset ~slot_size:slot.size

  let add_slot_offset_to_set (type a) (slot : a slot) set =
    let first_used_offset_by_slot, _ = range_used_by slot in
    update_set_for_slot slot set;
    assert (
      not
        (Numeric_types.Int.Map.mem first_used_offset_by_slot set.allocated_slots));
    set.allocated_slots
      <- Numeric_types.Int.Map.add first_used_offset_by_slot (Slot slot)
           set.allocated_slots

  let add_slot_offset state slot offset =
    assert (
      match slot.pos with Unassigned -> true | Removed | Assigned _ -> false);
    slot.pos <- Assigned offset;
    List.iter (add_slot_offset_to_set slot) slot.sets;
    state.used_offsets
      <- Exported_offset.add_slot_to_exported_offsets state.used_offsets
           slot.desc offset ~slot_size:slot.size

  let mark_slot_as_removed (type a) state (slot : a slot) =
    match slot.pos with
    | Removed -> Misc.fatal_error "Slot already marked as removed"
    | Unassigned -> (
      slot.pos <- Removed;
      match slot.desc with
      | Function_slot function_slot ->
        let (info : EO.function_slot_info) = EO.Dead_function_slot in
        state.used_offsets
          <- EO.add_function_slot_offset state.used_offsets function_slot info
      | Unboxed_slot v | Scannable_value_slot v ->
        let (info : EO.value_slot_info) = EO.Dead_value_slot in
        state.used_offsets <- EO.add_value_slot_offset state.used_offsets v info
      )
    | Assigned _ ->
      Misc.fatal_error "Cannot remove slot which is already assigned"

  (* Blocks with tag Closure_tag *)

  let add_unallocated_slot_to_set (type a) state (slot : a slot) set =
    slot.sets <- set :: slot.sets;
    update_set_for_slot slot set;
    match slot.desc with
    | Function_slot _ ->
      state.function_slots_to_assign <- slot :: state.function_slots_to_assign
    | Unboxed_slot _ ->
      state.unboxed_slots_to_assign <- slot :: state.unboxed_slots_to_assign
    | Scannable_value_slot _ ->
      state.value_slots_to_assign <- slot :: state.value_slots_to_assign

  let add_allocated_slot_to_set slot set =
    slot.sets <- set :: slot.sets;
    add_slot_offset_to_set slot set

  let update_metadata_for_function_slot set slot =
    slot.occurrences <- slot.occurrences + 1;
    slot.lowest_num_slots_in_sets
      <- min slot.lowest_num_slots_in_sets set.num_function_slots

  let update_metadata_for_unboxed_slot set slot =
    slot.occurrences <- slot.occurrences + 1;
    slot.lowest_num_slots_in_sets
      <- min slot.lowest_num_slots_in_sets set.num_value_slots

  let update_metadata_for_value_slot set slot =
    slot.occurrences <- slot.occurrences + 1;
    slot.lowest_num_slots_in_sets
      <- min slot.lowest_num_slots_in_sets set.num_value_slots

  (* priority work queue *)

  let compare_priority slot1 slot2 =
    match slot1.occurrences, slot2.occurrences with
    (* slots not shared are put at the end of the work queue, in an order that
       doesn't matter since they are not shared *)
    | 1, 1 -> 0
    | 1, _ -> 1
    | _, 1 -> -1
    | _, _ -> (
      match
        compare slot1.lowest_num_slots_in_sets slot2.lowest_num_slots_in_sets
      with
      | 0 -> (
        (* slots with the largest number of occurrences first *)
        match compare slot2.occurrences slot1.occurrences with
        | 0 ->
          (* slots of size 3 before those of size 2 *)
          compare slot2.size slot1.size
        | c -> c)
      | c ->
        (* slots with the smallest "lowest number of slots" in sets first *)
        c)

  (* Accumulator state *)

  let use_function_slot_info state c info =
    state.used_offsets <- EO.add_function_slot_offset state.used_offsets c info

  let add_function_slot state function_slot slot =
    state.function_slots
      <- Function_slot.Map.add function_slot slot state.function_slots

  let use_unboxed_slot_info state var info =
    state.used_offsets <- EO.add_value_slot_offset state.used_offsets var info

  let add_unboxed_slot state var slot =
    state.unboxed_slots <- Value_slot.Map.add var slot state.unboxed_slots

  let use_value_slot_info state var info =
    state.used_offsets <- EO.add_value_slot_offset state.used_offsets var info

  let add_value_slot state var slot =
    state.value_slots <- Value_slot.Map.add var slot state.value_slots

  let find_function_slot state closure =
    Function_slot.Map.find_opt closure state.function_slots

  let find_unboxed_slot state var =
    Value_slot.Map.find_opt var state.unboxed_slots

  let find_value_slot state var = Value_slot.Map.find_opt var state.value_slots

  (* Create slots (and create the cross-referencing). *)

  let create_function_slot set state get_code_metadata function_slot
      (code_id : Function_declarations.code_id_in_function_declaration) =
    if Compilation_unit.is_current
         (Function_slot.get_compilation_unit function_slot)
    then (
      let size =
        match code_id with
        | Deleted { function_slot_size } -> function_slot_size
        | Code_id code_id ->
          let code_metadata = get_code_metadata code_id in
          let module CM = Code_metadata in
          let is_tupled = CM.is_tupled code_metadata in
          let params_arity = CM.params_arity code_metadata in
          let arity = Flambda_arity.num_params params_arity in
          if (arity = 0 || arity = 1) && not is_tupled then 2 else 3
      in
      let s = create_slot ~size (Function_slot function_slot) Unassigned in
      add_function_slot state function_slot s;
      add_unallocated_slot_to_set state s set;
      s)
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
        let offset = Exported_offset.from_exported_offset offset in
        let s =
          create_slot ~size (Function_slot function_slot) (Assigned offset)
        in
        use_function_slot_info state function_slot info;
        add_function_slot state function_slot s;
        add_allocated_slot_to_set s set;
        s

  let create_unboxed_slot set state value_slot size =
    if Compilation_unit.is_current (Value_slot.get_compilation_unit value_slot)
    then (
      let s = create_slot ~size (Unboxed_slot value_slot) Unassigned in
      add_unboxed_slot state value_slot s;
      add_unallocated_slot_to_set state s set;
      s)
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
      | Some (Live_value_slot { offset; is_scanned; size = sz } as info) ->
        if is_scanned || sz <> size
        then
          Misc.fatal_errorf
            "The unboxed slot %a existed but was not unboxed or of a different \
             size in the original compilation unit, this should not happen."
            Value_slot.print value_slot;
        let offset = Exported_offset.from_exported_offset offset in
        let s = create_slot ~size (Unboxed_slot value_slot) (Assigned offset) in
        use_unboxed_slot_info state value_slot info;
        add_unboxed_slot state value_slot s;
        add_allocated_slot_to_set s set;
        s

  let create_value_slot set state value_slot =
    if Compilation_unit.is_current (Value_slot.get_compilation_unit value_slot)
    then (
      let s =
        create_slot ~size:1 (Scannable_value_slot value_slot) Unassigned
      in
      add_value_slot state value_slot s;
      add_unallocated_slot_to_set state s set;
      s)
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
      | Some (Live_value_slot { offset; is_scanned; size = sz } as info) ->
        if (not is_scanned) || sz <> 1
        then
          Misc.fatal_errorf
            "The value slot %a existed but was unboxed or of a different size \
             in the original compilation unit, this should not happen."
            Value_slot.print value_slot;
        let offset = Exported_offset.from_exported_offset offset in
        let s =
          create_slot ~size:1 (Scannable_value_slot value_slot)
            (Assigned offset)
        in
        use_value_slot_info state value_slot info;
        add_value_slot state value_slot s;
        add_allocated_slot_to_set s set;
        s

  let create_slots_for_set state ~get_code_metadata set_of_closures =
    let env_map = Set_of_closures.value_slots set_of_closures in
    let closure_map =
      let function_decls = Set_of_closures.function_decls set_of_closures in
      Function_declarations.funs function_decls
    in
    let set =
      create_set
        ~num_value_slots:(Value_slot.Map.cardinal env_map)
        ~num_function_slots:(Function_slot.Map.cardinal closure_map)
    in
    state.sets_of_closures <- set :: state.sets_of_closures;
    (* Fill closure slots *)
    Function_slot.Map.iter
      (fun function_slot code_id ->
        let s =
          match
            Function_slot.Map.find_opt function_slot state.function_slots
          with
          | None ->
            create_function_slot set state get_code_metadata function_slot
              code_id
          | Some s ->
            s.sets <- set :: s.sets;
            update_set_for_slot s set;
            s
        in
        update_metadata_for_function_slot set s)
      closure_map;
    (* Fill value slot slots *)
    Value_slot.Map.iter
      (fun value_slot _ ->
        let kind = Value_slot.kind value_slot in
        let size, is_unboxed =
          match Flambda_kind.With_subkind.kind kind with
          | Region | Rec_info ->
            Misc.fatal_errorf "Value slot %a has Region or Rec_info kind"
              Value_slot.print value_slot
          | Naked_number
              ( Naked_immediate | Naked_float | Naked_float32 | Naked_int32
              | Naked_int64 | Naked_nativeint ) ->
            1, true
          (* flambda2 only supports 64-bit targets for now, so naked numbers can
             only be of size 1 *)
          | Naked_number Naked_vec128 -> 2, true
          | Value -> (
            match[@ocaml.warning "-4"]
              Flambda_kind.With_subkind.subkind kind
            with
            | Tagged_immediate -> 1, true
            | _ -> 1, false)
        in
        if is_unboxed
        then
          let s =
            match Value_slot.Map.find_opt value_slot state.unboxed_slots with
            | None -> create_unboxed_slot set state value_slot size
            | Some s ->
              s.sets <- set :: s.sets;
              update_set_for_slot s set;
              s
          in
          update_metadata_for_unboxed_slot set s
        else
          let s =
            match Value_slot.Map.find_opt value_slot state.value_slots with
            | None -> create_value_slot set state value_slot
            | Some s ->
              s.sets <- set :: s.sets;
              update_set_for_slot s set;
              s
          in
          update_metadata_for_value_slot set s)
      env_map

  (* Find the first space available to fit a given slot.

     This function returns the first free offset with enough space to fit the
     slot (potential header included), but points at the start of the free space
     (so the header word for function slots).

     In this function, for function slots, we manipulate offsets that include
     the header (for both Infix header and Closure header). That means that we
     start the search at offset -1, which is a valid offset for the first
     function slot.

     Note that since we enforce that each set of closures must have at least one
     function slot (and that function slot must have a smaller offset than value
     slots), we also guarantee that a value slot cannot be assigned at offset
     -1. *)

  let first_free_offset (type a) (slot : a slot) set start =
    (* space needed to fit the slot *)
    let needed_space =
      match slot.desc with
      | Function_slot _ -> slot.size + 1 (* header word *)
      | Unboxed_slot _ | Scannable_value_slot _ -> slot.size
    in
    (* Ensure that for value slots, we are after all function slots. *)
    let curr =
      match slot.desc with
      | Function_slot _ -> start
      | Unboxed_slot _ ->
        (* first_slot_after_function_slots is always >=0, thus ensuring we do
           not place a value slot at offset -1 *)
        max start set.first_slot_after_function_slots
      | Scannable_value_slot _ -> max start set.first_slot_after_unboxed_slots
    in
    (* Adjust a starting position to not point in the middle of a block.
       Additionally, ensure the value slot slots are put after the function
       slots. *)
    let curr =
      match
        Numeric_types.Int.Map.find_last (fun i -> i <= curr) set.allocated_slots
      with
      | exception Not_found -> curr
      | _, Slot s ->
        let _first_used_by_s, first_free_after_s = range_used_by s in
        max curr first_free_after_s
    in
    (* find the first available space for the slot. *)
    let rec loop curr =
      match
        Numeric_types.Int.Map.find_first
          (fun i -> i >= curr)
          set.allocated_slots
      with
      | exception Not_found -> curr
      | _, Slot next_slot ->
        let next_slot_start, first_free_after_next_slot =
          range_used_by next_slot
        in
        let available_space = next_slot_start - curr in
        assert (available_space >= 0);
        if available_space >= needed_space
        then curr
        else loop first_free_after_next_slot
    in
    loop curr

  (* Loop to find the first free offset available for a slot given the set of
     sets in which it appears. *)
  let rec first_available_offset slot ~minimal_offset = function
    | [set] ->
      let offset = first_free_offset slot set minimal_offset in
      Exported_offset.mk slot.desc ~first_offset_used_including_header:offset
    | sets ->
      let offset =
        List.fold_left
          (fun offset set ->
            let new_offset = first_free_offset slot set offset in
            assert (new_offset >= offset);
            new_offset)
          minimal_offset sets
      in
      if minimal_offset = offset
      then
        Exported_offset.mk slot.desc ~first_offset_used_including_header:offset
      else first_available_offset slot ~minimal_offset:offset sets

  (* Assign offsets to function slots *)

  let assign_slot_offset state slot =
    match slot.pos with
    | Unassigned ->
      let offset = first_available_offset slot ~minimal_offset:~-1 slot.sets in
      add_slot_offset state slot offset
    | Assigned _pos -> Misc.fatal_error "Slot has already been assigned"
    | Removed ->
      Misc.fatal_error
        "Slot has been explicitly removed, it cannot be assigned anymore"

  let assign_function_slot_offsets ~used_function_slots state =
    let function_slots_to_assign =
      List.sort compare_priority state.function_slots_to_assign
    in
    state.function_slots_to_assign <- [];
    List.iter
      (function
        | { desc = Function_slot f; _ } as slot ->
          if function_slot_is_used ~used_function_slots f
          then assign_slot_offset state slot
          else
            assign_slot_offset state slot
            (* CR chambart/gbury: we currently do not track the used function
               slots precisely enough in simplify/data_flow *)
            (* else mark_slot_as_removed state slot *))
      function_slots_to_assign

  let assign_unboxed_slot_offsets ~used_unboxed_slots state =
    let unboxed_slots_to_assign =
      List.sort compare_priority state.unboxed_slots_to_assign
    in
    state.unboxed_slots_to_assign <- [];
    List.iter
      (function
        | { desc = Unboxed_slot v; _ } as slot ->
          if unboxed_slot_is_used ~used_unboxed_slots v
          then assign_slot_offset state slot
          else mark_slot_as_removed state slot)
      unboxed_slots_to_assign

  let assign_value_slot_offsets ~used_value_slots state =
    let value_slots_to_assign =
      List.sort compare_priority state.value_slots_to_assign
    in
    state.value_slots_to_assign <- [];
    List.iter
      (function
        | { desc = Scannable_value_slot v; _ } as slot ->
          if value_slot_is_used ~used_value_slots v
          then assign_slot_offset state slot
          else mark_slot_as_removed state slot)
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
  let live_slots state
      { value_slots_in_normal_projections;
        function_slots_in_normal_projections;
        _
      } =
    let live_function_slots =
      Function_slot.Set.filter
        (fun function_slot ->
          if Compilation_unit.is_current
               (Function_slot.get_compilation_unit function_slot)
          then (
            match find_function_slot state function_slot with
            | Some _ -> true
            | None ->
              state.used_offsets
                <- EO.add_function_slot_offset state.used_offsets function_slot
                     Dead_function_slot;
              false)
          else true)
        function_slots_in_normal_projections
    in
    let live_value_slots =
      Value_slot.Set.filter
        (fun value_slot ->
          if Compilation_unit.is_current
               (Value_slot.get_compilation_unit value_slot)
          then
            (* a value slot appears in a set of closures iff it has a slot *)
            match
              ( find_value_slot state value_slot,
                find_unboxed_slot state value_slot )
            with
            | None, None ->
              state.used_offsets
                <- EO.add_value_slot_offset state.used_offsets value_slot
                     Dead_value_slot;
              false
            | _ -> true
          else true)
        value_slots_in_normal_projections
    in
    let live_value_slots, live_unboxed_slots =
      Value_slot.Set.partition
        (fun value_slot -> Option.is_some (find_value_slot state value_slot))
        live_value_slots
    in
    live_function_slots, live_unboxed_slots, live_value_slots

  (* Transform an internal accumulator state for slots into an actual mapping
     that assigns offsets. *)
  let finalize ~used_slots state =
    add_used_imported_offsets ~used_slots state;
    let used_function_slots, used_unboxed_slots, used_value_slots =
      live_slots state used_slots
    in
    assign_function_slot_offsets ~used_function_slots state;
    assign_unboxed_slot_offsets ~used_unboxed_slots state;
    assign_value_slot_offsets ~used_value_slots state;
    { used_value_slots =
        Value_slot.Set.union used_value_slots used_unboxed_slots;
      exported_offsets = state.used_offsets
    }
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
  let state = Greedy.create_initial_state () in
  Misc.try_finally
    (fun () ->
      List.iter (Greedy.create_slots_for_set state ~get_code_metadata) l;
      Greedy.finalize ~used_slots state)
    ~always:(fun () ->
      if Flambda_features.dump_slot_offsets ()
      then Format.eprintf "%a@." Greedy.print state)
