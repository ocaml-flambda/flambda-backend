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

open! Flambda.Import
module EO = Exported_offsets

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

let keep_value_slot ~used_value_slots v =
  match (used_value_slots : _ Or_unknown.t) with
  | Unknown -> true
  | Known used_value_slots -> value_slot_is_used ~used_value_slots v

(* CR gbury: considering that the goal is to have `offsets` significantly
   smaller than the `imported_offsets`, it might be better for performance to
   check whether the function slot is already in the offsets before looking it
   up in the imported offsets ? *)
let reexport_function_slots function_slot_set offsets =
  let imported_offsets = EO.imported_offsets () in
  Function_slot.Set.fold
    (fun function_slot offsets ->
      if Compilation_unit.is_current
           (Function_slot.get_compilation_unit function_slot)
      then offsets
      else
        match EO.function_slot_offset imported_offsets function_slot with
        | None ->
          Misc.fatal_errorf
            "Function slot %a is used in the current compilation unit, but not \
             present in the imported offsets."
            Function_slot.print function_slot
        | Some info -> EO.add_function_slot_offset offsets function_slot info)
    function_slot_set offsets

let reexport_value_slots value_slot_set offsets =
  let imported_offsets = EO.imported_offsets () in
  Value_slot.Set.fold
    (fun value_slot offsets ->
      if Compilation_unit.is_current
           (Value_slot.get_compilation_unit value_slot)
      then offsets
      else
        match EO.value_slot_offset imported_offsets value_slot with
        | None ->
          Misc.fatal_errorf
            "value slot %a is used in the current compilation unit, but not \
             present in the imported offsets."
            Value_slot.print value_slot
        | Some info -> EO.add_value_slot_offset offsets value_slot info)
    value_slot_set offsets

(* Compute offsets ("indexes") for slots within a block having tag Closure_tag.

   A particular function slot or value slot might appear in more than one such
   block. However at present we insist that slots must have the same offset in
   all blocks where they appear.

   We assume an ideal (i.e. compact) layout for a block describing a set of
   closures is the following: *)
(*
 * |----------------------|
 * | Closure_tag header   |
 * |----------------------| offset 0
 * | function slot 0      | <- pointer to the block
 * | (pos 0, size 2 or 3) |
 * |----------------------|
 * | Infix_tag header     |
 * |----------------------|
 * | function slot 1      |
 * | (pos x, size 2 or 3) |  (x=3 if function slot 0 is of size 2 for instance)
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
 * | value slot 0         | <- start of the environment part of the block
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

   Starting from OCaml 4.12, all function slots must occur before all value
   slots, since the offset to the start of the environment is recorded in the
   arity field of each function slot.

   This additional requirement makes more constraints impossible to satisfy (to
   be clear, there are situations impossible to satisfy regardless of this
   requirement; it's just that this requirement makes some situations that were
   previously possible to satisfy be now unsatisfiable).

   For instance, it is perfectly possible to have a situation where a value slot
   has been fixed at offset 3 (because it is in a simple set of closures with
   one function of arity > 1 in another cmx), however it is in a set of closures
   with more than one function slot in the current compilation unit. In this
   case, it is impossible to make all the function slots fit before the value
   slot in the block. *)

type layout_slot =
  | Value_slot of Value_slot.t
  | Infix_header
  | Function_slot of Function_slot.t

type layout =
  { startenv : int;
    empty_env : bool;
    slots : (int * layout_slot) list
  }

let order_function_slots env l acc =
  List.fold_left
    (fun acc function_slot ->
      match EO.function_slot_offset env function_slot with
      | Some Dead_function_slot -> acc
      | Some (Live_function_slot { size = _; offset }) ->
        Numeric_types.Int.Map.add offset (Function_slot function_slot) acc
      | None ->
        Misc.fatal_errorf "No function_slot offset for %a" Function_slot.print
          function_slot)
    acc l

let order_value_slots env l acc =
  List.fold_left
    (fun acc value_slot ->
      match EO.value_slot_offset env value_slot with
      | Some Dead_value_slot -> acc
      | Some (Live_value_slot { offset }) ->
        Numeric_types.Int.Map.add offset (Value_slot value_slot) acc
      | None ->
        Misc.fatal_errorf "No value slot offset for %a" Value_slot.print
          value_slot)
    acc l

let layout_aux j slot (startenv, acc_slots) =
  match slot with
  (* Starting from OCaml 4.12, all function slots *must* precede all value
     slots. The algorithms in this file should thus only generate slot
     assignments that respect this invariant. If that is not the case, this is a
     fatal error given that the start of the environment (i.e. the offset of the
     first value slot, with the added property that all slots after that are
     value slots (or at least scannable by the GC)), is needed by the GC when
     scanning the block. Thus, if we see a function slot, we check that then the
     environment has not started yet (i.e. we have not seen any value slots). *)
  | Function_slot _ when j = 0 ->
    assert (acc_slots = []);
    assert (startenv = None);
    (* see comment above *)
    let acc_slots = [0, slot] in
    startenv, acc_slots
  | Function_slot _ ->
    assert (startenv = None);
    (* see comment above *)
    let acc_slots = (j, slot) :: (j - 1, Infix_header) :: acc_slots in
    startenv, acc_slots
  | Value_slot _ ->
    let startenv =
      match startenv with
      | Some i ->
        assert (i < j);
        startenv
      | None -> Some j
    in
    let acc_slots = (j, slot) :: acc_slots in
    startenv, acc_slots
  | Infix_header ->
    (* Internal invariant: such layout slots are not generated by the {order}
       function, so they should not appear. *)
    assert false

let layout env function_slots value_slots =
  let map =
    Numeric_types.Int.Map.empty
    |> order_value_slots env value_slots
    |> order_function_slots env function_slots
  in
  let startenv_opt, acc_slots =
    Numeric_types.Int.Map.fold layout_aux map (None, [])
  in
  let startenv, empty_env =
    (* If there are no value slots, the start of env is considered to be the
       (non-existing) slot after the last slot used, and if the set is empty,
       the value does not matter. *)
    match startenv_opt, acc_slots with
    | Some i, _ -> i, false
    | None, [] -> 0, true
    | None, (j, Function_slot function_slot) :: _ -> begin
      match EO.function_slot_offset env function_slot with
      | Some (Live_function_slot { size; _ }) -> j + size, true
      | Some Dead_function_slot | None ->
        (* the function slot was found earlier during the call to
           order_function_slots *)
        assert false
    end
    | None, (_, Infix_header) :: _ ->
      (* Cannot happen because a infix header is *always* preceded by a function
         slot (because the slot list is reversed) *)
      assert false
    | None, (_, Value_slot _) :: _ ->
      (* Cannot happen because if there is a value slot in the acc, then
         startenv_opt should be Some _ *)
      assert false
  in
  let slots = List.rev acc_slots in
  (* The Gc assumes that a Closure_tag block actually starts with a function
     slot at offset 0. Or more precisely, the GC unconditionally reads the
     second field of a Closure_tag block to find out the start of environment.
     Thus we add a check here to ensure that the slots start with a function
     slot at offset 0. *)
  match slots with
  | (0, Function_slot _) :: _ -> { startenv; slots; empty_env }
  | _ ->
    Misc.fatal_error
      "Sets of function slots must start with a function slot at offset 0"

let print_layout_slot fmt = function
  | Value_slot v -> Format.fprintf fmt "value_slot %a" Value_slot.print v
  | Infix_header -> Format.fprintf fmt "infix_header"
  | Function_slot cid ->
    Format.fprintf fmt "function_slot %a" Function_slot.print cid

let print_layout fmt l =
  Format.fprintf fmt "@[<v>startenv: %d;@ " l.startenv;
  List.iter
    (fun (i, slot) ->
      Format.fprintf fmt "@[<h>%d %a@]@," i print_layout_slot slot)
    l.slots;
  Format.fprintf fmt "@]"

(* Greedy algorithm *)

module Greedy = struct
  (** Greedy algorithm for assigning offsets ("indexes") to slots.

      Slots are assigned using a "first comes, first served" basis, filling
      upwards from 0.

      As much as is possible, the algorithm tries to put all the function slots
      first, and then all the value slots; however, that may be impossible
      because of constraints read from a .cmx file.

      This strategy should be able to correctly compute offsets for all
      legitimate situations, with no expected blowup of computation time.
      However the generated offsets can be far from optimal (i.e. leave more
      holes than necessary). *)

  (* Internal types *)

  type slot_desc =
    | Function_slot of Function_slot.t
    | Value_slot of Value_slot.t

  type slot_pos =
    | Assigned of int
    | Unassigned
    | Removed

  type set_of_closures =
    { id : int;
      (* Info about start of environment *)
      mutable first_slot_used_by_value_slots : int;
      mutable first_slot_after_function_slots : int;
      (* Slots to be allocated *)
      mutable unallocated_function_slots : slot list;
      mutable unallocated_value_slots : slot list;
      mutable allocated_slots : slot Numeric_types.Int.Map.t
    }

  and slot =
    { desc : slot_desc;
      mutable pos : slot_pos;
      mutable size : int;
      mutable sets : set_of_closures list
    }

  (** Intermediate state to store offsets for function and value slots before
      computing the actual offsets of these elements within a block. *)
  type state =
    { used_offsets : EO.t;
      function_slots : slot Function_slot.Map.t;
      value_slots : slot Value_slot.Map.t;
      sets_of_closures : set_of_closures list
    }

  (* Create structures *)

  (* create a fresh slot (with no position allocated yet) *)
  let create_slot size desc = { desc; size; pos = Unassigned; sets = [] }

  let make_set =
    let c = ref 0 in
    fun _ ->
      incr c;
      { id = !c;
        first_slot_after_function_slots = 0;
        first_slot_used_by_value_slots = max_int;
        unallocated_function_slots = [];
        unallocated_value_slots = [];
        allocated_slots = Numeric_types.Int.Map.empty
      }

  let create_initial_state () =
    { used_offsets = EO.empty;
      function_slots = Function_slot.Map.empty;
      value_slots = Value_slot.Map.empty;
      sets_of_closures = []
    }

  (* debug printing *)
  let print_set_id fmt s = Format.fprintf fmt "%d" s.id

  let print_set_ids fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_set_id s) l

  let print_desc fmt = function
    | Function_slot c -> Format.fprintf fmt "%a" Function_slot.print c
    | Value_slot v -> Format.fprintf fmt "%a" Value_slot.print v

  let print_slot_desc fmt s = print_desc fmt s.desc

  let print_slot_descs fmt l =
    List.iter (function s -> Format.fprintf fmt "%a,@ " print_slot_desc s) l

  let print_slot_pos fmt = function
    | Assigned i -> Format.fprintf fmt "%d" i
    | Unassigned -> Format.fprintf fmt "?"
    | Removed -> Format.fprintf fmt "x"

  let print_slot fmt s =
    Format.fprintf fmt "@[<h>[pos: %a;@ size: %d;@ sets: %a;@ desc: %a]@]@,"
      print_slot_pos s.pos s.size print_set_ids s.sets print_desc s.desc

  let print_slots fmt map =
    Numeric_types.Int.Map.iter (fun _ slot -> print_slot fmt slot) map

  let print_set fmt s =
    Format.fprintf fmt
      "@[<v 2>%d:@ @[<v>first_slot_after_function_slots: %d;@ \
       first_slot_used_by_value_slots: %d;@ unallocated_function_slots: \
       @[<v>%a@];@ unallocated_value_slots: @[<v>%a@];@ allocated: \
       @[<v>%a@]@]@]"
      s.id s.first_slot_after_function_slots s.first_slot_used_by_value_slots
      print_slot_descs s.unallocated_function_slots print_slot_descs
      s.unallocated_value_slots print_slots s.allocated_slots

  let print_sets fmt l =
    List.iter (function s -> Format.fprintf fmt "%a@ " print_set s) l

  let [@ocamlformat "disable"] print fmt state =
    Format.fprintf fmt
      "@[<v 2>Sets of closures:@ %a@]"
      print_sets state.sets_of_closures
  [@@warning "-32"]

  (* Keep the value slots offsets in sets up-to-date *)

  let update_set_for_slot slot set =
    begin
      match slot.pos with
      | Unassigned | Removed -> ()
      | Assigned i -> begin
        match slot.desc with
        | Value_slot _ ->
          set.first_slot_used_by_value_slots
            <- min set.first_slot_used_by_value_slots i
        | Function_slot _ ->
          set.first_slot_after_function_slots
            <- max set.first_slot_after_function_slots (i + slot.size)
      end
    end;
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

  let add_slot_offset env slot offset =
    assert (slot.pos = Unassigned);
    slot.pos <- Assigned offset;
    List.iter (add_slot_offset_to_set offset slot) slot.sets;
    match slot.desc with
    | Function_slot c ->
      let (info : EO.function_slot_info) =
        EO.Live_function_slot { offset; size = slot.size }
      in
      EO.add_function_slot_offset env c info
    | Value_slot v ->
      let (info : EO.value_slot_info) = EO.Live_value_slot { offset } in
      EO.add_value_slot_offset env v info

  let mark_slot_as_removed env slot =
    match slot.pos with
    | Removed -> env
    | Unassigned -> (
      slot.pos <- Removed;
      match slot.desc with
      | Function_slot _ ->
        Misc.fatal_error "Function_slot cannot be removed currently"
      | Value_slot v ->
        let (info : EO.value_slot_info) = EO.Dead_value_slot in
        EO.add_value_slot_offset env v info)
    | Assigned _ ->
      Misc.fatal_error "Cannot remove value slot which is already assigned"

  (* Sets of Function_slots *)

  let add_set_to_state state set =
    { state with sets_of_closures = set :: state.sets_of_closures }

  let add_unallocated_slot_to_set slot set =
    slot.sets <- set :: slot.sets;
    update_set_for_slot slot set;
    match slot.desc with
    | Function_slot _ ->
      set.unallocated_function_slots <- slot :: set.unallocated_function_slots
    | Value_slot _ ->
      set.unallocated_value_slots <- slot :: set.unallocated_value_slots

  (* Accumulator state *)

  let use_function_slot_info state c info =
    let used_offsets = EO.add_function_slot_offset state.used_offsets c info in
    { state with used_offsets }

  let add_function_slot state function_slot slot =
    let function_slots =
      Function_slot.Map.add function_slot slot state.function_slots
    in
    { state with function_slots }

  let use_value_slot_info state var info =
    let used_offsets = EO.add_value_slot_offset state.used_offsets var info in
    { state with used_offsets }

  let add_value_slot state var slot =
    { state with value_slots = Value_slot.Map.add var slot state.value_slots }

  let find_function_slot state closure =
    Function_slot.Map.find_opt closure state.function_slots

  let find_value_slot state var = Value_slot.Map.find_opt var state.value_slots

  (* Create slots (and create the cross-referencing). *)

  let rec create_function_slots set state get_code_metadata = function
    | [] -> state
    | (c, code_id) :: r ->
      let s, state =
        match find_function_slot state c with
        | Some s -> s, state
        | None -> (
          if Compilation_unit.is_current (Function_slot.get_compilation_unit c)
          then
            let code_metadata = get_code_metadata code_id in
            let module CM = Code_metadata in
            let is_tupled = CM.is_tupled code_metadata in
            let params_arity = CM.params_arity code_metadata in
            let arity = List.length params_arity in
            let size = if arity = 1 && not is_tupled then 2 else 3 in
            let s = create_slot size (Function_slot c) in
            s, add_function_slot state c s
          else
            (* We should be guaranteed that the corresponding compilation unit's
               cmx file has been read during the downward traversal. *)
            let imported_offsets = EO.imported_offsets () in
            match EO.function_slot_offset imported_offsets c with
            | None ->
              (* This means that there is no cmx for the given function slot
                 (either because of opaque, (or missing cmx ?), or that the
                 offset is missing from the cmx. In any case, this is a hard
                 error: the function slot must have been given an offset by its
                 own compilation unit, and we must know it to avoid choosing a
                 different one. *)
              Misc.fatal_errorf
                "Could not find the offset for function slot %a from another \
                 compilation unit (because of -opaque, or missing cmx)."
                Function_slot.print c
            | Some Dead_function_slot ->
              Misc.fatal_errorf
                "The function slot %a is dead in its original compilation \
                 unit, it should not occur in a set of closures in this \
                 compilation unit."
                Function_slot.print c
            | Some (Live_function_slot { offset; size } as info) ->
              let s =
                { desc = Function_slot c;
                  size;
                  pos = Assigned offset;
                  sets = []
                }
              in
              s, add_function_slot (use_function_slot_info state c info) c s)
      in
      let () = add_unallocated_slot_to_set s set in
      create_function_slots set state get_code_metadata r

  let rec create_value_slots set state = function
    | [] -> state
    | v :: r ->
      let s, state =
        match find_value_slot state v with
        | Some s -> s, state
        | None -> (
          if Compilation_unit.is_current (Value_slot.get_compilation_unit v)
          then
            let s = create_slot 1 (Value_slot v) in
            s, add_value_slot state v s
          else
            (* Same as the comments for the function_slots *)
            let imported_offsets = EO.imported_offsets () in
            match EO.value_slot_offset imported_offsets v with
            | None ->
              (* See comment for the function_slot *)
              Misc.fatal_errorf
                "Could not find the offset for value slot %a from another \
                 compilation unit (because of -opaque, or missing cmx)."
                Value_slot.print v
            | Some Dead_value_slot ->
              Misc.fatal_errorf
                "The value slot %a has been removed by its original \
                 compilation unit, it should not occur in a set of closures in \
                 this compilation unit."
                Value_slot.print v
            | Some (Live_value_slot { offset } as info) ->
              let s =
                { desc = Value_slot v;
                  size = 1;
                  pos = Assigned offset;
                  sets = []
                }
              in
              s, add_value_slot (use_value_slot_info state v info) v s)
      in
      let () = add_unallocated_slot_to_set s set in
      create_value_slots set state r

  let create_slots_for_set state get_code_metadata set_id =
    let set = make_set set_id in
    let state = add_set_to_state state set in
    (* Fill closure slots *)
    let function_decls = Set_of_closures.function_decls set_id in
    let closure_map = Function_declarations.funs function_decls in
    let function_slots = Function_slot.Map.bindings closure_map in
    let state =
      create_function_slots set state get_code_metadata function_slots
    in
    (* Fill value slot slots *)
    let env_map = Set_of_closures.value_slots set_id in
    let value_slots = List.map fst (Value_slot.Map.bindings env_map) in
    let state = create_value_slots set state value_slots in
    state

  (* Folding functions. To avoid pathological cases in allocating slots to
     offsets, folding on slots is done by consuming the first unallocated slot
     of each set of closures, and then repeating until all slots have been
     consumed. *)

  let rec fold_on_unallocated_function_slots f acc state =
    let has_work_been_done = ref false in
    let aux acc set =
      match set.unallocated_function_slots with
      | [] -> acc
      | slot :: r ->
        has_work_been_done := true;
        set.unallocated_function_slots <- r;
        f acc slot
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done
    then res
    else fold_on_unallocated_function_slots f res state

  let rec fold_on_unallocated_value_slots ~used_value_slots f_kept f_removed acc
      state =
    let has_work_been_done = ref false in
    let rec aux acc set =
      match set.unallocated_value_slots with
      | [] -> acc
      | ({ desc = Value_slot v; _ } as slot) :: r ->
        set.unallocated_value_slots <- r;
        if keep_value_slot ~used_value_slots v
        then begin
          has_work_been_done := true;
          f_kept acc slot
        end
        else aux (f_removed acc slot) set
      | { desc = Function_slot _; _ } :: _ -> assert false
      (* invariant *)
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done
    then res
    else
      fold_on_unallocated_value_slots ~used_value_slots f_kept f_removed res
        state

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

  let assign_slot_offset env slot =
    match slot.pos with
    | Unassigned ->
      let offset = first_available_offset slot 0 slot.sets in
      add_slot_offset env slot offset
    | Assigned _pos -> env
    | Removed ->
      Misc.fatal_error
        "Slot has been explicitly removed, it cannot be assigned anymore"

  let assign_function_slot_offsets state env =
    fold_on_unallocated_function_slots assign_slot_offset env state

  let assign_value_slot_offsets ~used_value_slots state env =
    fold_on_unallocated_value_slots ~used_value_slots assign_slot_offset
      mark_slot_as_removed env state

  (* Ensure function slots/value slots that are used in projections in the
     current compilation unit are present in the offsets returned by finalize *)
  let imported_and_used_offsets ~used_slots state =
    match (used_slots : _ Or_unknown.t) with
    | Known
        { function_slots_in_normal_projections;
          all_function_slots;
          value_slots_in_normal_projections;
          all_value_slots
        } ->
      state.used_offsets
      |> reexport_function_slots function_slots_in_normal_projections
      |> reexport_function_slots all_function_slots
      |> reexport_value_slots value_slots_in_normal_projections
      |> reexport_value_slots all_value_slots
    | Unknown -> EO.imported_offsets ()

  (* We only want to keep value slots that appear in the creation of a set of
     closures, *and* appear as projection (at normal name mode). And we need to
     mark value_slots/ids that are not live, as dead in the exported_offsets, so
     that later compilation unit do not mistake that for a missing offset info
     on a value_slot/id. *)
  let live_value_slots state offsets used_slots =
    match (used_slots : used_slots Or_unknown.t) with
    | Unknown -> Or_unknown.Unknown, offsets
    | Known
        { value_slots_in_normal_projections;
          function_slots_in_normal_projections;
          _
        } ->
      let offsets =
        Function_slot.Set.fold
          (fun function_slot acc ->
            if Compilation_unit.is_current
                 (Function_slot.get_compilation_unit function_slot)
            then
              match find_function_slot state function_slot with
              | Some _ -> acc
              | None ->
                EO.add_function_slot_offset acc function_slot Dead_function_slot
            else acc)
          function_slots_in_normal_projections offsets
      in
      let offsets = ref offsets in
      let used_value_slots =
        Value_slot.Set.filter
          (fun value_slot ->
            if Compilation_unit.is_current
                 (Value_slot.get_compilation_unit value_slot)
            then (
              (* a value slot appears in a set of closures iff it has a slot *)
              match find_value_slot state value_slot with
              | Some _ -> true
              | None ->
                offsets
                  := EO.add_value_slot_offset !offsets value_slot
                       Dead_value_slot;
                false)
            else true)
          value_slots_in_normal_projections
      in
      Or_unknown.Known used_value_slots, !offsets

  (* Transform an internal accumulator state for slots into an actual mapping
     that assigns offsets. *)
  let finalize ~used_slots state =
    let offsets = imported_and_used_offsets ~used_slots state in
    let used_value_slots, offsets = live_value_slots state offsets used_slots in
    let offsets = assign_function_slot_offsets state offsets in
    let offsets = assign_value_slot_offsets ~used_value_slots state offsets in
    used_value_slots, offsets
end

type t = Set_of_closures.t list

let print fmt l =
  Format.fprintf fmt "@[<hv>%a@]" (Format.pp_print_list Set_of_closures.print) l

let create () = []

let add_set_of_closures l ~is_phantom set_of_closures =
  if is_phantom then l else set_of_closures :: l

let add_offsets_from_function l1 ~from_function:l2 =
  (* Order is irrelevant *)
  List.rev_append l2 l1

let finalize_offsets ~get_code_metadata ~used_slots l =
  let state = ref (Greedy.create_initial_state ()) in
  Misc.try_finally
    (fun () ->
      List.iter
        (fun set_of_closures ->
          state
            := Greedy.create_slots_for_set !state get_code_metadata
                 set_of_closures)
        l;
      Greedy.finalize ~used_slots !state)
    ~always:(fun () ->
      if Flambda_features.dump_slot_offsets ()
      then Format.eprintf "%a@." Greedy.print !state)

let function_slot_symbol function_slot =
  let compunit = Function_slot.get_compilation_unit function_slot in
  let name = Compilation_unit.get_linkage_name compunit in
  Format.asprintf "%a__%s" Linkage_name.print name
    (Function_slot.to_string function_slot)

let code_symbol ~function_slot_symbol =
  Format.asprintf "%s_code" function_slot_symbol
