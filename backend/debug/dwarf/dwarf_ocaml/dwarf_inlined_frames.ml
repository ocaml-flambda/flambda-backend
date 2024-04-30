(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Asm_targets
open! Dwarf_low
open! Dwarf_high
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module IF = Inlined_frame_ranges
module K = IF.Inlined_frames.Key
module L = Linear
module String = Misc.Stdlib.String

type ranges =
  | Contiguous of
      { start_pos : Asm_label.t;
        start_pos_offset : int;
        end_pos : Asm_label.t;
        end_pos_offset : int
      }
  | Discontiguous of
      Dwarf_4_range_list_entry.t list * Range_list.t * Address_index.Pair.Set.t

let create_contiguous_range_list_and_summarise subrange =
  let start_pos = IF.Subrange.start_pos subrange in
  let start_pos_offset = IF.Subrange.start_pos_offset subrange in
  let end_pos = IF.Subrange.end_pos subrange in
  let end_pos_offset = IF.Subrange.end_pos_offset subrange in
  Contiguous
    { start_pos = Asm_label.create_int Text start_pos;
      start_pos_offset;
      end_pos = Asm_label.create_int Text end_pos;
      end_pos_offset
    }

let create_discontiguous_range_list_entry state dwarf_4_range_list_entries
    range_list summary subrange =
  let start_pos = IF.Subrange.start_pos subrange in
  let start_pos_offset = IF.Subrange.start_pos_offset subrange in
  let end_pos = IF.Subrange.end_pos subrange in
  let end_pos_offset = IF.Subrange.end_pos_offset subrange in
  let start_of_code_symbol = DS.start_of_code_symbol state in
  let start_inclusive =
    Address_table.add (DS.address_table state)
      (Asm_label.create_int Text start_pos)
      ~adjustment:start_pos_offset ~start_of_code_symbol
  in
  let end_exclusive =
    Address_table.add (DS.address_table state)
      (Asm_label.create_int Text end_pos)
      ~adjustment:end_pos_offset ~start_of_code_symbol
  in
  let range_list_entry : Range_list_entry.entry =
    (* DWARF-5 spec page 54 line 1. *)
    Startx_endx { start_inclusive; end_exclusive; payload = () }
  in
  let range_list_entry =
    Range_list_entry.create range_list_entry ~start_of_code_symbol
  in
  (* We still use the [Range_list] when emitting DWARF-4 (even though it is a
     DWARF-5 structure) for the purposes of de-duplicating ranges. *)
  let range_list = Range_list.add range_list range_list_entry in
  let summary =
    Address_index.Pair.Set.add (start_inclusive, end_exclusive) summary
  in
  match !Dwarf_flags.gdwarf_version with
  | Four ->
    let range_list_entry =
      Dwarf_4_range_list_entry.create_range_list_entry
        ~start_of_code_symbol:(DS.start_of_code_symbol state)
        ~first_address_when_in_scope:(Asm_label.create_int Text start_pos)
        ~first_address_when_not_in_scope:(Asm_label.create_int Text end_pos)
        ~first_address_when_not_in_scope_offset:(Some end_pos_offset)
    in
    DS.Debug.log "range_list_entry: start=%d end=%d+%d\n%!" start_pos end_pos
      end_pos_offset;
    range_list_entry :: dwarf_4_range_list_entries, range_list, summary
  | Five -> dwarf_4_range_list_entries, range_list, summary

let create_discontiguous_range_list_and_summarise state range =
  let dwarf_4_range_list_entries, range_list, summary =
    IF.Range.fold range
      ~init:([], Range_list.create (), Address_index.Pair.Set.empty)
      ~f:(fun (dwarf_4_range_list_entries, range_list, summary) subrange ->
        create_discontiguous_range_list_entry state dwarf_4_range_list_entries
          range_list summary subrange)
  in
  Discontiguous (dwarf_4_range_list_entries, range_list, summary)

let create_range_list_and_summarise state range =
  match IF.Range.get_singleton range with
  | No_ranges -> None
  | One_subrange subrange ->
    Some (create_contiguous_range_list_and_summarise subrange)
  | More_than_one_subrange ->
    Some (create_discontiguous_range_list_and_summarise state range)

(* "Summaries", sets of pairs of the starting and ending points of ranges, are
   used to dedup entries in the range list table. We do this for range lists but
   not yet for location lists since deduping entries in the latter would involve
   comparing DWARF location descriptions. *)
module All_summaries = Identifiable.Make (struct
  include Address_index.Pair.Set

  let hash t = Hashtbl.hash (elements t)
end)

let die_for_inlined_frame state ~compilation_unit_proto_die ~parent
    range_list_attributes block =
  let abstract_instance_symbol =
    Dwarf_abstract_instances.find state ~compilation_unit_proto_die block
  in
  let abstract_instance =
    match abstract_instance_symbol with
    | Ok abstract_instance_symbol ->
      [DAH.create_abstract_origin ~die_symbol:abstract_instance_symbol]
    | External_unit { demangled_name; fun_symbol } ->
      (* CR mshinwell: fix references to DIEs across object files

         Note from discussion with gbury 2024-04-26: there is going to be a
         problem if a piece of code in a different unit actually got deleted,
         and in the current unit an inlining stack mentions it. *)
      (* For references to DIEs in other units, we reconstitute as many of their
         attributes as we can and put them directly into the DIE for the inlined
         frame, making use of DWARF-5 spec page 85, line 30 onwards. This won't
         provide parameter information for the functions concerned, but will do
         for now, until we sort out how to properly reference DIEs across units
         (in a way which will also work on macOS). In particular it should
         otherwise suffice for backtraces. *)
      [ DAH.create_name (Asm_symbol.encode fun_symbol);
        DAH.create_linkage_name ~linkage_name:demangled_name;
        DAH.create_external ~is_visible_externally:true ]
  in
  let block : Debuginfo.item = List.hd (Debuginfo.to_items block) in
  Proto_die.create ~parent:(Some parent) ~tag:Inlined_subroutine
    ~attribute_values:
      (abstract_instance @ range_list_attributes
      @ [DAH.create_call_file (Dwarf_state.get_file_num state block.dinfo_file)]
      @ (if block.dinfo_line >= 0
        then [DAH.create_call_line block.dinfo_line]
        else [])
      @
      if block.dinfo_char_start >= 0
      then [DAH.create_call_column block.dinfo_char_start]
      else [])
    ()

let create_range_list_attributes_and_summarise state range all_summaries =
  match create_range_list_and_summarise state range with
  | None -> [], all_summaries
  | Some (Contiguous { start_pos; start_pos_offset; end_pos; end_pos_offset })
    ->
    (* Save space by avoiding the emission of a range list. *)
    let start_pos_offset = Targetint.of_int start_pos_offset in
    let end_pos_offset = Targetint.of_int end_pos_offset in
    let low_pc =
      DAH.create_low_pc_with_offset start_pos ~offset_in_bytes:start_pos_offset
    in
    let high_pc =
      DAH.create_high_pc_offset ~low_pc:start_pos
        ~low_pc_offset_in_bytes:start_pos_offset ~high_pc:end_pos
        ~high_pc_offset_in_bytes:end_pos_offset
    in
    [low_pc; high_pc], all_summaries
  | Some (Discontiguous (dwarf_4_range_list_entries, _range_list, summary)) -> (
    match All_summaries.Map.find summary all_summaries with
    | exception Not_found ->
      let range_list_attributes =
        match !Dwarf_flags.gdwarf_version with
        | Four ->
          let range_list =
            Dwarf_4_range_list.create
              ~range_list_entries:dwarf_4_range_list_entries
          in
          let range_list_attribute =
            Debug_ranges_table.insert (DS.debug_ranges_table state) ~range_list
          in
          [range_list_attribute]
        | Five ->
          (* CR mshinwell: implement DWARF-5 support *)
          (* let range_list_index = Range_list_table.add (DS.range_list_table
             state) range_list in DAH.create_ranges range_list_index *)
          Misc.fatal_error "not yet implemented"
      in
      let all_summaries =
        All_summaries.Map.add summary range_list_attributes all_summaries
      in
      range_list_attributes, all_summaries
    | range_list_attributes -> range_list_attributes, all_summaries)

let rec create_down_to_innermost_frame fundecl state ~compilation_unit_proto_die
    ~(prefix : Debuginfo.item list) ~(blocks_outermost_first : Debuginfo.t)
    scope_proto_dies all_summaries ~parent_die range inlined_frame_ranges =
  DS.Debug.log ">> create_down_to_innermost_frame: %a || %a\n%!"
    Debuginfo.print_compact_extended
    (Debuginfo.of_items prefix)
    Debuginfo.print_compact_extended blocks_outermost_first;
  match Debuginfo.to_items blocks_outermost_first with
  | [] ->
    (* Empty inlining stack for some reason, just ignore it. *)
    scope_proto_dies, all_summaries
  | block_item :: deeper_blocks -> (
    let block = Debuginfo.of_items [block_item] in
    DS.Debug.log "...the current block is %a\n%!"
      Debuginfo.print_compact_extended block;
    (* The key of [scope_proto_dies] is the current prefix concatenated to to
       the current block. It seems like maybe just the current block could be
       used, but that would cause incorrect conflation of DIEs when a given
       function symbol occurs more than once in any particular inlining stack.
       (For example if [f] is inlined into itself, then two separate DIEs should
       be produced.) *)
    let scope_key = Debuginfo.of_items (prefix @ [block_item]) in
    match K.Map.find scope_key scope_proto_dies with
    | existing_die ->
      DS.Debug.log "prefix+block already has a proto DIE (ref %a)\n%!"
        Asm_label.print
        (Proto_die.reference existing_die);
      create_down_to_innermost_frame fundecl state ~compilation_unit_proto_die
        ~prefix:(prefix @ [block_item])
        ~blocks_outermost_first:(Debuginfo.of_items deeper_blocks)
        scope_proto_dies all_summaries ~parent_die:existing_die range
        inlined_frame_ranges
    | exception Not_found ->
      (* See comment in the [dwarf] function below. The DIEs for everything
         except the innermost inlined frame should already exist because of the
         order of iteration over ranges. *)
      (match deeper_blocks with
      | [] -> ()
      | _ :: _ ->
        Misc.fatal_errorf
          "Dwarf_inlined_frames.create_down_to_innermost_frame:@ Expected DIE \
           for %a to already have been created.@ Full inlining stack is:@ %a \
           || %a@ All ranges for %s:@ %a%!"
          Debuginfo.print_compact_extended block
          Debuginfo.print_compact_extended
          (Debuginfo.of_items prefix)
          Debuginfo.print_compact_extended blocks_outermost_first
          fundecl.L.fun_name IF.print inlined_frame_ranges);
      DS.Debug.log "New DIE will be needed, parent DIE ref is %a\n%!"
        Asm_label.print
        (Proto_die.reference parent_die);
      let range_list_attributes, all_summaries =
        create_range_list_attributes_and_summarise state range all_summaries
      in
      let inlined_subroutine_die =
        die_for_inlined_frame state ~compilation_unit_proto_die
          ~parent:parent_die range_list_attributes block
      in
      DS.Debug.log "Our DIE ref (DW_TAG_inlined_subroutine) for %a is %a\n%!"
        Debuginfo.print_compact_extended block Asm_label.print
        (Proto_die.reference inlined_subroutine_die);
      let scope_proto_dies =
        K.Map.add scope_key inlined_subroutine_die scope_proto_dies
      in
      scope_proto_dies, all_summaries)

let dwarf state (fundecl : L.fundecl) inlined_frame_ranges ~function_proto_die =
  DS.Debug.log "\n\nDwarf_inlined_frames.dwarf: function proto DIE is %a\n%!"
    Asm_label.print
    (Proto_die.reference function_proto_die);
  let all_blocks = IF.all_indexes inlined_frame_ranges in
  let scope_proto_dies, _all_summaries =
    IF.Inlined_frames.Index.Set.fold
      (fun (block_with_parents : Debuginfo.t) (scope_proto_dies, all_summaries) ->
        DS.Debug.log "--------------------------------------------------\n";
        DS.Debug.log "START: %a\n%!" Debuginfo.print_compact_extended
          block_with_parents;
        (* The head of [block_with_parents] always corresponds to [fundecl] and
           thus will be associated with [function_proto_die]. As such we don't
           need to create any DW_TAG_inlined_subroutine DIEs for it. *)
        let first_item, parents_outermost_first =
          (* "Outermost" = less deep inlining *)
          let block_with_parents = Debuginfo.to_items block_with_parents in
          match block_with_parents with
          | [] ->
            Misc.fatal_errorf "Empty debuginfo in function %s" fundecl.fun_name
          | first_item :: parents -> first_item, Debuginfo.of_items parents
        in
        DS.Debug.log "Having removed fundecl item: %a\n%!"
          Debuginfo.print_compact_extended parents_outermost_first;
        let compilation_unit_proto_die = DS.compilation_unit_proto_die state in
        let scope_proto_dies, all_summaries =
          (* We only need to look the range up once for the current
             [Debuginfo.t] (which contains all blocks in the inlining stack
             under consideration). This might seem wrong in the case where a
             function [f0] is inlined into a function [f] and we are considering
             an inlining stack coming from the instructions whose debuginfo has
             [f; f0]: would we not fail to extend the range of [f] correctly
             across all of its instructions (rather than just the ones coming
             from [f0])? The answer is no, because this loop will also receive a
             range for [f] itself, and that will have been first because
             [Index.Set.fold] operates in ascending order. See assertion
             above. *)
          DS.Debug.log
            "finding ranges for key (current block + all parents): %a\n%!"
            K.print block_with_parents;
          match IF.find inlined_frame_ranges block_with_parents with
          | range ->
            create_down_to_innermost_frame fundecl state
              ~compilation_unit_proto_die ~prefix:[first_item]
              ~blocks_outermost_first:parents_outermost_first scope_proto_dies
              all_summaries ~parent_die:function_proto_die range
              inlined_frame_ranges
          | exception Not_found ->
            Misc.fatal_errorf
              "Function %s:@ couldn't find block_with_parents=%a.@ All ranges:"
              fundecl.L.fun_name Debuginfo.print_compact_extended
              block_with_parents IF.print inlined_frame_ranges
        in
        scope_proto_dies, all_summaries)
      all_blocks
      (K.Map.empty, All_summaries.Map.empty)
  in
  scope_proto_dies
