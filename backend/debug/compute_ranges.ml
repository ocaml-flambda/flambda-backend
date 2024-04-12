(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
module L = Linear

module Make (S : Compute_ranges_intf.S_functor) = struct
  module Subrange_state = S.Subrange_state
  module Subrange_info = S.Subrange_info
  module Range_info = S.Range_info

  let rewrite_label env label =
    match Numbers.Int.Map.find label env with
    | exception Not_found -> label
    | label -> label

  module Subrange = struct
    (* CR-soon mshinwell: Check that function epilogues, including returns in
       the middle of functions, work ok in the debugger. *)
    type t =
      { start_pos : L.label;
        start_pos_offset : int;
        end_pos : L.label;
        end_pos_offset : int;
        subrange_info : Subrange_info.t
      }

    let print ppf
        { start_pos; start_pos_offset; end_pos; end_pos_offset; subrange_info }
        =
      Format.fprintf ppf
        "@[<hov 1>((start_pos@ L%d)@ (start_pos_offset@ %d)@ (end_pos@ L%d)@ \
         (end_pos_offset@ %d)@ (subrange_info@ %a))@]"
        start_pos start_pos_offset end_pos end_pos_offset Subrange_info.print
        subrange_info

    let create ~(start_insn : L.instruction) ~start_pos ~start_pos_offset
        ~end_pos ~end_pos_offset ~subrange_info =
      match start_insn.desc with
      | Llabel _ ->
        { start_pos; start_pos_offset; end_pos; end_pos_offset; subrange_info }
      | _ ->
        Misc.fatal_errorf "Subrange.create: bad [start_insn]: %a"
          Printlinear.instr start_insn

    let start_pos t = t.start_pos

    let start_pos_offset t = t.start_pos_offset

    let end_pos t = t.end_pos

    let end_pos_offset t = t.end_pos_offset

    let info t = t.subrange_info

    let rewrite_labels t ~env =
      let start_pos = rewrite_label env t.start_pos in
      let end_pos = rewrite_label env t.end_pos in
      if start_pos = end_pos && t.start_pos_offset = 0 && t.end_pos_offset = 0
      then None
      else Some { t with start_pos; end_pos }
  end

  module Range = struct
    type t =
      { mutable subranges : Subrange.t list;
        mutable min_pos_and_offset : (L.label * int) option;
        range_info : Range_info.t
      }

    let print ppf { subranges; min_pos_and_offset; range_info } =
      Format.fprintf ppf
        "@[<hov 1>((subranges@ %a)@ (min_pos_and_offset@ %a) (range_info@ \
         %a))@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Subrange.print)
        subranges
        (Misc.Stdlib.Option.print (fun ppf (pos, offset) ->
             Format.fprintf ppf "@[<hov 1>((pos@ L%d)@ (offset@ %d))@]" pos
               offset))
        min_pos_and_offset Range_info.print range_info

    let create range_info =
      { subranges = []; min_pos_and_offset = None; range_info }

    let info t = t.range_info

    let add_subrange t ~subrange =
      let start_pos = Subrange.start_pos subrange in
      let start_pos_offset = Subrange.start_pos_offset subrange in
      (match t.min_pos_and_offset with
      | None -> t.min_pos_and_offset <- Some (start_pos, start_pos_offset)
      | Some (min_pos, min_pos_offset) ->
        (* This may seem dubious, but is correct by virtue of the way label
           counters are allocated sequentially and the fact that, below, we go
           through the code from lowest (code) address to highest. As such the
           label with the highest integer value should be the one with the
           highest address, and vice-versa. (Note that we also exploit the
           ordering when constructing DWARF-4 location lists, to ensure that
           they are sorted in increasing program counter order by start
           address.) *)
        let c = compare start_pos min_pos in
        if c < 0 || (c = 0 && start_pos_offset < min_pos_offset)
        then t.min_pos_and_offset <- Some (start_pos, start_pos_offset));
      t.subranges <- subrange :: t.subranges

    let estimate_lowest_address t =
      (* See assumption described in compute_ranges_intf.ml. *)
      t.min_pos_and_offset

    let fold t ~init ~f = List.fold_left f init t.subranges

    let no_subranges t = match t.subranges with [] -> true | _ -> false

    let rewrite_labels_and_remove_empty_subranges t ~env =
      let subranges =
        List.filter_map
          (fun subrange -> Subrange.rewrite_labels subrange ~env)
          t.subranges
      in
      match subranges with
      | [] -> { t with subranges; min_pos_and_offset = None }
      | subranges ->
        let min_pos_and_offset =
          Option.map
            (fun (label, offset) -> rewrite_label env label, offset)
            t.min_pos_and_offset
        in
        { t with subranges; min_pos_and_offset }
  end

  type t = { ranges : Range.t S.Index.Tbl.t }

  let print ppf { ranges } =
    Format.fprintf ppf "@[<hov 1>(ranges@ %a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         (fun ppf (index, range) ->
           Format.fprintf ppf "@[<hov 1>(%a@ %a)@]" S.Index.print index
             Range.print range))
      (S.Index.Tbl.to_list ranges)

  module KM = S.Key.Map
  module KS = S.Key.Set

  (* Whilst this pass is not DWARF-specific, the output of this pass uses the
     conventions of the DWARF specification (e.g. DWARF-4 spec. section 2.6.2,
     page 30) in the sense that starting addresses of ranges are treated as
     inclusive and ending addresses as exclusive.

     Imagine that, for a given [key], the program counter (PC) is exactly at the
     start of [insn]; that instruction has not yet been executed. Assume a
     immediately-previous instruction exists called [prev_insn]. Intuitively,
     this function calculates which available subranges are to start and stop at
     that point, but these notions are subtle.

     There are eight cases, referenced in the code below.

     1. First four cases: [key] is currently unavailable, i.e. it is not a
     member of [known_available_after_prev_insn].

     (a) [key] is not in [S.available_before insn] and neither is it in
     [S.available_across insn]. There is nothing to do.

     (b) [key] is not in [S.available_before insn] but it is in
     [S.available_across insn]. A new range is created with the starting
     position being one byte after the first machine instruction of [insn] and
     left open.

     It might seem like this case 1 (b) is impossible, likewise for 2 (b) below,
     since "available across" should always be a subset of "available before".
     However this does not hold in general: see the comment in
     available_ranges_vars.ml.

     (c) [key] is in [S.available_before insn] but it is not in
     [S.available_across insn]. A new range is created with the starting
     position being the first machine instruction of [insn] and the ending
     position being the next machine address after that.

     (d) [key] is in [S.available_before insn] and it is also in
     [S.available_across insn]. A new range is created with the starting
     position being the first machine instruction of [insn] and left open.

     2. Second four cases: [key] is already available, i.e. a member of
     [known_available_after_prev_insn].

     (a) [key] is not in [S.available_before insn] and neither is it in
     [S.available_across insn]. The range endpoint is given as the address of
     the first machine instruction of [insn]. Since endpoint bounds are
     exclusive (see above) then [key] will not be shown as available when the
     debugger is standing on [insn].

     (b) [key] is not in [S.available_before insn] but it is in
     [S.available_across insn]. The range endpoint is given as the address of
     the first machine instruction of [insn]; and a new range is opened in the
     same way as for case 1 (b), above.

     (c) [key] is in [S.available_before insn] but it is not in
     [S.available_across insn]. This will only happen when calculating
     variables' available ranges for operation (i.e. [Lop]) instructions (for
     example calls or allocations). To give a good user experience it is
     necessary to show availability when the debugger is standing on the very
     first instruction of the operation but not thereafter. As such we terminate
     the range one byte beyond the first machine instruction of [insn].

     (d) [key] is in [S.available_before insn] and it is also in it is in
     [S.available_across insn]. The existing range remains open. *)

  type action =
    | Open_one_byte_subrange
    | Open_subrange
    | Open_subrange_one_byte_after
    | Close_subrange
    | Close_subrange_one_byte_after

  let print_action ppf action =
    match action with
    | Open_one_byte_subrange -> Format.fprintf ppf "Open_one_byte_subrange"
    | Open_subrange -> Format.fprintf ppf "Open_subrange"
    | Open_subrange_one_byte_after ->
      Format.fprintf ppf "Open_subrange_one_byte_after"
    | Close_subrange -> Format.fprintf ppf "Close_subrange"
    | Close_subrange_one_byte_after ->
      Format.fprintf ppf "Close_subrange_one_byte_after"

  let actions_at_instruction0 ~(insn : L.instruction)
      ~(prev_insn : L.instruction option) ~known_available_after_prev_insn
      ~available_before ~available_across =
    ignore insn;
    ignore prev_insn;
    let case_1b =
      KS.diff available_across
        (KS.union known_available_after_prev_insn available_before)
    in
    let case_1c =
      KS.diff available_before
        (KS.union known_available_after_prev_insn available_across)
    in
    let case_1d =
      KS.diff
        (KS.inter available_before available_across)
        known_available_after_prev_insn
    in
    let case_2a =
      KS.diff known_available_after_prev_insn
        (KS.union available_before available_across)
    in
    let case_2b =
      KS.inter known_available_after_prev_insn
        (KS.diff available_across available_before)
    in
    let case_2c =
      KS.diff
        (KS.inter known_available_after_prev_insn available_before)
        available_across
    in
    let handle case action result =
      (* We use [K.all_parents] here to circumvent a potential performance
         problem. In the case of lexical blocks, there may be long chains of
         blocks and their parents, yet the innermost block determines the rest
         of the chain. As such [S] (which comes from lexical_block_ranges.ml)
         only needs to use the innermost blocks in the "available before" sets,
         keeping things fast---but we still populate ranges for all parent
         blocks, thus avoiding any post-processing, by using [K.all_parents]
         here. *)
      KS.fold
        (fun key result ->
          List.fold_left
            (fun result key -> (key, action) :: result)
            result
            (key :: S.Key.all_parents key))
        case result
    in
    (* Ranges must be closed before they are opened---otherwise, when a variable
       moves between registers at a range boundary, we might end up with no open
       range for that variable. Note that the pipeline below constructs the
       [actions] list in reverse order---later functions in the pipeline produce
       actions nearer the head of the list. *)
    []
    |> handle case_1b Open_subrange_one_byte_after
    |> handle case_1c Open_one_byte_subrange
    |> handle case_1d Open_subrange
    |> handle case_2a Close_subrange
    |> handle case_2b Open_subrange_one_byte_after
    |> handle case_2b Close_subrange
    |> handle case_2c Close_subrange_one_byte_after

  let actions_at_instruction ~(insn : L.instruction)
      ~(prev_insn : L.instruction option) ~known_available_after_prev_insn =
    let available_before = S.available_before insn in
    let available_across = S.available_across insn in
    if !Flambda_backend_flags.dranges
    then
      Format.eprintf "canonicalised available_before:@ %a\n"
        (Misc.Stdlib.Option.print KS.print)
        available_before;
    if !Flambda_backend_flags.dranges
    then
      Format.eprintf "canonicalised available_across:@ %a\n"
        (Misc.Stdlib.Option.print KS.print)
        available_across;
    match available_before, available_across with
    | None, None | None, Some _ | Some _, None ->
      (* If availability information isn't known for the current instruction,
         just skip to the next instruction. *)
      []
    | Some available_before, Some available_across ->
      actions_at_instruction0 ~insn ~prev_insn ~known_available_after_prev_insn
        ~available_before ~available_across

  let rec process_instruction t (fundecl : L.fundecl) ~fun_contains_calls
      ~fun_num_stack_slots ~(first_insn : L.instruction) ~(insn : L.instruction)
      ~(prev_insn : L.instruction option) ~currently_open_subranges
      ~subrange_state =
    if !Flambda_backend_flags.dranges
    then Format.eprintf "process_instruction:@ %a\n" Printlinear.instr insn;
    let used_label = ref None in
    let get_label () =
      match !used_label with
      | Some label_and_insn -> label_and_insn
      | None ->
        (* Note that we can't reuse an existing label in the code since we rely
           on the ordering of range-related labels. *)
        let label = Cmm.new_label () in
        let label_insn : L.instruction =
          { desc = Llabel { label; section_name = None };
            next = insn;
            arg = [||];
            res = [||];
            dbg = insn.dbg;
            fdo = insn.fdo;
            live = insn.live;
            available_before = insn.available_before;
            available_across = insn.available_across
          }
        in
        used_label := Some (label, label_insn);
        label, label_insn
    in
    let open_subrange key ~start_pos_offset ~currently_open_subranges =
      if KM.mem key currently_open_subranges
      then Misc.fatal_errorf "Key %a already has an open range" S.Key.print key;
      (* If the range is later discarded, the inserted label may actually be
         useless, but this doesn't matter. It does not generate any code. *)
      if !Flambda_backend_flags.dranges
      then Format.eprintf "opening subrange for %a\n%!" S.Key.print key;
      let label, label_insn = get_label () in
      KM.add key (label, start_pos_offset, label_insn) currently_open_subranges
    in
    let close_subrange key ~end_pos_offset ~currently_open_subranges =
      if !Flambda_backend_flags.dranges
      then Format.eprintf "closing subrange for key %a\n" S.Key.print key;
      match KM.find key currently_open_subranges with
      | exception Not_found ->
        Misc.fatal_errorf
          "No subrange is open for key %a, currently_open_subranges:@ (%a)"
          S.Key.print key
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun ppf (key, (label, start_pos_offset, label_insn)) ->
               Format.fprintf ppf
                 "@[<hov 1>((key@ %a)@ (label@ L%d)@ (start_pos_offset@ %d)@ \
                  (insn@ %a))@]"
                 S.Key.print key label start_pos_offset Printlinear.instr
                 label_insn))
          (KM.bindings currently_open_subranges)
      | start_pos, start_pos_offset, start_insn -> (
        let currently_open_subranges = KM.remove key currently_open_subranges in
        match Range_info.create fundecl key ~start_insn with
        | None -> currently_open_subranges
        | Some (index, range_info) ->
          let range =
            match S.Index.Tbl.find t.ranges index with
            | range -> range
            | exception Not_found ->
              let range = Range.create range_info in
              S.Index.Tbl.add t.ranges index range;
              range
          in
          let label, _label_insn = get_label () in
          let subrange_info =
            Subrange_info.create key subrange_state ~fun_contains_calls
              ~fun_num_stack_slots
          in
          let subrange =
            Subrange.create ~start_insn ~start_pos ~start_pos_offset
              ~end_pos:label ~end_pos_offset ~subrange_info
          in
          Range.add_subrange range ~subrange;
          currently_open_subranges)
    in
    let actions =
      match[@ocaml.warning "-4"] insn.desc with
      | Lend ->
        (* This has special handling below *)
        []
      | _ ->
        let known_available_after_prev_insn =
          KM.bindings currently_open_subranges |> List.map fst |> KS.of_list
        in
        actions_at_instruction ~insn ~prev_insn ~known_available_after_prev_insn
    in
    (* Apply actions *)
    let no_actions = List.compare_length_with actions 0 = 0 in
    if !Flambda_backend_flags.dranges && no_actions
    then Format.eprintf "no actions to apply\n%!";
    if !Flambda_backend_flags.dranges && not no_actions
    then Format.eprintf "applying actions:\n%!";
    let currently_open_subranges =
      List.fold_left
        (fun currently_open_subranges (key, (action : action)) ->
          if !Flambda_backend_flags.dranges
          then
            Format.eprintf "  --> action for key %a: %a\n" S.Key.print key
              print_action action;
          match action with
          | Open_one_byte_subrange ->
            let currently_open_subranges =
              open_subrange key ~start_pos_offset:0 ~currently_open_subranges
            in
            close_subrange key ~end_pos_offset:1 ~currently_open_subranges
          | Open_subrange ->
            open_subrange key ~start_pos_offset:0 ~currently_open_subranges
          | Open_subrange_one_byte_after ->
            open_subrange key ~start_pos_offset:1 ~currently_open_subranges
          | Close_subrange ->
            close_subrange key ~end_pos_offset:0 ~currently_open_subranges
          | Close_subrange_one_byte_after ->
            close_subrange key ~end_pos_offset:1 ~currently_open_subranges)
        currently_open_subranges actions
    in
    if !Flambda_backend_flags.dranges && not no_actions
    then Format.eprintf "finished applying actions.\n%!";
    (* Close all subranges if at last instruction *)
    let currently_open_subranges =
      match insn.desc with
      | Lend ->
        if !Flambda_backend_flags.dranges
        then Format.eprintf "closing subranges for last insn\n%!";
        let currently_open_subranges =
          KM.fold
            (fun key _ currently_open_subranges ->
              close_subrange key ~end_pos_offset:0 ~currently_open_subranges)
            currently_open_subranges currently_open_subranges
        in
        assert (KM.is_empty currently_open_subranges);
        currently_open_subranges
      | _ -> currently_open_subranges
    in
    let first_insn =
      match !used_label with
      | None -> first_insn
      | Some (_label, label_insn) -> (
        assert (label_insn.L.next == insn);
        (* (Note that by virtue of [Lprologue], we can insert labels prior to
           the first assembly instruction of the function.) *)
        match prev_insn with
        | None ->
          (* The label becomes the new first instruction. *)
          label_insn
        | Some prev_insn ->
          assert (prev_insn.L.next == insn);
          prev_insn.next <- label_insn;
          first_insn)
    in
    (if !Dwarf_flags.ddebug_invariants
    then
      let currently_open_subranges =
        KS.of_list
          (List.map
             (fun (key, _datum) -> key)
             (KM.bindings currently_open_subranges))
      in
      match S.available_across insn with
      | None | Some Unreachable -> ()
      | Some (Ok _ as should_be_open) -> (
        match KS.diff should_be_open currently_open_subranges with
        | Unreachable -> assert false
        | Ok not_open_but_should_be as not_open_but_should_be' ->
          (* Avoid having [KS.is_empty], which seems a bit tricky to think
             about, just for this check. *)
          if not (S.Key.Raw_set.is_empty not_open_but_should_be)
          then
            Misc.fatal_errorf
              "%s: ranges for %a not open across the following instruction:\n\
               %a\n\
               available_across:@ %a\n\
               currently_open_subranges: %a" fundecl.fun_name KS.print
              not_open_but_should_be' Printlinear.instr
              { insn with L.next = L.end_instr }
              KS.print should_be_open KS.print currently_open_subranges));
    match insn.desc with
    | Lend -> first_insn
    | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Llabel _ | Lbranch _
    | Lcondbranch _ | Lcondbranch3 _ | Lswitch _ | Lentertrap | Lpushtrap _
    | Lpoptrap | Ladjust_stack_offset _ | Lraise _ | Lstackcheck _ ->
      let subrange_state =
        Subrange_state.advance_over_instruction subrange_state insn
      in
      process_instruction t fundecl ~fun_contains_calls ~fun_num_stack_slots
        ~first_insn ~insn:insn.next ~prev_insn:(Some insn)
        ~currently_open_subranges ~subrange_state

  let process_instructions t fundecl ~fun_contains_calls ~fun_num_stack_slots
      ~first_insn =
    let subrange_state = Subrange_state.create () in
    process_instruction t fundecl ~fun_contains_calls ~fun_num_stack_slots
      ~first_insn ~insn:first_insn ~prev_insn:None
      ~currently_open_subranges:KM.empty ~subrange_state

  let all_indexes t =
    S.Index.Set.of_list (List.map fst (S.Index.Tbl.to_list t.ranges))

  let empty = { ranges = S.Index.Tbl.create 1 }

  let create (fundecl : L.fundecl) =
    if !Flambda_backend_flags.dranges
    then Format.eprintf "Compute_ranges for %s\n" fundecl.fun_name;
    let t = { ranges = S.Index.Tbl.create 42 } in
    let first_insn =
      process_instructions t fundecl
        ~fun_contains_calls:fundecl.fun_contains_calls
        ~fun_num_stack_slots:fundecl.fun_num_stack_slots
        ~first_insn:fundecl.fun_body
    in
    let fundecl : L.fundecl = { fundecl with fun_body = first_insn } in
    t, fundecl

  let iter t ~f = S.Index.Tbl.iter (fun index range -> f index range) t.ranges

  let fold t ~init ~f =
    S.Index.Tbl.fold (fun index range acc -> f acc index range) t.ranges init

  let find t index = S.Index.Tbl.find t.ranges index

  let rewrite_labels_and_remove_empty_subranges_and_ranges t ~env =
    let ranges = S.Index.Tbl.create 42 in
    S.Index.Tbl.iter
      (fun index range ->
        let range =
          Range.rewrite_labels_and_remove_empty_subranges range ~env
        in
        if not (Range.no_subranges range)
        then S.Index.Tbl.add ranges index range)
      t.ranges;
    { ranges }
end
