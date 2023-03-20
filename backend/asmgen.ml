(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open Config
open Clflags
open Misc
open Cmm

open Dwarf_ocaml

module String = Misc.Stdlib.String

type error =
  | Assembler_error of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error

let cmm_invariants ppf fd_cmm =
  let print_fundecl =
    if !Clflags.dump_cmm then Printcmm.fundecl
    else fun ppf fdecl -> Format.fprintf ppf "%s" fdecl.fun_name
  in
  if !Clflags.cmm_invariants && Cmm_invariants.run ppf fd_cmm then
    Misc.fatal_errorf "Cmm invariants failed on following fundecl:@.%a@."
      print_fundecl fd_cmm;
  fd_cmm

let liveness phrase = Liveness.fundecl phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let pass_dump_cfg_if ppf flag message c =
  if !flag then
    fprintf ppf "*** %s@.%a@." message (Cfg_with_layout.dump ~msg:"") c;
  c

let start_from_emit = ref true

let should_save_before_emit () =
  should_save_ir_after Compiler_pass.Scheduling && (not !start_from_emit)

let should_save_cfg_before_emit () =
  should_save_ir_after Compiler_pass.Simplify_cfg && (not !start_from_emit)

let linear_unit_info =
  { Linear_format.unit = Compilation_unit.dummy;
    items = [];
  }

let new_cfg_unit_info () =
  { Cfg_format.unit = Compilation_unit.dummy;
    items = [];
  }

let cfg_unit_info = new_cfg_unit_info ()

module Compiler_pass_map = Map.Make(Compiler_pass)

let (pass_to_cfg : Cfg_format.cfg_unit_info Compiler_pass_map.t) =
  Compiler_pass_map.empty
  |> Compiler_pass_map.add Compiler_pass.Selection (new_cfg_unit_info ())

let reset () =
  Checkmach.reset_unit_info ();
  start_from_emit := false;
  Compiler_pass_map.iter (fun pass (cfg_unit_info : Cfg_format.cfg_unit_info) ->
    if should_save_ir_after pass then begin
      cfg_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
      cfg_unit_info.items <- [];
    end)
    pass_to_cfg;
  if should_save_before_emit () then begin
    linear_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
    linear_unit_info.items <- [];
  end;
  if should_save_cfg_before_emit () then begin
    cfg_unit_info.unit <- Compilation_unit.get_current_or_dummy ();
    cfg_unit_info.items <- [];
  end

let save_data dl =
  Compiler_pass_map.iter (fun pass (cfg_unit_info: Cfg_format.cfg_unit_info) ->
    if should_save_ir_after pass && (not !start_from_emit) then begin
      cfg_unit_info.items <- Cfg_format.(Data dl) :: cfg_unit_info.items
    end)
    pass_to_cfg;
  if should_save_before_emit () then begin
    linear_unit_info.items <- Linear_format.(Data dl) :: linear_unit_info.items
  end;
  if should_save_cfg_before_emit () then begin
    cfg_unit_info.items <- Cfg_format.(Data dl) :: cfg_unit_info.items
  end;
  dl

let save_linear f =
  if should_save_before_emit () then begin
    linear_unit_info.items <- Linear_format.(Func f) :: linear_unit_info.items
  end;
  f

let save_cfg f =
  if should_save_cfg_before_emit () then begin
    cfg_unit_info.items <- Cfg_format.(Cfg f) :: cfg_unit_info.items
  end;
  f

let save_mach_as_cfg pass f =
  if should_save_ir_after pass && (not !start_from_emit) then begin
    let cfg =
      Cfgize.fundecl f ~before_register_allocation:false ~preserve_orig_labels:false ~simplify_terminators:true
    in
    let cfg_unit_info = Compiler_pass_map.find pass pass_to_cfg in
    cfg_unit_info.items <- Cfg_format.(Cfg cfg) :: cfg_unit_info.items
  end;
  f

let write_ir prefix =
  Compiler_pass_map.iter (fun pass (cfg_unit_info : Cfg_format.cfg_unit_info)  ->
    if should_save_ir_after pass && (not !start_from_emit) then begin
      let filename = Compiler_pass.(to_output_filename pass ~prefix) in
      cfg_unit_info.items <- List.rev cfg_unit_info.items;
      Cfg_format.save filename cfg_unit_info end)
    pass_to_cfg;
  if should_save_before_emit () then begin
    let filename = Compiler_pass.(to_output_filename Scheduling ~prefix) in
    linear_unit_info.items <- List.rev linear_unit_info.items;
    Linear_format.save filename linear_unit_info
  end;
  if should_save_cfg_before_emit () then begin
    let filename = Compiler_pass.(to_output_filename Simplify_cfg ~prefix) in
    cfg_unit_info.items <- List.rev cfg_unit_info.items;
    Cfg_format.save filename cfg_unit_info
  end

let should_emit () =
  not (should_stop_after Compiler_pass.Scheduling)

let should_use_linscan fd =
  !use_linscan ||
  List.mem Cmm.Use_linscan_regalloc fd.Mach.fun_codegen_options

let if_emit_do f x = if should_emit () then f x else ()
let emit_begin_assembly ~init_dwarf:init_dwarf =
  if_emit_do (fun init_dwarf -> Emit.begin_assembly ~init_dwarf) init_dwarf
let emit_end_assembly filename =
  if_emit_do
   (fun dwarf ->
     try
       Emit.end_assembly dwarf
     with Emitaux.Error e ->
       raise (Error (Asm_generation(filename, e))))

let emit_data = if_emit_do Emit.data
let emit_fundecl ~dwarf =
  if_emit_do
    (fun (fundecl : Linear.fundecl) ->
      try
        let () = Profile.record ~accumulate:true "emit" Emit.fundecl fundecl in
        match dwarf with
        | None -> ()
        | Some dwarf ->
          let fun_end_label =
            Asm_targets.Asm_label.create_int Text fundecl.fun_end_label
          in
          let fundecl : Dwarf_concrete_instances.fundecl =
            { fun_name = fundecl.fun_name;
              fun_dbg = fundecl.fun_dbg;
              fun_end_label;
            }
          in
          Dwarf.dwarf_for_fundecl dwarf fundecl
    with Emitaux.Error e ->
      raise (Error (Asm_generation(fundecl.Linear.fun_name, e))))

let rec regalloc ~ppf_dump round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf_dump dump_live "Liveness analysis" fd;
  let num_stack_slots =
    if should_use_linscan fd then begin
      (* Linear Scan *)
      Interval.build_intervals fd;
      if !dump_interval then Printmach.intervals ppf_dump ();
      Linscan.allocate_registers()
    end else begin
      (* Graph Coloring *)
      Interf.build_graph fd;
      if !dump_interf then Printmach.interferences ppf_dump ();
      if !dump_prefer then Printmach.preferences ppf_dump ();
      Coloring.allocate_registers()
    end
  in
  dump_if ppf_dump dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd num_stack_slots in
  dump_if ppf_dump dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl newfd; regalloc ~ppf_dump (round + 1) newfd
  end else begin
    (* Ensure the hooks are called only once. *)
    Compiler_hooks.execute Compiler_hooks.Mach_reload newfd;
    newfd
  end

let (++) x f = f x

let ocamlcfg_verbose =
  match Sys.getenv_opt "OCAMLCFG_VERBOSE" with
  | Some "1" -> true
  | Some _ | None -> false

let recompute_liveness_on_cfg (cfg_with_layout : Cfg_with_layout.t) : Cfg_with_layout.t =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = { Cfg_liveness.before = Reg.Set.empty; across = Reg.Set.empty; } in
  begin match Cfg_liveness.Liveness.run cfg ~init ~map:Cfg_liveness.Liveness.Instr () with
    | Ok (liveness : Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t) ->
      let set_liveness (instr : _ Cfg.instruction) =
        match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
        | None ->
          Misc.fatal_errorf "Missing liveness information for instruction %d in function %s@."
            instr.id
            cfg.Cfg.fun_name
        | Some { Cfg_liveness.before = _; across } ->
          instr.live <- across
      in
      Cfg.iter_blocks cfg ~f:(fun _label block ->
          Cfg.BasicInstructionList.iter block.body ~f:set_liveness;
          set_liveness block.terminator;
        );
    | Aborted _ -> .
    | Max_iterations_reached ->
      Misc.fatal_errorf "Unable to compute liveness from CFG for function %s@."
        cfg.Cfg.fun_name;
  end;
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      Cfg.BasicInstructionList.filter_left block.body ~f:(fun instr ->
          not (Cfg.is_noop_move instr)));
  let layout : Label.t list =
    ListLabels.filter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
        Cfg.mem_block (Cfg_with_layout.cfg cfg_with_layout) label)
  in
  let result =
    Cfg_with_layout.create
      cfg
      ~layout
      ~preserve_orig_labels:false
      ~new_labels:Label.Set.empty
  in
  Eliminate_fallthrough_blocks.run result;
  Merge_straightline_blocks.run result;
  Eliminate_dead_code.run_dead_block result;
  Simplify_terminator.run cfg;
  result

let test_cfgize (f : Mach.fundecl) (res : Linear.fundecl) : unit =
  if ocamlcfg_verbose then begin
    Format.eprintf "processing function %s...\n%!" f.Mach.fun_name;
  end;
  (* We do not simplify terminators here because it interferes with liveness
     when we have a terminator with:
     (i) all its edges leading to the same block;
     (ii) a condition making a pseudo-register live.
    In such a case, the terminator would be simplified to a mere jump, the
    condition would disappear, and the pseudo-register would no longer be
    live. Is it fine in itself, but would break the equivalence check. *)
  let result =
    Cfgize.fundecl
      f
      ~before_register_allocation:false
      ~preserve_orig_labels:false
      ~simplify_terminators:false
  in
  let expected = Linear_to_cfg.run res ~preserve_orig_labels:false in
  Eliminate_fallthrough_blocks.run expected;
  Merge_straightline_blocks.run expected;
  Eliminate_dead_code.run_dead_block expected;
  Simplify_terminator.run (Cfg_with_layout.cfg expected);
  let result = recompute_liveness_on_cfg result in
  Cfg_equivalence.check_cfg_with_layout ~mach:f expected result;
  if ocamlcfg_verbose then begin
    Format.eprintf "the CFG on both code paths are equivalent for function %s.\n%!"
      f.Mach.fun_name;
  end

let reorder_blocks_random ppf_dump cl =
  match !Flambda_backend_flags.reorder_blocks_random with
  | None -> cl
  | Some seed ->
     (* Initialize random state based on user-provided seed and function name.
        Per-function random state (instead of per call to ocamlopt)
        is good for debugging: it gives us deterministic builds
        for each user-provided seed, regardless
        of the order of files on the command line. *)
     let fun_name = (Cfg_with_layout.cfg cl).fun_name in
     let random_state = Random.State.make [| seed; Hashtbl.hash fun_name |] in
     Cfg_with_layout.reorder_blocks_random ~random_state cl;
     pass_dump_cfg_if ppf_dump Flambda_backend_flags.dump_cfg
       "After reorder_blocks_random" cl

let cfgize (f : Mach.fundecl) : Cfg_with_layout.t =
  if ocamlcfg_verbose then begin
    Format.eprintf "Asmgen.cfgize on function %s...\n%!" f.Mach.fun_name;
  end;
  Cfgize.fundecl
    f
    ~before_register_allocation:true
    ~preserve_orig_labels:false
    ~simplify_terminators:true

type register_allocator =
  | Upstream
  | IRC
  | LS

let register_allocator () : register_allocator = ignore (Upstream, IRC); LS

let compile_fundecl ?dwarf ~ppf_dump ~funcnames fd_cmm =
  Proc.init ();
  Reg.reset();
  fd_cmm
  ++ Profile.record ~accumulate:true "cmm_invariants" (cmm_invariants ppf_dump)
  ++ Profile.record ~accumulate:true "selection"
       (Selection.fundecl ~future_funcnames:funcnames)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_sel
  ++ pass_dump_if ppf_dump dump_selection "After instruction selection"
  ++ Profile.record ~accumulate:true "save_mach_as_cfg"
       (save_mach_as_cfg Compiler_pass.Selection)
  ++ Profile.record ~accumulate:true "polling"
       (Polling.instrument_fundecl ~future_funcnames:funcnames)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_polling
  ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_combine
  ++ pass_dump_if ppf_dump dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_cse
  ++ pass_dump_if ppf_dump dump_cse "After CSE"
  ++ Profile.record ~accumulate:true "checkmach" (Checkmach.fundecl ppf_dump)
  ++ Profile.record ~accumulate:true "regalloc" (fun (fd : Mach.fundecl) ->
      (* CR-someday xclerc for xclerc: we still interpret `-linscan` (and
         equivalent) as meaning "Upstream's linscan" for the time being. We
         may switch to CFG's linscan when, after large-scale testing, we are
         confident the CFG version is ready for production. In the meantime,
         CFG's linscan is enabled solely by passing `-cfg-regalloc ls` on
         the command line or putting "cfg-regalloc=ls" in the `OCAMLPARAM`
         environment variable. *)
    let force_linscan = should_use_linscan fd in
    match force_linscan, register_allocator () with
    | false, ((IRC | LS) as regalloc) ->
      fd
      ++ Profile.record ~accumulate:true "irc" (fun fd ->
        let cfg =
          fd
          ++ Profile.record ~accumulate:true "cfgize" cfgize
          ++ Cfg_with_liveness.make
          ++ Profile.record ~accumulate:true "cfg_deadcode" Cfg_deadcode.run
        in
        let cfg_description =
          match !Flambda_backend_flags.cfg_regalloc_validate with
          | false -> None
          | true ->
            Some (Profile.record ~accumulate:true "cfg_create_description"
                    Cfg_regalloc_validate.Description.create (Cfg_with_liveness.cfg_with_layout cfg))
        in
        cfg
        ++ begin match regalloc with
          | IRC -> Profile.record ~accumulate:true "cfg_irc" Cfg_irc.run
          | LS -> Profile.record ~accumulate:true "cfg_ls" Cfg_ls.run
          | Upstream -> assert false
        end
        ++ Cfg_with_liveness.cfg_with_layout
        ++ begin match cfg_description with
          | None -> Fun.id
          | Some cfg_description -> Profile.record ~accumulate:true "cfg_validate_description" (Cfg_regalloc_validate.run cfg_description)
        end
        ++ Profile.record ~accumulate:true "cfg_simplify" Cfg_regalloc_utils.simplify_cfg
        ++ Profile.record ~accumulate:true "cfg_to_linear" Cfg_to_linear.run)
    | true, _ | false, Upstream ->
      fd
      ++ Profile.record ~accumulate:true "default" (fun fd ->
        let res =
          fd
          ++ Profile.record ~accumulate:true "liveness" liveness
          ++ Profile.record ~accumulate:true "deadcode" Deadcode.fundecl
          ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_live
          ++ pass_dump_if ppf_dump dump_live "Liveness analysis"
          ++ Profile.record ~accumulate:true "spill" Spill.fundecl
          ++ Profile.record ~accumulate:true "liveness" liveness
          ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_spill
          ++ pass_dump_if ppf_dump dump_spill "After spilling"
          ++ Profile.record ~accumulate:true "split" Split.fundecl
          ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_split
          ++ pass_dump_if ppf_dump dump_split "After live range splitting"
          ++ Profile.record ~accumulate:true "liveness" liveness
          ++ Profile.record ~accumulate:true "regalloc" (regalloc ~ppf_dump 1)
          ++ Profile.record ~accumulate:true "available_regs" Available_regs.fundecl
        in
        res
        ++ Profile.record ~accumulate:true "linearize" (fun (f : Mach.fundecl) ->
            let res = Linearize.fundecl f in
            if !Flambda_backend_flags.cfg_equivalence_check then begin
              test_cfgize f res;
            end;
            res)
        ++ pass_dump_linear_if ppf_dump dump_linear "Linearized code"))
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Linear
  ++ Profile.record ~accumulate:true "reorder_blocks" (fun (fd : Linear.fundecl) ->
    if !Flambda_backend_flags.use_ocamlcfg then begin
      fd
      ++ Profile.record ~accumulate:true "linear_to_cfg"
           (Linear_to_cfg.run ~preserve_orig_labels:false)
      ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg
      ++ pass_dump_cfg_if ppf_dump Flambda_backend_flags.dump_cfg "After linear_to_cfg"
      ++ Profile.record ~accumulate:true "save_cfg" save_cfg
      ++ Profile.record ~accumulate:true "cfg_reorder_blocks" (reorder_blocks_random ppf_dump)
      ++ Profile.record ~accumulate:true "cfg_to_linear" Cfg_to_linear.run
      ++ pass_dump_linear_if ppf_dump dump_linear "After cfg_to_linear"
    end else
      fd)
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf_dump dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "save_linear" save_linear
  ++ Profile.record ~accumulate:true "emit_fundecl" (emit_fundecl ~dwarf)

let compile_data dl =
  dl
  ++ save_data
  ++ emit_data

let compile_phrases ?dwarf ~ppf_dump ps =
    let funcnames =
      List.fold_left (fun s p ->
          match p with
          | Cfunction fd -> String.Set.add fd.fun_name s
          | Cdata _ -> s)
        String.Set.empty ps
    in
    let rec compile ~funcnames ps =
      match ps with
      | [] -> ()
      | p :: ps ->
          if !dump_cmm then fprintf ppf_dump "%a@." Printcmm.phrase p;
          match p with
          | Cfunction fd ->
            compile_fundecl ?dwarf ~ppf_dump ~funcnames fd;
            compile ~funcnames:(String.Set.remove fd.fun_name funcnames) ps
          | Cdata dl ->
            compile_data dl;
            compile ~funcnames ps
    in
    compile ~funcnames ps

let compile_phrase ?dwarf ~ppf_dump p =
  compile_phrases ?dwarf ~ppf_dump [p]

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ?dwarf ~ppf_dump f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ?dwarf ~ppf_dump ph
       | _ -> ())
    (Cmm_helpers.generic_functions true
       (Cmm_helpers.Generic_fns_tbl.of_fns
          (Compilenv.current_unit_infos ()).ui_generic_fns))

let compile_unit ~output_prefix ~asm_filename ~keep_asm ~obj_filename ~may_reduce_heap
        ~ppf_dump gen =
  reset ();
  let create_asm = should_emit () &&
                   (keep_asm || not !Emitaux.binary_backend_available) in
  Emitaux.create_asm_file := create_asm;
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
       if create_asm then Emitaux.output_channel := open_out asm_filename;
       Misc.try_finally
         (fun () ->
            gen ();
            Checkmach.record_unit_info ppf_dump;
            write_ir output_prefix)
         ~always:(fun () ->
             if create_asm then close_out !Emitaux.output_channel)
         ~exceptionally:(fun () ->
             if create_asm && not keep_asm then remove_file asm_filename);
       if should_emit () then begin
         if may_reduce_heap then
           Emitaux.reduce_heap_size ~reset:(fun () ->
            reset ();
            (* note: we need to preserve the persistent env, because it is
               used to populate fields of the record written as the cmx file
               afterwards. *)
            Typemod.reset ~preserve_persistent_env:true;
            Emitaux.reset ();
            Reg.reset ());
         let assemble_result =
           Profile.record "assemble"
             (Proc.assemble_file asm_filename) obj_filename
         in
         if assemble_result <> 0
         then raise(Error(Assembler_error asm_filename));
       end;
       if create_asm && not keep_asm then remove_file asm_filename
    )

let build_dwarf ~asm_directives:(module Asm_directives : Asm_targets.Asm_directives_intf.S) sourcefile =
  let unit_name =
    (* CR lmaurer: This doesn't actually need to be an [Ident.t] *)
    Symbol.for_current_unit ()
    |> Symbol.linkage_name
    |> Linkage_name.to_string
    |> Ident.create_persistent
  in
  let code_begin =
    Cmm_helpers.make_symbol "code_begin" |> Asm_targets.Asm_symbol.create
  in
  let code_end =
    Cmm_helpers.make_symbol "code_end" |> Asm_targets.Asm_symbol.create
  in
  Dwarf.create
    ~sourcefile
    ~unit_name
    ~asm_directives:(module Asm_directives)
    ~get_file_id:(Emitaux.get_file_num ~file_emitter:X86_dsl.D.file)
    ~code_begin ~code_end

let build_asm_directives () : (module Asm_targets.Asm_directives_intf.S) = (
    module Asm_targets.Asm_directives.Make(struct

      let emit_line str = X86_dsl.D.comment str

      let get_file_num file_name =
        Emitaux.get_file_num ~file_emitter:X86_dsl.D.file file_name

      let debugging_comments_in_asm_files =
        !Flambda_backend_flags.dasm_comments

      module D = struct
        open X86_ast

        include X86_dsl.D

        type data_type =
          | NONE | DWORD | QWORD

        type nonrec constant = constant
        let const_int64 num = Const num
        let const_label str = ConstLabel str
        let const_add c1 c2 = ConstAdd (c1, c2)
        let const_sub c1 c2 = ConstSub (c1, c2)

        let label ?data_type str =
          let typ =
            Option.map
              (function
                | NONE -> X86_ast.NONE
                | DWORD -> X86_ast.DWORD
                | QWORD -> X86_ast.QWORD)
              data_type
          in
          label ?typ str
      end
    end)
  )

let emit_begin_assembly_with_dwarf unix ~disable_dwarf ~emit_begin_assembly ~sourcefile () =
  if !Flambda_backend_flags.internal_assembler then
    (X86_proc.register_internal_assembler (Internal_assembler.assemble unix);
    Emitaux.binary_backend_available := true;
    Emitaux.create_asm_file := !Clflags.keep_asm_file)
  else ();
  let no_dwarf () =
    emit_begin_assembly ~init_dwarf:(fun () -> ());
    None
  in
  let can_emit =
    !Clflags.debug
    && not !Dwarf_flags.restrict_to_upstream_dwarf
    && not disable_dwarf
  in
  match can_emit, Target_system.architecture (), Target_system.derived_system () with
  | true, X86_64, _ ->
    let asm_directives = build_asm_directives () in
    let (module Asm_directives : Asm_targets.Asm_directives_intf.S) = asm_directives in
    let dwarf = ref None in
    emit_begin_assembly ~init_dwarf:(fun () ->
        Asm_targets.Asm_label.initialize ~new_label:Cmm.new_label;
        Asm_directives.initialize ();
        dwarf := Some (build_dwarf ~asm_directives sourcefile)
    );
    !dwarf
  | true, _, _ -> no_dwarf ()
  | false, _, _ -> no_dwarf ()

let end_gen_implementation0 unix ?toplevel ~ppf_dump ~sourcefile make_cmm =
  let dwarf =
    emit_begin_assembly_with_dwarf unix ~disable_dwarf:false ~emit_begin_assembly
      ~sourcefile ()
  in
  make_cmm ()
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cmm
  ++ Profile.record "compile_phrases" (compile_phrases ?dwarf ~ppf_dump)
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ~ppf_dump f);
  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)
  compile_phrase ~ppf_dump ?dwarf
    (Cmm_helpers.reference_symbols
       (List.filter_map (fun prim ->
           if not (Primitive.native_name_is_external prim) then None
           else Some (Primitive.native_name prim))
          !Translmod.primitive_declarations));
  emit_end_assembly sourcefile dwarf

let end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile clambda =
  end_gen_implementation0 unix ?toplevel ~ppf_dump ~sourcefile (fun () ->
    Profile.record "cmm" Cmmgen.compunit clambda)

type middle_end =
     backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

let asm_filename output_prefix =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then output_prefix ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm

let compile_implementation unix ?toplevel ~backend ~filename ~prefixname
      ~middle_end ~ppf_dump (program : Lambda.program) =
  compile_unit ~ppf_dump ~output_prefix:prefixname
    ~asm_filename:(asm_filename prefixname) ~keep_asm:!keep_asm_file
    ~obj_filename:(prefixname ^ ext_obj)
    ~may_reduce_heap:(Option.is_none toplevel)
    (fun () ->
      Compilation_unit.Set.iter Compilenv.require_global
        program.required_globals;
      let clambda_with_constants =
        middle_end ~backend ~filename ~prefixname ~ppf_dump program
      in
      end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile:filename
        clambda_with_constants)

let compile_implementation_flambda2 unix ?toplevel ?(keep_symbol_tables=true)
    ~filename ~prefixname ~size:module_block_size_in_words ~compilation_unit
    ~module_initializer ~flambda2 ~ppf_dump ~required_globals () =
  compile_unit ~ppf_dump ~output_prefix:prefixname
    ~asm_filename:(asm_filename prefixname) ~keep_asm:!keep_asm_file
    ~obj_filename:(prefixname ^ ext_obj)
    ~may_reduce_heap:(Option.is_none toplevel)
    (fun () ->
      Compilation_unit.Set.iter Compilenv.require_global
        required_globals;
      let cmm_phrases =
        flambda2 ~ppf_dump ~prefixname ~filename ~compilation_unit
          ~module_block_size_in_words ~module_initializer
          ~keep_symbol_tables
      in
      end_gen_implementation0 unix ?toplevel ~ppf_dump ~sourcefile:filename
        (fun () -> cmm_phrases))

let linear_gen_implementation unix filename =
  let open Linear_format in
  let linear_unit_info, _ = restore filename in
  let current_package = Compilation_unit.Prefix.from_clflags () in
  let saved_package =
    Compilation_unit.for_pack_prefix linear_unit_info.unit
  in
  if not (Compilation_unit.Prefix.equal current_package saved_package)
  then raise(Error(Mismatched_for_pack saved_package));
  let emit_item ~dwarf = function
    | Data dl -> emit_data dl
    | Func f -> emit_fundecl ~dwarf f
  in
  start_from_emit := true;
  let dwarf =
    emit_begin_assembly_with_dwarf unix ~disable_dwarf:false
      ~emit_begin_assembly ~sourcefile:filename ()
  in
  Profile.record "Emit" (List.iter (emit_item ~dwarf)) linear_unit_info.items;
  emit_end_assembly filename dwarf

let compile_implementation_linear unix output_prefix ~progname =
  compile_unit ~may_reduce_heap:true ~output_prefix
    ~asm_filename:(asm_filename output_prefix) ~keep_asm:!keep_asm_file
    ~obj_filename:(output_prefix ^ ext_obj)
    (fun () ->
      linear_gen_implementation unix progname)

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file
  | Mismatched_for_pack saved ->
    let msg prefix =
      if Compilation_unit.Prefix.is_empty prefix
      then "without -for-pack"
      else "with -for-pack " ^ Compilation_unit.Prefix.to_string prefix
     in
     fprintf ppf
       "This input file cannot be compiled %s: it was generated %s."
       (msg (Compilation_unit.Prefix.from_clflags ()))
       (msg saved)
  | Asm_generation(fn, err) ->
     fprintf ppf
       "Error producing assembly code for %s: %a"
       fn Emitaux.report_error err

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
