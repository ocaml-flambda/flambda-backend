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

module String = Misc.Stdlib.String

type error =
  | Assembler_error of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error

let cmm_invariants ppf fd_cmm =
  let print_fundecl =
    if !Clflags.dump_cmm then Printcmm.fundecl
    else fun ppf fdecl -> Format.fprintf ppf "%s" fdecl.fun_name.sym_name
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
  Zero_alloc_checker.reset_unit_info ();
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
let emit_begin_assembly unix = if_emit_do Emit.begin_assembly unix
let emit_end_assembly filename () =
  if_emit_do (fun () ->
    try Emit.end_assembly ()
     with Emitaux.Error e ->
       raise (Error (Asm_generation(filename, e))))
    ()

let emit_data dl = if_emit_do Emit.data dl
let emit_fundecl f =
  if_emit_do
    (fun (fundecl : Linear.fundecl) ->
      try
        Profile.record ~accumulate:true "emit" Emit.fundecl fundecl
    with Emitaux.Error e ->
      raise (Error (Asm_generation(fundecl.Linear.fun_name, e))))
    f

let rec regalloc ~ppf_dump round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf_dump dump_live "Liveness analysis" fd;
  let num_stack_slots =
    if should_use_linscan fd then begin
      (* Linear Scan *)
      let intervals = Interval.build_intervals fd in
      if !dump_interval then Printmach.intervals ppf_dump intervals;
      Linscan.allocate_registers intervals
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
  | GI
  | IRC
  | LS

let default_allocator = Upstream

let register_allocator fd : register_allocator =
  match String.lowercase_ascii !Flambda_backend_flags.regalloc with
  | "cfg" -> if should_use_linscan fd then LS else IRC
  | "gi" -> GI
  | "irc" -> IRC
  | "ls" -> LS
  | "upstream" -> Upstream
  | "" -> default_allocator
  | other -> Misc.fatal_errorf "unknown register allocator (%S)" other

let compile_fundecl ~ppf_dump ~funcnames fd_cmm =
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
  ++ Profile.record ~accumulate:true "checkmach"
       (Zero_alloc_checker.fundecl ~future_funcnames:funcnames ppf_dump)
  ++ (fun fd ->
      match !Flambda_backend_flags.cfg_cse_optimize with
      | false ->
        fd
        ++ pass_dump_if ppf_dump dump_combine "Before allocation combining"
        ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
        ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_combine
        ++ pass_dump_if ppf_dump dump_combine "After allocation combining"
        ++ Profile.record ~accumulate:true "cse" CSE.fundecl
        ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Mach_cse
        ++ pass_dump_if ppf_dump dump_cse "After CSE"
      | true ->
        (* Will happen after `Cfgize`. *)
        (match register_allocator fd with
         | Upstream ->
           fatal_error "-cfg-cse-optimize should only be used with a CFG register allocator"
         | GI | IRC | LS ->
           ());
        fd)
  ++ Profile.record ~accumulate:true "regalloc" (fun (fd : Mach.fundecl) ->
    match register_allocator fd with
      | ((GI | IRC | LS) as regalloc) ->
      fd
      ++ Profile.record ~accumulate:true "cfg" (fun fd ->
        let cfg =
          fd
          ++ Profile.record ~accumulate:true "cfgize" cfgize
          ++ (fun cfg_with_layout ->
              match !Flambda_backend_flags.cfg_cse_optimize with
              | false -> cfg_with_layout
              | true ->
                cfg_with_layout
                ++ Profile.record ~accumulate:true "cfg_comballoc" Cfg_comballoc.run
                ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg_combine
                ++ Profile.record ~accumulate:true "cfg_cse" CSE.cfg_with_layout
                ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg_cse)
          ++ Cfg_with_infos.make
          ++ Profile.record ~accumulate:true "cfg_deadcode" Cfg_deadcode.run
        in
        let cfg_description =
            Regalloc_validate.Description.create (Cfg_with_infos.cfg_with_layout cfg)
        in
        cfg
        ++ begin match regalloc with
          | GI -> Profile.record ~accumulate:true "cfg_gi" Regalloc_gi.run
          | IRC -> Profile.record ~accumulate:true "cfg_irc" Regalloc_irc.run
          | LS -> Profile.record ~accumulate:true "cfg_ls" Regalloc_ls.run
          | Upstream -> assert false
        end
        ++ Cfg_with_infos.cfg_with_layout
        ++ Profile.record ~accumulate:true "cfg_validate_description" (Regalloc_validate.run cfg_description)
        ++ Profile.record ~accumulate:true "cfg_simplify" Regalloc_utils.simplify_cfg
          (* CR-someday gtulbalecu: The peephole optimizations must not affect liveness, otherwise
             we would have to recompute it here. Recomputing it here breaks the CI because
             the liveness_analysis algorithm does not work properly after register allocation. *)
        ++ Profile.record ~accumulate:true "peephole_optimize_cfg" Peephole_optimize.peephole_optimize_cfg
        ++ (fun (cfg_with_layout : Cfg_with_layout.t) ->
          match !Flambda_backend_flags.cfg_stack_checks with
          | false -> cfg_with_layout
          | true -> Cfg_stack_checks.cfg cfg_with_layout)
        ++ Profile.record ~accumulate:true "save_cfg" save_cfg
        ++ Profile.record ~accumulate:true "cfg_reorder_blocks"
             (reorder_blocks_random ppf_dump)
        ++ Profile.record ~accumulate:true "cfg_to_linear" Cfg_to_linear.run)
    | Upstream ->
      fd
      ++ Profile.record ~accumulate:true "default" (fun fd ->
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
        ++ Profile.record ~accumulate:true "available_regs"
          (fun (fundecl : Mach.fundecl) ->
            (* Skip DWARF variable range generation for complicated functions
               to avoid high compilation speed penalties *)
            let total_num_stack_slots =
              Array.fold_left (+) 0 fundecl.fun_num_stack_slots
            in
            if total_num_stack_slots
               > !Dwarf_flags.dwarf_max_function_complexity
            then fundecl
            else Available_regs.fundecl fundecl)
        ++ pass_dump_if ppf_dump Flambda_backend_flags.davail
             "Register availability analysis"
        ++ Profile.record ~accumulate:true "cfgize"
             (Cfgize.fundecl
                ~before_register_allocation:false
                ~preserve_orig_labels:false
                ~simplify_terminators:true)
        ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cfg
        ++ pass_dump_cfg_if ppf_dump Flambda_backend_flags.dump_cfg "After cfgize"
        ++ (fun (cfg_with_layout : Cfg_with_layout.t) ->
          match !Flambda_backend_flags.cfg_stack_checks with
          | false -> cfg_with_layout
          | true -> Cfg_stack_checks.cfg cfg_with_layout)
        ++ Profile.record ~accumulate:true "save_cfg" save_cfg
        ++ Profile.record ~accumulate:true "cfg_reorder_blocks"
             (reorder_blocks_random ppf_dump)
        ++ Profile.record ~accumulate:true "cfg_to_linear" Cfg_to_linear.run)
  ++ pass_dump_linear_if ppf_dump dump_linear "Linearized code")
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Linear
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf_dump dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "save_linear" save_linear
  ++ (fun (fd : Linear.fundecl) ->
    match !Flambda_backend_flags.cfg_stack_checks with
    | false -> Stack_check.linear fd
    | true -> fd)
  ++ Profile.record ~accumulate:true "emit_fundecl" emit_fundecl

let compile_data dl =
  dl
  ++ save_data
  ++ emit_data

let compile_phrases ~ppf_dump ps =
    let funcnames =
      List.fold_left (fun s p ->
          match p with
          | Cfunction fd -> String.Set.add fd.fun_name.sym_name s
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
            compile_fundecl ~ppf_dump ~funcnames fd;
            compile ~funcnames:(String.Set.remove fd.fun_name.sym_name funcnames) ps
          | Cdata dl ->
            compile_data dl;
            compile ~funcnames ps
    in
    compile ~funcnames ps

let compile_phrase ~ppf_dump p =
  compile_phrases ~ppf_dump [p]

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ~ppf_dump f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name.sym_name ->
           compile_phrase ~ppf_dump ph
       | _ -> ())
    (Generic_fns.compile ~shared:true
       (Generic_fns.Tbl.of_fns
          (Compilenv.current_unit_infos ()).ui_generic_fns))

let compile_unit ~output_prefix ~asm_filename ~keep_asm ~obj_filename ~may_reduce_heap
        ~ppf_dump gen =
  reset ();
  let create_asm = should_emit () &&
                   (keep_asm || not !Emitaux.binary_backend_available) in
  X86_proc.create_asm_file := create_asm;
  let remove_asm_file () =
    (* if [should_emit ()] is [false] then no assembly is generated,
       so the (empty) temporary file should be deleted. *)
    if not create_asm || not keep_asm then remove_file asm_filename
  in
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
       if create_asm then Emitaux.output_channel := open_out asm_filename;
       Misc.try_finally
         (fun () ->
            gen ();
            Zero_alloc_checker.record_unit_info ppf_dump;
            Compiler_hooks.execute Compiler_hooks.Check_allocations
              Zero_alloc_checker.iter_witnesses;
            write_ir output_prefix)
         ~always:(fun () ->
             if create_asm then close_out !Emitaux.output_channel)
         ~exceptionally:remove_asm_file;
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
       remove_asm_file ()
    )

let end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile make_cmm =
  Emitaux.Dwarf_helpers.init ~disable_dwarf:false sourcefile;
  emit_begin_assembly unix;
  make_cmm ()
  ++ (fun x -> if Clflags.should_stop_after Compiler_pass.Middle_end then exit 0 else x)
  ++ Compiler_hooks.execute_and_pipe Compiler_hooks.Cmm
  ++ Profile.record "compile_phrases" (compile_phrases ~ppf_dump)
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ~ppf_dump f);
  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)
  compile_phrase ~ppf_dump
    (Cmm_helpers.reference_symbols
       (List.filter_map (fun prim ->
           if not (Primitive.native_name_is_external prim) then None
           else Some (Cmm.global_symbol (Primitive.native_name prim)))
          !Translmod.primitive_declarations));
  emit_end_assembly sourcefile ()

type direct_to_cmm =
     ppf_dump:Format.formatter
  -> prefixname:string
  -> filename:string
  -> Lambda.program
  -> Cmm.phrase list

type pipeline =
  | Direct_to_cmm of direct_to_cmm

let asm_filename output_prefix =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then output_prefix ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm

let compile_implementation unix ?toplevel ~pipeline
      ~filename ~prefixname ~ppf_dump (program : Lambda.program) =
  compile_unit ~ppf_dump ~output_prefix:prefixname
    ~asm_filename:(asm_filename prefixname) ~keep_asm:!keep_asm_file
    ~obj_filename:(prefixname ^ ext_obj)
    ~may_reduce_heap:(Option.is_none toplevel)
    (fun () ->
      Compilation_unit.Set.iter Compilenv.require_global
        program.required_globals;
      Compilenv.record_external_symbols ();
      match pipeline with
      | Direct_to_cmm direct_to_cmm ->
        let cmm_phrases =
          direct_to_cmm ~ppf_dump ~prefixname ~filename program
        in
        end_gen_implementation unix ?toplevel ~ppf_dump ~sourcefile:filename
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
  let emit_item = function
    | Data dl -> emit_data dl
    | Func f -> emit_fundecl f
  in
  start_from_emit := true;
  Emitaux.Dwarf_helpers.init ~disable_dwarf:false filename;
  emit_begin_assembly unix;
  Profile.record "Emit" (List.iter emit_item) linear_unit_info.items;
  emit_end_assembly filename ()

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
