open Cfg_intf.S
module DLL = Flambda_backend_utils.Doubly_linked_list

module Instruction = struct
  type 'a t =
    { mutable desc : 'a;
      mutable arg : Reg.t array;
      mutable res : Reg.t array;
      mutable id : int
    }

  let make ~remove_locs ({ desc; arg; res; id } : 'a t) : 'a instruction =
    let map_regs arr =
      Array.map
        (fun (r : Reg.t) ->
          { r with
            loc =
              (if remove_locs && not (Reg.is_preassigned r)
              then Unknown
              else r.loc)
          })
        arr
    in
    { desc;
      arg = map_regs arg;
      res = map_regs res;
      id;
      dbg = Debuginfo.none;
      fdo = None;
      irc_work_list = Unknown_list;
      live = Reg.Set.empty;
      stack_offset = 0;
      ls_order = -1;
      available_before = None;
      available_across = None;
    }
end

module Basic = struct
  type t = basic Instruction.t

  let make ~remove_locs (t : t) : basic instruction =
    Instruction.make ~remove_locs t
end

module Terminator = struct
  type t = terminator Instruction.t

  let make ~remove_locs (t : t) : terminator instruction =
    Instruction.make ~remove_locs t
end

module Block = struct
  type t =
    { start : Label.t;
      mutable body : Basic.t list;
      terminator : Terminator.t;
      exn : Label.t option
    }

  let make ~remove_regalloc ~remove_locs ({ start; body; terminator; exn } : t)
      : Cfg.basic_block =
    let body =
      List.map (Basic.make ~remove_locs) body
      |> List.filter (function
           | { desc = Op (Spill | Reload); _ } -> not remove_regalloc
           | _ -> true)
    in
    let terminator = Terminator.make ~remove_locs terminator in
    let can_raise = Cfg.can_raise_terminator terminator.desc in
    { start;
      body = DLL.of_list body;
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = 0;
      exn;
      can_raise;
      is_trap_handler = false;
      dead = false;
      cold = false
    }
end

module Cfg_desc = struct
  type t =
    { mutable fun_args : Reg.t array;
      blocks : Block.t list;
      fun_contains_calls : bool
    }

  let make ~remove_regalloc ~remove_locs
      ({ fun_args; blocks; fun_contains_calls } : t) : Cfg_with_layout.t =
    let cfg =
      Cfg.create ~fun_name:"foo" ~fun_args:(Array.copy fun_args) ~fun_dbg:Debuginfo.none
        ~fun_fast:false ~fun_contains_calls
        ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
    in
    List.iter
      (fun (block : Block.t) ->
        assert (not (Label.Tbl.mem cfg.blocks block.start));
        Label.Tbl.replace cfg.blocks block.start
          (Block.make ~remove_regalloc ~remove_locs block))
      blocks;
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        Cfg.successor_labels ~normal:true ~exn:false block
        |> Label.Set.iter (fun suc ->
               let suc = Label.Tbl.find cfg.blocks suc in
               suc.predecessors <- Label.Set.add block.start suc.predecessors);
        Cfg.successor_labels ~normal:false ~exn:true block
        |> Label.Set.iter (fun suc ->
               let suc = Label.Tbl.find cfg.blocks suc in
               suc.predecessors <- Label.Set.add block.start suc.predecessors;
               suc.is_trap_handler <- true))
      cfg.blocks;
    let cfg_layout =
      Cfg_with_layout.create ~layout:(DLL.make_empty ())
        ~preserve_orig_labels:true ~new_labels:Label.Set.empty cfg
    in
    (if not remove_locs
    then
      (* If we leave in the locations we want to have the actual stack slot
         count. *)
      let update_stack_slots i =
        let update_slot (r : Reg.t) =
          match r.loc, Proc.stack_slot_class r.typ with
          | Stack (Local idx), stack_class ->
            cfg.fun_num_stack_slots.(stack_class)
              <- max cfg.fun_num_stack_slots.(stack_class) (idx + 1)
          | _ -> ()
        in
        Array.iter update_slot i.arg;
        Array.iter update_slot i.res
      in
      Cfg_with_layout.iter_instructions ~instruction:update_stack_slots
        ~terminator:update_stack_slots cfg_layout);
    cfg_layout

  let make_pre t = make ~remove_regalloc:true ~remove_locs:true t

  let make_post t = make ~remove_regalloc:false ~remove_locs:false t
end

let entry_label =
  Cfg_desc.make_post
    { fun_args = [||]; blocks = []; fun_contains_calls = false }
  |> Cfg_with_layout.cfg |> Cfg.entry_label
  (* CR xclerc for xclerc: that test relies on the use of the polymorphic
          comparison over CFG values, but that can no longer be used since instruction
          lists now contain circular values.
     let () =
       let made_cfg =
         ({ fun_args = [| Proc.phys_reg 0 |];
            blocks =
              [ { start = entry_label;
                  body = [];
                  exn = None;
                  terminator =
                    { id = 1;
                      desc = Return;
                      arg = [| Proc.phys_reg 0 |];
                      res = [||]
                    }
                } ];
            fun_contains_calls = false
          }
           : Cfg_desc.t)
         |> Cfg_desc.make_post
       in
       let cfg =
         Cfg.create ~fun_name:"foo"
           ~fun_args:[| Proc.phys_reg 0 |]
           ~fun_dbg:Debuginfo.none ~fun_fast:false ~fun_contains_calls:false
           ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
       in
       Label.Tbl.add cfg.Cfg.blocks (Cfg.entry_label cfg)
         { start = Cfg.entry_label cfg;
           body = Cfg.BasicInstructionList.make_empty ();
           exn = None;
           can_raise = false;
           is_trap_handler = false;
           predecessors = Label.Set.empty;
           stack_offset = 0;
           dead = false;
           cold = false;
           terminator =
             { desc = Return;
               arg = [| Proc.phys_reg 0 |];
               res = [||];
               dbg = Debuginfo.none;
               fdo = None;
               stack_offset = 0;
               id = 1;
               live = Reg.Set.empty;
               irc_work_list = Unknown_list
             }
         };
       let cfg =
         cfg
         |> Cfg_with_layout.create ~layout:[] ~preserve_orig_labels:true
              ~new_labels:Label.Set.empty
       in
       assert (made_cfg = cfg);
       ()
  *)
  [@@ocamlformat "wrap-comments=false"]

exception Break_test

let move_param_label = entry_label + 1

let call_label = entry_label + 2

let move_tmp_res_label = entry_label + 3

let add_label = entry_label + 4

let return_label = entry_label + 5

let new_label i = entry_label + 6 + i

let int = Array.init 8 (fun _ -> Reg.create Int)

let val_ = Array.init 8 (fun _ -> Reg.create Val)

let _addr = Array.init 8 (fun _ -> Reg.create Addr)

let float = Array.init 8 (fun _ -> Reg.create Float)

let base_templ () : Cfg_desc.t * (unit -> int) =
  let make_id =
    let last_id = ref 2 in
    fun () ->
      last_id := !last_id + 1;
      !last_id
  in
  let make_locs regs f =
    let locs = f (Array.map (fun (r : Reg.t) -> r.typ) regs) in
    let regs =
      Array.map2
        (fun (reg : Reg.t) (loc : Reg.t) -> { reg with loc = loc.loc })
        regs locs
    in
    regs, locs
  in
  (* This is one possible representation of code:

     [fun f x y a -> let y = f x y a in let x = x + y in x] *)
  let int_arg1 = int.(0) in
  let int_arg2 = int.(1) in
  let stack_loc = Reg.at_location int_arg1.typ (Stack (Local 0)) in
  let args, arg_locs =
    make_locs [| val_.(0); int_arg1; int_arg2; float.(0) |] Proc.loc_parameters
  in
  let int_arg1 = args.(1) in
  let int_arg2 = args.(2) in
  let tmp_results, tmp_result_locs =
    make_locs [| int.(2) |] Proc.loc_results_return
  in
  let results, result_locs = make_locs [| int.(3) |] Proc.loc_results_return in
  let make_moves src dst =
    Array.map2
      (fun src dst : Basic.t ->
        { id = make_id (); desc = Op Move; arg = [| src |]; res = [| dst |] })
      src dst
    |> Array.to_list
  in
  ( { fun_args = Array.copy arg_locs;
      blocks =
        [ { start = entry_label;
            body = [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always move_param_label;
                arg = [||];
                res = [||]
              }
          };
          { start = move_param_label;
            body =
              make_moves arg_locs args
              @ [ { Instruction.id = make_id ();
                    desc = Op Spill;
                    arg = [| int_arg1 |];
                    res = [| stack_loc |]
                  } ]
              @ make_moves args arg_locs;
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always call_label;
                arg = [||];
                res = [||]
              }
          };
          { start = call_label;
            body = [];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Call { op = Indirect; label_after = move_tmp_res_label };
                arg = arg_locs;
                res = tmp_result_locs
              }
          };
          { start = move_tmp_res_label;
            body =
              make_moves tmp_result_locs tmp_results
              @ make_moves tmp_results [| int_arg2 |]
              @ [ { Instruction.id = make_id ();
                    desc = Op Reload;
                    arg = [| stack_loc |];
                    res = [| int_arg1 |]
                  } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always add_label;
                arg = [||];
                res = [||]
              }
          };
          { start = add_label;
            body =
              [ { Instruction.id = make_id ();
                  desc = Op (Intop Mach.Iadd);
                  arg = [| int_arg1; int_arg2 |];
                  res = [| int_arg1 |]
                } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always return_label;
                arg = [||];
                res = [||]
              }
          };
          { start = return_label;
            body =
              make_moves [| int_arg1 |] results
              @ make_moves results result_locs
              @ [ { id = make_id ();
                    desc = Reloadretaddr;
                    arg = [||];
                    res = [||]
                  } ];
            exn = None;
            terminator =
              { id = make_id (); desc = Return; arg = result_locs; res = [||] }
          } ];
      fun_contains_calls = true
    },
    make_id )

let check name f ~exp_std ~exp_err =
  let before, after = f () in
  let with_wrap_ppf ppf f =
    Format.pp_print_flush ppf ();
    let buf = Buffer.create 0 in
    let ppf_buf = Format.formatter_of_buffer buf in
    let old_out_func = Format.pp_get_formatter_out_functions ppf () in
    Format.pp_set_formatter_out_functions ppf
      (Format.pp_get_formatter_out_functions ppf_buf ());
    let res = f () in
    Format.pp_print_flush ppf ();
    Format.pp_set_formatter_out_functions ppf old_out_func;
    res, buf |> Buffer.to_bytes |> Bytes.to_string |> String.trim
  in
  let ((), err_out), std_out =
    with_wrap_ppf Format.std_formatter (fun () ->
        with_wrap_ppf Format.err_formatter (fun () ->
            try
              let desc =
                try
                  Misc.protect_refs
                    [R (Flambda_backend_flags.regalloc_validate, true)]
                    (fun () -> Regalloc_validate.Description.create before)
                with Misc.Fatal_error ->
                  Format.printf
                    "fatal exception raised when creating description";
                  raise Break_test
              in
              let desc =
                match desc with
                | None ->
                  Format.printf "description was not generated";
                  raise Break_test
                | Some desc -> desc
              in
              let res =
                try Regalloc_validate.test desc after
                with Misc.Fatal_error ->
                  Format.printf
                    "fatal exception raised when validating description";
                  raise Break_test
              in
              match res with
              | Ok cfg ->
                if cfg = after
                then ()
                else Format.printf "Validation changed cfg"
              | Error error ->
                Format.printf "Validation failed: %a"
                  Regalloc_validate.Error.print error
            with Break_test -> ()))
  in
  if exp_std = std_out && exp_err = err_out
  then Format.printf "%s: OK\n%!" name
  else
    let print_as_text msg text =
      Format.printf "@?@[<h 2>%s:" msg;
      if String.length text > 0 then Format.force_newline ();
      Format.pp_print_text Format.std_formatter text;
      Format.printf "@]\n";
      ()
    in
    Format.printf "%s: FAILED\n" name;
    print_as_text "Expected std" exp_std;
    print_as_text "Got std" std_out;
    print_as_text "Expected err" exp_err;
    print_as_text "Got err" err_out;
    Format.printf "Std as string literal:\n%S\n" std_out;
    Format.printf "Err as string literal:\n%S\n" err_out;
    Format.print_flush ();
    (* CR azewierzejew for azewierzejew: Fix how the files are saved. *)
    Cfg_with_layout.save_as_dot ~filename:"/tmp/before.dot" before
      "test-cfg-before";
    Cfg_with_layout.save_as_dot ~filename:"/tmp/after.dot" after
      "test-cfg-after";
    Format.printf "The failing cfgs were put in /tmp/[before|after].dot\n";
    Format.print_flush ();
    exit 1

let ( .&() ) (cfg : Cfg_desc.t) (label : Label.t) : Block.t =
  List.find (fun (block : Block.t) -> block.start = label) cfg.blocks

let ( .!() ) (block : Block.t) (index : int) : Basic.t =
  List.nth block.body index

(* let () = check "IRC works on base templ" (fun templ _ -> let cfg =
   Cfg_desc.make templ in cfg, Regalloc_irc.run cfg) ~exp_std:"" ~exp_err:"" *)

let () =
  check "Duplicate instruction found when creating description"
    (fun () ->
      let templ, _ = base_templ () in
      let block = templ.&(add_label) in
      (* Duplicate the instruction. *)
      block.body <- List.hd block.body :: block.body;
      let cfg = Cfg_desc.make_pre templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 8 while adding a basic \
       instruction to the description"

let () =
  check "Duplicate terminator found when creating description"
    (fun () ->
      let templ, _ = base_templ () in
      (* Change id of one terminator to the id of the other one. *)
      templ.&(add_label).terminator.id <- templ.&(call_label).terminator.id;
      let cfg = Cfg_desc.make_pre templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 13 while adding a terminator \
       instruction to the description"

let () =
  check "Regalloc specific instructions are checked when creating description"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg = Cfg_desc.make ~remove_regalloc:false ~remove_locs:true templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:">> Fatal error: instruction 19 is a spill"

let () =
  check "Terminator result count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(call_label).terminator.res <- [||];
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 13 results: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Instruction result count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(add_label).!(0).res <- [||];
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 8 results: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Terminator argument count"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(return_label).terminator.arg <- [||];
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 3 arguments: register array length \
       has changed. Before: 1. Now: 0."

let () =
  check "Function argument isn't preassigned"
    (fun () ->
      let templ, _make_id = base_templ () in
      templ.fun_args.(0) <- Reg.dummy;
      let cfg1 = Cfg_desc.make_pre templ in
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when creating description"
    ~exp_err:
      ">> Fatal error: Register in function arguments that isn't preassigned: \
       I/0"

let () =
  check "Function argument count changed"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.fun_args <- Array.sub templ.fun_args 0 1;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In function arguments: register array length has \
       changed. Before: 4. Now: 1."

let () =
  check "Function argument precoloring changed"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.fun_args.(0) <- templ.fun_args.(1);
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In function arguments: changed preassigned register's \
       location from %rax to %rbx"

let () =
  check "Location can't be unknown after allocation"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg = Cfg_desc.make_pre templ in
      cfg, cfg)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: instruction 20 has a register (V/53) with an unknown \
       location"

let () =
  check "Precoloring can't change"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(move_param_label).!(7).res <- templ.&(move_param_label).!(6).res;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: In instruction's no 17 results: changed preassigned \
       register's location from %rdi to %rbx"

let () =
  check "Duplicate instruction found when validating description"
    (fun () ->
      let templ, _ = base_templ () in
      (* The spill has the same id as reload instruction. *)
      templ.&(move_param_label).!(4).id <- templ.&(move_tmp_res_label).!(2).id;
      let cfg1 = Cfg_desc.make_pre templ in
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Duplicate instruction no. 10 while checking a basic \
       instruction in the new CFG"

let () =
  check "Regalloc changed existing instruction into regalloc instruction"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(move_param_label).!(3).desc <- Op Spill;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation changed existing instruction no. 23 \
       into a register allocation specific instruction"


(* CR xclerc for xclerc: same as above (polymorphic commpare on values
   with cycles).
   let () =
  check "Regalloc added non-regalloc specific instr"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let block = templ.&(add_label) in
      let r = (List.hd block.body).res in
      block.body
        <- { desc = Op Move; id = make_id (); arg = r; res = r } :: block.body;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation added non-regalloc specific \
       instruction no. 26"
*)

  (* CR xclerc for xclerc: same as above (polymorphic commpare on values
     with cycles).
     let () =
       check "Regalloc added a 'goto' and a block"
         (fun () ->
           let templ, make_id = base_templ () in
           let cfg1 = Cfg_desc.make_pre templ in
           let tmp_label = new_label 1 in
           let templ =
             { templ with
               blocks =
                 { start = tmp_label;
                   exn = None;
                   body = [];
                   terminator =
                     { desc = Always return_label;
                       res = [||];
                       arg = [||];
                       id = make_id ()
                     }
                 }
                 :: templ.blocks
             }
           in
           templ.&(add_label).terminator.desc <- Always tmp_label;
           let cfg2 = Cfg_desc.make_post templ in
           cfg1, cfg2)
         ~exp_std:"" ~exp_err:""
  *)
  [@@ocamlformat "wrap-comments=false"]

let () =
  check "Regalloc added a fallthrough block that goes to the wrong label"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Always call_label;
                  res = [||];
                  arg = [||];
                  id = make_id ()
                }
            }
            :: templ.blocks
        }
      in
      templ.&(add_label).terminator.desc <- Always tmp_label;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: When checking equivalence of labels before and after \
       allocation got different successor id's. Successor (label, instr id) \
       before: (6, 6). Successor (label, instr id) after: (8, 13)."

let () =
  check "Regalloc added a not allowed terminator and a block"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Return; res = [||]; arg = [||]; id = make_id () }
            }
            :: templ.blocks
        }
      in
      templ.&(add_label).terminator.desc <- Always tmp_label;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Register allocation added a terminator no. 26 but \
       that's not allowed for this type of terminator: Return"

let () =
  check "Regalloc reordered instructions between blocks"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let add_body = templ.&(add_label).body in
      templ.&(add_label).body <- [];
      templ.&(return_label).body <- add_body @ templ.&(return_label).body;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The instruction's no. 8 successor id has changed. \
       Before allocation: 7. After allocation (ignoring instructions added by \
       allocation): 6."

let () =
  check "Regalloc reordered instructions within a block"
    (fun () ->
      let templ, _make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let block = templ.&(move_tmp_res_label) in
      block.body
        <- (block.body |> List.rev |> function
            | i1 :: i2 :: t -> i2 :: i1 :: t
            | l -> l |> List.rev);
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The instruction's no. 12 successor id has changed. \
       Before allocation: 11. After allocation (ignoring instructions added by \
       allocation): 9."

let () =
  check "Regalloc added a loop"
    (fun () ->
      let templ, make_id = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      let tmp_label = new_label 1 in
      let templ =
        { templ with
          blocks =
            { start = tmp_label;
              exn = None;
              body = [];
              terminator =
                { desc = Always tmp_label;
                  res = [||];
                  arg = [||];
                  id = make_id ()
                }
            }
            :: templ.blocks
        }
      in
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Visiting the same block 8 without knowing the successor \
       instruction's id. That means there's a loop consisting of only \
       instructions added by the register allocator."

let () =
  check "Regalloc changed instruction desc"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(add_label).!(0).desc <- Op (Intop Isub);
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:">> Fatal error: The desc of instruction with id 8 changed"

let () =
  check "Regalloc changed terminator desc"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(return_label).terminator.desc <- Raise Raise_regular;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: The desc of terminator with id 3 changed, before: \
       Return, after: Raise."

let () =
  check "Deleted instruction"
    (fun () ->
      let templ, _ = base_templ () in
      let cfg1 = Cfg_desc.make_pre templ in
      templ.&(add_label).body <- List.tl templ.&(add_label).body;
      let cfg2 = Cfg_desc.make_post templ in
      cfg1, cfg2)
    ~exp_std:"fatal exception raised when validating description"
    ~exp_err:
      ">> Fatal error: Instruction no. 8 was deleted by register allocator"

let make_loop ~loop_loc_first n =
  let make_id =
    let last_id = ref 2 in
    fun () ->
      last_id := !last_id + 1;
      !last_id
  in
  let make_locs regs f =
    let locs = f (Array.map (fun (r : Reg.t) -> r.typ) regs) in
    let regs =
      Array.map2
        (fun (reg : Reg.t) (loc : Reg.t) -> { reg with loc = loc.loc })
        regs locs
    in
    regs, locs
  in
  let loop_loc_label, loop_reg_label =
    let l1 = new_label 1 in
    let l2 = new_label 2 in
    if loop_loc_first then l1, l2 else l2, l1
  in
  let stack_loc =
    let locs =
      Array.init (n + 1) (fun i -> Reg.at_location Int (Stack (Local i)))
    in
    fun i -> locs.(i)
  in
  let int_arg1 = int.(0) in
  let int_arg2 = int.(1) in
  let int_arg3 = int.(2) in
  let args, arg_locs =
    make_locs [| int_arg1; int_arg2; int_arg3 |] Proc.loc_parameters
  in
  let int_arg1 = args.(0) in
  let int_arg2 = args.(1) in
  let int_arg3 = args.(2) in
  let extra_regs =
    Array.init n (fun _ -> { (Reg.create Int) with loc = int_arg3.loc })
  in
  let results, result_locs = make_locs [| int_arg1 |] Proc.loc_results_return in
  let make_moves src dst =
    Array.map2
      (fun src dst : Basic.t ->
        { id = make_id (); desc = Op Move; arg = [| src |]; res = [| dst |] })
      src dst
    |> Array.to_list
  in
  let templ : Cfg_desc.t =
    { fun_args = arg_locs;
      blocks =
        [ { start = entry_label;
            body = [{ id = make_id (); desc = Prologue; arg = [||]; res = [||] }];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always move_param_label;
                arg = [||];
                res = [||]
              }
          };
          { start = move_param_label;
            body =
              make_moves arg_locs args
              (* Move [arg3] to all [extra_regs]. *)
              @ List.init n (fun n ->
                    { Instruction.id = make_id ();
                      desc = Op Move;
                      arg = [| int_arg3 |];
                      res = [| extra_regs.(n) |]
                    })
              (* Spill [arg3] to locations [0;n-1] *)
              @ List.init n (fun n ->
                    { Instruction.id = make_id ();
                      desc = Op Spill;
                      arg = [| int_arg3 |];
                      res = [| stack_loc n |]
                    })
              (* Spill [arg2] to location n. If we spilled [arg3] the code would
                 be correct. *)
              @ [ { Instruction.id = make_id ();
                    desc = Op Spill;
                    arg = [| int_arg2 |];
                    res = [| stack_loc n |]
                  } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_loc_label;
                      eq = loop_reg_label;
                      gt = move_tmp_res_label;
                      is_signed = true;
                      imm = Some 0
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = loop_loc_label;
            body =
              (* Rotate all locations by one index. *)
              List.init n (fun n ->
                  (* Move loc i+1 to i. *)
                  [ { Instruction.id = make_id ();
                      desc = Op Reload;
                      arg = [| stack_loc (n + 1) |];
                      res = [| int_arg3 |]
                    };
                    { Instruction.id = make_id ();
                      desc = Op Spill;
                      arg = [| int_arg3 |];
                      res = [| stack_loc n |]
                    } ])
              |> List.concat;
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_loc_label;
                      eq = loop_reg_label;
                      gt = move_tmp_res_label;
                      is_signed = true;
                      imm = Some 1
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = loop_reg_label;
            body =
              (* Rotate all regs by one index. *)
              List.init (n - 1) (fun n ->
                  (* Move reg i+1 to i. *)
                  { Instruction.id = make_id ();
                    desc = Op Move;
                    arg = [| extra_regs.(n + 1) |];
                    res = [| extra_regs.(n) |]
                  });
            exn = None;
            terminator =
              { id = make_id ();
                desc =
                  Int_test
                    { lt = loop_reg_label;
                      eq = move_tmp_res_label;
                      gt = move_tmp_res_label;
                      is_signed = true;
                      imm = Some 2
                    };
                arg = [| int_arg1 |];
                res = [||]
              }
          };
          { start = move_tmp_res_label;
            body =
              (* Require that extra reg 0 is in location 0. This will break
                 after loop is run at least [n] times because then the spilled
                 [arg2] in location n will rotate over to location 0. For that
                 reason the fix-point algorithm will also have to run n
                 times. *)
              [ { Instruction.id = make_id ();
                  desc = Op (Const_int (Nativeint.of_int 1));
                  arg = [||];
                  res = [| int_arg1 |]
                };
                (* Load extra reg 0 from location 0.*)
                { Instruction.id = make_id ();
                  desc = Op Reload;
                  arg = [| stack_loc 0 |];
                  res = [| extra_regs.(0) |]
                };
                (* Add the extra reg 0 to accumalated result. *)
                { Instruction.id = make_id ();
                  desc = Op (Intop Iadd);
                  arg = [| int_arg1; extra_regs.(0) |];
                  res = [| int_arg1 |]
                } ];
            exn = None;
            terminator =
              { id = make_id ();
                desc = Always return_label;
                arg = [||];
                res = [||]
              }
          };
          { start = return_label;
            body =
              make_moves [| int_arg1 |] results
              @ make_moves results result_locs
              @ [ { id = make_id ();
                    desc = Reloadretaddr;
                    arg = [||];
                    res = [||]
                  } ];
            exn = None;
            terminator =
              { id = make_id (); desc = Return; arg = result_locs; res = [||] }
          } ];
      fun_contains_calls = true
    }
  in
  Cfg_desc.make_pre templ, Cfg_desc.make_post templ

let test_loop ~loop_loc_first n =
  assert (n >= 2);
  let start_time = Sys.time () in
  check
    (Printf.sprintf "Check loop with %d locations" n)
    (fun () -> make_loop ~loop_loc_first n)
    ~exp_std:
      "Validation failed: Bad equations at entry point, reason: Unsatisfiable \
       equations when removing result equations.\n\
       Existing equation has to agree on 0 or 2 sides (cannot be exactly 1) \
       with the removed equation.\n\
       Existing equation R[%rdi]=%rbx.\n\
       Removed equation: R[%rbx]=%rbx.\n\
       Equations: R[%rax]=%rax R[%rdi]=%rbx R[%rdi]=%rdi\n\
       Function argument descriptions: R[%rax], R[%rbx], R[%rdi]\n\
       Function argument locations: %rax, %rbx, %rdi"
    ~exp_err:"";
  let end_time = Sys.time () in
  Format.printf "  Time of loop test: %fs\n" (end_time -. start_time);
  ()

let () = test_loop ~loop_loc_first:true 2

let () = test_loop ~loop_loc_first:true 5

let () = test_loop ~loop_loc_first:true 10

let () = test_loop ~loop_loc_first:true 20

let () = test_loop ~loop_loc_first:false 2

let () = test_loop ~loop_loc_first:false 5

let () = test_loop ~loop_loc_first:false 10

let () = test_loop ~loop_loc_first:false 20
