(* MIT License

   Copyright (c) 2024 Jane Street Group LLC

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Flambda_backend_utils.Doubly_linked_list

(* Instruction ids. *)
let instr_id = InstructionId.make_sequence ()

let reset_instr_id () = InstructionId.reset instr_id

let next_instr_id () = InstructionId.get_and_incr instr_id

let make_instr desc arg res dbg =
  { Cfg.desc;
    arg;
    res;
    dbg;
    fdo = Fdo_info.none;
    live = Reg.Set.empty;
    stack_offset = Cfg.invalid_stack_offset;
    id = next_instr_id ();
    irc_work_list = Unknown_list;
    ls_order = 0;
    (* CR mshinwell/xclerc: should this be [None]? *)
    available_before =
      Some (Reg_availability_set.Ok Reg_with_debug_info.Set.empty);
    available_across = None
  }

type t =
  { mutable entry : Cfg.basic_block;
    mutable exit : Cfg.basic_block;
    layout : Cfg.basic_block DLL.t
  }

let exit_has_never_terminator sub_cfg =
  Cfg.is_never_terminator sub_cfg.exit.terminator.desc

let make_never_block ?label () : Cfg.basic_block =
  Cfg.make_empty_block ?label (make_instr Cfg.Never [||] [||] Debuginfo.none)

let make_empty () =
  let exit = make_never_block () in
  let entry =
    Cfg.make_empty_block
      (make_instr (Cfg.Always exit.start) [||] [||] Debuginfo.none)
  in
  let layout = DLL.make_empty () in
  DLL.add_end layout entry;
  DLL.add_end layout exit;
  { entry; exit; layout }

let start_label sub_cfg = sub_cfg.entry.start

let add_block_at_start sub_cfg block =
  DLL.add_begin sub_cfg.layout block;
  sub_cfg.entry <- block

let add_empty_block_at_start sub_cfg ~label =
  Cfg.make_empty_block ~label
    (make_instr (Cfg.Always (start_label sub_cfg)) [||] [||] Debuginfo.none)
  |> add_block_at_start sub_cfg

let add_block sub_cfg block =
  DLL.add_end sub_cfg.layout block;
  sub_cfg.exit <- block

let add_never_block sub_cfg ~label =
  add_block sub_cfg (make_never_block ~label ())

let add_instruction_at_start sub_cfg desc arg res dbg =
  (* We don't check [exit_has_never_terminator] since we're adding at the start,
     and this function is only used in very specific situations (note comment in
     the interface). *)
  DLL.add_begin sub_cfg.entry.body (make_instr desc arg res dbg)

let add_instruction' sub_cfg instr =
  assert (exit_has_never_terminator sub_cfg);
  DLL.add_end sub_cfg.exit.body instr

let add_instruction sub_cfg desc arg res dbg =
  add_instruction' sub_cfg (make_instr desc arg res dbg)

let set_terminator sub_cfg desc arg res dbg =
  assert (Cfg.is_never_terminator sub_cfg.exit.terminator.desc);
  sub_cfg.exit.terminator <- make_instr desc arg res dbg

let link_if_needed ~(from : Cfg.basic_block) ~(to_ : Cfg.basic_block) () =
  if Cfg.is_never_terminator from.terminator.desc
  then
    from.terminator
      <- { from.terminator with desc = Always to_.start; id = next_instr_id () }

let iter_basic_blocks sub_cfg ~f = DLL.iter sub_cfg.layout ~f

let exists_basic_blocks sub_cfg ~f = DLL.exists sub_cfg.layout ~f

let transfer ~from ~to_ = DLL.transfer ~from:from.layout ~to_:to_.layout ()

let join ~from ~to_ =
  List.iter (fun from -> transfer ~from ~to_) from;
  let join_block = make_never_block () in
  List.iter (fun from -> link_if_needed ~from:from.exit ~to_:join_block ()) from;
  add_block to_ join_block

let join_tail ~from ~to_ =
  List.iter (fun from -> transfer ~from ~to_) from;
  add_never_block to_ ~label:(Cmm.new_label ())

let update_exit_terminator ?arg sub_cfg desc =
  sub_cfg.exit.terminator
    <- { sub_cfg.exit.terminator with
         desc;
         id = next_instr_id ();
         arg = Option.value arg ~default:sub_cfg.exit.terminator.arg
       }

let mark_as_trap_handler sub_cfg = sub_cfg.entry.is_trap_handler <- true

let dump sub_cfg =
  let liveness = InstructionId.Tbl.create 32 in
  DLL.iter sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
      Format.eprintf "Block %a@." Label.print block.start;
      Regalloc_irc_utils.log_body_and_terminator block.body block.terminator
        liveness)

(* note: `dump` is for debugging, and thus not always in use. *)
let (_ : t -> unit) = dump
