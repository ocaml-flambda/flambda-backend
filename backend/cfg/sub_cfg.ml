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

module DLL = Flambda_backend_utils.Doubly_linked_list

type t =
  { entry : Cfg.basic_block;
    exit : Cfg.basic_block;
    layout : Cfg.basic_block DLL.t
  }

let exit_has_never_terminator sub_cfg =
  Cfg.is_never_terminator sub_cfg.exit.terminator.desc

let make_never_block ?label () : Cfg.basic_block =
  Cfg.make_empty_block ?label
    (Cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)

let make_empty () =
  let exit = make_never_block () in
  let entry =
    Cfg.make_empty_block
      (Cfg.make_instr (Cfg.Always exit.start) [||] [||] Debuginfo.none)
  in
  let layout = DLL.make_empty () in
  DLL.add_end layout entry;
  DLL.add_end layout exit;
  { entry; exit; layout }

let start_label sub_cfg = sub_cfg.entry.start

let add_block_at_start sub_cfg block =
  DLL.add_begin sub_cfg.layout block;
  { sub_cfg with entry = block }

let add_empty_block_at_start sub_cfg ~label =
  Cfg.make_empty_block ~label
    (Cfg.make_instr (Cfg.Always (start_label sub_cfg)) [||] [||] Debuginfo.none)
  |> add_block_at_start sub_cfg

let add_block sub_cfg block =
  DLL.add_end sub_cfg.layout block;
  { sub_cfg with exit = block }

let add_never_block sub_cfg ~label =
  add_block sub_cfg (make_never_block ~label ())

let add_instruction_at_start sub_cfg desc arg res dbg =
  (* We don't check [exit_has_never_terminator] since we're adding at the start,
     and this function is only used in very specific situations (note comment in
     the interface). *)
  DLL.add_begin sub_cfg.entry.body (Cfg.make_instr desc arg res dbg)

let add_instruction' sub_cfg instr =
  assert (exit_has_never_terminator sub_cfg);
  DLL.add_end sub_cfg.exit.body instr

let add_instruction sub_cfg desc arg res dbg =
  add_instruction' sub_cfg (Cfg.make_instr desc arg res dbg)

let set_terminator sub_cfg desc arg res dbg =
  assert (Cfg.is_never_terminator sub_cfg.exit.terminator.desc);
  sub_cfg.exit.terminator <- Cfg.make_instr desc arg res dbg

let link_if_needed ~(from : Cfg.basic_block) ~(to_ : Cfg.basic_block) () =
  if Cfg.is_never_terminator from.terminator.desc
  then
    from.terminator
      <- { from.terminator with
           desc = Always to_.start;
           id = Cfg.next_instr_id ()
         }

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
         id = Cfg.next_instr_id ();
         arg = Option.value arg ~default:sub_cfg.exit.terminator.arg
       }

let mark_as_trap_handler sub_cfg ~exn_label =
  sub_cfg.entry.start <- exn_label;
  sub_cfg.entry.is_trap_handler <- true

let dump sub_cfg =
  let liveness = Cfg_dataflow.Instr.Tbl.create 32 in
  DLL.iter sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
      Format.eprintf "Block %a@." Label.print block.start;
      Regalloc_irc_utils.log_body_and_terminator ~indent:0 block.body
        block.terminator liveness)

(* note: `dump` is for debugging, and thus not always in use. *)
let (_ : t -> unit) = dump
