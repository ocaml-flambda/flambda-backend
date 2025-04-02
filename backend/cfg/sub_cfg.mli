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

(** A "sub" CFG is the counterpart of an instruction list in the original Mach
    selection pass.

    It is essentially a collection of blocks (stored as a layout, i.e. as a
    list), with two designated blocks:

    - an entry block;

    - an exit block.

    The exit block is where more instructions are being added, which means that
    the terminator of an in-construction "sub" CFG is `Never`, and will be
    changed only when no additional instructions will be inserted to the
    block. *)

type t

val exit_has_never_terminator : t -> bool

val make_empty : unit -> t

val add_empty_block_at_start : t -> label:Label.t -> unit

val add_never_block : t -> label:Label.t -> unit

(** Use [add_instruction] in preference to this function. *)
val add_instruction_at_start :
  t -> Cfg.basic -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

(** [add_instruction] can only be called when the terminator is [Never]. *)
val add_instruction :
  t -> Cfg.basic -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

(** [add_instruction'] can only be called when the terminator is [Never]. *)
val add_instruction' : t -> Cfg.basic Cfg.instruction -> unit

(** [set_terminator] can only be called when the terminator is [Never]. *)
val set_terminator :
  t -> Cfg.terminator -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

val iter_basic_blocks : t -> f:(Cfg.basic_block -> unit) -> unit

val exists_basic_blocks : t -> f:(Cfg.basic_block -> bool) -> bool

val join : from:t list -> to_:t -> unit

val join_tail : from:t list -> to_:t -> unit

val update_exit_terminator : ?arg:Reg.t array -> t -> Cfg.terminator -> unit

val start_label : t -> Label.t

val mark_as_trap_handler : t -> exn_label:Label.t -> unit

val dump : t -> unit
