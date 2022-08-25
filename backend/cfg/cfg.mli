(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

val verbose : bool ref

include module type of struct
  include Cfg_intf.S
end

type basic_block =
  { start : Label.t;
    mutable body : basic instruction list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
        (** All predecessors, both normal and exceptional paths. *)
    mutable stack_offset : int;
        (** Stack offset of the start of the block. Used for emitting adjust
            trap on edges from one block to the next. *)
    mutable exn : Label.t option;
        (** All possible handlers of a raise that (1) can be triggered either by
            an explicit raise, or instructions such as calls and allocations,
            that appear(s) in this block; and (2) are within the same function.
            [exns] is a subset of the trap handler block labels of the cfg. *)
    mutable can_raise : bool;
        (** Does this block contain any instruction that can raise, such as a
            call, bounds check, allocation, or an explicit [raise]? *)
    mutable is_trap_handler : bool;
        (** Is this block a trap handler (i.e. is it an exn successor of another
            block) or not? *)
    mutable dead : bool
        (** This block must be unreachable from function entry. This field is
            set during cfg construction (if trap stacks are unresolved) and used
            during dead block elimination for checking. *)
        (* CR-someday gyorsh: The current implementation allows multiple
           pushtraps in each block means that different trap stacks are
           associated with the block at different points. At most one
           instruction in each block can raise, and always the last one. After
           we split the blocks based on Pushtrap/Poptrap, each block will have a
           unique trap stack associated with it. [exns] will not be needed, as
           the exn-successor will be uniquely determined by can_raise + top of
           trap stack. *)
  }

(** Control Flow Graph of a function. *)
type t = private
  { blocks : basic_block Label.Tbl.t;  (** Map from labels to blocks *)
    fun_name : string;  (** Function name, used for printing messages *)
    fun_args : Reg.t array;
        (** Function arguments. When Cfg is constructed from Linear, this
            information is not needed (Linear.fundecl does not have fun_args
            field) and [fun_args] is an empty array as a dummy value. *)
    fun_dbg : Debuginfo.t;  (** Dwarf debug info for function entry. *)
    entry_label : Label.t;
        (** This label must be the first in all layouts of this cfg. *)
    fun_fast : bool;  (** Precomputed based on cmmgen information. *)
    fun_contains_calls : bool;  (** Precomputed at selection time. *)
    fun_num_stack_slots : int array
        (** Precomputed at register allocation time *)
  }

val create :
  fun_name:string ->
  fun_args:Reg.t array ->
  fun_dbg:Debuginfo.t ->
  fun_fast:bool ->
  fun_contains_calls:bool ->
  fun_num_stack_slots:int array ->
  t

val fun_name : t -> string

val entry_label : t -> Label.t

val predecessor_labels : basic_block -> Label.t list

(** [exn] does not account for exceptional flow from the block that goes outside
    of the function. *)
val successor_labels : normal:bool -> exn:bool -> basic_block -> Label.Set.t

val replace_successor_labels :
  t -> normal:bool -> exn:bool -> basic_block -> f:(Label.t -> Label.t) -> unit

(** Returns [true] iff the passed block raises an exn that is not handled in
    this function, [can_raise_interproc] implies [can_raise] but not necessarily
    vice versa. *)
val can_raise_interproc : basic_block -> bool

val mem_block : t -> Label.t -> bool

val remove_block_exn : t -> Label.t -> unit

val get_block : t -> Label.t -> basic_block option

val get_block_exn : t -> Label.t -> basic_block

val iter_blocks : t -> f:(Label.t -> basic_block -> unit) -> unit

val fold_blocks : t -> f:(Label.t -> basic_block -> 'a -> 'a) -> init:'a -> 'a

val register_predecessors_for_all_blocks : t -> unit

(** Printing *)

val print_terminator : Format.formatter -> terminator instruction -> unit

val print_basic : Format.formatter -> basic instruction -> unit

val print_instruction' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  [`Basic of basic instruction | `Terminator of terminator instruction] ->
  unit

val print_instruction :
  Format.formatter ->
  [`Basic of basic instruction | `Terminator of terminator instruction] ->
  unit

(* CR-someday gyorsh: Current version of cfg is a half-way house in terms of its
   exception handling. It has a lot of redundancy and the result of the
   computation is not used.

   Redundancy: linear_to_cfg reconstructs intraprocedural exception handling
   stacks from linear IR and annotates each block with this information.
   However, CFG instructions still include the original push/poptraps from
   Linear.

   To remove these push/poptraps from CFG IR, we need to split blocks at every
   push/poptrap. Then, we can annotate the blocks with the top of the trap
   stack, instead of carrying the copy of the stack. *)

(* CR-someday gyorsh: store label after separately and update after
   reordering. *)

val can_raise_terminator : terminator -> bool

val can_raise_basic : basic -> bool

val can_raise_operation : operation -> bool

val is_pure_terminator : terminator -> bool

val is_pure_basic : basic -> bool

val is_noop_move : basic instruction -> bool

val set_stack_offset : 'a instruction -> int -> 'a instruction

val set_live : 'a instruction -> Reg.Set.t -> 'a instruction

val string_of_irc_work_list : irc_work_list -> string

val dump_basic : Format.formatter -> basic -> unit

val dump_terminator : ?sep:string -> Format.formatter -> terminator -> unit
