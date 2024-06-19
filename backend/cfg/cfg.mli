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

type basic_instruction_list =
  basic instruction Flambda_backend_utils.Doubly_linked_list.t

type basic_block =
  { start : Label.t;
    body : basic_instruction_list;
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
    mutable dead : bool;
        (** This block must be unreachable from function entry. This field is
            set during cfg construction (if trap stacks are unresolved) and used
            during dead block elimination for checking. *)
    mutable cold : bool
        (* CR-someday gyorsh: The current implementation allows multiple
           pushtraps in each block means that different trap stacks are
           associated with the block at different points. At most one
           instruction in each block can raise, and always the last one. After
           we split the blocks based on Pushtrap/Poptrap, each block will have a
           unique trap stack associated with it. [exns] will not be needed, as
           the exn-successor will be uniquely determined by can_raise + top of
           trap stack. *)
  }

(* Subset of Cmm.codegen_option. *)
type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Assume_zero_alloc of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check_zero_alloc of
      { strict : bool;
        loc : Location.t
      }

val of_cmm_codegen_option : Cmm.codegen_option list -> codegen_option list

(** Control Flow Graph of a function. *)
type t =
  { blocks : basic_block Label.Tbl.t;  (** Map from labels to blocks *)
    fun_name : string;  (** Function name, used for printing messages *)
    fun_args : Reg.t array;
        (** Function arguments. When Cfg is constructed from Linear, this
            information is not needed (Linear.fundecl does not have fun_args
            field) and [fun_args] is an empty array as a dummy value. *)
    fun_codegen_options : codegen_option list;
        (** Code generation options passed from Cmm. *)
    fun_dbg : Debuginfo.t;  (** Dwarf debug info for function entry. *)
    entry_label : Label.t;
        (** This label must be the first in all layouts of this cfg. *)
    fun_contains_calls : bool;
        (** Precomputed during selection and poll insertion. *)
    fun_num_stack_slots : int array;
        (** Precomputed at register allocation time *)
    fun_poll : Lambda.poll_attribute (* Whether to insert polling points. *)
  }

val create :
  fun_name:string ->
  fun_args:Reg.t array ->
  fun_codegen_options:codegen_option list ->
  fun_dbg:Debuginfo.t ->
  fun_contains_calls:bool ->
  fun_num_stack_slots:int array ->
  fun_poll:Lambda.poll_attribute ->
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

val first_instruction_id : basic_block -> int

val first_instruction_stack_offset : basic_block -> int

val mem_block : t -> Label.t -> bool

val add_block_exn : t -> basic_block -> unit

val remove_block_exn : t -> Label.t -> unit

val remove_blocks : t -> Label.Set.t -> unit

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

   CFG instructions still include push/poptraps. To remove these push/poptraps
   from CFG IR, we need to split blocks at every push/poptrap. Then, we can
   annotate the blocks with the top of the trap stack, instead of carrying the
   copy of the stack. *)

(* CR-someday gyorsh: store label after separately and update after
   reordering. *)

val can_raise_terminator : terminator -> bool

val is_pure_terminator : terminator -> bool

val is_pure_basic : basic -> bool

val is_pure_operation : operation -> bool

val is_noop_move : basic instruction -> bool

val set_stack_offset : 'a instruction -> int -> unit

val string_of_irc_work_list : irc_work_list -> string

val dump_basic : Format.formatter -> basic -> unit

val dump_terminator : ?sep:string -> Format.formatter -> terminator -> unit

val make_instruction :
  desc:'a ->
  ?arg:Reg.t array ->
  ?res:Reg.t array ->
  ?dbg:Debuginfo.t ->
  ?fdo:Fdo_info.t ->
  ?live:Reg.Set.t ->
  stack_offset:int ->
  id:int ->
  ?irc_work_list:irc_work_list ->
  ?ls_order:int ->
  ?available_before:Reg_availability_set.t option ->
  ?available_across:Reg_availability_set.t option ->
  unit ->
  'a instruction
