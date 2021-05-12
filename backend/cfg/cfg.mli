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
    trap_depth : int;
        (** Trap depth of the start of the block. Used for cross checking the
            construction of [exns], and for emitting adjust trap on edges
            from one block to the next. *)
    mutable exns : Label.Set.t;
        (** All possible handlers of a raise that (1) can be triggered either
            by an explicit raise, or instructions such as calls and
            allocations, that appear(s) in this block; and (2) are within the
            same function. [exns] is a subset of the trap handler block
            labels of the cfg. *)
    mutable can_raise : bool;
        (** Does this block contain any instruction that can raise, such as a
            call, bounds check, allocation, or an explicit [raise]? *)
    mutable can_raise_interproc : bool;
        (** This block raises an exn that is not handled in this function,
            [can_raise_interproc] implies [can_raise] but not necessarily
            vice versa. *)
    mutable is_trap_handler : bool;
        (** Is this block a trap handler (i.e. is it an exn successor of
            another block) or not? *)
    mutable dead : bool
        (** This block must be unreachable from function entry. This field is
            set during cfg construction (if trap stacks are unresolved) and
            used during dead block elimination for checking. *)
        (* CR-someday gyorsh: The current implementation allows multiple
           pushtraps in each block means that different trap stacks are
           associated with the block at different points, and a raise from
           this block can go to different handlers, depending on which one is
           on the top of the stack. After we split the blocks based on
           Pushtrap/Poptrap, each block will have a unique trap stack
           associated with it. [exns] will not be needed, as the
           exn-successor will be uniquely determined by can_raise + top of
           trap stack. [trap_depth] will not needed, as it can be derived
           with the length of the trap stack. *)
  }

(** Control Flow Graph of a function. *)
type t = private
  { blocks : basic_block Label.Tbl.t;  (** Map from labels to blocks *)
    fun_name : string;  (** Function name, used for printing messages *)
    entry_label : Label.t;
        (** This label must be the first in all layouts of this cfg. *)
    mutable fun_tailrec_entry_point_label : Label.t
        (** When a [Prologue] is absent, this is the same as [entry_label].
            Otherwise, the [Prologue] falls through to this label. *)
  }

val create : fun_name:string -> fun_tailrec_entry_point_label:Label.t -> t

val fun_name : t -> string

val entry_label : t -> Label.t

val fun_tailrec_entry_point_label : t -> Label.t

val predecessor_labels : basic_block -> Label.t list

val successor_labels :
  t -> normal:bool -> exn:bool -> basic_block -> Label.Set.t
(** [exn] does not account for exceptional flow from the block that goes
    outside of the function. *)

val replace_successor_labels :
  t ->
  normal:bool ->
  exn:bool ->
  basic_block ->
  f:(Label.t -> Label.t) ->
  unit

val mem_block : t -> Label.t -> bool

val remove_block_exn : t -> Label.t -> unit

val get_block : t -> Label.t -> basic_block option

val get_block_exn : t -> Label.t -> basic_block

val set_fun_tailrec_entry_point_label : t -> Label.t -> unit

val iter_blocks : t -> f:(Label.t -> basic_block -> unit) -> unit

(** Printing *)

val print_terminator :
  out_channel -> ?sep:string -> terminator instruction -> unit

val print_basic : out_channel -> basic instruction -> unit

(* CR-someday gyorsh: Current version of cfg is a half-way house in terms of
   its exception handling. It has a lot of redundancy and the result of the
   computation is not used.

   Redundancy: linear_to_cfg reconstructs intraprocedural exception handling
   stacks from linear IR and annotates each block with this information.
   However, CFG instructions still include the original push/poptraps from
   Linear.

   To remove these push/poptraps from CFG IR, we need to split blocks at
   every push/poptrap. Then, we can annotate the blocks with the top of the
   trap stack, instead of carrying the copy of the stack. *)

(* CR-someday gyorsh: store label after separately and update after
   reordering. *)
