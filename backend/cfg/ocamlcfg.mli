(** External interface to the ocamlcfg library. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Label : sig
  type t = int
end

module Cfg : sig
  include module type of struct
    include Cfg_intf.S
  end

  module Basic_block : sig
    (** The implementation of type [t] is a mutable structure. *)
    type t

    val start : t -> Label.t

    val body : t -> basic instruction list

    val terminator : t -> terminator instruction
  end

  (** The implementation of type [t] is a mutable structure. *)
  type t

  val iter_blocks : t -> f:(Label.t -> Basic_block.t -> unit) -> unit

  val get_block : t -> Label.t -> Basic_block.t option

  (** [successor_labels] only returns non-exceptional edges. We need to pass
      [t] because the successor label of terminator (Tailcall Self) is
      recorded in [t], and not in the basic_block. *)
  val successor_labels : t -> Basic_block.t -> Label.t list

  val predecessor_labels : Basic_block.t -> Label.t list

  val fun_name : t -> string

  val entry_label : t -> Label.t

  val fun_tailrec_entry_point_label : t -> Label.t
end

module Cfg_with_layout : sig
  type t

  val cfg : t -> Cfg.t

  val layout : t -> Label.t list

  val set_layout : t -> Label.t list -> unit

  val save_as_dot :
    t ->
    ?show_instr:bool ->
    ?show_exn:bool ->
    ?annotate_block:(Label.t -> string) ->
    ?annotate_succ:(Label.t -> Label.t -> string) ->
    string ->
    unit

  val print : t -> out_channel -> string -> unit

  val preserve_orig_labels : t -> bool

  (** eliminate_* can call simplify_terminators *)
  val eliminate_dead_blocks : t -> unit

  (** eliminate fallthrough implies dead block elimination *)
  val eliminate_fallthrough_blocks : t -> unit

  val of_linear : Linear.fundecl -> preserve_orig_labels:bool -> t

  val to_linear : t -> Linear.instruction
end

module Passes : sig
  val add_extra_debug : Cfg.t -> file:string -> unit

  val simplify_terminators : Cfg.t -> unit
end

module Util : sig
  val verbose : bool ref

  val print_assembly : Cfg.Basic_block.t list -> unit
end
