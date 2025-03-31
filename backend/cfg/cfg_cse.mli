[@@@ocaml.warning "+a-30-40-41-42"]

(* Classification of operations *)

type op_class =
  | Op_pure (* pure arithmetic, produce one or several result *)
  | Op_load of Simple_operation.mutable_flag (* memory load *)
  | Op_store of bool (* memory store, false = init, true = assign *)
  | Op_other (* anything else that does not allocate nor store in memory *)

(* We maintain sets of equations of the form valnums = operation(valnums) plus a
   mapping from registers to valnums (value numbers). *)

module type Operation = sig
  type t
end

(* Common subexpression elimination by value numbering over extended basic
   blocks. *)

class cse_generic :
  object
    (* The following methods can be overridden to handle processor-specific
       operations. *)
    method class_of_operation : Operation.t -> op_class

    method is_cheap_operation : Operation.t -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

    (* The following method is the entry point and should not be overridden *)
    method cfg_with_layout : Cfg_with_layout.t -> Cfg_with_layout.t
  end
