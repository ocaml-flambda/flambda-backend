[@@@ocaml.warning "+a-30-40-41-42"]

(* Common subexpression elimination by value numbering over extended basic
   blocks. *)

open CSE_utils

class cse_generic :
  object
    (* The following methods can be overridden to handle processor-specific
       operations. *)
    method class_of_operation : Cfg.operation -> op_class

    method is_cheap_operation : Cfg.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

    (* The following method is the entry point and should not be overridden *)
    method cfg_with_layout : Cfg_with_layout.t -> Cfg_with_layout.t
  end
