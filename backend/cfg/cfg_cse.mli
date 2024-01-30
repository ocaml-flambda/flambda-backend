[@@@ocaml.warning "+a-30-40-41-42"]

open CSE_utils

class cse_generic :
  object
    method class_of_operation : Cfg.operation -> op_class

    method is_cheap_operation : Cfg.operation -> bool

    method cfg_with_layout : Cfg_with_layout.t -> Cfg_with_layout.t
  end
