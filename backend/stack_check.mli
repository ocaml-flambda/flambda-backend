[@@@ocaml.warning "+a-30-40-41-42"]

val stack_threshold_size : int

val frame_size : stack_offset:int -> frame_required:bool -> num_stack_slots:int array ->  int

val linear : Linear.fundecl -> Linear.fundecl

val cfg : Cfg_with_layout.t -> Cfg_with_layout.t

