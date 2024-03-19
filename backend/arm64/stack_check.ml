# 2 "backend/arm64/stack_check.ml"

[@@@ocaml.warning "+a-30-40-41-42"]

let stack_threshold_size = 0

let frame_size
  : stack_offset:int -> frame_required:bool -> num_stack_slots:int array ->  int
  = fun ~stack_offset:_ ~frame_required:_ ~num_stack_slots:_ ->
    Misc.fatal_error "stack checks are not supported on arm64"

let linear
  : Linear.fundecl -> Linear.fundecl
  = fun _fundecl ->
    Misc.fatal_error "stack checks are not supported on arm64"
