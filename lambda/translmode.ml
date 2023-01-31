open Types
open Lambda
let transl_alloc_mode alloc_mode =
  match Alloc_mode.constrain_lower alloc_mode with
  | Global -> alloc_heap
  | Local -> alloc_local

let transl_modify_mode alloc_mode =
  match Alloc_mode.constrain_lower alloc_mode with
  | Global -> modify_heap
  | Local -> modify_maybe_stack