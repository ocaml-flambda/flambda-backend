open Lambda
open Mode

let transl_locality_mode locality =
  match Locality.constrain_lower locality with
  | Global -> alloc_heap
  | Local -> alloc_local

let transl_alloc_mode mode =
(* we only take the locality axis *)
  transl_locality_mode (Alloc.locality mode)

let transl_modify_mode locality =
  match Locality.constrain_lower locality with
  | Global -> modify_heap
  | Local -> modify_maybe_stack