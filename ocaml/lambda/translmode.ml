open Lambda
open Mode

let transl_locality_mode = function
  | Locality.Const.Global -> alloc_heap
  | Locality.Const.Local -> alloc_local

let transl_locality_mode_l locality =
  Locality.constrain_lower locality
  |> transl_locality_mode

let transl_locality_mode_r locality =
  (* r mode are for allocations; must already have been constrained_upper at
     this stage *)
  Locality.check_const locality
  |> Option.get
  |> transl_locality_mode

let transl_alloc_mode_l mode =
(* we only take the locality axis *)
  Alloc.locality mode
  |> transl_locality_mode_l

let transl_alloc_mode_r mode =
  (* we only take the locality axis *)
  Alloc.locality mode
  |> transl_locality_mode_r

let transl_modify_mode locality =
  match Locality.constrain_lower locality with
  | Global -> modify_heap
  | Local -> modify_maybe_stack