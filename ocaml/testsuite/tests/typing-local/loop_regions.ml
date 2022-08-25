(* TEST
   * stack-allocation
   ** native
      reference = "${test_source_directory}/loop_regions.stack.reference"
   * no-stack-allocation
   ** native
      reference = "${test_source_directory}/loop_regions.heap.reference"
 *)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
external opaque_local : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let print_offsets (name,allocs) =
  let p xs = String.concat "; " (List.map Int.to_string xs) in
  Printf.printf "%25s: [%s]\n%!"
    name (p allocs)

(* local_ for allocates in parent region *)
let loc_for () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for i = 0 to 0 do local_
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Non local loop allocates in its own region *)
let nonloc_for () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for i = 0 to 0 do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Local while body should allocate in parent *)
let loc_while_body () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  let cond = ref true in
  while !cond do local_
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ();
    cond := false
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Nonlocal while body should allocate in its own region*)
let nonloc_while_body () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  let cond = ref true in
  while !cond do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ();
    cond := false
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Local while condition should allocate in parent *)
let loc_while_cond () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  while local_
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ();
    false
  do
    ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Nonlocal while condition should allocate in its own region *)
let nonloc_while_cond () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  while
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (opaque_local z) in
    offset_loop := local_stack_offset ();
    false
  do
    ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let () =
  List.iter print_offsets [
    "local for",           loc_for ();
    "non-local for",       nonloc_for ();
    "local while body",    loc_while_body ();
    "nonlocal while body", nonloc_while_body ();
    "local while cond",    loc_while_cond ();
    "nonlocal while cond", nonloc_while_cond ()
  ]
