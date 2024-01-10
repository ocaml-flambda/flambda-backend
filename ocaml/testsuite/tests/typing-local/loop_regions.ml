(* TEST
   * stack-allocation
   ** native
      reference = "${test_source_directory}/loop_regions.stack.reference"
   * no-stack-allocation
   ** native
      reference = "${test_source_directory}/loop_regions.heap.reference"
 *)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
let local_stack_offset () = local_stack_offset () / (Sys.word_size / 8)

let print_offsets (name,allocs) =
  let p xs = String.concat "; " (List.map Int.to_string xs) in
  Printf.printf "%25s: [%s]\n%!"
    name (p allocs)

(* local_ for allocates in parent region *)
let loc_for () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for i = 0 to 0 do [%exclave] (
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ()
  )
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Non local loop allocates in its own region *)
let nonloc_for () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for i = 0 to 0 do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Local while body should allocate in parent *)
let loc_while_body () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  let cond = ref true in
  while !cond do [%exclave] (
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ();
    cond := false
  )
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
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ();
    cond := false
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Local while condition should allocate in parent *)
let[@inline never] loc_while_cond () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  while [%exclave] (
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ();
    Sys.opaque_identity false
  )
  do
    ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

(* Nonlocal while condition should allocate in its own region *)
let[@inline never] nonloc_while_cond () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  while
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    offset_loop := local_stack_offset ();
    Sys.opaque_identity false
  do
    ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let[@inline never] loc_func () =
  let offset_func = ref (-1) in
  let fun_exclave r =
    [%exclave] (
        let z = local_ (Some (Sys.opaque_identity 42)) in
        let _ = (Sys.opaque_identity z) in
        r := local_stack_offset ()
    ) in
  let offset1 = local_stack_offset () in
  fun_exclave offset_func;
  let offset2 = local_stack_offset () in
  [offset1; !offset_func; offset2]


let[@inline never] nonloc_func () =
  let offset_func = ref (-1) in
  let fun_nonexclave r =
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = (Sys.opaque_identity z) in
    r := local_stack_offset ()
  in
  let offset1 = local_stack_offset () in
  fun_nonexclave offset_func;
  let offset2 = local_stack_offset () in
  [offset1; !offset_func; offset2]

let () =
  List.iter print_offsets [
    "local for",           loc_for ();
    "non-local for",       nonloc_for ();
    "local while body",    loc_while_body ();
    "nonlocal while body", nonloc_while_body ();
    "local while cond",    loc_while_cond ();
    "nonlocal while cond", nonloc_while_cond ();
    "local func",          loc_func ();
    "nonlocal func",       nonloc_func ();
  ]
