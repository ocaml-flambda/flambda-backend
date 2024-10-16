(* TEST
   flags += "-extension-universe alpha";
   expect;
   reference = "${test_source_directory}/overwriting_map.reference";
*)

(* CR uniqueness: To run this test replace 'expect' above by 'native'
   and delete the expect block. *)

let rec map f xs =
  match xs with
  | hd :: tl as xs -> overwrite_ xs with f hd :: map f tl
  | [] -> []

let () =
  let xs = [1;2;3] in

  let baseline_allocation = Gc.allocated_bytes() -. Gc.allocated_bytes() in
  let before = Gc.allocated_bytes () in
  let _ = Sys.opaque_identity (map (fun x -> x + 1) xs) in
  let after = Gc.allocated_bytes () in
  let bytes_per_word = Sys.word_size / 8 in
  let delta =
    int_of_float ((after -. before) -. baseline_allocation) / bytes_per_word
  in
  let msg =
    match delta with
    | 0 -> "No Allocation"
    | n -> "Allocated " ^ string_of_int n ^ " words"
  in
  Printf.printf "%15s: %s\n" "List.map" msg

[%%expect{|
Line 3, characters 22-57:
3 |   | hd :: tl as xs -> overwrite_ xs with f hd :: map f tl
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
