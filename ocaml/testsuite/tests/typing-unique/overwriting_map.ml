(* TEST
   flags += "-extension-universe alpha";
   expect;
   reference = "${test_source_directory}/overwriting_map.reference";
*)

let rec map f xs =
  match xs with
  | hd :: tl -> overwrite_ xs with f hd :: map f tl
  | [] -> []

let () =
  let xs = [1;2;3] in

  let prebefore = Gc.allocated_bytes () in
  let before = Gc.allocated_bytes () in
  let _ = Sys.opaque_identity (map (fun x -> x + 1) xs) in
  let after = Gc.allocated_bytes () in
  let delta =
    int_of_float ((after -. before) -. (before -. prebefore))
    / (Sys.word_size/8)
  in
  let msg =
    match delta with
    | 0 -> "No Allocation"
    | n -> "Allocation"
  in
  Printf.printf "%15s: %s\n" "List.map" msg

[%%expect{|
Line 3, characters 16-51:
3 |   | hd :: tl -> overwrite_ xs with f hd :: map f tl
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
