(* Minimizer : delete the last file *)

open Utils

let rec rem_n_fst n l = if n = 0 then l else rem_n_fst (n - 1) (List.tl l)
let rem_n_last n l = List.rev (rem_n_fst n (List.rev l))

let split_last l =
  match List.rev l with [] -> assert false | h :: q -> (h, List.rev q)

let to_remove c (output_files, inputs) =
  let to_rem = ref [] in
  let min_of = ref output_files in
  while List.length !min_of >= 1 && raise_error (make_command c !min_of) do
    let hd, tl = split_last !min_of in
    to_rem := hd :: !to_rem;
    min_of := tl
  done;
  List.iter (fun s -> Stdlib.ignore (Sys.command ("rm " ^ s))) (List.tl !to_rem);
  let nb_to_rem = List.length !to_rem - 1 in
  (* Format.printf "nb_to_rem %n @." nb_to_rem; *)
  (rem_n_last nb_to_rem output_files, rem_n_last nb_to_rem inputs)
