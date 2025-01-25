(* TEST
   flags = "-flambda2-advanced-meet";
   native;
*)

(* This test checks for an issue when unboxing closures under variants.
   It is only relevant at higher optimisation levels. *)
[@@@ocaml.flambda_o3]

let maybe_make_closure () =
  if Sys.opaque_identity true
  then None
  else (
    let zero = Sys.opaque_identity #0L in
    (* This closure will be unboxed, with a value slot of kind Naked_int64 *)
    Some (fun () -> zero))
;;

(* First unboxing: generate the extra args, including poison constants *)
let f () =
  match maybe_make_closure () with
  | None -> #0L
  | Some get -> get ()
;;

(* Resimplifying [f] triggers an error if the poison constants don't have
   the right kind *)
let g () = f ()
