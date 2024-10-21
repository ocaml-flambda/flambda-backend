(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

let[@inline never] bang () = raise Exit

let[@inline never] labelled_arguments_partial k =
  let[@inline never] f ~a = ignore a; k (); fun ~b -> ignore b; () in
  let partial =  Sys.opaque_identity (f ~b:1) in
  partial ~a:();
  42

let () =
  Printexc.record_backtrace true;
  match
    labelled_arguments_partial @@ fun _ ->
    bang ()
  with
  | _ -> assert false
  | exception Exit ->
     Printexc.print_backtrace stdout

(* TEST
 flags = "-g";
 {
   reference = "${test_source_directory}/names_partial_application.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/names_partial_application.opt.reference";
   native;
 }
*)
