(* TEST
 stack-allocation;
 native;
*)

(* PR902 (return mode on second application expression in a split
   overapplication) *)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
external is_stack : local_ 'a -> bool = "caml_obj_is_stack"

let f2 p () = p

let f1 () x : (unit -> local_ (int * int)) = exclave_
  (* This local allocation should end up in the caller's region, because
     we should have got here via one of the caml_applyL functions.  If the
     return mode of the second application in the expansion of the
     overapplication below is wrongly Heap, then caml_apply will be used
     instead, which will open its own region for this allocation. *)
  let p = local_ (x, x) in
  ((opaque_identity f2) p) [@nontail]

let[@inline never] to_be_overapplied () () = Sys.opaque_identity f1

let () =
  let start_offset = local_stack_offset () in
  let p = to_be_overapplied () () () 42 () in
  let end_offset = local_stack_offset () in
  assert (is_stack p);
  let ok = end_offset - start_offset = Sys.word_size (* eight words *) in
  Printf.printf "PR902: %s\n" (if ok then "ok" else "FAIL")
