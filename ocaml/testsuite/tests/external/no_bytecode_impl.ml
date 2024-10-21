(* TEST *)

external foo : unit -> unit = "caml_no_bytecode_impl"

(* All we really want to check is that this links without a missing primitive
   error on bytecode *)
let () =
  try
    foo ();
    assert false
  with (Failure _) -> ()
