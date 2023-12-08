(* TEST
   modules = "max_wosize_stub.c"
   * runtime5
     * native
*)

(* This test demonstrates that you can't make a block that's
   larger than what's supported by the header format. This test is
   very much tied to the current format of the header, so if we
   change that format substantially, we should delete the test or
   revamp it.
*)

external header_reserved_bits : unit -> int = "caml_header_reserved_bits"
let header_reserved_bits = header_reserved_bits ()

let () =
  let num_non_wosize_bits =
    8 (* tag bits *)
    + 2 (* GC color bits *)
    + 1 (* local scanning bit *)
    + header_reserved_bits
  in
  let max_allowed_size = (1 lsl (Sys.word_size - num_non_wosize_bits)) - 1 in
  try
    ignore (Array.make (max_allowed_size + 1) 0 : int array);
    failwith "should have raised"
  with _ -> ()
