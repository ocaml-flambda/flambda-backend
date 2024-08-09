(* TEST_BELOW
(* Blank lines added here to preserve locations. *)






*)

let rec f x =
  if not (x = 0 || x = 10000 || x = 20000)
  then 1 + f (x + 1)
  else
    try
      Sys.with_async_exns (fun () -> 1 + f (x + 1))
    with Stack_overflow ->
      print_string "x = "; print_int x; print_newline();
      raise Stack_overflow

let _ =
 let p = Sys.opaque_identity (ref 42) in
 begin
  try
    Sys.with_async_exns (fun () -> ignore(f 0))
  with Stack_overflow ->
    print_string "Stack overflow caught"; print_newline()
 end ;
 for i = 1 to 1000 do ignore (Sys.opaque_identity (ref 1_000_000)) done;
 (* GPR#1289 *)
 Printexc.record_backtrace true;
 begin
  try
    Sys.with_async_exns (fun () -> ignore(f 0))
  with Stack_overflow ->
    print_string "second Stack overflow caught";
    print_newline();
    (* Try to make the backtrace reasonably stable.
       Note that Closure produces an empty backtrace here. *)
    let backtrace =
      Printexc.get_backtrace ()
      |> String.split_on_char '\n'
      |> List.filter (fun s -> String.length s > 0)
      |> List.rev
      |> List.hd
    in
    print_endline backtrace
 end;
 print_string "!p = "; print_int !p; print_newline ()

(* TEST
 flags = "-w -a";
 ocamlrunparam += "l=100000";
 no-tsan; (* TSan does not support call stacks bigger than 64k frames *)
 flambda;
 {
   reference = "${test_source_directory}/stackoverflow.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/stackoverflow.opt.reference";
   native;
 }
*)
