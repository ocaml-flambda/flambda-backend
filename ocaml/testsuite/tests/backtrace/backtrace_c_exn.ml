(* TEST
   modules = "backtrace_c_exn_.c"
   flags = "-g"
   ocamlrunparam += ",b=1"
   * runtime4
    ** bytecode
      reference = "${test_source_directory}/backtrace_c_exn.byte4.reference"
    ** native
      reference = "${test_source_directory}/backtrace_c_exn.opt4.reference"
   * runtime5
      reference = "${test_source_directory}/backtrace_c_exn.byte.reference"
*)

(* CR mshinwell: it isn't clear to me why the 5 reference output here
   is not worse.  It seems to have lost the stack frames on the C side.
   (The reference file does match upstream 5.)
*)

(* https://github.com/ocaml-multicore/ocaml-multicore/issues/498 *)
external stubbed_raise : unit -> unit = "caml_498_raise"

let raise_exn () = (failwith [@inlined never]) "exn" [@@inline never]

let () = Callback.register "test_raise_exn" raise_exn

let () =
  try
    stubbed_raise ()
  with
  | exn ->
    Printexc.to_string exn |> print_endline;
    Printexc.print_backtrace stdout
