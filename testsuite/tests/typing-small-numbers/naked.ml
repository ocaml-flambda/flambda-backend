(* TEST
   {
   flags = "-extension small_numbers";
   compiler_reference = "${test_source_directory}/${test_file}.enabled.reference";
   compiler_output = "${test_source_directory}/${test_file}.enabled.output";
   toplevel;
   }
   {
   flags = "-extension small_numbers_beta";
   compiler_reference = "${test_source_directory}/${test_file}.beta.reference";
   compiler_output = "${test_source_directory}/${test_file}.beta.output";
   toplevel;
   }
   {
   flags = "-extension-universe upstream_compatible";
   compiler_reference = "${test_source_directory}/${test_file}.upstream_compatible.reference";
   compiler_output = "${test_source_directory}/${test_file}.upstream_compatible.output";
   toplevel;
   }
   {
   flags = "-no-extension small_numbers";
   compiler_reference = "${test_source_directory}/${test_file}.disabled.reference";
   compiler_output = "${test_source_directory}/${test_file}.disabled.output";
   toplevel;
   }
*)

type t = float32#;;

external ignore : t -> unit = "%ignore"
let () = ignore #1.0s;;
let () = ignore #1.s;;
let () = ignore #1e10s;;
let () = ignore #1e+1s;;
let () = ignore #1e-1s;;
let () = ignore #0x111.000s;;
let () = ignore #0x1.4p+0s;;
let () = ignore #0xf.ffffffffffff8p+1020s;;
let () = ignore #0x8p-972s;;
let () = ignore #0xc.d5e6fp+1_24s;;
let () =
  match #0.0s with
  | _ -> ()
;;

type t = char#;;

external ignore : t -> unit = "%ignore"
let () = ignore #'a';;
let () = ignore #'\000';;
let () = ignore #'\x80';;
let () = ignore (-#'\x80');;
let () =
  match #'\x80' with
| #'a'..#'z' -> ()
| #'\x00' -> ()
| _ -> ()

type t = int8#;;

external ignore : t -> unit = "%ignore"
let () = ignore #1y;;
let () = ignore (-#1y);;
let () = ignore #0xfffy;;
let () = ignore (-#0o7y);;
let () = ignore (-#0b1010y);;
let () =
  match -#0xfy with
  | -#0xey -> ()
  | #1y -> ()
  | #2y..#12y -> ()
  | _ -> ()

type t = int16#;;

external ignore : t -> unit = "%ignore"
let () = ignore #1w;;
let () = ignore (-#1w);;
let () = ignore #0xfffw;;
let () = ignore (-#0o7w);;
let () = ignore (-#0b1010w);;
let () =
  match -#0xfw with
| -#0xew -> ()
| #1w -> ()
| #2w..#12w -> ()
| _ -> ()

type t = int#;;

external ignore : t -> unit = "%ignore"
let () = ignore #1i;;
let () = ignore (-#1i);;
let () = ignore #0xfffi;;
let () = ignore (-#0o7i);;
let () = ignore (-#0b1010i);;
let () =
  match -#0xfi with
| -#0xei -> ()
| #1i -> ()
| _ -> ()
