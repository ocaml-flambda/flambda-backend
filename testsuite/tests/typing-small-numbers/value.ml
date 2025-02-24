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

type t = float32;;

let _ = 1.0s;;
let _ = 1.s;;
let _ = 1e10s;;
let _ = 1e+1s;;
let _ = 1e-1s;;
let _ = 0x111.000s;;
let _ = 0x1.4p+0s;;
let _ = 0xf.ffffffffffff8p+1020s;;
let _ = 0x8p-972s;;
let _ = 0xc.d5e6fp+1_24s;;

let () =
  match 0.0s with
  | _ -> ()
;;

type t = int8;;

let _ = 1y;;
let _ = -1y;;
let _ = 0xfffy;;
let _ = -0o7y;;
let _ = -0b1010y;;
let () =
  match -0xfy with
  | -0xey -> ()
  | 1y -> ()
  | 2y..12y -> ()
  | _ -> ()

type t = int16;;

let _ = 1w;;
let _ = -#1w;;
let _ = 0xfffw;;
let _ = -0o7w;;
let _ = -0b1010w;;
let () =
  match -0xfw with
  | 1w..2w -> ()
  | _ -> ()

let () =
  match -0xfw with
  | -0xew -> ()
  | 1w -> ()
  | _ -> ()
