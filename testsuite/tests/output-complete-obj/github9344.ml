(* TEST
<<<<<<< HEAD
 use_runtime = "false";
 setup-ocamlc.byte-build-env;
 flags = "-w -a -output-complete-exe -ccopt -I${ocamlsrcdir}/${runtime_dir}";
 program = "github9344";
 ocamlc.byte;
 program = "sh ${test_source_directory}/github9344.sh";
 run;
 check-program-output;
||||||| 121bedcfd2

use_runtime = "false"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-w -a -output-complete-exe -ccopt -I${ocamlsrcdir}/runtime"
program = "github9344"
*** run
program = "sh ${test_source_directory}/github9344.sh"
**** check-program-output
=======
 use_runtime = "false";
 setup-ocamlc.byte-build-env;
 flags = "-w -a -output-complete-exe -ccopt -I${ocamlsrcdir}/runtime";
 program = "github9344";
 ocamlc.byte;
 program = "sh ${test_source_directory}/github9344.sh";
 run;
 check-program-output;
>>>>>>> 5.2.0
*)

raise Not_found
