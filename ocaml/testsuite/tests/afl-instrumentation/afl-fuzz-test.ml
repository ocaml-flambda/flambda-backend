<<<<<<< HEAD
(* TEST
   reason = "Broken, ask mshinwell if you want to try to fix it";
   skip;
||||||| 121bedcfd2
(* TEST (* Just a test-driver *)
   * native-compiler
   ** script
       script = "sh ${test_source_directory}/has-afl-fuzz.sh"
       readonly_files = "readline.ml"
   *** setup-ocamlopt.byte-build-env
   **** ocamlopt.byte
         program = "${test_build_directory}/readline"
         flags = "-afl-instrument"
         all_modules = "readline.ml"
   ***** run
=======
(* TEST
 native-compiler;
 script = "sh ${test_source_directory}/has-afl-fuzz.sh";
 readonly_files = "readline.ml";
 script;
 setup-ocamlopt.byte-build-env;
 program = "${test_build_directory}/readline";
 flags = "-afl-instrument";
 all_modules = "readline.ml";
 ocamlopt.byte;
 run;
>>>>>>> 5.2.0
*)
<<<<<<< HEAD
(* TEST
 native-compiler;
 script = "sh ${test_source_directory}/has-afl-fuzz.sh";
 readonly_files = "readline.ml";
 script;
 setup-ocamlopt.byte-build-env;
 program = "${test_build_directory}/readline";
 flags = "-afl-instrument";
 all_modules = "readline.ml";
 ocamlopt.byte;
 run;
*)

(* No code here, this file is a pure test script. *)
||||||| 121bedcfd2
=======

(* No code here, this file is a pure test script. *)
>>>>>>> 5.2.0
