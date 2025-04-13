(* TEST (* DO NOT EDIT. Instead edit test_byte.ml and run gen-native.sh. *)
 readonly_files = "\
   bad_access_submodule_directly.ml bad_access_submodule_directly.reference \
   bad_use_util_wrongly.ml bad_use_util_wrongly.reference \
   basic.ml basic.mli basic__.ml \
   export_fancy_q_impl.ml export_fancy_q_impl.mli export_fancy_q_impl__.ml \
   fancy.ml fancy.mli flourish.ml flourish.mli ornament.ml ornament.mli fancy__.ml \
   main.ml main.mli main__.ml \
   main.cmx.objinfo.reference \
   main_basic.ml main_basic.mli main_basic__.ml \
   main_basic.cmx.objinfo.reference main_basic__.cmx.objinfo.reference \
   p.mli p__.ml \
   p_int.ml p_int.mli p_int__.ml \
   p_string.ml p_string.mli p_string__.ml \
   q.mli q__.ml \
   q_impl.ml q_impl__.ml \
   test.reference \
   use_fancy_q_impl.ml use_fancy_q_impl__.ml \
   util.ml util.mli util__.ml \
 ";
 {
   setup-ocamlopt.byte-build-env;

   (* Hide annoying optimization settings coming from CI *)
   set OCAMLPARAM = "";

   script = "\
     mkdir \
       basic export_fancy_q_impl fancy instances main main_basic p p_int p_string \
       q q_impl use_fancy_q_impl util \
   ";
   script;

   src = "basic.ml basic.mli basic__.ml";
   dst = "basic/";
   copy;

   src = "export_fancy_q_impl.ml export_fancy_q_impl.mli export_fancy_q_impl__.ml";
   dst = "export_fancy_q_impl/";
   copy;

   src = "\
     bad_access_submodule_directly.ml fancy.ml fancy.mli flourish.ml flourish.mli \
     ornament.ml ornament.mli fancy__.ml \
   ";
   dst = "fancy/";
   copy;

   src = "main.ml main.mli main__.ml";
   dst = "main/";
   copy;

   src = "main_basic.ml main_basic.mli main_basic__.ml";
   dst = "main_basic/";
   copy;

   src = "p.mli p__.ml";
   dst = "p/";
   copy;

   src = "p_int.ml p_int.mli p_int__.ml";
   dst = "p_int/";
   copy;

   src = "p_string.ml p_string.mli p_string__.ml";
   dst = "p_string/";
   copy;

   src = "q.mli q__.ml";
   dst = "q/";
   copy;

   src = "q_impl.ml q_impl__.ml";
   dst = "q_impl/";
   copy;

   src = "use_fancy_q_impl.ml use_fancy_q_impl__.ml";
   dst = "use_fancy_q_impl/";
   copy;

   src = "util.ml util.mli util__.ml";
   dst = "util/";
   copy;

   set flg_alias_deps = "-w -53";
   set flg = "$flg_alias_deps -no-alias-deps";
   set flg_int_iface = "$flg -w -49";
   set flg_instance = "-H instances -w -24 -w -58";

   (* Need to turn off the fallback inlining heuristic because it marks all
   instantiating functors as [@inline never] in classic mode *)
   ocamlopt_flags = "-no-flambda2-expert-fallback-inlining-heuristic";

   flags = "$flg_int_iface";
   module = "p/p__.ml";
   ocamlopt.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
   ocamlopt.byte;

   flags = "$flg_int_iface";
   module = "q/q__.ml";
   ocamlopt.byte;

   flags = "$flg -as-parameter -I q -open Q__ -open No_direct_access_to_q";
   module = "q/q.mli";
   ocamlopt.byte;

   set flg_basic = "\
     $flg -parameter P -I p -I basic \
     -open Basic__ -open No_direct_access_to_basic \
   ";

   flags = "$flg_int_iface";
   module = "basic/basic__.ml";
   ocamlopt.byte;

   flags = "$flg_basic";
   module = "basic/basic.mli basic/basic.ml";
   ocamlopt.byte;

   set flg_fancy = "\
     $flg -parameter P -parameter Q -I p -I q -I basic -I fancy -I util \
     -open Fancy__ -open No_direct_access_to_fancy \
   ";

   flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
   module = "fancy/fancy__.ml";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy__Flourish.cmi";
   module = "fancy/flourish.mli";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy__Flourish.cmx";
   module = "fancy/flourish.ml";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy__Ornament.cmi";
   module = "fancy/ornament.mli";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy__Ornament.cmx";
   module = "fancy/ornament.ml";
   ocamlopt.byte;

   {
     flags = "$flg_fancy -o fancy/fancy__Bad_access_submodule_directly.cmx";
     module = "fancy/bad_access_submodule_directly.ml";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_access_submodule_directly.output";
     ocamlopt.byte;

     compiler_reference = "bad_access_submodule_directly.reference";
     check-ocamlopt.byte-output;
   }

   (* Unindenting since we're really just resuming the main sequence of events and
      I'm not going to indent all that *)
 {

   flags = "$flg_fancy -o fancy/fancy.cmi -w -49";
   module = "fancy/fancy.mli";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy.cmx -w -49";
   module = "fancy/fancy.ml";
   ocamlopt.byte;

   set flg_p_int = "\
     $flg -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlopt.byte;

   flags = "$flg_p_int -as-argument-for P";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlopt.byte;

   set flg_p_string = "\
     $flg -I p -I p_string \
     -open P_string__ -open No_direct_access_to_p_string \
   ";

   flags = "$flg_int_iface";
   module = "p_string/p_string__.ml";
   ocamlopt.byte;

   flags = "$flg_p_string -as-argument-for P";
   module = "p_string/p_string.mli p_string/p_string.ml";
   ocamlopt.byte;

   set flg_basic_p_int = "$flg_instance -I basic -I p -I p_int";

   flags = "$flg_basic_p_int -instantiate";
   module = "";
   program = "instances/basic-P_int.cmx";
   all_modules = "basic/basic.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_basic_p_string = "$flg_instance -I basic -I p -I p_string";

   flags = "$flg_basic_p_string -instantiate";
   module = "";
   program = "instances/basic-P_string.cmx";
   all_modules = "basic/basic.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   set flg_main_basic = "\
     $flg -I p_int -I p_string -I basic -I main_basic -I instances \
     -open Main_basic__ -open No_direct_access_to_main_basic \
   ";

   flags = "$flg_int_iface";
   module = "main_basic/main_basic__.ml";
   ocamlopt.byte;

   {
     program = "-no-approx -no-code main_basic/main_basic__.cmi main_basic/main_basic__.cmx";
     output = "main_basic__.cmx.objinfo.output";
     ocamlobjinfo;

     reference = "main_basic__.cmx.objinfo.reference";
     check-program-output;
   }

 {
   flags = "$flg_main_basic";
   module = "main_basic/main_basic.mli main_basic/main_basic.ml";
   ocamlopt.byte;

   {
     program = "-no-approx -no-code main_basic/main_basic.cmi main_basic/main_basic.cmx";
     output = "main_basic.cmx.objinfo.output";
     ocamlobjinfo;

     reference = "main_basic.cmx.objinfo.reference";
     check-program-output;
   }

 {
   set flg_q_impl = "\
     $flg -I q -I q_impl \
     -open Q_impl__ -open No_direct_access_to_q_impl \
   ";

   flags = "$flg_int_iface";
   module = "q_impl/q_impl__.ml";
   ocamlopt.byte;

   flags = "$flg_q_impl -as-argument-for Q";
   module = "q_impl/q_impl.ml";
   ocamlopt.byte;

   set flg_fancy_p_int =
     "$flg_instance -I basic -I fancy -I p -I p_int -I q -I q_impl";

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__-P_int-Q_impl";
   all_modules = "fancy/fancy__.cmx p_int/p_int.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_int-Q_impl";
   all_modules = "fancy/fancy__Flourish.cmx p_int/p_int.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_int-Q_impl";
   all_modules = "fancy/fancy__Ornament.cmx p_int/p_int.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy-P_int-Q_impl";
   all_modules = "fancy/fancy.cmx p_int/p_int.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   set flg_fancy_p_string =
     "$flg_instance -I fancy -I p -I p_string -I q -I q_impl";

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__-P_string-Q_impl";
   all_modules = "fancy/fancy__.cmx p_string/p_string.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_string-Q_impl";
   all_modules = "fancy/fancy__Flourish.cmx p_string/p_string.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_string-Q_impl";
   all_modules = "fancy/fancy__Ornament.cmx p_string/p_string.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy-P_string-Q_impl";
   all_modules = "fancy/fancy.cmx p_string/p_string.cmx q_impl/q_impl.cmx";
   ocamlopt.byte;

   set flg_util = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy -I util \
     -open Util__ -open No_direct_access_to_util \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlopt.byte;

   flags = "$flg_util";
   module = "util/util.mli util/util.ml";
   ocamlopt.byte;

   {
     flags = "$flg -I p_int -I util";
     module = "bad_use_util_wrongly.ml";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_use_util_wrongly.output";
     ocamlopt.byte;

     compiler_reference = "bad_use_util_wrongly.reference";
     check-ocamlopt.byte-output;
   }

 {
   set flg_util_p_int = "$flg_instance -I util -I p -I p_int";

   flags = "$flg_util_p_int -instantiate";
   module = "";
   program = "instances/util__-P_int.cmx";
   all_modules = "util/util__.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_util_p_int -instantiate";
   module = "";
   program = "instances/util-P_int.cmx";
   all_modules = "util/util.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_export_fancy_q_impl = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy \
     -I export_fancy_q_impl -I util \
     -open Export_fancy_q_impl__ -open No_direct_access_to_export_fancy_q_impl \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "export_fancy_q_impl/export_fancy_q_impl__.ml";
   ocamlopt.byte;

   flags = "$flg_export_fancy_q_impl";
   module = "\
     export_fancy_q_impl/export_fancy_q_impl.mli \
     export_fancy_q_impl/export_fancy_q_impl.ml \
   ";
   ocamlopt.byte;

   set flg_export_fancy_q_impl_p_int =
     "$flg_instance -I export_fancy_q_impl -I p_int";

   flags = "$flg_export_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/export_fancy_q_impl__-P_int.cmx";
   all_modules = "export_fancy_q_impl/export_fancy_q_impl__.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_export_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/export_fancy_q_impl-P_int.cmx";
   all_modules = "export_fancy_q_impl/export_fancy_q_impl.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_use_fancy_q_impl = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy \
     -I export_fancy_q_impl -I use_fancy_q_impl -I util \
     -open Use_fancy_q_impl__ -open No_direct_access_to_use_fancy_q_impl \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "use_fancy_q_impl/use_fancy_q_impl__.ml";
   ocamlopt.byte;

   flags = "$flg_use_fancy_q_impl";
   module = "use_fancy_q_impl/use_fancy_q_impl.ml";
   ocamlopt.byte;

   set flg_use_fancy_q_impl_p_int =
     "$flg_instance -I use_fancy_q_impl -I p_int";

   flags = "$flg_use_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/use_fancy_q_impl__-P_int.cmx";
   all_modules = "use_fancy_q_impl/use_fancy_q_impl__.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_use_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/use_fancy_q_impl-P_int.cmx";
   all_modules = "use_fancy_q_impl/use_fancy_q_impl.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_main = "\
     $flg -I p_int -I p_string -I q_impl -I fancy -I basic -I main \
     -H export_fancy_q_impl -I use_fancy_q_impl -I util -I instances \
     -open Main__ -open No_direct_access_to_main \
   ";

   flags = "$flg_int_iface";
   module = "main/main__.ml";
   ocamlopt.byte;

   flags = "$flg_main";
   module = "main/main.mli main/main.ml";
   ocamlopt.byte;

   check-ocamlopt.byte-output;

   {
     program = "-no-approx -no-code main/main.cmi main/main.cmx";
     output = "main.cmx.objinfo.output";
     ocamlobjinfo;

     reference = "main.cmx.objinfo.reference";
     check-program-output;
   }

 {
   flags = "\
     $flg -H p -H p_int -H p_string -H q -H q_impl -H fancy -H basic \
     -I main -I main_basic \
   ";
   module = "test_native.ml";
   ocamlopt.byte;

   flags = "$flg";
   module = "";
   program = "$test_build_directory/test_native.exe";
   all_modules = "\
     basic/basic.cmx \
     fancy/fancy__.cmx \
     fancy/fancy__Flourish.cmx \
     fancy/fancy__Ornament.cmx \
     fancy/fancy.cmx \
     p_int/p_int.cmx \
     p_string/p_string.cmx \
     instances/basic-P_int.cmx \
     instances/basic-P_string.cmx \
     main_basic/main_basic.cmx \
     util/util__.cmx \
     util/util.cmx \
     instances/util__-P_int.cmx \
     instances/util-P_int.cmx \
     instances/fancy__-P_int-Q_impl.cmx \
     instances/fancy__Flourish-P_int-Q_impl.cmx \
     instances/fancy__Ornament-P_int-Q_impl.cmx \
     instances/fancy-P_int-Q_impl.cmx \
     instances/fancy__-P_string-Q_impl.cmx \
     instances/fancy__Flourish-P_string-Q_impl.cmx \
     instances/fancy__Ornament-P_string-Q_impl.cmx \
     instances/fancy-P_string-Q_impl.cmx \
     export_fancy_q_impl/export_fancy_q_impl__.cmx \
     export_fancy_q_impl/export_fancy_q_impl.cmx \
     use_fancy_q_impl/use_fancy_q_impl__.cmx \
     use_fancy_q_impl/use_fancy_q_impl.cmx \
     instances/export_fancy_q_impl__-P_int.cmx \
     instances/export_fancy_q_impl-P_int.cmx \
     instances/use_fancy_q_impl__-P_int.cmx \
     instances/use_fancy_q_impl-P_int.cmx \
     main/main.cmx \
     test_native.cmx \
   ";
   ocamlopt.byte;

   (* Workaround for ocamltest bug: [script] breaks [run] unless you set
      [stdout] and [stderr] *)
   stdout = "test.output";
   stderr = "test.output";
   output = "test.output";
   run;

   reference = "test.reference";
   check-program-output;
 }}}}}}
*)

let () =
  ignore (Main_basic.run () : _);
  ignore (Main.run () : _);
