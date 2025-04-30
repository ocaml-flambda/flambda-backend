(* TEST
 readonly_files = "\
   bad_access_submodule_directly.ml bad_access_submodule_directly.reference \
   bad_use_util_wrongly.ml bad_use_util_wrongly.reference \
   basic.ml basic.mli basic__.ml \
   export_fancy_q_impl.ml export_fancy_q_impl.mli export_fancy_q_impl__.ml \
   fancy.ml fancy.mli flourish.ml flourish.mli ornament.ml ornament.mli fancy__.ml \
   main.ml main.mli main__.ml \
   main.cmo.objinfo.reference \
   main_basic.ml main_basic.mli main_basic__.ml \
   main_basic.cmo.objinfo.reference main_basic__.cmo.objinfo.reference \
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
   setup-ocamlc.byte-build-env;

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
   ocamlc.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
   ocamlc.byte;

   flags = "$flg_int_iface";
   module = "q/q__.ml";
   ocamlc.byte;

   flags = "$flg -as-parameter -I q -open Q__ -open No_direct_access_to_q";
   module = "q/q.mli";
   ocamlc.byte;

   set flg_basic = "\
     $flg -parameter P -I p -I basic \
     -open Basic__ -open No_direct_access_to_basic \
   ";

   flags = "$flg_int_iface";
   module = "basic/basic__.ml";
   ocamlc.byte;

   flags = "$flg_basic";
   module = "basic/basic.mli basic/basic.ml";
   ocamlc.byte;

   set flg_fancy = "\
     $flg -parameter P -parameter Q -I p -I q -I basic -I fancy -I util \
     -open Fancy__ -open No_direct_access_to_fancy \
   ";

   flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
   module = "fancy/fancy__.ml";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy__Flourish.cmi";
   module = "fancy/flourish.mli";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy__Flourish.cmo";
   module = "fancy/flourish.ml";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy__Ornament.cmi";
   module = "fancy/ornament.mli";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy__Ornament.cmo";
   module = "fancy/ornament.ml";
   ocamlc.byte;

   {
     flags = "$flg_fancy -o fancy/fancy__Bad_access_submodule_directly.cmo";
     module = "fancy/bad_access_submodule_directly.ml";
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_access_submodule_directly.output";
     ocamlc.byte;

     compiler_reference = "bad_access_submodule_directly.reference";
     check-ocamlc.byte-output;
   }

   (* Unindenting since we're really just resuming the main sequence of events and
      I'm not going to indent all that *)
 {

   flags = "$flg_fancy -o fancy/fancy.cmi -w -49";
   module = "fancy/fancy.mli";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy.cmo -w -49";
   module = "fancy/fancy.ml";
   ocamlc.byte;

   set flg_p_int = "\
     $flg -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlc.byte;

   flags = "$flg_p_int -as-argument-for P";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlc.byte;

   set flg_p_string = "\
     $flg -I p -I p_string \
     -open P_string__ -open No_direct_access_to_p_string \
   ";

   flags = "$flg_int_iface";
   module = "p_string/p_string__.ml";
   ocamlc.byte;

   flags = "$flg_p_string -as-argument-for P";
   module = "p_string/p_string.mli p_string/p_string.ml";
   ocamlc.byte;

   set flg_basic_p_int = "$flg_instance -I basic -I p -I p_int";

   flags = "$flg_basic_p_int -instantiate";
   module = "";
   program = "instances/basic-P_int.cmo";
   all_modules = "basic/basic.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_basic_p_string = "$flg_instance -I basic -I p -I p_string";

   flags = "$flg_basic_p_string -instantiate";
   module = "";
   program = "instances/basic-P_string.cmo";
   all_modules = "basic/basic.cmo p_string/p_string.cmo";
   ocamlc.byte;

   set flg_main_basic = "\
     $flg -I p_int -I p_string -I basic -I main_basic -I instances \
     -open Main_basic__ -open No_direct_access_to_main_basic \
   ";

   flags = "$flg_int_iface";
   module = "main_basic/main_basic__.ml";
   ocamlc.byte;

   {
     program = "-no-approx -no-code main_basic/main_basic__.cmi main_basic/main_basic__.cmo";
     output = "main_basic__.cmo.objinfo.output";
     ocamlobjinfo;

     reference = "main_basic__.cmo.objinfo.reference";
     check-program-output;
   }

 {
   flags = "$flg_main_basic";
   module = "main_basic/main_basic.mli main_basic/main_basic.ml";
   ocamlc.byte;

   {
     program = "-no-approx -no-code main_basic/main_basic.cmi main_basic/main_basic.cmo";
     output = "main_basic.cmo.objinfo.output";
     ocamlobjinfo;

     reference = "main_basic.cmo.objinfo.reference";
     check-program-output;
   }

 {
   set flg_q_impl = "\
     $flg -I q -I q_impl \
     -open Q_impl__ -open No_direct_access_to_q_impl \
   ";

   flags = "$flg_int_iface";
   module = "q_impl/q_impl__.ml";
   ocamlc.byte;

   flags = "$flg_q_impl -as-argument-for Q";
   module = "q_impl/q_impl.ml";
   ocamlc.byte;

   set flg_fancy_p_int =
     "$flg_instance -I basic -I fancy -I p -I p_int -I q -I q_impl";

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__-P_int-Q_impl";
   all_modules = "fancy/fancy__.cmo p_int/p_int.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_int-Q_impl";
   all_modules = "fancy/fancy__Flourish.cmo p_int/p_int.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_int-Q_impl";
   all_modules = "fancy/fancy__Ornament.cmo p_int/p_int.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy-P_int-Q_impl";
   all_modules = "fancy/fancy.cmo p_int/p_int.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   set flg_fancy_p_string =
     "$flg_instance -I fancy -I p -I p_string -I q -I q_impl";

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__-P_string-Q_impl";
   all_modules = "fancy/fancy__.cmo p_string/p_string.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_string-Q_impl";
   all_modules = "fancy/fancy__Flourish.cmo p_string/p_string.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_string-Q_impl";
   all_modules = "fancy/fancy__Ornament.cmo p_string/p_string.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy-P_string-Q_impl";
   all_modules = "fancy/fancy.cmo p_string/p_string.cmo q_impl/q_impl.cmo";
   ocamlc.byte;

   set flg_util = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy -I util \
     -open Util__ -open No_direct_access_to_util \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlc.byte;

   flags = "$flg_util";
   module = "util/util.mli util/util.ml";
   ocamlc.byte;

   {
     flags = "$flg -I p_int -I util";
     module = "bad_use_util_wrongly.ml";
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_use_util_wrongly.output";
     ocamlc.byte;

     compiler_reference = "bad_use_util_wrongly.reference";
     check-ocamlc.byte-output;
   }

 {
   set flg_util_p_int = "$flg_instance -I util -I p -I p_int";

   flags = "$flg_util_p_int -instantiate";
   module = "";
   program = "instances/util__-P_int.cmo";
   all_modules = "util/util__.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_util_p_int -instantiate";
   module = "";
   program = "instances/util-P_int.cmo";
   all_modules = "util/util.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_export_fancy_q_impl = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy \
     -I export_fancy_q_impl -I util \
     -open Export_fancy_q_impl__ -open No_direct_access_to_export_fancy_q_impl \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "export_fancy_q_impl/export_fancy_q_impl__.ml";
   ocamlc.byte;

   flags = "$flg_export_fancy_q_impl";
   module = "\
     export_fancy_q_impl/export_fancy_q_impl.mli \
     export_fancy_q_impl/export_fancy_q_impl.ml \
   ";
   ocamlc.byte;

   set flg_export_fancy_q_impl_p_int =
     "$flg_instance -I export_fancy_q_impl -I p_int";

   flags = "$flg_export_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/export_fancy_q_impl__-P_int.cmo";
   all_modules = "export_fancy_q_impl/export_fancy_q_impl__.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_export_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/export_fancy_q_impl-P_int.cmo";
   all_modules = "export_fancy_q_impl/export_fancy_q_impl.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_use_fancy_q_impl = "\
     $flg -parameter P -I p -I q_impl -I basic -I fancy \
     -I export_fancy_q_impl -I use_fancy_q_impl -I util \
     -open Use_fancy_q_impl__ -open No_direct_access_to_use_fancy_q_impl \
   ";

   flags = "$flg_int_iface -parameter P -I p";
   module = "use_fancy_q_impl/use_fancy_q_impl__.ml";
   ocamlc.byte;

   flags = "$flg_use_fancy_q_impl";
   module = "use_fancy_q_impl/use_fancy_q_impl.ml";
   ocamlc.byte;

   set flg_use_fancy_q_impl_p_int =
     "$flg_instance -I use_fancy_q_impl -I p_int";

   flags = "$flg_use_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/use_fancy_q_impl__-P_int.cmo";
   all_modules = "use_fancy_q_impl/use_fancy_q_impl__.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_use_fancy_q_impl_p_int -instantiate";
   module = "";
   program = "instances/use_fancy_q_impl-P_int.cmo";
   all_modules = "use_fancy_q_impl/use_fancy_q_impl.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_main = "\
     $flg -I p_int -I p_string -I q_impl -I fancy -I basic -I main \
     -H export_fancy_q_impl -I use_fancy_q_impl -I util -I instances \
     -open Main__ -open No_direct_access_to_main \
   ";

   flags = "$flg_int_iface";
   module = "main/main__.ml";
   ocamlc.byte;

   flags = "$flg_main";
   module = "main/main.mli main/main.ml";
   ocamlc.byte;

   check-ocamlc.byte-output;

   {
     program = "-no-approx -no-code main/main.cmi main/main.cmo";
     output = "main.cmo.objinfo.output";
     ocamlobjinfo;

     reference = "main.cmo.objinfo.reference";
     check-program-output;
   }

 {
   flags = "\
     $flg -H p -H p_int -H p_string -H q -H q_impl -H fancy -H basic \
     -I main -I main_basic \
   ";
   module = "test_byte.ml";
   ocamlc.byte;

   flags = "$flg";
   module = "";
   program = "$test_build_directory/test_byte.bc";
   all_modules = "\
     basic/basic.cmo \
     fancy/fancy__.cmo \
     fancy/fancy__Flourish.cmo \
     fancy/fancy__Ornament.cmo \
     fancy/fancy.cmo \
     p_int/p_int.cmo \
     p_string/p_string.cmo \
     instances/basic-P_int.cmo \
     instances/basic-P_string.cmo \
     main_basic/main_basic.cmo \
     util/util__.cmo \
     util/util.cmo \
     instances/util__-P_int.cmo \
     instances/util-P_int.cmo \
     instances/fancy__-P_int-Q_impl.cmo \
     instances/fancy__Flourish-P_int-Q_impl.cmo \
     instances/fancy__Ornament-P_int-Q_impl.cmo \
     instances/fancy-P_int-Q_impl.cmo \
     instances/fancy__-P_string-Q_impl.cmo \
     instances/fancy__Flourish-P_string-Q_impl.cmo \
     instances/fancy__Ornament-P_string-Q_impl.cmo \
     instances/fancy-P_string-Q_impl.cmo \
     export_fancy_q_impl/export_fancy_q_impl__.cmo \
     export_fancy_q_impl/export_fancy_q_impl.cmo \
     use_fancy_q_impl/use_fancy_q_impl__.cmo \
     use_fancy_q_impl/use_fancy_q_impl.cmo \
     instances/export_fancy_q_impl__-P_int.cmo \
     instances/export_fancy_q_impl-P_int.cmo \
     instances/use_fancy_q_impl__-P_int.cmo \
     instances/use_fancy_q_impl-P_int.cmo \
     main/main.cmo \
     test_byte.cmo \
   ";
   ocamlc.byte;

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
