(* TEST
 readonly_files = "\
   basic.ml basic.mli basic__.ml \
   fancy.ml fancy.mli flourish.ml flourish.mli ornament.ml ornament.mli fancy__.ml \
   main.ml main.mli main__.ml \
   main_basic.ml main_basic.mli main_basic__.ml \
   p.mli p__.ml \
   p_int.ml p_int.mli p_int__.ml \
   p_string.ml p_string.mli p_string__.ml \
   test_byte.reference \
 ";
 {
   setup-ocamlc.byte-build-env;

   script = "mkdir basic fancy main main_basic p p_int p_string instances";
   script;

   src = "basic.ml basic.mli basic__.ml";
   dst = "basic/";
   copy;

   src = "fancy.ml fancy.mli flourish.ml flourish.mli ornament.ml ornament.mli fancy__.ml";
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

   set flg_alias_deps = "-w -53";
   set flg = "$flg_alias_deps -no-alias-deps";
   set flg_int_iface = "$flg -w -49";
   set flg_instance = "-H instances -w -24";

   flags = "$flg_int_iface";
   module = "p/p__.ml";
   ocamlc.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
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

(* CR-soon lmaurer: Uncomment this (and remove the extra [open] lines in
   [fancy.ml] et al.) once PR #3489 is in.

   set flg_fancy = "\
     $flg -parameter P -I p -I basic -I fancy \
     -open Fancy__ -open No_direct_access_to_fancy \
   ";
*)

   (* CR-soon lmaurer: Delete this when PR #3489 is in. *)
   set flg_fancy = "\
     $flg -parameter P -I p -I basic -I fancy \
   ";

   flags = "${flg_int_iface} -I p -parameter P";
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

   flags = "$flg_fancy -o fancy/fancy.cmi";
   module = "fancy/fancy.mli";
   ocamlc.byte;

   flags = "$flg_fancy -o fancy/fancy.cmo";
   module = "fancy/fancy.ml";
   ocamlc.byte;

   set flg_p_int = "\
     $flg -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";

   flags = "${flg_int_iface}";
   module = "p_int/p_int__.ml";
   ocamlc.byte;

   flags = "$flg_p_int -as-argument-for P";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlc.byte;

   set flg_p_string = "\
     $flg -I p -I p_string \
     -open P_string__ -open No_direct_access_to_p_string \
   ";

   flags = "${flg_int_iface}";
   module = "p_string/p_string__.ml";
   ocamlc.byte;

   flags = "$flg_p_string -as-argument-for P";
   module = "p_string/p_string.mli p_string/p_string.ml";
   ocamlc.byte;

   set flg_basic_p_int = "$flg_basic $flg_instance -I p_int";

   flags = "$flg_basic_p_int -instantiate";
   module = "";
   program = "instances/basic-P_int.cmo";
   all_modules = "basic/basic.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_basic_p_string = "$flg_basic $flg_instance -I p_string";

   flags = "$flg_basic_p_string -instantiate";
   module = "";
   program = "instances/basic-P_string.cmo";
   all_modules = "basic/basic.cmo p_string/p_string.cmo";
   ocamlc.byte;

   set flg_main_basic = "\
     $flg -I p -I p_int -I p_string -I basic -I main_basic -I instances \
     -open Main_basic__ -open No_direct_access_to_main_basic \
   ";

   flags = "$flg_int_iface";
   module = "main_basic/main_basic__.ml";
   ocamlc.byte;

   flags = "$flg_main_basic";
   module = "main_basic/main_basic.mli main_basic/main_basic.ml";
   ocamlc.byte;

   set flg_fancy_p_int = "$flg_fancy $flg_instance -I p_int";

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__-P_int";
   all_modules = "fancy/fancy__.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_int";
   all_modules = "fancy/fancy__Flourish.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_int";
   all_modules = "fancy/fancy__Ornament.cmo p_int/p_int.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy-P_int";
   all_modules = "fancy/fancy.cmo p_int/p_int.cmo";
   ocamlc.byte;

   set flg_fancy_p_string = "$flg_fancy $flg_instance -I p_string";

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__-P_string";
   all_modules = "fancy/fancy__.cmo p_string/p_string.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_string";
   all_modules = "fancy/fancy__Flourish.cmo p_string/p_string.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_string";
   all_modules = "fancy/fancy__Ornament.cmo p_string/p_string.cmo";
   ocamlc.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy-P_string";
   all_modules = "fancy/fancy.cmo p_string/p_string.cmo";
   ocamlc.byte;

   set flg_main = "\
     $flg -I p -I p_int -I p_string -I fancy -I basic -I main -I instances \
     -open Main__ -open No_direct_access_to_main \
   ";

   flags = "$flg_int_iface";
   module = "main/main__.ml";
   ocamlc.byte;

   flags = "$flg_main";
   module = "main/main.mli main/main.ml";
   ocamlc.byte;

   check-ocamlc.byte-output;

   flags = "\
     $flg -H p -H p_int -H p_string -H fancy -H basic -H instances \
     -I main -I main_basic \
   ";
   module = "test_byte.ml";
   ocamlc.byte;

   flags = "$flg";
   module = "";
   program = "${test_build_directory}/test_byte.bc";
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
     instances/fancy__-P_int.cmo \
     instances/fancy__Flourish-P_int.cmo \
     instances/fancy__Ornament-P_int.cmo \
     instances/fancy-P_int.cmo \
     instances/fancy__-P_string.cmo \
     instances/fancy__Flourish-P_string.cmo \
     instances/fancy__Ornament-P_string.cmo \
     instances/fancy-P_string.cmo \
     main/main.cmo \
     test_byte.cmo \
   ";
   ocamlc.byte;

   (* Workaround for ocamltest bug: [script] breaks [run] unless you set
      [stdout] and [stderr] *)
   stdout = "test_byte.output";
   stderr = "test_byte.output";
   output = "test_byte.output";
   run;

   reference = "test_byte.reference";
   check-program-output;
 }
*)

let () =
  ignore (Main_basic.run () : _);
  ignore (Main.run () : _);
