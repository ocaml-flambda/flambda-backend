(* TEST (* DO NOT EDIT. Instead edit test_byte.ml and run gen-native.sh. *)
 readonly_files = "\
   basic.ml basic.mli basic__.ml \
   fancy.ml fancy.mli flourish.ml flourish.mli ornament.ml ornament.mli fancy__.ml \
   main.ml main.mli main__.ml \
   main_basic.ml main_basic.mli main_basic__.ml \
   p.mli p__.ml \
   p_int.ml p_int.mli p_int__.ml \
   p_string.ml p_string.mli p_string__.ml \
   test_native.reference \
 ";
 {
   setup-ocamlopt.byte-build-env;

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
   ocamlopt.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
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

   flags = "$flg_fancy -o fancy/fancy.cmi";
   module = "fancy/fancy.mli";
   ocamlopt.byte;

   flags = "$flg_fancy -o fancy/fancy.cmx";
   module = "fancy/fancy.ml";
   ocamlopt.byte;

   set flg_p_int = "\
     $flg -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";

   flags = "${flg_int_iface}";
   module = "p_int/p_int__.ml";
   ocamlopt.byte;

   flags = "$flg_p_int -as-argument-for P";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlopt.byte;

   set flg_p_string = "\
     $flg -I p -I p_string \
     -open P_string__ -open No_direct_access_to_p_string \
   ";

   flags = "${flg_int_iface}";
   module = "p_string/p_string__.ml";
   ocamlopt.byte;

   flags = "$flg_p_string -as-argument-for P";
   module = "p_string/p_string.mli p_string/p_string.ml";
   ocamlopt.byte;

   set flg_basic_p_int = "$flg_basic $flg_instance -I p_int";

   flags = "$flg_basic_p_int -instantiate";
   module = "";
   program = "instances/basic-P_int.cmx";
   all_modules = "basic/basic.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_basic_p_string = "$flg_basic $flg_instance -I p_string";

   flags = "$flg_basic_p_string -instantiate";
   module = "";
   program = "instances/basic-P_string.cmx";
   all_modules = "basic/basic.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   set flg_main_basic = "\
     $flg -I p -I p_int -I p_string -I basic -I main_basic -I instances \
     -open Main_basic__ -open No_direct_access_to_main_basic \
   ";

   flags = "$flg_int_iface";
   module = "main_basic/main_basic__.ml";
   ocamlopt.byte;

   flags = "$flg_main_basic";
   module = "main_basic/main_basic.mli main_basic/main_basic.ml";
   ocamlopt.byte;

   set flg_fancy_p_int = "$flg_fancy $flg_instance -I p_int";

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__-P_int";
   all_modules = "fancy/fancy__.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_int";
   all_modules = "fancy/fancy__Flourish.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_int";
   all_modules = "fancy/fancy__Ornament.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_int -instantiate";
   module = "";
   program = "instances/fancy-P_int";
   all_modules = "fancy/fancy.cmx p_int/p_int.cmx";
   ocamlopt.byte;

   set flg_fancy_p_string = "$flg_fancy $flg_instance -I p_string";

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__-P_string";
   all_modules = "fancy/fancy__.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Flourish-P_string";
   all_modules = "fancy/fancy__Flourish.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy__Ornament-P_string";
   all_modules = "fancy/fancy__Ornament.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   flags = "$flg_fancy_p_string -instantiate";
   module = "";
   program = "instances/fancy-P_string";
   all_modules = "fancy/fancy.cmx p_string/p_string.cmx";
   ocamlopt.byte;

   set flg_main = "\
     $flg -I p -I p_int -I p_string -I fancy -I basic -I main -I instances \
     -open Main__ -open No_direct_access_to_main \
   ";

   flags = "$flg_int_iface";
   module = "main/main__.ml";
   ocamlopt.byte;

   flags = "$flg_main";
   module = "main/main.mli main/main.ml";
   ocamlopt.byte;

   check-ocamlopt.byte-output;

   flags = "\
     $flg -H p -H p_int -H p_string -H fancy -H basic -H instances \
     -I main -I main_basic \
   ";
   module = "test_native.ml";
   ocamlopt.byte;

   flags = "$flg";
   module = "";
   program = "${test_build_directory}/test_native.exe";
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
     instances/fancy__-P_int.cmx \
     instances/fancy__Flourish-P_int.cmx \
     instances/fancy__Ornament-P_int.cmx \
     instances/fancy-P_int.cmx \
     instances/fancy__-P_string.cmx \
     instances/fancy__Flourish-P_string.cmx \
     instances/fancy__Ornament-P_string.cmx \
     instances/fancy-P_string.cmx \
     main/main.cmx \
     test_native.cmx \
   ";
   ocamlopt.byte;

   (* Workaround for ocamltest bug: [script] breaks [run] unless you set
      [stdout] and [stderr] *)
   stdout = "test_native.output";
   stderr = "test_native.output";
   output = "test_native.output";
   run;

   reference = "test_native.reference";
   check-program-output;
 }
*)

let () =
  ignore (Main_basic.run () : _);
  ignore (Main.run () : _);
