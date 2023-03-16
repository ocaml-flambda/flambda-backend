open! Translate
open! Normalize

let get_module_info = Flambda2.get_module_info

let extension = ".fl"

(** Test file locations **)
let alpha_test_dir = "./alpha/"
let test_dir = "./tests/"

let print = Flambda2_core.print

(** Parsing **)
let parse_flambda file : Flambda_unit.t =
  match Parse_flambda.parse file with
  | Ok unit -> unit
  | Error e ->
    (match e with
      | Parsing_error (msg, loc) ->
        Format.eprintf "%a:@.Syntax error: %s@." Location.print_loc loc msg
      | Lexing_error (error, loc) ->
        Format.eprintf "%a:@.Lex error: %a@." Location.print_loc loc
          Flambda_lex.pp_error error);
    exit 1

(** Test suite for checking alpha-equivalence checker [Validate.equiv].
   To add more test cases, add a pair of files that should be alpha-equivalent
    to this list. **)
let alpha_equivalence_suite =
  [
    ("simple.fl", "simple-alpha.fl");
    ("simple-cont.fl", "simple-cont-alpha.fl");
    ("simple-static.fl", "simple-static-not-alpha.fl");
    ("closures.fl", "closures-alpha.fl")
  ]

let check_alpha_equivalence file1 file2 : unit =
  let comp_unit =
    Parse_flambda.make_compilation_unit ~extension ~filename:file1 () in
  Compilation_unit.set_current (Some comp_unit);

  Format.fprintf Format.std_formatter
   "..............................................................................@.";
  let fl_output = parse_flambda (alpha_test_dir ^ file1) in
  let core_output = flambda_unit_to_core fl_output in

  let fl_output_alpha = parse_flambda (alpha_test_dir ^ file2) in
  let core_output_alpha = flambda_unit_to_core fl_output_alpha in

  (* Alpha equivalence check *)
  print Format.std_formatter core_output;
  Format.fprintf Format.std_formatter
    "@.------------------------------------------------------------------------------@.";
  print Format.std_formatter core_output_alpha;

  let alpha_eq = Equiv.core_eq core_output core_output_alpha in

  Format.fprintf Format.std_formatter
    "@..............................[α-equivalent?:%s]............................@.@."
    (alpha_eq |> Equiv.eq_string |> String.uppercase_ascii)

let alpha_equivalence_test_suite (_ : unit) =
  Format.fprintf Format.std_formatter "⟪α-Equivalence Test Suite⟫@.@.";
  let _ = List.map (fun (e1, e2) ->
    check_alpha_equivalence e1 e2) alpha_equivalence_suite
  in ()

let simplify_term file : unit =
  let comp_unit =
    Parse_flambda.make_compilation_unit ~extension ~filename:file () in
  Compilation_unit.set_current (Some comp_unit);
  let fl_output :Flambda_unit.t = parse_flambda (test_dir ^ file) in

  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in

  let {Simplify.unit = simplify_result ; _ } =
    Simplify.run ~cmx_loader ~round:0 fl_output in

  Format.fprintf Format.std_formatter
    "@.[Flambda_unit exprs]-------------------------------------------------------@.@.";
  Format.fprintf Format.std_formatter "%a@." Flambda_unit.print fl_output;
  Format.fprintf Format.std_formatter
    "---------------------------↓↓--[simplify]--↓↓-------------------------------@.";
  Format.fprintf Format.std_formatter "%a@.@." Flambda_unit.print simplify_result;

  let src_core = flambda_unit_to_core fl_output in
  let tgt_core = flambda_unit_to_core simplify_result in

  Format.fprintf Format.std_formatter
    "-----------------------------------------------------------------------------@.";
  Format.fprintf Format.std_formatter
   "⟪ Translated Core Exprs ⟫----------------------------------------------------@.";
  Format.fprintf Format.std_formatter
    "-----------------------------------------------------------------------------@.@.";

  print Format.std_formatter src_core;
  Format.fprintf Format.std_formatter
    "@.-----------------------------↓↓--[simplify]--↓↓-------------------------------@.";
  print Format.std_formatter tgt_core;
  Format.fprintf Format.std_formatter
    "@.@.------------------------------------------------------------------------------@."

let normalize_term file : unit =
  let comp_unit =
    Parse_flambda.make_compilation_unit ~extension ~filename:file () in
  Compilation_unit.set_current (Some comp_unit);
  let fl_output :Flambda_unit.t = parse_flambda (test_dir ^ file) in

  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in

  let {Simplify.unit = simplify_result ; _ } =
    Simplify.run ~cmx_loader ~round:0 fl_output in

  Format.fprintf Format.std_formatter
    "@.[Flambda_unit exprs]-------------------------------------------------------@.@.";
  Format.fprintf Format.std_formatter "%a@." Flambda_unit.print fl_output;
  Format.fprintf Format.std_formatter
    "---------------------------↓↓--[simplify]--↓↓-------------------------------@.";
  Format.fprintf Format.std_formatter "%a@.@." Flambda_unit.print simplify_result;

  let src_core = flambda_unit_to_core fl_output in
  let tgt_core = flambda_unit_to_core simplify_result in

  Format.fprintf Format.std_formatter
    "\t\t\t\tNormalizing...\t\t\t@.";
  Format.fprintf Format.std_formatter
    "------------------------------------------------------------------------------@.";

  let src_core = src_core |> normalize in
  let tgt_core = tgt_core |> normalize in

  let alpha_eq = Equiv.core_eq src_core tgt_core in

  Format.fprintf Format.std_formatter
  "@.--------------------------------------------------------------------[original]@.";
  print Format.std_formatter src_core;
  Format.fprintf Format.std_formatter
    "@.-------------------------------------------------------------------[simplified]@.";
  print Format.std_formatter tgt_core;
  Format.fprintf Format.std_formatter
    "@..............................[α-equivalent?:%s]............................."
    (alpha_eq |> Equiv.eq_string |> String.uppercase_ascii);
  Format.fprintf Format.std_formatter
    "@.==============================================================================@.";
  ()

(** Top-level driver for alpha equivalence checker and verbose validator tests **)
let () =
  (* alpha_equivalence_test_suite (); *)
  (* normalize_term "arith.fl"; *)
  ()
