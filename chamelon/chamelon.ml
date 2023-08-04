(** Minimizer **)

open Utils
open Iterator
open Typedtree
open Cmt_format

(* ______ COMMAND SETUP ______ *)
let usage_msg =
  "minimize <file1> [<file2>] ... -c \"<command>\" [-m <minimizers>] [-e \
   <error>] [-t <typing command>] [-o <output>]"

let input_files = ref []
let arg_minimizers = ref ""
let command = ref ""
let typing_command = ref ""
let output_file = ref ""
let test = ref false
let anon_fun filename = input_files := filename :: !input_files

let spec_list =
  [
    ("-c", Arg.Set_string command, "Set command");
    ("-m", Arg.Set_string arg_minimizers, "Set minimizers");
    ("-e", Arg.Set_string Utils.error_str, "Set error to preserve");
    ( "-t",
      Arg.Set_string typing_command,
      "Set command to use to generate cmt file" );
    ("-o", Arg.Set_string output_file, "Set output file/folder");
    ("--test", Arg.Set test, "Run only first iteration of minimizer");
  ]

let () = Arg.parse spec_list anon_fun usage_msg

let all_minimizers =
  List.fold_left
    (fun minimizers m -> Smap.add m.minimizer_name m minimizers)
    Smap.empty
    [
      Deletelines.minimizer;
      Flatteningmodules.minimizer;
      Inlinefunction.minimizer;
      Inlinenever.minimizer;
      Reducedef.minimizer;
      Reduceexpr.minimizer;
      (* Reduceexpr_typesafe.minimizer; *)
      Remdef.minimizer;
      Removeattributes.minimizer;
      Removeconsfields.minimizer;
      Removedeadcode.minimizer;
      Removeunit.minimizer;
      Removeunusedargs.minimizer;
      Removeunusedrec.minimizer;
      Sequentializefunctions.minimizer;
      Simplifyapplication.minimizer;
      Simplifymatch.minimizer;
      Simplifysequences.minimizer;
      Simplifytypes.minimizer;
    ]

let default_iteration =
  [
    "delete-lines";
    "reduce-expr";
    "remove-dead-code";
    "inline-never";
    "remove-unit";
    "reduce-def";
    "remove-dead-code";
    "simplify-sequences";
    "remove-unused-args";
    "remove-unused-rec";
    "sequentialize-functions";
    "simplify-sequences";
    "sequentialize-functions";
    "inline-function";
    "simplify-application";
    "simplify-match";
    "simplify-application";
    "simplify-match";
    "flatten-modules";
    (* "remove-attributes"; *)
    "simplify-types";
    "remove-cons-fields";
  ]

let minimizers_to_run =
  List.map
    (fun name ->
      try Smap.find name all_minimizers
      with Not_found ->
        Format.eprintf "Minimizer %S not found@." name;
        exit 1)
    (if !arg_minimizers = "" then default_iteration
    else String.split_on_char ',' !arg_minimizers)

(* ______ ONE FILE MINIMIZATION ______ *)

(** [one_file_minimize c map file] minimizes [file] in the file set [map]
  regarding to the command [c] *)
let one_file_minimize c (map : structure Smap.t) file : structure Smap.t * bool
    =
  if !test then (
    if List.compare_length_with minimizers_to_run 1 <> 0 then (
      Format.eprintf "Please provide exactly one minimizer in test mode@.";
      exit 1);
    apply_minimizer true map file (List.hd minimizers_to_run) c)
  else (
    Format.eprintf "Starting to minimize %s @." file;
    List.fold_left
      (fun (nmap, b) minimizer ->
        let nmap, has_changed = apply_minimizer false nmap file minimizer c in
        (nmap, b || has_changed))
      (map, false) minimizers_to_run)

let main () =
  (* PARSING COMMAND AND READING FILES*)
  let file_names = List.rev !input_files in
  let cmt_command =
    if !typing_command = "" then !command else !typing_command
  in
  let cmt_infos = generate_cmt cmt_command file_names in
  let file_strs =
    List.map (fun cmt_info -> extract_cmt cmt_info.cmt_annots) cmt_infos
  in

  (* CHECKING ERROR PRESENCE *)
  let c =
    List.fold_left (fun c output -> c ^ " " ^ output) !command file_names
  in
  if not (raise_error c) then (
    Format.eprintf "This command does not raise the error %S. @."
      !Utils.error_str;
    exit 1);

  if List.length file_names = 1 then (
    (* MONOFILE MINIMIZATION*)
    let input = List.hd file_names in
    let output_file =
      if !output_file = "" then
        String.sub input 0 (String.length input - 3) ^ "_min.ml"
      else !output_file
    in
    let c = !command ^ " " ^ output_file in
    let input_str = ref (List.hd file_strs) in
    update_single output_file !input_str;
    let has_changed = ref true in
    while !has_changed do
      let a, b =
        one_file_minimize c (Smap.singleton output_file !input_str) output_file
      in
      input_str := Smap.find output_file a;
      has_changed := b
    done;
    let a, _ =
      apply_minimizer false
        (Smap.singleton output_file !input_str)
        output_file Remdef.minimizer c
    in
    input_str := Smap.find output_file a)
  else
    (* MULTIFILE MINIMIZATION *)
    let output_dir =
      if !output_file = "" then "minimized_res" else !output_file
    in
    Stdlib.ignore (Sys.command ("cp -R . " ^ output_dir ^ "/"));
    Sys.chdir output_dir;
    (* MINIMIZING FILES *)
    let rfile_names = ref file_names in
    let rfile_strs = ref file_strs in
    let str_map =
      List.fold_left2
        (fun map key str -> Smap.add key str map)
        Smap.empty file_names file_strs
    in
    let c =
      ref
        (List.fold_left (fun c output -> c ^ " " ^ output) !command file_names)
    in
    let has_changed = ref true in
    let nmap = ref str_map in
    while !has_changed do
      (* REMOVING FILES *)
      let fn, fs =
        Mergefiles.merge_strategy !command
          (Removefiles.to_remove !command (!rfile_names, !rfile_strs))
      in
      rfile_names := fn;
      rfile_strs := fs;
      nmap :=
        List.fold_left2
          (fun map key str -> Smap.add key str map)
          Smap.empty file_names file_strs;
      c := make_command !command fn;
      let a, b =
        List.fold_left
          (fun (map, b) name ->
            let nmap, ch = one_file_minimize !c map name in
            (nmap, b || ch))
          (!nmap, false) file_names
      in
      nmap := a;
      has_changed := b
    done;
    Sys.chdir ".."

let _ = main ()
