(******************************************************************************
 *                             flambda-backend                                *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Shapes


(* Argument Parsing *)
let easily_readable = ref false
let verbose = ref false
let output_file = ref None
let include_dirs = ref []
let files = ref []

let spec_list = [
  ("-readable", Arg.Set easily_readable, "Output in easily readable format");
  ("-verbose", Arg.Set verbose, "Print errors instead of failing silently");
  ("-output-file", Arg.String (fun s -> output_file := Some s), "Optional output file; prints to stdout if not present");
  ("-I", Arg.String (fun s -> include_dirs := !include_dirs @ [s]), "A directory with .cmi files to include for lookups");
]

let parse_arguments () =
  Arg.parse spec_list (fun a -> files := !files @ [a])
    "Usage: externals.exe <options> <files>\nOptions are:";;


(* Pretty Printing for Externals in Readable Format*)
(*
let pp_ext_funs ~human_readable fmt extfuns =
  let pp_extfun_serialized fmt extfuns =
    Format.pp_print_string fmt (Shapes.serialize_extfuns extfuns)
  in
  let pp_extfun_readable fmt extfuns = pp_extfuns fmt extfuns in
  if human_readable
  then pp_extfun_readable fmt extfuns
  else pp_extfun_serialized fmt extfuns
;;

let output_shapes ~output_file ~human_readable externals =
  match output_file with
  | None -> pp_ext_funs ~human_readable Format.std_formatter externals
  | Some file ->
    Out_channel.with_file ~binary:false file ~f:(fun out ->
      let fmt = Format.formatter_of_out_channel out in
      pp_ext_funs ~human_readable fmt externals;
      Format.pp_print_newline fmt ();
      Out_channel.flush out)
;;

(* Typed Extraction *)
let extract_shapes_from_cmt ~verbose file =
  match Cmt_format.read_cmt file with
  | exception Sys_error s ->
    if verbose then Format.eprintf "Exception raised while reading .cmt file: %s" s;
    []
  | exception _ ->
    if verbose then Format.eprintf "Exception raised while reading .cmt file %s" file;
    []
  | { cmt_annots = Implementation tt; _ } ->
    Traverse_typed_tree.extract_from_typed_tree tt
  | _ -> assert false
;;

let extract_shapes_from_cmts ~includes ~verbose files =
  Clflags.include_dirs := includes @ !Clflags.include_dirs;
  Compmisc.init_path ();
  List.iter
    ~f:(fun file ->
      if not (String.is_suffix file ~suffix:".cmt")
      then raise_s [%sexp "input error: only .cmt files will be parsed", (file : string)])
    files;
  List.concat_map ~f:(extract_shapes_from_cmt ~verbose) files
;;


let externals_version = "v0.1"

let extract_and_output_from_cmts ~human_readable ~includes ~output_file ~verbose files =
  let externals = extract_shapes_from_cmts ~includes ~verbose files in
  (* There is no need to remove compiler primitives from the list of externals that we found
     because this is now done while traversing the parse tree to avoid the extraction from
     peeking into types of compiler primitives when its not needed *)
  output_shapes
    ~output_file
    ~human_readable
    { version = externals_version; extfuns = externals }
;; *)

let _ = parse_arguments ();
  let _ = Traverse_typed_tree.test in
  Format.printf "Easily readable: %b, verbose: %b, output file: %s, include dirs: %s, files: %s@."
        !easily_readable !verbose (match !output_file with None -> "None" | Some s -> s) (String.concat "," !include_dirs) (String.concat "," !files)
