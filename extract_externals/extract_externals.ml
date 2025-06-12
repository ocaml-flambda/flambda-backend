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

(* Argument Parsing *)
let easily_readable = ref false

let verbose = ref false

let output_file = ref None

let include_dirs = ref []

let hidden_include_dirs = ref []

let open_modules = ref []

let files = ref []

let spec_list =
  [ "-readable", Arg.Set easily_readable, "Output in easily readable format";
    "-verbose", Arg.Set verbose, "Print errors instead of failing silently";
    ( "-output-file",
      Arg.String (fun s -> output_file := Some s),
      "Optional output file; prints to stdout if not present" );
    ( "-I",
      Arg.String
        (fun s ->
          include_dirs
            := List.rev_append (String.split_on_char ',' s) !include_dirs),
      "A directory with .cmi files to include for lookups" );
    ( "-H",
      Arg.String
        (fun s ->
          hidden_include_dirs
            := List.rev_append (String.split_on_char ',' s) !hidden_include_dirs),
      "Hidden includes" );
    ( "-open",
      Arg.String
        (fun s ->
          open_modules
            := List.rev_append (String.split_on_char ',' s) !open_modules),
      "Modules to open" ) ]

let parse_arguments () =
  Arg.parse spec_list
    (fun a -> files := !files @ [a])
    "Usage: externals.exe <options> <files>\nOptions are:"

(* Pretty Printing for Externals in Readable Format*)

let pp_ext_funs ~readable fmt extfuns =
  if readable
  then Vicuna_value_shapes.print_extfuns_readable fmt extfuns
  else Vicuna_value_shapes.print_extfuns fmt extfuns

let output_shapes ~output_file ~readable externals =
  match output_file with
  | None -> pp_ext_funs ~readable Format.std_formatter externals
  | Some file ->
    Out_channel.with_open_bin file (fun out ->
        let fmt = Format.formatter_of_out_channel out in
        pp_ext_funs ~readable fmt externals;
        Format.pp_print_newline fmt ();
        Out_channel.flush out)

(* Typed Extraction *)
let extract_shapes_from_cmt ~verbose file =
  match Cmt_format.read_cmt file with
  | exception Sys_error s ->
    if verbose
    then Format.eprintf "Exception raised while reading .cmt file: %s\n" s;
    []
  | exception _ ->
    if verbose
    then Format.eprintf "Exception raised while reading .cmt file %s\n" file;
    []
  | { cmt_annots = Implementation tt; _ } ->
    Vicuna_traverse_typed_tree.extract_from_typed_tree tt
  | _ -> assert false

let extract_shapes_from_cms ~verbose file =
  match Cms_format.read file with
  | exception Sys_error s ->
    if verbose
    then Format.eprintf "Exception raised while reading .cms file: %s\n" s;
    []
  | exception _ ->
    if verbose
    then Format.eprintf "Exception raised while reading .cms file %s\n" file;
    []
  | { cms_externals; _ } -> Array.to_list cms_externals

type externals_source_file =
  | CMT of string
  | CMS of string

let extract_shapes_from_file ~verbose file =
  match file with
  | CMS file -> extract_shapes_from_cms ~verbose file
  | CMT file -> extract_shapes_from_cmt ~verbose file

let extract_shapes_from_files ~verbose files =
  Clflags.include_dirs := !include_dirs @ !Clflags.include_dirs;
  Clflags.open_modules := !open_modules @ !Clflags.open_modules;
  Clflags.hidden_include_dirs
    := !hidden_include_dirs @ !Clflags.hidden_include_dirs;
  Compmisc.init_path ();
  let files =
    List.map
      (fun file ->
        match file with
        | _ when String.ends_with file ~suffix:".cms" -> CMS file
        | _ when String.ends_with file ~suffix:".cmt" -> CMT file
        | _ ->
          Misc.fatal_errorf
            "File %s is neither a .cms nor a .cmt file; aborting\n" file)
      files
  in
  List.concat_map (extract_shapes_from_file ~verbose) files

let externals_version = "v0.1"

let extract_and_output_from_cmts ~readable ~output_file ~verbose files =
  let externals = extract_shapes_from_files ~verbose files in
  output_shapes ~output_file ~readable
    { version = externals_version; extfuns = externals }

let _ =
  parse_arguments ();
  extract_and_output_from_cmts ~readable:!easily_readable
    ~output_file:!output_file ~verbose:!verbose !files
