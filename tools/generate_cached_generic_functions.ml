(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2023 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

open Printf
open Misc
open Config

module CU = Compilation_unit

let make_cached_generic_functions unix ~ppf_dump genfns =
  Location.input_name := "caml_cached_generic_functions"; (* set name of "current" input *)
  let startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_cached_generic_functions")
  in
  Compilenv.reset startup_comp_unit;
  Emit.begin_assembly unix;
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Profile.record_call "genfns" (fun () ->
    List.iter compile_phrase (Generic_fns.compile ~shared:true genfns));
 Emit.end_assembly ()

let cached_generic_functions unix ~ppf_dump output_name genfns =
  Profile.record_call output_name (fun () ->
    let startup = output_name ^ ext_asm in
    Profile.record_call "compile_unit" (fun () ->
      let obj_filename = output_name ^ ext_obj in
      Asmgen.compile_unit ~output_prefix:output_name
        ~asm_filename:startup ~keep_asm:false
        ~obj_filename
        ~may_reduce_heap:true
        ~ppf_dump
        (fun () ->
          make_cached_generic_functions unix ~ppf_dump genfns);
      obj_filename
    );
  )

let main filename =
  let unix = (module Unix : Compiler_owee.Unix_intf.S) in
  Clflags.native_code := true;
  Clflags.use_linscan := true;
  Compmisc.init_path ();
  let file_prefix = Filename.remove_extension filename ^ ext_lib in
  let genfns_partitions = Generic_fns.Cache.all () in
  let objects = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter remove_file !objects)
    (fun () ->
       Hashtbl.iter (fun name partition ->
         let output_name = Filename.temp_file ("cached-generated-" ^ name) "" in
         let obj =
           cached_generic_functions
             unix ~ppf_dump:Format.std_formatter output_name partition
         in
         objects := obj :: !objects
       ) genfns_partitions;
       ignore (Ccomp.create_archive file_prefix !objects : int))

let arg_usage =
  Printf.sprintf "%s FILE : Generate an obj file containing cached generatic functions named FILE" Sys.argv.(0)

let main () =
  Memtrace.trace_if_requested ~context:"ocamlopt" ();
  Arg.parse_expand [] main arg_usage;
  exit 0

let _ = main ()
