(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the OCaml toplevels *)

open Ocamltest_stdlib

module type Toplevel = sig
  include Ocaml_tools.Tool

  val backend : Ocaml_backends.t
  val compiler : Ocaml_compilers.compiler
end

type toplevel = (module Toplevel)

let toplevel
  ~(name : string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
  ~(backend : Ocaml_backends.t)
  ~(compiler : Ocaml_compilers.compiler)
= (module struct include (val Ocaml_tools.tool
  ~name:name
  ~family:"toplevel"
  ~flags:flags
  ~directory:directory
  ~exit_status_variable:exit_status_variable
  ~reference_variable:reference_variable
  ~output_variable:output_variable
  )
  let backend = backend
  let compiler = compiler
  let reference_file env prefix =
    let default = reference_file env prefix in
    if Sys.file_exists default then default else
    let suffix = reference_filename_suffix env in
    let mk s = (Filename.make_filename prefix s) ^ suffix in
    let filename = mk
      (Ocaml_backends.string_of_backend backend) in
    if Sys.file_exists filename then filename else
    mk "compilers"

end : Toplevel)

let ocaml = toplevel
  ~name: Ocaml_commands.ocamlrun_ocaml
  ~flags: ""
  ~directory: "ocaml"
  ~exit_status_variable: Ocaml_variables.ocaml_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~backend: Ocaml_backends.Bytecode
  ~compiler: Ocaml_compilers.ocamlc_byte

let ocamlnat = toplevel
  ~name: Ocaml_files.ocamlnat
  ~flags: "-S" (* Keep intermediate assembly files *)
  ~directory: "ocamlnat"
  ~exit_status_variable: Ocaml_variables.ocamlnat_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Native
  ~compiler: Ocaml_compilers.ocamlc_opt
