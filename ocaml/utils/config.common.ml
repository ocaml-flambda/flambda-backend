#2 "utils/config.common.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Portions of the Config module common to both the boot and main compiler. *)

(* The main OCaml version string has moved to ../build-aux/ocaml_version.m4 *)
let version = Sys.ocaml_version

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

(* When artifacts are incompatible with upstream OCaml, ocaml-jst uses
   magic numbers ending in 5xx. (The AST remains
   compatible, so use upstream numbers) *)
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD:ocaml/utils/config.common.ml
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD:ocaml/utils/config.common.ml
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD
let exec_magic_number = "Caml1999X512"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I513"
and cmo_magic_number = "Caml1999O513"
and cma_magic_number = "Caml1999A513"
and cmx_magic_number =
  if flambda || flambda2 then
    "Caml2021y515"
  else
    "Caml2021Y515"
and cmxa_magic_number =
  if flambda || flambda2 then
    "Caml2021z515"
  else
    "Caml2021Z515"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D514"
and cmt_magic_number = "Caml1999T513"
and cms_magic_number = "Caml1999S511"
and linear_magic_number = "Caml1999L513"
and cfg_magic_number = "Caml2021G513"
=======
let exec_magic_number = "Caml1999X511"
=======
let exec_magic_number = "Caml1999X520"
>>>>>>> db638e1ef1d923c67cd7142850e6693243f6cbfa:ocaml/utils/config.common.ml
=======
let exec_magic_number = "Caml1999X511"
>>>>>>> 0d4056a108c984b74ebed35634ddd3dad4394d30:ocaml/utils/config.mlp
=======
let exec_magic_number = "Caml1999X520"
>>>>>>> c3b2b912cfac7d208d5daafaf044062285c3037a:ocaml/utils/config.common.ml
=======
let exec_magic_number = "Caml1999X511"
>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc:ocaml/utils/config.mlp
=======
let exec_magic_number = "Caml1999X520"
>>>>>>> 7e235784151b8ed7eff585d541925760d5b3dfeb:ocaml/utils/config.common.ml
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I520"
and cmo_magic_number = "Caml1999O520"
and cma_magic_number = "Caml1999A520"
and cmx_magic_number =
  if flambda || flambda2 then
    "Caml2021y520"
  else
    "Caml2021Y520"
and cmxa_magic_number =
  if flambda || flambda2 then
    "Caml2021z520"
  else
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD:ocaml/utils/config.common.ml
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD:ocaml/utils/config.common.ml
<<<<<<< HEAD:ocaml/utils/config.mlp
=======
>>>>>>> 0d4056a108c984b74ebed35634ddd3dad4394d30:ocaml/utils/config.mlp
=======
>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc:ocaml/utils/config.mlp
    "Caml2021Z514"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D513"
and cmt_magic_number = "Caml1999T512"
and cms_magic_number = "Caml1999S510"
and linear_magic_number = "Caml1999L512"
and cfg_magic_number = "Caml2021G512"
<<<<<<< HEAD:ocaml/utils/config.common.ml
<<<<<<< HEAD:ocaml/utils/config.common.ml
>>>>>>> 16edf2fd3875f1fd183a82f318d80aa7856d66d8
=======
=======
>>>>>>> c3b2b912cfac7d208d5daafaf044062285c3037a:ocaml/utils/config.common.ml
=======
>>>>>>> 7e235784151b8ed7eff585d541925760d5b3dfeb:ocaml/utils/config.common.ml
    "Caml2021Z520"
and ast_impl_magic_number = "Caml1999M033"
and ast_intf_magic_number = "Caml1999N033"
and cmxs_magic_number = "Caml1999D520"
and cmt_magic_number = "Caml1999T520"
and cms_magic_number = "Caml1999S520"
and linear_magic_number = "Caml1999L520"
and cfg_magic_number = "Caml2021G520"

let safe_string = true
let default_safe_string = true
let naked_pointers = false
let flambda_backend = true
<<<<<<< HEAD:ocaml/utils/config.mlp
<<<<<<< HEAD:ocaml/utils/config.mlp
>>>>>>> db638e1ef1d923c67cd7142850e6693243f6cbfa:ocaml/utils/config.common.ml
=======
>>>>>>> 0d4056a108c984b74ebed35634ddd3dad4394d30:ocaml/utils/config.mlp
=======
>>>>>>> c3b2b912cfac7d208d5daafaf044062285c3037a:ocaml/utils/config.common.ml
=======
>>>>>>> 31dc1f33938b757dd9a502596e73c170d4c676bc:ocaml/utils/config.mlp
=======
>>>>>>> 7e235784151b8ed7eff585d541925760d5b3dfeb:ocaml/utils/config.common.ml

let interface_suffix = ref ".mli"

let max_tag = 243
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 32 (* see runtime/caml/config.h *)
let stack_safety_margin = 6
let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"
type configuration_value =
  | String of string
  | Int of int
  | Bool of bool

let configuration_variables () =
  let p x v = (x, String v) in
  let p_int x v = (x, Int v) in
  let p_bool x v = (x, Bool v) in
[
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "ccomp_type" ccomp_type;
  p "c_compiler" c_compiler;
  p "ocamlc_cflags" ocamlc_cflags;
  p "ocamlc_cppflags" ocamlc_cppflags;
  p "ocamlopt_cflags" ocamlopt_cflags;
  p "ocamlopt_cppflags" ocamlopt_cppflags;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "native_c_compiler" native_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p_bool "native_compiler" native_compiler;
  p "architecture" architecture;
  p "model" model;
  p_int "int_size" Sys.int_size;
  p_int "word_size" Sys.word_size;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p_bool "with_cpp_mangling" with_cpp_mangling;
  p "ext_exe" ext_exe;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;
  p_bool "flambda" flambda;
  p_bool "flambda2" flambda2;
  p_bool "safe_string" safe_string;
  p_bool "default_safe_string" default_safe_string;
  p_bool "flat_float_array" flat_float_array;
  p_bool "function_sections" function_sections;
  p_bool "afl_instrument" afl_instrument;
  p_bool "windows_unicode" windows_unicode;
  p_bool "supports_shared_libraries" supports_shared_libraries;
  p_bool "native_dynlink" native_dynlink;
  p_bool "naked_pointers" naked_pointers;
(*
  Disabled in flambda-backend (for now)
  p_bool "compression_supported" (Marshal.compression_supported());
*)

  p "exec_magic_number" exec_magic_number;
  p "cmi_magic_number" cmi_magic_number;
  p "cmo_magic_number" cmo_magic_number;
  p "cma_magic_number" cma_magic_number;
  p "cmx_magic_number" cmx_magic_number;
  p "cmxa_magic_number" cmxa_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmxs_magic_number" cmxs_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  p "cms_magic_number" cms_magic_number;
  p "linear_magic_number" linear_magic_number;

  p_bool "flambda_backend" flambda_backend;
  p_bool "probes" probes;
  p_bool "stack_allocation" stack_allocation;
]

let print_config_value oc = function
  | String s ->
      Printf.fprintf oc "%s" s
  | Int n ->
      Printf.fprintf oc "%d" n
  | Bool p ->
      Printf.fprintf oc "%B" p

let print_config oc =
  let print (x, v) =
    Printf.fprintf oc "%s: %a\n" x print_config_value v in
  List.iter print (configuration_variables ());
  flush oc

let config_var x =
  match List.assoc_opt x (configuration_variables()) with
  | None -> None
  | Some v ->
      let s = match v with
        | String s -> s
        | Int n -> Int.to_string n
        | Bool b -> string_of_bool b
      in
      Some s

let merlin = false
