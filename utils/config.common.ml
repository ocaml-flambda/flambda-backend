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

<<<<<<< HEAD:utils/config.mlp
let ccomp_type = "%%CCOMPTYPE%%"
let c_compiler = "%%CC%%"
let c_output_obj = "%%OUTPUTOBJ%%"
let c_has_debug_prefix_map = %%CC_HAS_DEBUG_PREFIX_MAP%%
let as_has_debug_prefix_map = %%AS_HAS_DEBUG_PREFIX_MAP%%
let ocamlc_cflags = "%%OCAMLC_CFLAGS%%"
let ocamlc_cppflags = "%%OCAMLC_CPPFLAGS%%"
(* #7678: ocamlopt uses these only to compile .c files, and the behaviour for
          the two drivers should be identical. *)
let ocamlopt_cflags = "%%OCAMLC_CFLAGS%%"
let ocamlopt_cppflags = "%%OCAMLOPT_CPPFLAGS%%"
let bytecomp_c_libraries = "%%BYTECCLIBS%%"
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ ocamlopt_cflags ^ " " ^ ocamlopt_cppflags
let native_c_libraries = "%%NATIVECCLIBS%%"
let native_pack_linker = "%%PACKLD%%"
let default_rpath = "%%RPATH%%"
let mksharedlibrpath = "%%MKSHAREDLIBRPATH%%"
let ar = "%%ARCMD%%"
let supports_shared_libraries = %%SUPPORTS_SHARED_LIBRARIES%%
let mkdll, mkexe, mkmaindll =
  (* @@DRA Cygwin - but only if shared libraries are enabled, which we
     should be able to detect? *)
  if Sys.win32 || Sys.cygwin && supports_shared_libraries then
    try
      let flexlink =
        let flexlink = Sys.getenv "OCAML_FLEXLINK" in
        let f i =
          let c = flexlink.[i] in
          if c = '/' && Sys.win32 then '\\' else c in
        (String.init (String.length flexlink) f) ^ " %%FLEXLINK_FLAGS%%" in
      flexlink ^ "%%FLEXLINK_DLL_LDFLAGS%%",
      flexlink ^ " -exe%%FLEXLINK_LDFLAGS%%",
      flexlink ^ " -maindll%%FLEXLINK_DLL_LDFLAGS%%"
    with Not_found ->
      "%%MKDLL%%", "%%MKEXE%%", "%%MKMAINDLL%%"
  else
    "%%MKDLL%%", "%%MKEXE%%", "%%MKMAINDLL%%"

let flambda = %%FLAMBDA%%
let flambda2 = %%FLAMBDA2%%
let with_flambda_invariants = %%WITH_FLAMBDA_INVARIANTS%%
let with_cmm_invariants = %%WITH_CMM_INVARIANTS%%
let flambda_backend = true
let safe_string = %%FORCE_SAFE_STRING%%
let default_safe_string = %%DEFAULT_SAFE_STRING%%
let windows_unicode = %%WINDOWS_UNICODE%% != 0
let naked_pointers = %%NAKED_POINTERS%%

let flat_float_array = %%FLAT_FLOAT_ARRAY%%

let function_sections = %%FUNCTION_SECTIONS%%
let probes = %%PROBES%%
let afl_instrument = %%AFL_INSTRUMENT%%

let stack_allocation = %%STACK_ALLOCATION%%
let poll_insertion = %%POLL_INSERTION%%

(* When artifacts are incompatible with upstream OCaml, ocaml-jst uses
   magic numbers ending in 5xx. (The AST remains
   compatible, so use upstream numbers) *)
let exec_magic_number = "Caml1999X511"
||||||| merged common ancestors:utils/config.mlp
let ccomp_type = "%%CCOMPTYPE%%"
let c_compiler = "%%CC%%"
let c_output_obj = "%%OUTPUTOBJ%%"
let c_has_debug_prefix_map = %%CC_HAS_DEBUG_PREFIX_MAP%%
let as_has_debug_prefix_map = %%AS_HAS_DEBUG_PREFIX_MAP%%
let ocamlc_cflags = "%%OCAMLC_CFLAGS%%"
let ocamlc_cppflags = "%%OCAMLC_CPPFLAGS%%"
(* #7678: ocamlopt uses these only to compile .c files, and the behaviour for
          the two drivers should be identical. *)
let ocamlopt_cflags = "%%OCAMLC_CFLAGS%%"
let ocamlopt_cppflags = "%%OCAMLOPT_CPPFLAGS%%"
let bytecomp_c_libraries = "%%BYTECCLIBS%%"
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ ocamlopt_cflags ^ " " ^ ocamlopt_cppflags
let native_c_libraries = "%%NATIVECCLIBS%%"
let native_pack_linker = "%%PACKLD%%"
let ranlib = "%%RANLIBCMD%%"
let default_rpath = "%%RPATH%%"
let mksharedlibrpath = "%%MKSHAREDLIBRPATH%%"
let ar = "%%ARCMD%%"
let supports_shared_libraries = %%SUPPORTS_SHARED_LIBRARIES%%
let mkdll, mkexe, mkmaindll =
  (* @@DRA Cygwin - but only if shared libraries are enabled, which we
     should be able to detect? *)
  if Sys.win32 || Sys.cygwin && supports_shared_libraries then
    try
      let flexlink =
        let flexlink = Sys.getenv "OCAML_FLEXLINK" in
        let f i =
          let c = flexlink.[i] in
          if c = '/' && Sys.win32 then '\\' else c in
        (String.init (String.length flexlink) f) ^ " %%FLEXLINK_FLAGS%%" in
      flexlink ^ "%%FLEXLINK_DLL_LDFLAGS%%",
      flexlink ^ " -exe%%FLEXLINK_LDFLAGS%%",
      flexlink ^ " -maindll%%FLEXLINK_DLL_LDFLAGS%%"
    with Not_found ->
      "%%MKDLL%%", "%%MKEXE%%", "%%MKMAINDLL%%"
  else
    "%%MKDLL%%", "%%MKEXE%%", "%%MKMAINDLL%%"

let flambda = %%FLAMBDA%%
let with_flambda_invariants = %%WITH_FLAMBDA_INVARIANTS%%
let with_cmm_invariants = %%WITH_CMM_INVARIANTS%%
let safe_string = %%FORCE_SAFE_STRING%%
let default_safe_string = %%DEFAULT_SAFE_STRING%%
let windows_unicode = %%WINDOWS_UNICODE%% != 0
let naked_pointers = %%NAKED_POINTERS%%

let flat_float_array = %%FLAT_FLOAT_ARRAY%%

let function_sections = %%FUNCTION_SECTIONS%%
let afl_instrument = %%AFL_INSTRUMENT%%

let exec_magic_number = "Caml1999X031"
=======
let exec_magic_number = "Caml1999X033"
>>>>>>> ocaml/5.1:utils/config.common.ml
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
<<<<<<< HEAD:utils/config.mlp
and cmi_magic_number = "Caml1999I512"
and cmo_magic_number = "Caml1999O512"
and cma_magic_number = "Caml1999A512"
||||||| merged common ancestors:utils/config.mlp
and cmi_magic_number = "Caml1999I031"
and cmo_magic_number = "Caml1999O031"
and cma_magic_number = "Caml1999A031"
=======
and cmi_magic_number = "Caml1999I033"
and cmo_magic_number = "Caml1999O033"
and cma_magic_number = "Caml1999A033"
>>>>>>> ocaml/5.1:utils/config.common.ml
and cmx_magic_number =
<<<<<<< HEAD:utils/config.mlp
  if flambda || flambda2 then
    "Caml2021y514"
||||||| merged common ancestors:utils/config.mlp
  if flambda then
    "Caml1999y031"
=======
  if flambda then
    "Caml1999y033"
>>>>>>> ocaml/5.1:utils/config.common.ml
  else
<<<<<<< HEAD:utils/config.mlp
    "Caml2021Y514"
||||||| merged common ancestors:utils/config.mlp
    "Caml1999Y031"
=======
    "Caml1999Y033"
>>>>>>> ocaml/5.1:utils/config.common.ml
and cmxa_magic_number =
<<<<<<< HEAD:utils/config.mlp
  if flambda || flambda2 then
    "Caml2021z514"
||||||| merged common ancestors:utils/config.mlp
  if flambda then
    "Caml1999z031"
=======
  if flambda then
    "Caml1999z033"
>>>>>>> ocaml/5.1:utils/config.common.ml
  else
<<<<<<< HEAD:utils/config.mlp
    "Caml2021Z514"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D513"
and cmt_magic_number = "Caml1999T512"
and cms_magic_number = "Caml1999S510"
and linear_magic_number = "Caml1999L512"
and cfg_magic_number = "Caml2021G512"
||||||| merged common ancestors:utils/config.mlp
    "Caml1999Z031"
and ast_impl_magic_number = "Caml1999M031"
and ast_intf_magic_number = "Caml1999N031"
and cmxs_magic_number = "Caml1999D031"
and cmt_magic_number = "Caml1999T031"
and linear_magic_number = "Caml1999L031"
=======
    "Caml1999Z033"
and ast_impl_magic_number = "Caml1999M033"
and ast_intf_magic_number = "Caml1999N033"
and cmxs_magic_number = "Caml1999D033"
and cmt_magic_number = "Caml1999T033"
and linear_magic_number = "Caml1999L033"

let safe_string = true
let default_safe_string = true
let naked_pointers = false
>>>>>>> ocaml/5.1:utils/config.common.ml

let interface_suffix = ref ".mli"

let max_tag = 243
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
<<<<<<< HEAD:utils/config.mlp
let stack_threshold = 256 (* see runtime/caml/config.h *)
let stack_safety_margin = 60

let architecture = "%%ARCH%%"
let model = "%%MODEL%%"
let system = "%%SYSTEM%%"

let asm = "%%ASM%%"
let asm_cfi_supported = %%ASM_CFI_SUPPORTED%%
let with_frame_pointers = %%WITH_FRAME_POINTERS%%
let with_cpp_mangling = %%WITH_CPP_MANGLING%%
let profinfo = %%WITH_PROFINFO%%
let profinfo_width = %%PROFINFO_WIDTH%%

let ext_exe = "%%EXE%%"
let ext_obj = "%%EXT_OBJ%%"
let ext_asm = "%%EXT_ASM%%"
let ext_lib = "%%EXT_LIB%%"
let ext_dll = "%%EXT_DLL%%"

let host = "%%HOST%%"
let target = "%%TARGET%%"

||||||| merged common ancestors:utils/config.mlp
let stack_threshold = 256 (* see runtime/caml/config.h *)
let stack_safety_margin = 60

let architecture = "%%ARCH%%"
let model = "%%MODEL%%"
let system = "%%SYSTEM%%"

let asm = "%%ASM%%"
let asm_cfi_supported = %%ASM_CFI_SUPPORTED%%
let with_frame_pointers = %%WITH_FRAME_POINTERS%%
let profinfo = %%WITH_PROFINFO%%
let profinfo_width = %%PROFINFO_WIDTH%%

let ext_exe = "%%EXE%%"
let ext_obj = "%%EXT_OBJ%%"
let ext_asm = "%%EXT_ASM%%"
let ext_lib = "%%EXT_LIB%%"
let ext_dll = "%%EXT_DLL%%"

let host = "%%HOST%%"
let target = "%%TARGET%%"

=======
let stack_threshold = 32 (* see runtime/caml/config.h *)
let stack_safety_margin = 6
>>>>>>> ocaml/5.1:utils/config.common.ml
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
<<<<<<< HEAD:utils/config.mlp
||||||| merged common ancestors:utils/config.mlp
  p "ranlib" ranlib;
=======
  p_bool "native_compiler" native_compiler;
>>>>>>> ocaml/5.1:utils/config.common.ml
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
  p_bool "compression_supported" (Marshal.compression_supported());

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
