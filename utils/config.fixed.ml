#2 "utils/config.fixed.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       David Allsopp, Tarides UK.                       *)
(*                                                                        *)
(*   Copyright 2022 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Configuration for the boot compiler. The compiler should refuse to bootstrap
   if configured with values which would contradict the configuration below.
   The values below are picked to trigger errors if accidentally used in the
   compiler (e.g. for the C compiler). *)

let boot_cannot_call s = "/ The boot compiler should not call " ^ s

let bindir = "/tmp"
let standard_library_default = "/tmp"
let ccomp_type = "n/a"
let c_compiler = boot_cannot_call "the C compiler"
let c_output_obj = ""
let c_has_debug_prefix_map = false
let as_has_debug_prefix_map = false
let ocamlc_cflags = ""
let ocamlc_cppflags = ""
let ocamlopt_cflags = ""
let ocamlopt_cppflags = ""
let bytecomp_c_libraries = ""
let bytecomp_c_compiler = ""
let native_c_compiler = c_compiler
let native_c_libraries = ""
let native_ldflags = ""
let native_pack_linker = boot_cannot_call "the linker"
let default_rpath = ""
let mksharedlibrpath = ""
let ar = boot_cannot_call "ar"
let supports_shared_libraries = false
let native_dynlink = false
let mkdll = native_pack_linker
let mkexe = native_pack_linker
let mkmaindll = native_pack_linker
let flambda = false
let flambda2 = false
let stack_allocation = false
let with_cpp_mangling = false
let probes = false
let poll_insertion = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let windows_unicode = false
let flat_float_array = true
let function_sections = false
let afl_instrument = false
let native_compiler = false
let tsan = false
let architecture = "none"
let model = "default"
let system = "unknown"
let asm = boot_cannot_call "the assembler"
let asm_cfi_supported = false
let with_frame_pointers = false
let with_address_sanitizer = false
let reserved_header_bits = 0
let custom_ops_struct_size = 64
let ext_exe = ".ex_The boot compiler should not be using Config.ext_exe"
let ext_obj = ".o_The boot compiler cannot process C objects"
let ext_asm = ".s_The boot compiler should not be using Config.ext_asm"
let ext_lib = ".a_The boot compiler cannot process C libraries"
let ext_dll = ".so_The boot compiler cannot load DLLs"
let host = "zinc-boot-ocaml"
let target = host
let systhread_supported = false
let flexdll_dirs = []
let ar_supports_response_files = true

(* mshinwell: at present always use runtime4 for bootstrap *)
let runtime5 = false
let no_stack_checks = false
(* This setting is only for bootstrap, does not affect dune-built compilers: *)
let naked_pointers = false
let parameterised_modules = true
