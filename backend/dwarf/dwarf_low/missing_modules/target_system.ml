type elf_abi = private
  | ARM_GNU_EABI
  | ARM_GNU_EABI_hard_float
  | AArch64
  | IA32
  | POWER_ELF32
  | POWER_ELF64v1
  | POWER_ELF64v2
  | X86_64
  | Z

type object_file_format_and_abi = private
  | A_out
  | ELF of elf_abi
  | Mach_O
  | PE
  | Unknown

type windows_system = private
  | Cygwin
  | MinGW
  | Native

type system = private
  | Linux
  | Windows of windows_system
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

type architecture = private
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z

type assembler = private
  | GAS_like
  | MacOS
  | MASM

type machine_width =
  | Thirty_two
  | Sixty_four

(** The target system of the OCaml compiler. *)
let system : unit -> system = failwith "not implemented"

(** The target object file format and ABI of the OCaml compiler. *)
let object_file_format_and_abi : unit -> object_file_format_and_abi = failwith "not implemented"

(** Whether the target system is a Windows platform. *)
let windows : unit -> bool = failwith "not implemented"

(** Whether the target system is a Windows 32-bit native platform (not
    MinGW or Cygwin). *)
let win32 : unit -> bool = failwith "not implemented"

(** Whether the target system is a Windows 64-bit native platform (not
    MinGW or Cygwin). *)
let win64 : unit -> bool = failwith "not implemented"

(** Whether the target system is Mac OS X, macOS, or some other system
    running a Darwin kernel and associated userland tools. *)
let macos_like : unit -> bool = failwith "not implemented"

(** The architecture of the target system. *)
let architecture : unit -> architecture = failwith "not implemented"

(** Convert an architecture to a string. *)
let string_of_architecture : architecture -> string = failwith "not implemented"

(** The assembler being used. *)
let assembler : unit -> assembler = failwith "not implemented"

(** The natural machine width of the target system. *)
let machine_width : unit -> machine_width = failwith "not implemented"
