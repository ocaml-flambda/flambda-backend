[@@@ocaml.warning "+a-4-30-40-41-42"]

type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z
  | Riscv

let architecture () : architecture =
  match Config.architecture with
  | "i386" -> IA32
  | "amd64" -> X86_64
  | "arm" -> ARM
  | "arm64" -> AArch64
  | "power" -> POWER
  | "s390x" -> Z
  | "riscv" -> Riscv
  | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

let is_64_bit () =
  match architecture () with
  | X86_64
  | AArch64
  | POWER
  | Z
  | Riscv -> true
  | IA32
  | ARM -> false

let is_32_bit () = not (is_64_bit ())

type derived_system =
  | Linux
  | MinGW_32
  | MinGW_64
  | Win32
  | Win64
  | Cygwin
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

let derived_system () : derived_system =
  (* Derived from [configure.ac] *)
  match architecture (), Config.model, Config.system with
  | IA32, _, "linux_elf" -> Linux
  | IA32, _, "bsd_elf" -> Generic_BSD
  | IA32, _, "beos" -> BeOS
  | IA32, _, "cygwin" -> Cygwin
  | IA32, _, "gnu" -> GNU
  | IA32, _, "mingw" -> MinGW_32
  | IA32, _, "win32" -> Win32
  | X86_64, _, "win64" -> Win64
  | POWER, "ppc64le", "elf" -> Linux
  | POWER, "ppc64", "elf" -> Linux
  | POWER, "ppc", "elf" -> Linux
  | Z, "z10", "elf" -> Linux
  | ARM, "armv6", "linux_eabihf" -> Linux
  | ARM, "armv7", "linux_eabihf" -> Linux
  | ARM, "armv8", "linux_eabihf" -> Linux
  | ARM, "armv8", "linux_eabi" -> Linux
  | ARM, "armv7", "linux_eabi" -> Linux
  | ARM, "armv6t2", "linux_eabi" -> Linux
  | ARM, "armv6", "linux_eabi" -> Linux
  | ARM, "armv6", "freebsd" -> FreeBSD
  | ARM, "armv6", "netbsd" -> NetBSD
  | ARM, "armv7", "netbsd" -> NetBSD
  | ARM, "armv5te", "linux_eabi" -> Linux
  | ARM, "armv5", "linux_eabi" -> Linux
  | ARM, _, "linux_eabihf" -> Linux
  | ARM, _, "linux_eabi" -> Linux
  | ARM, _, "bsd" -> OpenBSD
  | X86_64, _, "linux" -> Linux
  | X86_64, _, "gnu" -> GNU
  | X86_64, _, "dragonfly" -> Dragonfly
  | X86_64, _, "solaris" -> Solaris
  | X86_64, _, "freebsd" -> FreeBSD
  | X86_64, _, "netbsd" -> NetBSD
  | X86_64, _, "openbsd" -> OpenBSD
  | AArch64, _, "macosx" -> MacOS_like
  | X86_64, _, "macosx" -> MacOS_like
  | X86_64, _, "mingw64" -> MinGW_64
  | AArch64, _, "linux" -> Linux
  | AArch64, _, "freebsd" -> FreeBSD
  | X86_64, _, "cygwin" -> Cygwin
  | Riscv, "riscv64", "linux" -> Linux
  | _, _, "unknown" -> Unknown
  | _, _, _ ->
    Misc.fatal_errorf
      "Cannot determine system type (model %s, system %s): ensure \
       `target_system.ml' matches `configure'"
      Config.model Config.system

let is_windows () =
  match derived_system () with
  | Linux | MacOS_like | FreeBSD | NetBSD | OpenBSD | Generic_BSD | Solaris
  | Dragonfly | GNU | BeOS | Unknown ->
    false
  | MinGW_32 | MinGW_64 | Win32 | Win64 | Cygwin -> true

type assembler =
  | GAS_like
  | MacOS
  | MASM

let assembler () =
  match derived_system () with
  | Win32 | Win64 -> MASM
  | MacOS_like -> MacOS
  | MinGW_32 | MinGW_64 | Cygwin | Linux | FreeBSD | NetBSD | OpenBSD
  | Generic_BSD | Solaris | GNU | Dragonfly | BeOS | Unknown ->
    GAS_like
