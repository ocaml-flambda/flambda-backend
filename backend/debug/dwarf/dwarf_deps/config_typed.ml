type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z

let architecture () : architecture =
  match Config.architecture with
  | "i386" -> IA32
  | "amd64" -> X86_64
  | "arm" -> ARM
  | "arm64" -> AArch64
  | "power" -> POWER
  | "s390x" -> Z
  | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

type windows_system =
  | Cygwin
  | MinGW
  | Native

type system =
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

let (_ : system) = Solaris

let system () : system =
  match architecture (), Config.model, Config.system with
  | IA32, _, "linux_aout" -> Linux
  | IA32, _, "linux_elf" -> Linux
  | IA32, _, "bsd_aout" -> Generic_BSD
  | IA32, _, "bsd_elf" -> Generic_BSD
  | IA32, _, "beos" -> BeOS
  | IA32, _, "cygwin" -> Windows Cygwin
  | (X86_64 | IA32), _, "macosx" -> MacOS_like
  | IA32, _, "gnu" -> GNU
  | IA32, _, "mingw" -> Windows MinGW
  | IA32, _, "win32" -> Windows Native
  | POWER, "ppc64le", "elf" -> Linux
  | POWER, "ppc64", "elf" -> Linux
  | POWER, "ppc", "elf" -> Linux
  | POWER, "ppc", "netbsd" -> NetBSD
  | POWER, "ppc", "bsd_elf" -> OpenBSD
  | Z, _, "elf" -> Linux
  | ARM, _, "linux_eabihf" -> Linux
  | ARM, _, "linux_eabi" -> Linux
  | ARM, _, "bsd" -> OpenBSD
  | X86_64, _, "linux" -> Linux
  | X86_64, _, "gnu" -> GNU
  | X86_64, _, "dragonfly" -> Dragonfly
  | X86_64, _, "freebsd" -> FreeBSD
  | X86_64, _, "netbsd" -> NetBSD
  | X86_64, _, "openbsd" -> OpenBSD
  | X86_64, _, "darwin" -> MacOS_like
  | X86_64, _, "mingw" -> Windows MinGW
  | AArch64, _, "linux" -> Linux
  | X86_64, _, "cygwin" -> Windows Cygwin
  | X86_64, _, "win64" -> Windows Native
  | _, _, "unknown" -> Unknown
  | _, _, _ ->
    Misc.fatal_errorf "Cannot determine system type (model %s, system %s): \
        ensure `target_system.ml' matches `configure'"
      Config.model Config.system

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
  match system (), Machine_width.of_int_exn Targetint.size with
  | Linux, _ -> Linux
  | Windows Cygwin, _ -> Cygwin
  | Windows MinGW, Thirty_two -> MinGW_32
  | Windows MinGW, Sixty_four -> MinGW_64
  | Windows Native, Thirty_two -> Win32
  | Windows Native, Sixty_four -> Win64
  | MacOS_like, _ -> MacOS_like
  | FreeBSD, _ -> FreeBSD
  | NetBSD, _ -> NetBSD
  | OpenBSD, _ -> OpenBSD
  | Generic_BSD, _ -> Generic_BSD
  | Solaris, _ -> Solaris
  | Dragonfly, _ -> Dragonfly
  | GNU, _ -> GNU
  | BeOS, _ -> BeOS
  | Unknown, _ -> Unknown