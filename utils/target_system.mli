type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z
  | Riscv

val architecture : unit -> architecture

val is_64_bit : unit -> bool

val is_32_bit : unit -> bool

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

val derived_system : unit -> derived_system

val is_windows : unit -> bool

type assembler =
  | GAS_like
  | MacOS
  | MASM

val assembler : unit -> assembler

type machine_width =
  | Thirty_two
  | Sixty_four

(** The natural machine width of the target system. *)
val machine_width : unit -> machine_width

type windows_system = private
  | Cygwin
  | MinGW
  | Native

(* CR sspies: Remove some of the systems below that are a bit dated. *)
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

val system: unit -> system

val windows : unit -> bool

val is_macos : unit -> bool

val is_gas : unit -> bool