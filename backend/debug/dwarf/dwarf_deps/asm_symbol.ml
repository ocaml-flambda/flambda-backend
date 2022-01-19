module Thing = struct
  type t = string

  let compare = String.compare

  let equal = String.equal

  let hash = Hashtbl.hash

  let output chan t = Printf.fprintf chan "%s" t

  let print = Format.pp_print_string
end

include Thing
include Identifiable.Make (Thing)

let create name = name

let escape name =
  let spec = ref false in
  for i = 0 to String.length name - 1 do
    match String.unsafe_get name i with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> ()
    | _ -> spec := true
  done;
  if not !spec
  then name
  else
    let b = Buffer.create (String.length name + 10) in
    String.iter
      (function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c ->
          Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c))
      name;
    Buffer.contents b

let symbol_prefix () =
  (* CR mshinwell: needs checking *)
  match Config_typed.architecture () with
  | IA32 | X86_64 -> begin
    match Config_typed.derived_system () with
    | Linux | Win32 | Win64 | MinGW_32 | MinGW_64 | Cygwin | FreeBSD | NetBSD
    | OpenBSD | Generic_BSD | Solaris | BeOS | GNU | Dragonfly | Unknown ->
      "" (* checked ok. *)
    | MacOS_like -> "_" (* checked ok. *)
  end
  | ARM | AArch64 | POWER | Z -> ""

let to_escaped_string ?suffix ~symbol_prefix t =
  let suffix = match suffix with None -> "" | Some suffix -> suffix in
  symbol_prefix ^ escape t ^ suffix

let encode ?without_prefix t =
  let symbol_prefix =
    match without_prefix with None -> symbol_prefix () | Some () -> ""
  in
  to_escaped_string ~symbol_prefix t
