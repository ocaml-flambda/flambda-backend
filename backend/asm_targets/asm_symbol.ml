(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
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

let should_be_escaped = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> false
  | _c -> true

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

let to_raw_string t = t

let escape name =
  let escaped_nb = ref 0 in
  for i = 0 to String.length name - 1 do
    if should_be_escaped (String.unsafe_get name i) then incr escaped_nb
  done;
  if !escaped_nb = 0
  then name
  else
    (* Each escaped character is replaced by 3 characters (a $, and 2 for its
       hexadecimal representation)*)
    let b = Buffer.create (String.length name + (2 * !escaped_nb)) in
    String.iter
      (fun c ->
        if should_be_escaped c
        then Printf.bprintf b "$%02x" (Char.code c)
        else Buffer.add_char b c)
      name;
    Buffer.contents b

let symbol_prefix () =
  (* CR mshinwell: needs checking *)
  match Target_system.architecture () with
  | IA32 | X86_64 | AArch64 -> (
    match Target_system.derived_system () with
    | Linux | Win32 | Win64 | MinGW_32 | MinGW_64 | Cygwin | FreeBSD | NetBSD
    | OpenBSD | Generic_BSD | Solaris | BeOS | GNU | Dragonfly | Unknown ->
      "" (* checked ok. *)
    | MacOS_like -> "_" (* checked ok. *))
  | ARM | POWER | Z | Riscv -> ""

let to_escaped_string ?suffix ~symbol_prefix t =
  let suffix = match suffix with None -> "" | Some suffix -> suffix in
  symbol_prefix ^ escape t ^ suffix

let encode ?without_prefix t =
  let symbol_prefix =
    match without_prefix with None -> symbol_prefix () | Some () -> ""
  in
  to_escaped_string ~symbol_prefix t
