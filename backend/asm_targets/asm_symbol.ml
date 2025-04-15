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

open! Int_replace_polymorphic_compare

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

let should_be_escaped = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> false
  | _c -> true

module Thing = struct
  type t =
    { name : string;
      without_prefix : bool
    }

  let compare { name = name1; without_prefix = without_prefix1 }
      { name = name2; without_prefix = without_prefix2 } =
    let cmp = String.compare name1 name2 in
    if cmp = 0
    then Bool.compare without_prefix1 without_prefix2
    else cmp

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  (* CR sspies: This changed from not having the prefix previously to now
     outputting the prefix as well. *)
  let output chan { name; without_prefix } =
    let symbol_prefix = if without_prefix then symbol_prefix () else "" in
    Printf.fprintf chan "%s%s" symbol_prefix name

  (* CR sspies: This changed from not having the prefix previously to now
     printing the prefix as well. *)
  let print fmt { name; without_prefix } =
    let symbol_prefix = if without_prefix then symbol_prefix () else "" in
    Format.pp_print_string fmt (symbol_prefix ^ name)
end

include Thing
include Identifiable.Make (Thing)

let create ?without_prefix name =
  let without_prefix = Option.is_some without_prefix in
  { name; without_prefix }

let to_raw_string { name; without_prefix } = name

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

let to_escaped_string ?suffix ~symbol_prefix t =
  let suffix = match suffix with None -> "" | Some suffix -> suffix in
  symbol_prefix ^ escape t ^ suffix

let encode t =
  let symbol_prefix = if t.without_prefix then "" else symbol_prefix () in
  to_escaped_string ~symbol_prefix t.name
