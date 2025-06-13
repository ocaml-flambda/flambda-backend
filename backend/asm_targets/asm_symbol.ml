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
      already_encoded : bool
    }

  let compare { name = name1; already_encoded = already_encoded1 }
      { name = name2; already_encoded = already_encoded2 } =
    let cmp = String.compare name1 name2 in
    if cmp = 0 then Bool.compare already_encoded1 already_encoded2 else cmp

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let output chan { name; already_encoded : _ } = Printf.fprintf chan "%s" name

  let print fmt { name; already_encoded : _ } = Format.pp_print_string fmt name
end

include Thing
include Identifiable.Make (Thing)

let create name = { name; already_encoded = false }

let create_without_encoding name = { name; already_encoded = true }

let to_raw_string { name; already_encoded : _ } = name

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

let encode { name; already_encoded } =
  if already_encoded
  then name
  else
    let symbol_prefix = symbol_prefix () in
    to_escaped_string ~symbol_prefix name

(* We predefine several common symbols that violate the standard escaping done
   by [encode]. *)
module Predef = struct
  let caml_call_gc = create_without_encoding "caml_call_gc"

  let caml_call_gc_avx = create_without_encoding "caml_call_gc_avx"

  let caml_call_gc_avx512 = create_without_encoding "caml_call_gc_avx512"

  let caml_c_call = create_without_encoding "caml_c_call"

  let caml_allocN = create_without_encoding "caml_allocN"

  let caml_allocN_avx = create_without_encoding "caml_allocN_avx"

  let caml_allocN_avx512 = create_without_encoding "caml_allocN_avx512"

  let caml_alloc1 = create_without_encoding "caml_alloc1"

  let caml_alloc1_avx = create_without_encoding "caml_alloc1_avx"

  let caml_alloc1_avx512 = create_without_encoding "caml_alloc1_avx512"

  let caml_alloc2 = create_without_encoding "caml_alloc2"

  let caml_alloc2_avx = create_without_encoding "caml_alloc2_avx"

  let caml_alloc2_avx512 = create_without_encoding "caml_alloc2_avx512"

  let caml_alloc3 = create_without_encoding "caml_alloc3"

  let caml_alloc3_avx = create_without_encoding "caml_alloc3_avx"

  let caml_alloc3_avx512 = create_without_encoding "caml_alloc3_avx512"

  let caml_ml_array_bound_error =
    create_without_encoding "caml_ml_array_bound_error"

  let caml_ml_array_align_error =
    create_without_encoding "caml_ml_array_align_error"

  let caml_raise_exn = create_without_encoding "caml_raise_exn"

  let stapsdt_base = create_without_encoding "_.stapsdt.base"

  let caml_probes_semaphore ~name =
    create_without_encoding ("caml_probes_semaphore_" ^ name)

  let caml_negf_mask = create "caml_negf_mask"

  let caml_absf_mask = create "caml_absf_mask"

  let caml_negf32_mask = create "caml_negf32_mask"

  let caml_absf32_mask = create "caml_absf32_mask"
end
