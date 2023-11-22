(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Defines the runtime tags used by the target runtime.
   Must be kept in sync with ocaml/stdlib/obj.ml and ocaml/runtime/caml/mlvalues.h *)

let first_non_constant_constructor_tag = 0
let last_non_constant_constructor_tag = 243
let forcing_tag = 244
let cont_tag = 245
let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250
let no_scan_tag = 251
let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag
let int_tag = 1000
let out_of_heap_tag = 1001
let unaligned_tag = 1002
