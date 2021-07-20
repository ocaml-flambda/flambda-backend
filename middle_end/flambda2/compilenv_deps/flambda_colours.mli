(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val normal : unit -> string

val prim_constructive : unit -> string
val prim_destructive : unit -> string
val prim_neither : unit -> string

val naked_number : unit -> string
val tagged_immediate : unit -> string
val constructor : unit -> string

val kind : unit -> string
val subkind : unit -> string

val top_or_bottom_type : unit -> string

val debuginfo : unit -> string

val discriminant : unit -> string
val name : unit -> string
val parameter : unit -> string
val symbol : unit -> string
val variable : unit -> string

val closure_element : unit -> string
val closure_var : unit -> string

val code_id : unit -> string

val expr_keyword : unit -> string
val static_keyword : unit -> string

val static_part : unit -> string

val continuation : unit -> string
val continuation_definition : unit -> string
val continuation_annotation : unit -> string

val name_abstraction : unit -> string

val rec_info : unit -> string

val coercion : unit -> string

val depth_variable : unit -> string

val elide : unit -> string

val error : unit -> string

val each_file : unit -> string

val lambda : unit -> string
