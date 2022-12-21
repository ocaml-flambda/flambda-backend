(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm

(** Create a C function call. *)
val extcall :
  dbg:Debuginfo.t ->
  returns:bool ->
  alloc:bool ->
  is_c_builtin:bool ->
  ty_args:exttype list ->
  string ->
  machtype ->
  expression list ->
  expression

(** [cextcall prim args dbg type_of_result] returns Cextcall operation that
    corresponds to [prim]. If [prim] is a C builtin supported on the target,
    returns [Cmm.operation] variant for [prim]'s intrinsics. *)
val cextcall :
  Primitive.description ->
  expression list ->
  Debuginfo.t ->
  machtype ->
  exttype list ->
  bool ->
  expression
