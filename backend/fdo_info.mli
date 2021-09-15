(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(** [Fdo_info] identifies instructions in low-level intermediate representation
    for the purpose of mapping execution counters directly back
    to them, instead of source locations.

    [Fdo_info] is added to Linear by ocamlfdo tool,
    and then emitted by the compiler as a discriminator on .loc directive.
    By default, the compiler uses [dbg] field of [Linear.instruction]
    to emit .loc directives.
    There are a couple of obstacles to using [dbg] field for FDO directly:
    1) [Debuginfo.item] does not have a field for discriminators.
       Adding it requires changes to the upstream part of the compiler,
       which is unmodified in flambda-backend.
    2) Many instructions do not have any associated Debuginfo,
       which is required to output .loc directives. To address it,
       ocamlfdo infers the missing ones, using the semantics of debug_line.
       The inferred information cannot be stored in [dbg] field,
       because [dbg] fields is also used for emitting frametable's debug info
       entries required for backtraces. FDO info should not affect backtraces.
*)
type info = private
  {
    dbg: Debuginfo.t;
    discriminator: int;
  }
type t = info option
val none : t
val is_none : t -> bool
val create : dbg:Debuginfo.t -> discriminator:int -> t

val equal_info : info -> info -> bool
val equal : t -> t -> bool
