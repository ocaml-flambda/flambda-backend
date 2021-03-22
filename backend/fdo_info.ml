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

type info =
  {
    dbg: Debuginfo.t;
    discriminator: int;
  }
type t = info option
let none = None
let is_none = Option.is_none
let create ~dbg ~discriminator =
  Some
    {
      dbg;
      discriminator;
    }
