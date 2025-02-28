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

let equal_info left right =
  Debuginfo.(Dbg.compare (get_dbg left.dbg) (get_dbg right.dbg) = 0)
  && Int.equal left.discriminator right.discriminator

let equal left right =
  Option.equal equal_info left right
