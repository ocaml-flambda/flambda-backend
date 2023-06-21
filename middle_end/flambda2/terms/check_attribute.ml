(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = Warnings.Checks.State.t

let print ppf t = Warnings.Checks.State.print ppf t

let from_lambda (c : Lambda.check_attribute)  =
  c.annotated.state

let equal x y = Warnings.Checks.State.equal x y

let default = Warnings.Checks.State.default

let is_default t = (equal t default)
