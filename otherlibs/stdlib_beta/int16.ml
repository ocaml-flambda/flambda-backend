(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type t = int16

include
  Int_wrapper.Make
    (Int_wrapper)
    (struct
      type nonrec t = t

      let int_size = 16

      external inject : t -> int = "%identity"

      external unchecked_project : int -> t = "%identity"
    end)
