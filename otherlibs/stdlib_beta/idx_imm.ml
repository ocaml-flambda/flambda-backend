(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Ryan Tjoa, Jane Street, New York                    *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type ('a, 'b : any) t : bits64 = ('a, 'b) idx_imm

external unsafe_get
  : 'a ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_imm -> ('b[@local_opt])
  = "%unsafe_get_idx_imm"
[@@layout_poly]
