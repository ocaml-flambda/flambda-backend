(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Pierre Chambart, OCamlPro                       *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type atom =
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int of Lambda.boxed_integer
  | Unboxed_vector of Lambda.boxed_vector

val fold_left_layout :
  ('acc -> Clambda.ulambda -> atom -> 'acc) ->
  'acc ->
  Clambda.ulambda ->
  Clambda_primitives.layout ->
  'acc

type decomposition =
  | Atom of
      { offset : int;
        layout : atom
      }
  | Product of decomposition array

val equal_decomposition : decomposition -> decomposition -> bool

val print_decomposition : Format.formatter -> decomposition -> unit

val decompose_free_vars :
  base_offset:int ->
  free_vars:('a * Clambda_primitives.layout) list ->
  ('a * decomposition) list
