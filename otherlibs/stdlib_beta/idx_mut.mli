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

(** Mutable indices into blocks. *)

(** An alias for the type of mutable indices into blocks. *)
type ('a, 'b : any) t : bits64 = ('a, 'b) idx_mut

(** [unsafe_get a i] uses the index [i] to access [a].

    It is unsafe, and may result in an out-of-bounds access, only for indices
    created outside the bounds of an array. *)
external unsafe_get
  : 'a ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> ('b[@local_opt])
  = "%unsafe_get_idx"
[@@layout_poly]

(** [unsafe_set a i v] uses the index [i] to set [a] to [v].

    It is unsafe, and may result in an out-of-bounds access, only for indices
    created outside the bounds of an array.

    It can take [a] locally and [v] globally because mutable indices (e.g. to
    array elements or mutable record fields) can only be created to elements
    with the [global] modality. *)
external unsafe_set
  : 'a ('b : any).
  ('a[@local_opt]) -> ('a, 'b) idx_mut -> 'b -> unit
  = "%unsafe_set_idx"
[@@layout_poly]
