(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type outcome =
  | Accept
  | Skip

module Make (Iterator : Leapfrog.Iterator) : sig
  type ('a, 'y, 's) instruction

  val advance : ('a, 'y, 's) instruction

  val pop : ('a, 'y, 's) instruction -> ('a, 'y, _ -> 's) instruction

  val open_ :
    'i Iterator.t -> ('a, 'y, 'i -> 's) instruction -> ('a, 'y, 's) instruction

  val action : 'a -> ('a, 'y, 's) instruction -> ('a, 'y, 's) instruction

  val yield :
    'y Heterogenous_list.Option_ref.hlist ->
    ('x, 'y Heterogenous_list.Constant.hlist, 's) instruction ->
    ('x, 'y Heterogenous_list.Constant.hlist, 's) instruction

  val set_output :
    'o option ref ->
    ('a, 'y, 'o -> 's) instruction ->
    ('a, 'y, 'o -> 's) instruction

  type 'a t

  val iterator : 's Iterator.hlist -> 's Heterogenous_list.Constant.hlist t

  type ('i, 'x, 'y, 's) compiled

  val compile :
    evaluate:('a -> 'i -> outcome) ->
    ('a, 'y, 's) instruction ->
    ('i, 'a, 'y, 's) compiled

  val create : 'i -> ('i, 'a, 'y, Heterogenous_list.nil) compiled -> 'y t

  val fold : ('y -> 'a -> 'a) -> 'y t -> 'a -> 'a

  val iter : ('y -> unit) -> 'y t -> unit
end
