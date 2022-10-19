(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Chambart, Nathanaëlle Courant, OCamlPro             *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** file sections cache *)

type t

val create : int array -> string -> in_channel -> first_section_offset:int -> t

val empty : t

val length : t -> int

val get : t -> int -> Obj.t

val to_array : t -> Obj.t array

val serialize : t -> string array * int array * int

val from_array : Obj.t array -> t

val concat : t -> t -> t
