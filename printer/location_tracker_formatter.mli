(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Mappings : sig
  module Item : sig
    type t
    val source : t -> Location.t
    val ir : t -> Location.t
    val label : t -> string option
  end
  type t = Item.t list
  val print : Format.formatter -> t -> unit
  val dump : file:string -> t -> unit
end

val activate_tracking : unit -> unit
val deactivate_tracking : unit -> unit
val is_tracking : unit -> bool

val with_location_mapping :
  ?label:string
  -> loc:Location.t
  -> Format.formatter
  -> (unit -> 'a)
  -> 'a

module Tracking_formatter : sig
  type t

  val create : file:string -> ppf:Format.formatter -> t
  val close : t -> unit
  val ppf : t -> Format.formatter
  val mappings : t -> Mappings.t
end
