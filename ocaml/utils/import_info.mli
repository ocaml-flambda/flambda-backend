(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Mark Shinwell, Jane Street UK Partnership LLP              *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CU := Compilation_unit

(* CR xclerc: I wonder whether it could be useful to have an abstract Array.t in
   this module. Indeed the import infos are now mutable; we could hide the
   mutability behind an abstract type. I reckon we use only a handful of array
   operations on such values, so it should not be too bad. If that happens, it
   should probably be in another PR.

   (We could also wait for immutable arrays.) *)

(* CR mshinwell/xclerc: maybe the reading and writing code should be put in
   here, or somewhere alongside, rather than being duplicated around the
   tree. *)

module Intf : sig
  type t

  val create : CU.Name.t -> crc_with_unit:(CU.t * Digest.t) option -> t

  val name : t -> CU.Name.t

  val impl : t -> CU.t option

  val crc : t -> Digest.t option

  val crc_with_unit : t -> (CU.t * Digest.t) option

  val has_name : t -> name:CU.Name.t -> bool

  val dummy : t
end

module Impl : sig
  type t

  val create : CU.t -> crc:Digest.t option -> t

  val name : t -> CU.Name.t

  val cu : t -> CU.t

  val crc : t -> Digest.t option

  val dummy : t
end
