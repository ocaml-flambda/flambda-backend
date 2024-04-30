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

(* CR mshinwell: maybe there should be a phantom type allowing to distinguish
   the .cmx case from the others. Unclear it's worth it.

   xclerc: I also wonder whether it could be useful to have an abstract Array.t
   in this module. Indeed the import infos are now mutable; we could hide the
   mutability behind an abstract type. I reckon we use only a handful of array
   operations on such values, so it should not be too bad. If that happens, it
   should probably be in another PR.

   (We could also wait for immutable arrays.) *)

(* CR mshinwell/xclerc: maybe the reading and writing code should be put in
   here, or somewhere alongside, rather than being duplicated around the
   tree. *)

(** Either an interface (.cmi) or implementation (.cmo/x) import. Should be
    avoided in new code, in preference to [Intf.t] or [Impl.t]. *)
type t

val create : CU.Name.t -> crc_with_unit:(CU.t * string) option -> t

val create_normal : CU.t -> crc:string option -> t

val name : t -> CU.Name.t

(** This function will cause a fatal error if a [CU.t] was not provided when the
    supplied value of type [t] was created. *)
val cu : t -> CU.t

val crc : t -> string option

val has_name : t -> name:CU.Name.t -> bool

val dummy : t

(** The preferred API to use for interface imports. An interface import might be
    a parameter, in which case it has a CRC but no [CU.t] (since a [CU.t] is for
    an implementation). *)
module Intf : sig
  type nonrec t = t

  val create_ordinary : CU.Name.t -> CU.t -> crc:Digest.t -> t

  val create_alias : CU.Name.t -> t

  val create_parameter : CU.Name.t -> crc:Digest.t -> t

  module Nonalias : sig
    module Sort : sig
      type t =
        | Ordinary of CU.t
        | Parameter
    end

    (** An [Intf.t] is equivalent to [CU.Name.t * Nonalias.t option] (use [create], [name],
        and [spec] to convert back and forth). *)
    type t = Sort.t * Digest.t
  end

  (** [create name spec] is [create_ordinary name cu crc] if [spec] is [Some (Ordinary cu,
      crc)], [create_parameter name crc] if [spec] is [Some (Parameter, crc)], and
      [create_alias] if [spec] is [None]. Useful when [spec] is coming out of [Consistbl].
  *)
  val create : CU.Name.t -> Nonalias.t option -> t

  val name : t -> CU.Name.t

  val nonalias : t -> Nonalias.t option

  val crc : t -> Digest.t option

  val has_name : t -> name:CU.Name.t -> bool

  val dummy : t
end

module Impl : sig
  type nonrec t = t

  (** The import info for an implementation we depend on and whose .cmx we actually
      loaded. *)
  val create_loaded : CU.t -> crc:Digest.t -> t

  (** The import info for an implementation we depend on but for which we never loaded a
      .cmx (and thus have no CRC for). *)
  val create_unloaded : CU.t -> t

  (** [create cu ~crc] is [create_loaded] if [crc] is [Some] and [create_unloaded] if
      [crc] is [None]. Useful when [crc] is coming out of [Consistbl]. *)
  val create : CU.t -> crc:Digest.t option -> t

  val name : t -> CU.Name.t

  val cu : t -> CU.t

  val crc : t -> Digest.t option

  val dummy : t
end
