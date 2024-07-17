# 2 "obj.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Operations on internal representations of values.

   Not for the casual user.
*)

type t

type raw_data = nativeint  (* @since 4.12 *)

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%obj_magic"
val is_block : t -> bool
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag" [@@noalloc]
val size : t -> int
val reachable_words : t -> int
  (**
     Computes the total size (in words, including the headers) of all
     heap blocks accessible from the argument.  Statically
     allocated blocks are included.

     @since 4.04
  *)

val uniquely_reachable_words : t array -> int array * int
(** For each element of the array, computes the total size (as defined
    above by [reachable_words]) of all heap blocks accessible from the
    argument but excluding all blocks accessible from any other arguments.

    Also returns a single number denoting the total memory reachable from
    at least two of the roots. We make no attempt to classify which two
    (or more) roots are responsible for this memory.
  *)

val field : t -> int -> t

(** When using flambda:

    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field].

    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    {!Sys.opaque_identity}, so any information about its contents will not
    be propagated.
*)
val set_field : t -> int -> t -> unit

val double_field : t -> int -> float  (* @since 3.11.2 *)
val set_double_field : t -> int -> float -> unit
  (* @since 3.11.2 *)

external raw_field : t -> int -> raw_data = "caml_obj_raw_field"
  (* @since 4.12 *)
external set_raw_field : t -> int -> raw_data -> unit
                                          = "caml_obj_set_raw_field"
  (* @since 4.12 *)

external new_block : int -> int -> t = "caml_obj_block"

external dup : t -> t = "%obj_dup"
(** [dup t] returns a shallow copy of [t].  However if [t] is immutable then
    it might be returned unchanged. *)

external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
         (* @since 3.12 *)
external with_tag : int -> t -> t = "caml_obj_with_tag"
  (* @since 4.09 *)

val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int

val forcing_tag : int
val cont_tag : int
val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int   (* both [string] and [bytes] *)
val double_tag : int
val double_array_tag : int
val custom_tag : int

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int   (* should never happen @since 3.11 *)

module Extension_constructor :
sig
  type t = extension_constructor
  val of_val : 'a -> t
  val name : t -> string
  val id : t -> int
end

module Ephemeron: sig
  (** Ephemeron with arbitrary arity and untyped *)

  type obj_t = t
  (** alias for {!Obj.t} *)

  type t
  (** an ephemeron cf {!Ephemeron} *)

  val create: int -> t
  (** [create n] returns an ephemeron with [n] keys.
      All the keys and the data are initially empty.
      The argument [n] must be between zero
      and {!max_ephe_length} (limits included).
  *)

  val length: t -> int
  (** return the number of keys *)

  val get_key: t -> int -> obj_t option

  val get_key_copy: t -> int -> obj_t option

  val set_key: t -> int -> obj_t -> unit

  val unset_key: t -> int -> unit

  val check_key: t -> int -> bool

  val blit_key : t -> int -> t -> int -> int -> unit

  val get_data: t -> obj_t option

  val get_data_copy: t -> obj_t option

  val set_data: t -> obj_t -> unit

  val unset_data: t -> unit

  val check_data: t -> bool

  val blit_data : t -> t -> unit

  val max_ephe_length: int
  (** Maximum length of an ephemeron, ie the maximum number of keys an
      ephemeron could contain *)
end

module Uniform_or_mixed : sig
  (** Blocks with a nominally scannable tag can still have a suffix of
      unscanned objects; such a block is "mixed". This contrasts with
      "uniform" blocks which are either all-scanned or all-unscanned.

      Note that this module can return different results for the scannable
      prefix len of a mixed block in native code vs. bytecode. That's
      because more fields are scanned in bytecode.
  *)

  type obj_t := t

  type t [@@immediate]

  type repr =
    | Uniform
    (** The block is tagged as not scannable or the block is tagged as scannable
        and all fields can be scanned. *)
    | Mixed of { scannable_prefix_len : int }
    (** The block is tagged as scannable but some fields can't be scanned. *)

  val repr : t -> repr

  external of_block : obj_t -> t = "caml_succ_scannable_prefix_len" [@@noalloc]

  val is_uniform : t -> bool
  (** Equivalent to [repr] returning [Uniform]. *)

  val is_mixed : t -> bool
  (** Equivalent to [repr] returning [Mixed _]. *)

  val mixed_scannable_prefix_len_exn : t -> int
  (** Returns the [scannable_prefix_len] without materializing the return
      value of [repr]. Raises if [is_mixed] is [false]. *)
end
