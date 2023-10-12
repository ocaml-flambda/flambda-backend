# 1 "obj.mli"
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

<<<<<<< HEAD
    [set_field] and [set_double_field] MUST NOT be called on immutable
    blocks.  (Blocks allocated in C stubs, or with [new_block] below,
    are always considered mutable.)
||||||| merged common ancestors
    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field] and [set_tag].  However, for
    [set_tag], in the case of immutable blocks where the middle-end optimizers
    never see code that discriminates on their tag (for example records), the
    operation should be safe.  Such uses are nonetheless discouraged.
=======
    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field].
>>>>>>> ocaml/5.1

    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    {!Sys.opaque_identity}, so any information about its contents will not
    be propagated.
*)
<<<<<<< HEAD
val set_field : t -> int -> t -> unit
||||||| merged common ancestors
external set_field : t -> int -> t -> unit = "%obj_set_field"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
  [@@ocaml.deprecated "Use with_tag instead."]
=======
external set_field : t -> int -> t -> unit = "%obj_set_field"
>>>>>>> ocaml/5.1

val double_field : t -> int -> float  (* @since 3.11.2 *)
val set_double_field : t -> int -> float -> unit
  (* @since 3.11.2 *)

external raw_field : t -> int -> raw_data = "caml_obj_raw_field"
  (* @since 4.12 *)
external set_raw_field : t -> int -> raw_data -> unit
                                          = "caml_obj_set_raw_field"
  (* @since 4.12 *)

external new_block : int -> int -> t = "caml_obj_block"
<<<<<<< HEAD

external dup : t -> t = "%obj_dup"
(** [dup t] returns a shallow copy of [t].  However if [t] is immutable then
    it might be returned unchanged. *)

||||||| merged common ancestors
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
  [@@ocaml.deprecated]
=======
external dup : t -> t = "caml_obj_dup"
>>>>>>> ocaml/5.1
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

module Closure : sig
  type info = {
    arity: int;
    start_env: int;
  }
  val info : t -> info
end

module Extension_constructor :
sig
  type t = extension_constructor
  val of_val : 'a -> t
  val name : t -> string
  val id : t -> int
end
<<<<<<< HEAD
val extension_constructor : 'a -> extension_constructor
  [@@ocaml.deprecated "use Obj.Extension_constructor.of_val"]
val extension_name : extension_constructor -> string
  [@@ocaml.deprecated "use Obj.Extension_constructor.name"]
val extension_id : extension_constructor -> int
  [@@ocaml.deprecated "use Obj.Extension_constructor.id"]
||||||| merged common ancestors
val extension_constructor : 'a -> extension_constructor
  [@@ocaml.deprecated "use Obj.Extension_constructor.of_val"]
val [@inline always] extension_name : extension_constructor -> string
  [@@ocaml.deprecated "use Obj.Extension_constructor.name"]
val [@inline always] extension_id : extension_constructor -> int
  [@@ocaml.deprecated "use Obj.Extension_constructor.id"]
=======
>>>>>>> ocaml/5.1

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
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.get_key} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.get_key} *)
=======
>>>>>>> ocaml/5.1

  val get_key_copy: t -> int -> obj_t option
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.get_key_copy} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.get_key_copy} *)
=======
>>>>>>> ocaml/5.1

  val set_key: t -> int -> obj_t -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.set_key} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.set_key} *)
=======
>>>>>>> ocaml/5.1

  val unset_key: t -> int -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.unset_key} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.unset_key} *)
=======
>>>>>>> ocaml/5.1

  val check_key: t -> int -> bool
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.check_key} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.check_key} *)
=======
>>>>>>> ocaml/5.1

  val blit_key : t -> int -> t -> int -> int -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.blit_key} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.blit_key} *)
=======
>>>>>>> ocaml/5.1

  val get_data: t -> obj_t option
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.get_data} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.get_data} *)
=======
>>>>>>> ocaml/5.1

  val get_data_copy: t -> obj_t option
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.get_data_copy} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.get_data_copy} *)
=======
>>>>>>> ocaml/5.1

  val set_data: t -> obj_t -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.set_data} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.set_data} *)
=======
>>>>>>> ocaml/5.1

  val unset_data: t -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.unset_data} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.unset_data} *)
=======
>>>>>>> ocaml/5.1

  val check_data: t -> bool
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.check_data} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.check_data} *)
=======
>>>>>>> ocaml/5.1

  val blit_data : t -> t -> unit
<<<<<<< HEAD
  (** Same as {!Stdlib.Ephemeron.K1.blit_data} *)
||||||| merged common ancestors
  (** Same as {!Ephemeron.K1.blit_data} *)
=======
>>>>>>> ocaml/5.1

  val max_ephe_length: int
  (** Maximum length of an ephemeron, ie the maximum number of keys an
      ephemeron could contain *)
end
