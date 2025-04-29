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

(* Description of primitive functions *)

type unboxed_integer = Unboxed_int64 | Unboxed_nativeint | Unboxed_int32
[@@deriving compare, enumerate, equal]
type unboxed_float = Unboxed_float64 | Unboxed_float32
[@@deriving compare, enumerate, equal]
type unboxed_vector = Unboxed_vec128
[@@deriving compare, enumerate, equal]

type boxed_integer = Boxed_int64 | Boxed_nativeint | Boxed_int32
[@@deriving compare, enumerate, equal]
type boxed_float = Boxed_float64 | Boxed_float32
[@@deriving compare, enumerate, equal]
type boxed_vector = Boxed_vec128
[@@deriving compare, enumerate, equal]

(* Representation of arguments/result for the native code version
   of a primitive *)
type native_repr =
  | Repr_poly
  | Same_as_ocaml_repr of Jkind_types.Sort.Const.t
  | Unboxed_float of boxed_float
  | Unboxed_vector of boxed_vector
  | Unboxed_integer of boxed_integer
  | Untagged_immediate
[@@deriving equal]

(* See [middle_end/semantics_of_primitives.mli] *)
type effects = No_effects | Only_generative_effects | Arbitrary_effects
[@@deriving compare, enumerate, equal]
type coeffects = No_coeffects | Has_coeffects
[@@deriving compare, enumerate, equal]

type mode =
  | Prim_local
  | Prim_global
  | Prim_poly
(* [Prim_poly] arguments and results are subject to mode inference,
   allowing e.g. (+.) to work on local or global floats. After
   typechecking, all [Prim_poly] modes on a given primitive application
   will be instantiated either all to [Local] or all to [Global] *)

type 'repr description_gen = private
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_c_builtin: bool;
    (* When [prim_c_builtin] is true, the native compiler is allowed to rewrite
       calls to the external C function that implements this primitive,
       based on its name [prim_name], into a predetermined instruction sequence.
       [prim_c_builtin] is ignored on compiler primitives
       whose name [prim_name] starts with %. *)
    prim_effects: effects;
    prim_coeffects: coeffects;
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: (mode * 'repr) list;
    prim_native_repr_res: mode * 'repr;
    prim_is_layout_poly: bool }

type description = native_repr description_gen

(* Invariant [List.length d.prim_native_repr_args = d.prim_arity] *)

(** [make_prim_repr_args n x] is the same as [List.init n (fun _ -> x)].
    Keeping this function here to be close to upstream. It's
    intended for use with [make]. *)
val make_prim_repr_args : int -> 'a -> 'a list

val make
  :  name:string
  -> alloc:bool
  -> c_builtin:bool
  -> effects:effects
  -> coeffects:coeffects
  -> native_name:string
  -> native_repr_args: (mode * 'repr) list
  -> native_repr_res: mode * 'repr
  -> is_layout_poly: bool
  -> 'repr description_gen

val parse_declaration
  :  Parsetree.value_description
  -> native_repr_args:(mode * native_repr) list
  -> native_repr_res:(mode * native_repr)
  -> is_layout_poly:bool
  -> description

val print
  :  description
  -> Outcometree.out_val_decl
  -> Outcometree.out_val_decl

val native_name: 'a description_gen -> string
val byte_name: 'a description_gen -> string


val unboxed_float : boxed_float -> unboxed_float
val unboxed_integer : boxed_integer -> unboxed_integer
val unboxed_vector : boxed_vector -> unboxed_vector

(** [native_name_is_externa] returns [true] iff the [native_name] for the
    given primitive identifies that the primitive is not implemented in the
    compiler itself. *)
val native_name_is_external : description -> bool

(** Check if a primitive has the correct native representations for its
    argument/return types. This check is done based on the primitive name and
    only imposes constraints on built-in primitives. Raises if the check
    fails. *)
val prim_has_valid_reprs : loc:Location.t -> description -> unit

(** Check if a primitive can have layout [any] anywhere within its type
    declaration. Returns [false] for built-in primitives that inspect
    the layout of type parameters ([%array_length] for example). *)
val prim_can_contain_layout_any : description -> bool

type error =
  | Old_style_float_with_native_repr_attribute
  | Old_style_float_with_non_value
  | Old_style_noalloc_with_noalloc_attribute
  | No_native_primitive_with_repr_attribute
  | No_native_primitive_with_non_value
  | Inconsistent_attributes_for_effects
  | Inconsistent_noalloc_attributes_for_effects
  | Invalid_representation_polymorphic_attribute
  | Invalid_native_repr_for_primitive of string

exception Error of Location.t * error
