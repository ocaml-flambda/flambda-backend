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

type boxed_integer = Pnativeint | Pint32 | Pint64

(* Representation of arguments/result for the native code version
   of a primitive *)
type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

(* See [middle_end/semantics_of_primitives.mli] *)
type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type mode =
  | Prim_local
  | Prim_global
  | Prim_poly
(* [Prim_poly] arguments and results are subject to mode inference,
   allowing e.g. (+.) to work on local or global floats. After
   typechecking, all [Prim_poly] modes on a given primitive application
   will be instantiated either all to [Local] or all to [Global] *)

type description = private
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
    prim_native_repr_args: (mode * native_repr) list;
    prim_native_repr_res: mode * native_repr }

(* Invariant [List.length d.prim_native_repr_args = d.prim_arity] *)

val simple
  :  name:string
  -> arity:int
  -> alloc:bool
  -> description

val make
  :  name:string
  -> alloc:bool
  -> c_builtin:bool
  -> effects:effects
  -> coeffects:coeffects
  -> native_name:string
  -> native_repr_args: (mode*native_repr) list
  -> native_repr_res: mode*native_repr
  -> description

val parse_declaration
  :  Parsetree.value_description
  -> native_repr_args:(mode*native_repr) list
  -> native_repr_res:(mode*native_repr)
  -> description

val print
  :  description
  -> Outcometree.out_val_decl
  -> Outcometree.out_val_decl

val native_name: description -> string
val byte_name: description -> string

(** [native_name_is_externa] returns [true] iff the [native_name] for the
    given primitive identifies that the primitive is not implemented in the
    compiler itself. *)
val native_name_is_external : description -> bool

val inst_mode : mode -> description -> description

type error =
  | Old_style_float_with_native_repr_attribute
  | Old_style_noalloc_with_noalloc_attribute
  | No_native_primitive_with_repr_attribute
  | Inconsistent_attributes_for_effects
  | Inconsistent_noalloc_attributes_for_effects

exception Error of Location.t * error
