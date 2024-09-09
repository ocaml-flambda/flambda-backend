(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent messages that the toplevel displays as normal
   results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

(** An [out_name] is a string representation of an identifier which can be
    rewritten on the fly to avoid name collisions *)
type out_name = { mutable printed_name: string }

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of out_name

type out_string =
  | Ostr_string
  | Ostr_bytes

type out_attribute =
  { oattr_name: string }

type out_value =
  | Oval_array of out_value list * Asttypes.mutable_flag
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_float32 of Obj.t (* We cannot use the [float32] type in the compiler. *)
  | Oval_int of int
  | Oval_int32 of int32
  | Oval_int64 of int64
  | Oval_nativeint of nativeint
  | Oval_list of out_value list
  | Oval_printer of (Format.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string * int * out_string (* string, size-to-print, kind *)
  | Oval_stuff of string
  | Oval_tuple of (string option * out_value) list
  | Oval_unboxed_tuple of (string option * out_value) list
  | Oval_variant of string * out_value option
  | Oval_lazy of out_value
type out_modality_legacy = Ogf_global

type out_modality_new = string

type out_modality =
  | Ogf_legacy of out_modality_legacy
  | Ogf_new of out_modality_new

type out_mutability =
  | Om_immutable
  | Om_mutable of string option



(** This definition avoids a cyclic dependency between Outcometree and Types. *)
type arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

type out_mode_legacy =
  | Omd_local
  | Omd_unique
  | Omd_once

type out_mode_new = string

type out_mode =
  | Omd_legacy of out_mode_legacy
  | Omd_new of out_mode_new

type out_arg_mode = out_mode list

type out_ret_mode =
  | Orm_not_arrow of out_mode list
  (** The ret type is not arrow, with modes annotating. *)
  | Orm_no_parens
  (** The ret type is arrow, and no need to print parens around the arrow *)
  | Orm_parens of out_mode list
  (** The ret type is arrow, and need to print parens around the arrow, with
      modes annotating. *)

(** Represents a constant jkind *)
type out_jkind_const =
  | Ojkind_const_default
  | Ojkind_const_abbreviation of string
  | Ojkind_const_mod of out_jkind_const * string list
  | Ojkind_const_with of out_jkind_const * out_type
  | Ojkind_const_kind_of of out_type
  | Ojkind_const_product of out_jkind_const list

and out_jkind =
  | Ojkind_const of out_jkind_const
  | Ojkind_var of string

(* should be empty if all the jkind annotations are missing *)
and out_vars_jkinds = (string * out_jkind option) list

and out_type_param = {
    ot_non_gen: bool;
    ot_name: string;
    ot_variance: Asttypes.variance * Asttypes.injectivity;
    ot_jkind : out_jkind option;
}

and out_type =
  | Otyp_abstract
  | Otyp_open
  | Otyp_alias of {non_gen:bool; aliased:out_type; alias:string}
  | Otyp_arrow of arg_label * out_arg_mode * out_type * out_ret_mode * out_type
  (* INVARIANT: the [out_ret_mode] is [Orm_not_arrow] unless the RHS [out_type]
    is [Otyp_arrow] *)
  | Otyp_class of out_ident * out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of { fields: (string * out_type) list; open_row:bool}
  | Otyp_record of (string * out_mutability * out_type * out_modality list) list
  | Otyp_stuff of string
  | Otyp_sum of out_constructor list
  | Otyp_tuple of (string option * out_type) list
  | Otyp_unboxed_tuple of (string option * out_type) list
  | Otyp_var of bool * string
  | Otyp_variant of out_variant * bool * (string list) option
  | Otyp_poly of out_vars_jkinds * out_type
  | Otyp_module of out_ident * (string * out_type) list
  | Otyp_attribute of out_type * out_attribute
  | Otyp_jkind_annot of out_type * out_jkind
      (* Currently only introduced with very explicit code in [Printtyp] and not
         synthesized directly from the [Typedtree] *)

and out_constructor = {
  ocstr_name: string;
  ocstr_args: (out_type * out_modality list) list;
  ocstr_return_type: (out_vars_jkinds * out_type) option;
}

and out_variant =
  | Ovar_fields of (string * bool * out_type list) list
  | Ovar_typ of out_type

type out_class_type =
  | Octy_constr of out_ident * out_type list
  | Octy_arrow of arg_label * out_type * out_class_type
  | Octy_signature of out_type option * out_class_sig_item list
and out_class_sig_item =
  | Ocsg_constraint of out_type * out_type
  | Ocsg_method of string * bool * bool * out_type
  | Ocsg_value of string * bool * bool * out_type

type out_module_type =
  | Omty_abstract
  | Omty_functor of (string option * out_module_type) option * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
  | Omty_alias of out_ident
  | Omty_strengthen of out_module_type * out_ident * bool
        (* the bool indicates whether we should print the unaliasable attribute *)
and out_sig_item =
  | Osig_class of
      bool * string * out_type_param list * out_class_type *
        out_rec_status
  | Osig_class_type of
      bool * string * out_type_param list * out_class_type *
        out_rec_status
  | Osig_typext of out_extension_constructor * out_ext_status
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type * out_rec_status
  | Osig_type of out_type_decl * out_rec_status
  | Osig_value of out_val_decl
  | Osig_ellipsis
and out_type_decl =
  { otype_name: string;
    otype_params: out_type_param list;
    otype_type: out_type;
    otype_private: Asttypes.private_flag;

    (* Some <=> we should print this annotation;
       see Note [When to print jkind annotations] in Printtyp, Case (C1) *)
    otype_jkind: out_jkind option;

    otype_unboxed: bool;
    otype_cstrs: (out_type * out_type) list }
and out_extension_constructor =
  { oext_name: string;
    oext_type_name: string;
    oext_type_params: string list;
    oext_args: (out_type * out_modality list) list;
    oext_ret_type: (out_vars_jkinds * out_type) option;
    oext_private: Asttypes.private_flag }
and out_type_extension =
  { otyext_name: string;
    otyext_params: string list;
    otyext_constructors: out_constructor list;
    otyext_private: Asttypes.private_flag }
and out_val_decl =
  { oval_name: string;
    oval_type: out_type;
    oval_modalities : out_modality_new list;
    (* Modalities on value descriptions are always new, even for [global_] *)
    oval_prims: string list;
    oval_attributes: out_attribute list }
and out_rec_status =
  | Orec_not
  | Orec_first
  | Orec_next
and out_ext_status =
  | Oext_first
  | Oext_next
  | Oext_exception

type out_phrase =
  | Ophr_eval of out_value * out_type
  | Ophr_signature of (out_sig_item * out_value option) list
  | Ophr_exception of (exn * out_value)
