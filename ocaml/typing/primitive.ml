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

open Misc
open Parsetree

type boxed_integer = Pnativeint | Pint32 | Pint64

type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type mode =
  | Prim_local
  | Prim_global
  | Prim_poly

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_c_builtin: bool;        (* Is the compiler allowed to replace it? *)
    prim_effects: effects;
    prim_coeffects: coeffects;
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: (mode * native_repr) list;
    prim_native_repr_res: mode * native_repr }

type error =
  | Old_style_float_with_native_repr_attribute
  | Old_style_noalloc_with_noalloc_attribute
  | No_native_primitive_with_repr_attribute
  | Inconsistent_attributes_for_effects
  | Inconsistent_noalloc_attributes_for_effects

exception Error of Location.t * error

let is_ocaml_repr = function
  | _, Same_as_ocaml_repr -> true
  | _, Unboxed_float
  | _, Unboxed_integer _
  | _, Untagged_int -> false

let is_unboxed = function
  | _, Same_as_ocaml_repr
  | _, Untagged_int -> false
  | _, Unboxed_float
  | _, Unboxed_integer _ -> true

let is_untagged = function
  | _, Untagged_int -> true
  | _, Same_as_ocaml_repr
  | _, Unboxed_float
  | _, Unboxed_integer _ -> false

let rec make_native_repr_args arity x =
  if arity = 0 then
    []
  else
    x :: make_native_repr_args (arity - 1) x

let simple ~name ~arity ~alloc =
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = alloc;
   prim_c_builtin = false;
   prim_effects = Arbitrary_effects;
   prim_coeffects = Has_coeffects;
   prim_native_name = "";
   prim_native_repr_args =
     make_native_repr_args arity (Prim_global, Same_as_ocaml_repr);
   prim_native_repr_res = (Prim_global, Same_as_ocaml_repr) }

let make ~name ~alloc ~c_builtin ~effects ~coeffects
      ~native_name ~native_repr_args ~native_repr_res =
  {prim_name = name;
   prim_arity = List.length native_repr_args;
   prim_alloc = alloc;
   prim_c_builtin = c_builtin;
   prim_effects = effects;
   prim_coeffects = coeffects;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res }

let parse_declaration valdecl ~native_repr_args ~native_repr_res =
  let arity = List.length native_repr_args in
  let name, native_name, old_style_noalloc, old_style_float =
    match valdecl.pval_prim with
    | name :: "noalloc" :: name2 :: "float" :: _ -> (name, name2, true, true)
    | name :: "noalloc" :: name2 :: _ -> (name, name2, true, false)
    | name :: name2 :: "float" :: _ -> (name, name2, false, true)
    | name :: "noalloc" :: _ -> (name, "", true, false)
    | name :: name2 :: _ -> (name, name2, false, false)
    | name :: _ -> (name, "", false, false)
    | [] ->
        fatal_error "Primitive.parse_declaration"
  in
  let noalloc_attribute =
    Attr_helper.has_no_payload_attribute ["noalloc"; "ocaml.noalloc"]
      valdecl.pval_attributes
  in
  let builtin_attribute =
    Attr_helper.has_no_payload_attribute ["builtin"; "ocaml.builtin"]
      valdecl.pval_attributes
  in
  let no_effects_attribute =
    Attr_helper.has_no_payload_attribute ["no_effects"; "ocaml.no_effects"]
      valdecl.pval_attributes
  in
  let only_generative_effects_attribute =
    Attr_helper.has_no_payload_attribute ["only_generative_effects";
                                          "ocaml.only_generative_effects"]
      valdecl.pval_attributes
  in
  if no_effects_attribute && only_generative_effects_attribute then
    raise (Error (valdecl.pval_loc,
                  Inconsistent_attributes_for_effects));
  let effects =
    if no_effects_attribute then No_effects
    else if only_generative_effects_attribute then Only_generative_effects
    else Arbitrary_effects
  in
  let no_coeffects_attribute =
    Attr_helper.has_no_payload_attribute ["no_coeffects"; "ocaml.no_coeffects"]
      valdecl.pval_attributes
  in
  let coeffects =
    if no_coeffects_attribute then No_coeffects
    else Has_coeffects
  in
  if old_style_float &&
     not (List.for_all is_ocaml_repr native_repr_args &&
          is_ocaml_repr native_repr_res) then
    raise (Error (valdecl.pval_loc,
                  Old_style_float_with_native_repr_attribute));
  if old_style_noalloc && noalloc_attribute then
    raise (Error (valdecl.pval_loc,
                  Old_style_noalloc_with_noalloc_attribute));
  (* The compiler used to assume "noalloc" with "float", we just make this
     explicit now (GPR#167): *)
  let old_style_noalloc = old_style_noalloc || old_style_float in
  if old_style_float then
    Location.deprecated valdecl.pval_loc
      "[@@unboxed] + [@@noalloc] should be used\n\
       instead of \"float\""
  else if old_style_noalloc then
    Location.deprecated valdecl.pval_loc
      "[@@noalloc] should be used instead of \"noalloc\"";
  if native_name = "" &&
     not (List.for_all is_ocaml_repr native_repr_args &&
          is_ocaml_repr native_repr_res) then
    raise (Error (valdecl.pval_loc,
                  No_native_primitive_with_repr_attribute));
  let noalloc = old_style_noalloc || noalloc_attribute in
  if noalloc && only_generative_effects_attribute then
    raise (Error (valdecl.pval_loc,
                  Inconsistent_noalloc_attributes_for_effects));
  let native_repr_args, native_repr_res =
    if old_style_float then
      (make_native_repr_args arity (Prim_global, Unboxed_float),
       (Prim_global, Unboxed_float))
    else
      (native_repr_args, native_repr_res)
  in
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = not noalloc;
   prim_c_builtin = builtin_attribute;
   prim_effects = effects;
   prim_coeffects = coeffects;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res }

open Outcometree

let rec add_native_repr_attributes ty attrs =
  match ty, attrs with
  | Otyp_arrow (label, a, b), attr_opt :: rest ->
    let b = add_native_repr_attributes b rest in
    let a =
      match attr_opt with
      | None -> a
      | Some attr -> Otyp_attribute (a, attr)
    in
    Otyp_arrow (label, a, b)
  | _, [Some attr] -> Otyp_attribute (ty, attr)
  | _ ->
    assert (List.for_all (fun x -> x = None) attrs);
    ty

let oattr_unboxed = { oattr_name = "unboxed" }
let oattr_untagged = { oattr_name = "untagged" }
let oattr_noalloc = { oattr_name = "noalloc" }
let oattr_builtin = { oattr_name = "builtin" }
let oattr_no_effects = { oattr_name = "no_effects" }
let oattr_only_generative_effects = { oattr_name = "only_generative_effects" }
let oattr_no_coeffects = { oattr_name = "no_coeffects" }

let print p osig_val_decl =
  let prims =
    if p.prim_native_name <> "" then
      [p.prim_name; p.prim_native_name]
    else
      [p.prim_name]
  in
  let for_all f =
    List.for_all f p.prim_native_repr_args && f p.prim_native_repr_res
  in
  let all_unboxed = for_all is_unboxed in
  let all_untagged = for_all is_untagged in
  let attrs = if p.prim_alloc then [] else [oattr_noalloc] in
  let attrs = if p.prim_c_builtin then oattr_builtin::attrs else attrs in
  let attrs = match p.prim_effects with
    | No_effects -> oattr_no_effects::attrs
    | Only_generative_effects -> oattr_only_generative_effects::attrs
    | Arbitrary_effects -> attrs
  in
  let attrs = match p.prim_coeffects with
    | No_coeffects -> oattr_no_coeffects::attrs
    | Has_coeffects -> attrs
  in
  let attrs =
    if all_unboxed then
      oattr_unboxed :: attrs
    else if all_untagged then
      oattr_untagged :: attrs
    else
      attrs
  in
  let attr_of_native_repr = function
    | _, Same_as_ocaml_repr -> None
    | _, Unboxed_float
    | _, Unboxed_integer _ -> if all_unboxed then None else Some oattr_unboxed
    | _, Untagged_int -> if all_untagged then None else Some oattr_untagged
  in
  let type_attrs =
    List.map attr_of_native_repr p.prim_native_repr_args @
    [attr_of_native_repr p.prim_native_repr_res]
  in
  { osig_val_decl with
    oval_prims = prims;
    oval_type = add_native_repr_attributes osig_val_decl.oval_type type_attrs;
    oval_attributes = attrs }

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name

let native_name_is_external p =
  let nat_name = native_name p in
  nat_name <> "" && nat_name.[0] <> '%'

let inst_mode mode p =
  let inst_repr = function
    | Prim_poly, r -> mode, r
    | (Prim_global|Prim_local) as m, r -> m, r
  in
  { p with
    prim_native_repr_args = List.map inst_repr p.prim_native_repr_args;
    prim_native_repr_res = inst_repr p.prim_native_repr_res }

let report_error ppf err =
  match err with
  | Old_style_float_with_native_repr_attribute ->
    Format.fprintf ppf "Cannot use \"float\" in conjunction with \
                        [%@unboxed]/[%@untagged]."
  | Old_style_noalloc_with_noalloc_attribute ->
    Format.fprintf ppf "Cannot use \"noalloc\" in conjunction with \
                        [%@%@noalloc]."
  | No_native_primitive_with_repr_attribute ->
    Format.fprintf ppf
      "[@The native code version of the primitive is mandatory@ \
       when attributes [%@untagged] or [%@unboxed] are present.@]"
  | Inconsistent_attributes_for_effects ->
    Format.fprintf ppf "At most one of [%@no_effects] and \
                        [%@only_generative_effects] can be specified."
  | Inconsistent_noalloc_attributes_for_effects ->
    Format.fprintf ppf "Cannot use [%@%@no_generative_effects] \
                        in conjunction with [%@%@noalloc]."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
