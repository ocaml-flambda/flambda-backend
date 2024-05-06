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

(* jkind depends on types defined in this file, but Jkind.Sort.equal_const is required
   to implement equal_native_repr. When jkind.ml is loaded, it fills this ref with
   the definition of Jkind.Sort.equal_const *)
let jkind_sort_equal_const = ref (fun _ _ ->
    failwith "jkind_sort_equal_const should be set by jkind.ml")

type boxed_integer = Pnativeint | Pint32 | Pint64

type vec128_type = Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2

type boxed_float = Pfloat64 | Pfloat32

type boxed_vector = Pvec128 of vec128_type

type native_repr =
  | Repr_poly
  | Same_as_ocaml_repr of Jkind_types.Sort.const
  | Unboxed_float of boxed_float
  | Unboxed_vector of boxed_vector
  | Unboxed_integer of boxed_integer
  | Untagged_int

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type mode =
  | Prim_local
  | Prim_global
  | Prim_poly

type 'repr description_gen =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_c_builtin: bool;        (* Is the compiler allowed to replace it? *)
    prim_effects: effects;
    prim_coeffects: coeffects;
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: (mode * 'repr) list;
    prim_native_repr_res: mode * 'repr;
    prim_is_layout_poly: bool }

type description = native_repr description_gen

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

type value_check = Bad_attribute | Bad_layout | Ok_value

let check_ocaml_value = function
  | _, Same_as_ocaml_repr Value -> Ok_value
  | _, Same_as_ocaml_repr _
  | _, Repr_poly -> Bad_layout
  | _, Unboxed_float _
  | _, Unboxed_vector _
  | _, Unboxed_integer _
  | _, Untagged_int -> Bad_attribute

let is_builtin_prim_name name = String.length name > 0 && name.[0] = '%'

let rec make_prim_repr_args arity x =
  if arity = 0 then
    []
  else
    x :: make_prim_repr_args (arity - 1) x

let make ~name ~alloc ~c_builtin ~effects ~coeffects
      ~native_name ~native_repr_args ~native_repr_res
      ~is_layout_poly =
  {prim_name = name;
   prim_arity = List.length native_repr_args;
   prim_alloc = alloc;
   prim_c_builtin = c_builtin;
   prim_effects = effects;
   prim_coeffects = coeffects;
   prim_native_name = native_name;
   prim_native_repr_args = native_repr_args;
   prim_native_repr_res = native_repr_res;
   prim_is_layout_poly = is_layout_poly }

let parse_declaration valdecl ~native_repr_args ~native_repr_res ~is_layout_poly =
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
  let is_builtin_prim = is_builtin_prim_name name in
  let prim_is_layout_poly =
    match is_builtin_prim, is_layout_poly with
    | false, true ->  raise (Error (valdecl.pval_loc,
                        Invalid_representation_polymorphic_attribute))
    | _, b -> b
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
  if old_style_float then
    List.iter
      (fun repr -> match check_ocaml_value repr with
         | Ok_value -> ()
         | Bad_attribute ->
           raise (Error (valdecl.pval_loc,
                         Old_style_float_with_native_repr_attribute))
         | Bad_layout ->
           raise (Error (valdecl.pval_loc,
                         Old_style_float_with_non_value)))
      (native_repr_res :: native_repr_args);
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
  if native_name = "" then
    List.iter
      (fun repr -> match check_ocaml_value repr with
         | Ok_value -> ()
         | Bad_attribute ->
           raise (Error (valdecl.pval_loc,
                         No_native_primitive_with_repr_attribute))
         | Bad_layout ->
           (* Built-in primitives don't need a native version. *)
           if not is_builtin_prim then
             raise (Error (valdecl.pval_loc,
                           No_native_primitive_with_non_value)))
      (native_repr_res :: native_repr_args);
  let noalloc = old_style_noalloc || noalloc_attribute in
  if noalloc && only_generative_effects_attribute then
    raise (Error (valdecl.pval_loc,
                  Inconsistent_noalloc_attributes_for_effects));
  let native_repr_args, native_repr_res =
    if old_style_float then
      (make_prim_repr_args arity (Prim_global, Unboxed_float Pfloat64),
       (Prim_global, Unboxed_float Pfloat64))
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
   prim_native_repr_res = native_repr_res;
   prim_is_layout_poly }

open Outcometree

let add_attribute_list ty attrs =
  List.fold_left (fun ty attr -> Otyp_attribute(ty, attr)) ty attrs

let rec add_native_repr_attributes ty attrs =
  match ty, attrs with
    (* Otyp_poly case might have been added in e.g. tree_of_value_description *)
  | Otyp_poly (vars, ty), _ -> Otyp_poly (vars, add_native_repr_attributes ty attrs)
  | Otyp_arrow (label, am, a, rm, r), attr_l :: rest ->
    let r = add_native_repr_attributes r rest in
    let a = add_attribute_list a attr_l in
    Otyp_arrow (label, am, a, rm, r)
  | _, [attr_l] -> add_attribute_list ty attr_l
  | _ ->
    assert (List.for_all (fun x -> x = []) attrs);
    ty

let oattr_unboxed = { oattr_name = "unboxed" }
let oattr_untagged = { oattr_name = "untagged" }
let oattr_noalloc = { oattr_name = "noalloc" }
let oattr_builtin = { oattr_name = "builtin" }
let oattr_no_effects = { oattr_name = "no_effects" }
let oattr_only_generative_effects = { oattr_name = "only_generative_effects" }
let oattr_no_coeffects = { oattr_name = "no_coeffects" }
let oattr_local_opt = { oattr_name = "local_opt" }
let oattr_layout_poly = { oattr_name = "layout_poly" }

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
  let is_unboxed = function
    | _, Same_as_ocaml_repr Value
    | _, Repr_poly
    | _, Untagged_int -> false
    | _, Unboxed_float _
    | _, Unboxed_vector _
    | _, Unboxed_integer _ -> true
    | _, Same_as_ocaml_repr _ ->
      (* We require [@unboxed] for non-value types in upstream-compatible code,
         but treat it as optional otherwise. We thus print the [@unboxed]
         attribute only in the case it's required and leave it out when it's
         not. That's why we call [erasable_extensions_only] here. *)
      Language_extension.erasable_extensions_only ()
  in
  let is_untagged = function
    | _, Untagged_int -> true
    | _, Same_as_ocaml_repr _
    | _, Unboxed_float _
    | _, Unboxed_vector _
    | _, Unboxed_integer _
    | _, Repr_poly -> false
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
  let attrs =
    if p.prim_is_layout_poly then
      oattr_layout_poly :: attrs
    else
      attrs
  in
  let attrs_of_mode_and_repr (m, repr) =
    (match m with
     | Prim_local | Prim_global -> []
     | Prim_poly -> [oattr_local_opt])
    @
    (match repr with
     | Same_as_ocaml_repr Value
     | Repr_poly -> []
     | Unboxed_float _
     | Unboxed_vector _
     | Unboxed_integer _ -> if all_unboxed then [] else [oattr_unboxed]
     | Untagged_int -> if all_untagged then [] else [oattr_untagged]
     | Same_as_ocaml_repr _->
      if all_unboxed || not (is_unboxed (m, repr))
      then []
      else [oattr_unboxed])
  in
  let type_attrs =
    List.map attrs_of_mode_and_repr p.prim_native_repr_args @
    [attrs_of_mode_and_repr p.prim_native_repr_res]
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

let vec128_name = function
  | Int8x16 -> "int8x16"
  | Int16x8 -> "int16x8"
  | Int32x4 -> "int32x4"
  | Int64x2 -> "int64x2"
  | Float32x4 -> "float32x4"
  | Float64x2 -> "float64x2"

let equal_boxed_integer bi1 bi2 =
  match bi1, bi2 with
  | Pnativeint, Pnativeint
  | Pint32, Pint32
  | Pint64, Pint64 ->
    true
  | (Pnativeint | Pint32 | Pint64), _ ->
    false

let equal_boxed_float f1 f2 =
  match f1, f2 with
  | Pfloat32, Pfloat32
  | Pfloat64, Pfloat64 -> true
  | (Pfloat32 | Pfloat64), _ -> false

let equal_vec128_type v1 v2 =
  match v1, v2 with
  | Int8x16, Int8x16 -> true
  | Int16x8, Int16x8 -> true
  | Int32x4, Int32x4 -> true
  | Int64x2, Int64x2 -> true
  | Float32x4, Float32x4 -> true
  | Float64x2, Float64x2 -> true
  | (Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2), _ -> false

let equal_boxed_vector_size bi1 bi2 =
  (* For the purposes of layouts/native representations,
     all 128-bit vector types are equal. *)
  match bi1, bi2 with
  | Pvec128 _, Pvec128 _ -> true

let equal_native_repr nr1 nr2 =
  match nr1, nr2 with
  | Repr_poly, Repr_poly -> true
  | Repr_poly, (Unboxed_float _ | Unboxed_integer _
               | Untagged_int | Unboxed_vector _ | Same_as_ocaml_repr _)
  | (Unboxed_float _ | Unboxed_integer _
    | Untagged_int | Unboxed_vector _ | Same_as_ocaml_repr _), Repr_poly -> false
  | Same_as_ocaml_repr s1, Same_as_ocaml_repr s2 -> !jkind_sort_equal_const s1 s2
  | Same_as_ocaml_repr _,
    (Unboxed_float _ | Unboxed_integer _ | Untagged_int |
     Unboxed_vector _) -> false
  | Unboxed_float f1, Unboxed_float f2 -> equal_boxed_float f1 f2
  | Unboxed_float _,
    (Same_as_ocaml_repr _ | Unboxed_integer _ | Untagged_int |
     Unboxed_vector _) -> false
  | Unboxed_vector vi1, Unboxed_vector vi2 -> equal_boxed_vector_size vi1 vi2
  | Unboxed_vector _,
    (Same_as_ocaml_repr _ | Unboxed_float _ | Untagged_int |
     Unboxed_integer _) -> false
  | Unboxed_integer bi1, Unboxed_integer bi2 -> equal_boxed_integer bi1 bi2
  | Unboxed_integer _,
    (Same_as_ocaml_repr _ | Unboxed_float _ | Untagged_int |
     Unboxed_vector _) -> false
  | Untagged_int, Untagged_int -> true
  | Untagged_int,
    (Same_as_ocaml_repr _ | Unboxed_float _ | Unboxed_integer _ |
     Unboxed_vector _) -> false

let equal_effects ef1 ef2 =
  match ef1, ef2 with
  | No_effects, No_effects -> true
  | No_effects, (Only_generative_effects | Arbitrary_effects) -> false
  | Only_generative_effects, Only_generative_effects -> true
  | Only_generative_effects, (No_effects | Arbitrary_effects) -> false
  | Arbitrary_effects, Arbitrary_effects -> true
  | Arbitrary_effects, (No_effects | Only_generative_effects) -> false

let equal_coeffects cf1 cf2 =
  match cf1, cf2 with
  | No_coeffects, No_coeffects -> true
  | No_coeffects, Has_coeffects -> false
  | Has_coeffects, Has_coeffects -> true
  | Has_coeffects, No_coeffects -> false

let native_name_is_external p =
  let nat_name = native_name p in
  nat_name <> "" && nat_name.[0] <> '%'

module Repr_check = struct

  type result =
    | Wrong_arity
    | Wrong_repr
    | Success

  let args_res_reprs prim =
    (prim.prim_native_repr_args @ [prim.prim_native_repr_res])
    |> List.map snd

  let is repr = equal_native_repr repr

  let any = fun _ -> true

  let value_or_unboxed_or_untagged = function
    | Same_as_ocaml_repr Value
    | Unboxed_float _ | Unboxed_integer _ | Unboxed_vector _
    | Untagged_int -> true
    | Same_as_ocaml_repr _ | Repr_poly -> false

  let check checks prim =
    let reprs = args_res_reprs prim in
    if List.length reprs <> List.length checks
    then Wrong_arity
    else
    if not (List.for_all2 (fun f x -> f x) checks reprs)
    then Wrong_repr
    else Success

  let exactly required =
    check (List.map is required)

  let same_arg_res_repr_with_arity arity prim =
    let repr = snd prim.prim_native_repr_res in
    exactly
      (List.init (arity+1) (fun _ -> repr))
      prim

  let no_non_value_repr prim =
    let arity = List.length prim.prim_native_repr_args in
    check
      (List.init (arity+1) (fun _ -> value_or_unboxed_or_untagged))
      prim
end

(* Note: [any] here is not the same as jkind [any]. It means we allow any
   [native_repr] for the corresponding argument or return.  It's [Typedecl]'s
   responsibility to check that types in externals are representable or marked
   with [@layout_poly] (see [make_native_repr] and the note above
   [error_if_containing_unexpected_jkind]).  Here we have more speicific checks
   for individual primitives. *)
let prim_has_valid_reprs ~loc prim =
  let open Repr_check in
  let check =
    match prim.prim_name with
    | "%identity"
    | "%opaque"
    | "%obj_magic" ->
      same_arg_res_repr_with_arity 1

    | "%ignore" ->
      check [any; is (Same_as_ocaml_repr Value)]
    | "%revapply" ->
      check [any; is (Same_as_ocaml_repr Value); any]
    | "%apply" ->
      check [is (Same_as_ocaml_repr Value); any; any]

    (* This doesn't prevent

       {|
          external get : float# array -> int -> int32# =
            "%array_safe_get"
       |}

       but the same is true for types:

       {|
          external get : float array -> int -> int32 =
            "%array_safe_get"
       |}
    *)
    | "%array_safe_get" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Value);
        any]
    | "%array_safe_set" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Value);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_unsafe_get" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Value);
        any]
    | "%array_unsafe_set" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Value);
        any;
        is (Same_as_ocaml_repr Value)]

    | "%array_safe_get_indexed_by_int64#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits64);
        any]
    | "%array_safe_set_indexed_by_int64#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits64);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_unsafe_get_indexed_by_int64#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits64);
        any]
    | "%array_unsafe_set_indexed_by_int64#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits64);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_safe_get_indexed_by_int32#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits32);
        any]
    | "%array_safe_set_indexed_by_int32#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits32);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_unsafe_get_indexed_by_int32#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits32);
        any]
    | "%array_unsafe_set_indexed_by_int32#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Bits32);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_safe_get_indexed_by_nativeint#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Word);
        any]
    | "%array_safe_set_indexed_by_nativeint#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Word);
        any;
        is (Same_as_ocaml_repr Value)]
    | "%array_unsafe_get_indexed_by_nativeint#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Word);
        any]
    | "%array_unsafe_set_indexed_by_nativeint#" ->
      check [
        is (Same_as_ocaml_repr Value);
        is (Same_as_ocaml_repr Word);
        any;
        is (Same_as_ocaml_repr Value)]

    | "%box_float" ->
      exactly [Same_as_ocaml_repr Float64; Same_as_ocaml_repr Value]
    | "%unbox_float" ->
      exactly [Same_as_ocaml_repr Value; Same_as_ocaml_repr Float64]
    | "%box_nativeint" ->
      exactly [Same_as_ocaml_repr Word; Same_as_ocaml_repr Value]
    | "%unbox_nativeint" ->
      exactly [Same_as_ocaml_repr Value; Same_as_ocaml_repr Word]
    | "%box_int32" ->
      exactly [Same_as_ocaml_repr Bits32; Same_as_ocaml_repr Value]
    | "%unbox_int32" ->
      exactly [Same_as_ocaml_repr Value; Same_as_ocaml_repr Bits32]
    | "%box_int64" ->
      exactly [Same_as_ocaml_repr Bits64; Same_as_ocaml_repr Value]
    | "%unbox_int64" ->
      exactly [Same_as_ocaml_repr Value; Same_as_ocaml_repr Bits64]

    (* Bigstring primitives *)
    | "%caml_bigstring_get32#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits32]
    | "%caml_bigstring_get32u#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits32]
    | "%caml_bigstring_get64#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits64]
    | "%caml_bigstring_get64u#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits64]

    (* CR layouts: add these when we have unboxed simd layouts *)
    (* | "%caml_bigstring_getu128#" ->
    | "%caml_bigstring_getu128u#" ->
    | "%caml_bigstring_geta128#" ->
    | "%caml_bigstring_geta128u#" -> *)

    | "%caml_bigstring_set32#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits32;
        Same_as_ocaml_repr Value]
    | "%caml_bigstring_set32u#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits32;
        Same_as_ocaml_repr Value]
    | "%caml_bigstring_set64#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits64;
        Same_as_ocaml_repr Value]
    | "%caml_bigstring_set64u#" ->
      exactly [
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Value;
        Same_as_ocaml_repr Bits64;
        Same_as_ocaml_repr Value]

    (* CR layouts: add these when we have unboxed simd layouts *)
    (* | "%caml_bigstring_setu128#" ->
    | "%caml_bigstring_setu128u#" ->
    | "%caml_bigstring_seta128#" ->
    | "%caml_bigstring_seta128u#" -> *)

    | name when is_builtin_prim_name name ->
      no_non_value_repr

    (* These can probably support non-value reprs if the need arises:
      {|
        | "%send"
        | "%sendself"
        | "%sendcache"
      |}
    *)
    | _ ->
      (* make no assumptions about external c primitives *)
      fun _ -> Success
  in
  match check prim with
  | Success -> ()
  | Wrong_arity ->
    (* There's already an arity check in translprim that catches this. We will
       defer to that logic.  We are only checking the arity of some built-in
       primitives here but not all, and it would be weird to raise different
       errors dependent on the [prim_name]. *)
    ()
  | Wrong_repr ->
    raise (Error (loc,
            Invalid_native_repr_for_primitive (prim.prim_name)))

let prim_can_contain_jkind_any prim =
  match prim.prim_name with
  | "%array_length"
  | "%array_safe_get"
  | "%array_safe_set"
  | "%array_unsafe_get"
  | "%array_unsafe_set"
  | "%array_safe_get_indexed_by_int64#"
  | "%array_safe_set_indexed_by_int64#"
  | "%array_unsafe_get_indexed_by_int64#"
  | "%array_unsafe_set_indexed_by_int64#"
  | "%array_safe_get_indexed_by_int32#"
  | "%array_safe_set_indexed_by_int32#"
  | "%array_unsafe_get_indexed_by_int32#"
  | "%array_unsafe_set_indexed_by_int32#"
  | "%array_safe_get_indexed_by_nativeint#"
  | "%array_safe_set_indexed_by_nativeint#"
  | "%array_unsafe_get_indexed_by_nativeint#"
  | "%array_unsafe_set_indexed_by_nativeint#" -> false
  | _ -> true

let report_error ppf err =
  match err with
  | Old_style_float_with_native_repr_attribute ->
    Format.fprintf ppf "Cannot use \"float\" in conjunction with \
                        [%@unboxed]/[%@untagged]."
  | Old_style_float_with_non_value ->
    Format.fprintf ppf "Cannot use \"float\" in conjunction with \
                        types of non-value layouts."
  | Old_style_noalloc_with_noalloc_attribute ->
    Format.fprintf ppf "Cannot use \"noalloc\" in conjunction with \
                        [%@%@noalloc]."
  | No_native_primitive_with_repr_attribute ->
    Format.fprintf ppf
      "@[The native code version of the primitive is mandatory@ \
       when attributes [%@untagged] or [%@unboxed] are present.@]"
  | No_native_primitive_with_non_value ->
    Format.fprintf ppf
      "@[The native code version of the primitive is mandatory@ \
       for types with non-value layouts.@]"
  | Inconsistent_attributes_for_effects ->
    Format.fprintf ppf "At most one of [%@no_effects] and \
                        [%@only_generative_effects] can be specified."
  | Inconsistent_noalloc_attributes_for_effects ->
    Format.fprintf ppf "Cannot use [%@%@no_generative_effects] \
                        in conjunction with [%@%@noalloc]."
  | Invalid_representation_polymorphic_attribute ->
    Format.fprintf ppf "Attribute [%@layout_poly] can only be used \
                        on built-in primitives."
  | Invalid_native_repr_for_primitive name ->
    Format.fprintf ppf
      "The primitive [%s] is used in an invalid declaration.@ \
       The declaration contains argument/return types with the@ \
       wrong layout."
      name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
