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

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ident.create_predef

let ident_int = ident_create "int"
and ident_char = ident_create "char"
and ident_bytes = ident_create "bytes"
and ident_float = ident_create "float"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_exn = ident_create "exn"
and ident_array = ident_create "array"
and ident_iarray = ident_create "iarray"
and ident_list = ident_create "list"
and ident_option = ident_create "option"
and ident_nativeint = ident_create "nativeint"
and ident_int32 = ident_create "int32"
and ident_int64 = ident_create "int64"
and ident_lazy_t = ident_create "lazy_t"
and ident_string = ident_create "string"
and ident_extension_constructor = ident_create "extension_constructor"
and ident_floatarray = ident_create "floatarray"
<<<<<<< HEAD
and ident_unboxed_float = ident_create "float#"
and ident_unboxed_nativeint = ident_create "nativeint#"
and ident_unboxed_int32 = ident_create "int32#"
and ident_unboxed_int64 = ident_create "int64#"

and ident_int8x16 = ident_create "int8x16"
and ident_int16x8 = ident_create "int16x8"
and ident_int32x4 = ident_create "int32x4"
and ident_int64x2 = ident_create "int64x2"
and ident_float32x4 = ident_create "float32x4"
and ident_float64x2 = ident_create "float64x2"
||||||| parent of 431cec26 (Start of implicit-source-positions)
=======
and ident_lexing_position = ident_create "lexing_position"
>>>>>>> 431cec26 (Start of implicit-source-positions)

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_iarray = Pident ident_iarray
and path_list = Pident ident_list
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_lazy_t = Pident ident_lazy_t
and path_string = Pident ident_string
and path_extension_constructor = Pident ident_extension_constructor
and path_floatarray = Pident ident_floatarray
<<<<<<< HEAD
and path_unboxed_float = Pident ident_unboxed_float
and path_unboxed_nativeint = Pident ident_unboxed_nativeint
and path_unboxed_int32 = Pident ident_unboxed_int32
and path_unboxed_int64 = Pident ident_unboxed_int64

and path_int8x16 = Pident ident_int8x16
and path_int16x8 = Pident ident_int16x8
and path_int32x4 = Pident ident_int32x4
and path_int64x2 = Pident ident_int64x2
and path_float32x4 = Pident ident_float32x4
and path_float64x2 = Pident ident_float64x2
||||||| parent of 431cec26 (Start of implicit-source-positions)
=======
and path_lexing_position = Pident ident_lexing_position
>>>>>>> 431cec26 (Start of implicit-source-positions)

let type_int = newgenty (Tconstr(path_int, [], ref Mnil))
and type_char = newgenty (Tconstr(path_char, [], ref Mnil))
and type_bytes = newgenty (Tconstr(path_bytes, [], ref Mnil))
and type_float = newgenty (Tconstr(path_float, [], ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, [], ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, [], ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, [], ref Mnil))
and type_array t = newgenty (Tconstr(path_array, [t], ref Mnil))
and type_iarray t = newgenty (Tconstr(path_iarray, [t], ref Mnil))
and type_list t = newgenty (Tconstr(path_list, [t], ref Mnil))
and type_option t = newgenty (Tconstr(path_option, [t], ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, [], ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, [], ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, [], ref Mnil))
and type_lazy_t t = newgenty (Tconstr(path_lazy_t, [t], ref Mnil))
and type_string = newgenty (Tconstr(path_string, [], ref Mnil))
and type_extension_constructor =
      newgenty (Tconstr(path_extension_constructor, [], ref Mnil))
and type_floatarray = newgenty (Tconstr(path_floatarray, [], ref Mnil))
<<<<<<< HEAD
and type_unboxed_float = newgenty (Tconstr(path_unboxed_float, [], ref Mnil))
and type_unboxed_nativeint =
      newgenty (Tconstr(path_unboxed_nativeint, [], ref Mnil))
and type_unboxed_int32 = newgenty (Tconstr(path_unboxed_int32, [], ref Mnil))
and type_unboxed_int64 = newgenty (Tconstr(path_unboxed_int64, [], ref Mnil))

and type_int8x16 = newgenty (Tconstr(path_int8x16, [], ref Mnil))
and type_int16x8 = newgenty (Tconstr(path_int16x8, [], ref Mnil))
and type_int32x4 = newgenty (Tconstr(path_int32x4, [], ref Mnil))
and type_int64x2 = newgenty (Tconstr(path_int64x2, [], ref Mnil))
and type_float32x4 = newgenty (Tconstr(path_float32x4, [], ref Mnil))
and type_float64x2 = newgenty (Tconstr(path_float64x2, [], ref Mnil))
||||||| parent of 431cec26 (Start of implicit-source-positions)
=======
and type_lexing_position = newgenty (Tconstr(path_lexing_position, [], ref Mnil))
>>>>>>> 431cec26 (Start of implicit-source-positions)

let ident_match_failure = ident_create "Match_failure"
and ident_out_of_memory = ident_create "Out_of_memory"
and ident_invalid_argument = ident_create "Invalid_argument"
and ident_failure = ident_create "Failure"
and ident_not_found = ident_create "Not_found"
and ident_sys_error = ident_create "Sys_error"
and ident_end_of_file = ident_create "End_of_file"
and ident_division_by_zero = ident_create "Division_by_zero"
and ident_stack_overflow = ident_create "Stack_overflow"
and ident_sys_blocked_io = ident_create "Sys_blocked_io"
and ident_assert_failure = ident_create "Assert_failure"
and ident_undefined_recursive_module =
        ident_create "Undefined_recursive_module"

let all_predef_exns = [
  ident_match_failure;
  ident_out_of_memory;
  ident_invalid_argument;
  ident_failure;
  ident_not_found;
  ident_sys_error;
  ident_end_of_file;
  ident_division_by_zero;
  ident_stack_overflow;
  ident_sys_blocked_io;
  ident_assert_failure;
  ident_undefined_recursive_module;
]

let path_match_failure = Pident ident_match_failure
and path_invalid_argument = Pident ident_invalid_argument
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Location.none;
    cd_attributes = [];
    cd_uid = Uid.of_predef_id id;
  }

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_void = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"
and ident_none = ident_create "None"
and ident_some = ident_create "Some"

let predef_jkind_annotation const =
  Option.map
    (fun const ->
       (* This is a bit of a hack: we're trying to figure out what a user
          could have written on a predef type declaration to give it the
          right kind. But this hack is OK as its result is just used in
          printing/untypeast.
       *)
       let user_written : _ Location.loc =
         { txt = Jane_asttypes.jkind_of_string (Jkind.string_of_const const);
           loc = Location.none;
         }
       in
       const, user_written)
    const

let option_argument_jkind = Jkind.value ~why:(
  Type_argument {parent_path = path_option; position = 1; arity = 1})

let list_argument_jkind = Jkind.value ~why:(
  Type_argument {parent_path = path_list; position = 1; arity = 1})

let mk_add_type add_type
      ?manifest type_ident
      ?(kind=Type_abstract Abstract_def)
      ?(jkind=Jkind.value ~why:(Primitive type_ident))
      (* [jkind_annotation] is just used for printing. It's best to
         provide it if the jkind is not implied by the kind of the
         type, as then the type, if printed, will be clearer.
      *)
      ?jkind_annotation
      env =
  let decl =
    {type_params = [];
     type_arity = 0;
     type_kind = kind;
     type_jkind = jkind;
     type_jkind_annotation = predef_jkind_annotation jkind_annotation;
     type_loc = Location.none;
     type_private = Asttypes.Public;
     type_manifest = manifest;
     type_variance = [];
     type_separability = [];
     type_is_newtype = false;
     type_expansion_scope = lowest_level;
     type_attributes = [];
     type_unboxed_default = false;
     type_uid = Uid.of_predef_id type_ident;
    }
  in
  add_type type_ident decl env

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let build_initial_env add_type add_extension empty_env =
  let add_type = mk_add_type add_type
  and add_type1 type_ident
        ?(kind=fun _ -> Type_abstract Abstract_def)
        ?(jkind=Jkind.value ~why:(Primitive type_ident))
        (* See the comment on the [jkind_annotation] argument to [mk_add_type]
        *)
        ?jkind_annotation
      ~variance ~separability env =
    let param = newgenvar (Jkind.value ~why:(
      Type_argument {parent_path = Path.Pident type_ident; position = 1; arity = 1})) in
    let decl =
      {type_params = [param];
       type_arity = 1;
       type_kind = kind param;
       type_jkind = jkind;
       type_jkind_annotation = predef_jkind_annotation jkind_annotation;
       type_loc = Location.none;
       type_private = Asttypes.Public;
       type_manifest = None;
       type_variance = [variance];
       type_separability = [separability];
       type_is_newtype = false;
       type_expansion_scope = lowest_level;
       type_attributes = [];
       type_unboxed_default = false;
       type_uid = Uid.of_predef_id type_ident;
      }
    in
    add_type type_ident decl env
  in
  let add_extension id args jkinds =
    add_extension id
      { ext_type_path = path_exn;
        ext_type_params = [];
        ext_args = Cstr_tuple (List.map (fun x -> (x, Unrestricted)) args);
        ext_arg_jkinds = jkinds;
        ext_constant = args = [];
        ext_ret_type = None;
        ext_private = Asttypes.Public;
        ext_loc = Location.none;
        ext_attributes = [Ast_helper.Attr.mk
                            (Location.mknoloc "ocaml.warn_on_literal_pattern")
                            (Parsetree.PStr [])];
        ext_uid = Uid.of_predef_id id;
      }
  in
  let variant constrs jkinds = Type_variant (constrs, Variant_boxed jkinds) in
  empty_env
  (* Predefined types *)
  |> add_type1 ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
  |> add_type1 ident_iarray
       ~variance:Variance.covariant
       ~separability:Separability.Ind
  |> add_type ident_bool
       ~kind:(variant [cstr ident_false []; cstr ident_true []]
                [| [| |]; [| |] |])
       ~jkind:(Jkind.immediate ~why:Enumeration)
  |> add_type ident_char ~jkind:(Jkind.immediate ~why:(Primitive ident_char))
      ~jkind_annotation:Immediate
  |> add_type ident_exn
       ~kind:Type_open
       ~jkind:(Jkind.value ~why:Extensible_variant)
  |> add_type ident_extension_constructor
  |> add_type ident_float
  |> add_type ident_floatarray
  |> add_type ident_int ~jkind:(Jkind.immediate ~why:(Primitive ident_int))
      ~jkind_annotation:Immediate
  |> add_type ident_int32
  |> add_type ident_int64
  |> add_type1 ident_lazy_t
       ~variance:Variance.covariant
       ~separability:Separability.Ind
  |> add_type1 ident_list
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_nil [];
                  cstr ident_cons [tvar, Unrestricted;
                                   type_list tvar, Unrestricted]]
           [| [| |]; [| list_argument_jkind;
                        Jkind.value ~why:Boxed_variant |] |] )
       ~jkind:(Jkind.value ~why:Boxed_variant)
  |> add_type ident_nativeint
  |> add_type1 ident_option
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_none []; cstr ident_some [tvar, Unrestricted]]
<<<<<<< HEAD
           [| [| |]; [| option_argument_jkind |] |])
       ~jkind:(Jkind.value ~why:Boxed_variant)
||||||| parent of 431cec26 (Start of implicit-source-positions)
           [| [| |]; [| Layout.value ~why:Type_argument |] |])
       ~layout:(Layout.value ~why:Boxed_variant)
=======
           [| [| |]; [| Layout.value ~why:Type_argument |] |])
       ~layout:(Layout.value ~why:Boxed_variant)
  |> add_type ident_lexing_position 
       ~kind:(
         let lbl (field, field_type, layout) = 
           let id = Ident.create_predef field in 
             {
               ld_id=id;
               ld_mutable=Immutable;
               ld_global=Unrestricted;
               ld_type=field_type;
               ld_layout=layout;
               ld_loc=Location.none;
               ld_attributes=[];
               ld_uid=Uid.of_predef_id id;
             }
         in
         let immediate = Layout.value ~why:(Primitive ident_int) in 
         let labels = List.map lbl [
           ("pos_fname", type_string, Layout.value ~why:(Primitive ident_string)); 
           ("pos_lnum", type_int, immediate); 
           ("pos_bol", type_int, immediate); 
           ("pos_cnum", type_int, immediate) ] 
         in 
         Type_record (
           labels, 
           (Record_boxed (List.map (fun label -> label.ld_layout) labels |> Array.of_list))
         )
       )
       ~layout:(Layout.value ~why:Boxed_record)
>>>>>>> 431cec26 (Start of implicit-source-positions)
  |> add_type ident_string
  |> add_type ident_unboxed_float
       ~jkind:(Jkind.float64 ~why:(Primitive ident_unboxed_float))
       ~jkind_annotation:Float64
  |> add_type ident_unboxed_nativeint
       ~jkind:(Jkind.word ~why:(Primitive ident_unboxed_nativeint))
       ~jkind_annotation:Word
  |> add_type ident_unboxed_int32
       ~jkind:(Jkind.bits32 ~why:(Primitive ident_unboxed_int32))
       ~jkind_annotation:Bits32
  |> add_type ident_unboxed_int64
       ~jkind:(Jkind.bits64 ~why:(Primitive ident_unboxed_int64))
       ~jkind_annotation:Bits64
  |> add_type ident_bytes
  |> add_type ident_unit
       ~kind:(variant [cstr ident_void []] [| [| |] |])
       ~jkind:(Jkind.immediate ~why:Enumeration)
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.value ~why:Tuple |]
  |> add_extension ident_division_by_zero [] [||]
  |> add_extension ident_end_of_file [] [||]
  |> add_extension ident_failure [type_string]
       [| Jkind.value ~why:(Primitive ident_string) |]
  |> add_extension ident_invalid_argument [type_string]
       [| Jkind.value ~why:(Primitive ident_string) |]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.value ~why:Tuple |]
  |> add_extension ident_not_found [] [||]
  |> add_extension ident_out_of_memory [] [||]
  |> add_extension ident_stack_overflow [] [||]
  |> add_extension ident_sys_blocked_io [] [||]
  |> add_extension ident_sys_error [type_string]
       [| Jkind.value ~why:(Primitive ident_string) |]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.value ~why:Tuple |]

let add_simd_extension_types add_type env =
  let add_type = mk_add_type add_type in
  env
  |> add_type ident_int8x16
  |> add_type ident_int16x8
  |> add_type ident_int32x4
  |> add_type ident_int64x2
  |> add_type ident_float32x4
  |> add_type ident_float64x2

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
