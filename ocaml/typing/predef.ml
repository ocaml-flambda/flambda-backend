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
and ident_float32 = ident_create "float32"
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
and ident_lexing_position = ident_create "lexing_position"

and ident_unboxed_float = ident_create "float#"
and ident_unboxed_float32 = ident_create "float32#"
and ident_unboxed_nativeint = ident_create "nativeint#"
and ident_unboxed_int32 = ident_create "int32#"
and ident_unboxed_int64 = ident_create "int64#"
and ident_or_null = ident_create "or_null"

and ident_int8x16 = ident_create "int8x16"
and ident_int16x8 = ident_create "int16x8"
and ident_int32x4 = ident_create "int32x4"
and ident_int64x2 = ident_create "int64x2"
and ident_float32x4 = ident_create "float32x4"
and ident_float64x2 = ident_create "float64x2"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
and path_float32 = Pident ident_float32
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
and path_lexing_position = Pident ident_lexing_position

and path_unboxed_float = Pident ident_unboxed_float
and path_unboxed_float32 = Pident ident_unboxed_float32
and path_unboxed_nativeint = Pident ident_unboxed_nativeint
and path_unboxed_int32 = Pident ident_unboxed_int32
and path_unboxed_int64 = Pident ident_unboxed_int64
and path_or_null = Pident ident_or_null

and path_int8x16 = Pident ident_int8x16
and path_int16x8 = Pident ident_int16x8
and path_int32x4 = Pident ident_int32x4
and path_int64x2 = Pident ident_int64x2
and path_float32x4 = Pident ident_float32x4
and path_float64x2 = Pident ident_float64x2

let type_int = newgenty (Tconstr(path_int, Unapplied, ref Mnil))
and type_char = newgenty (Tconstr(path_char, Unapplied, ref Mnil))
and type_bytes = newgenty (Tconstr(path_bytes, Unapplied, ref Mnil))
and type_float = newgenty (Tconstr(path_float, Unapplied, ref Mnil))
and type_float32 = newgenty (Tconstr(path_float32, Unapplied, ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, Unapplied, ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, Unapplied, ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, Unapplied, ref Mnil))
and type_array t = newgenty (Tconstr(path_array, AppArgs.one t, ref Mnil))
and type_iarray t = newgenty (Tconstr(path_iarray, AppArgs.one t, ref Mnil))
and type_list t = newgenty (Tconstr(path_list, AppArgs.one t, ref Mnil))
and type_option t = newgenty (Tconstr(path_option, AppArgs.one t, ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, Unapplied, ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, Unapplied, ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, Unapplied, ref Mnil))
and type_lazy_t t = newgenty (Tconstr(path_lazy_t, AppArgs.one t, ref Mnil))
and type_string = newgenty (Tconstr(path_string, Unapplied, ref Mnil))
and type_extension_constructor =
      newgenty (Tconstr(path_extension_constructor, Unapplied, ref Mnil))
and type_floatarray = newgenty (Tconstr(path_floatarray, Unapplied, ref Mnil))
and type_lexing_position = newgenty (Tconstr(path_lexing_position, Unapplied, ref Mnil))

and type_unboxed_float = newgenty (Tconstr(path_unboxed_float, Unapplied, ref Mnil))
and type_unboxed_float32 = newgenty (Tconstr(path_unboxed_float32, Unapplied, ref Mnil))
and type_unboxed_nativeint =
      newgenty (Tconstr(path_unboxed_nativeint, Unapplied, ref Mnil))
and type_unboxed_int32 = newgenty (Tconstr(path_unboxed_int32, Unapplied, ref Mnil))
and type_unboxed_int64 = newgenty (Tconstr(path_unboxed_int64, Unapplied, ref Mnil))
and type_or_null t = newgenty (Tconstr(path_or_null, AppArgs.one t, ref Mnil))

and type_int8x16 = newgenty (Tconstr(path_int8x16, Unapplied, ref Mnil))
and type_int16x8 = newgenty (Tconstr(path_int16x8, Unapplied, ref Mnil))
and type_int32x4 = newgenty (Tconstr(path_int32x4, Unapplied, ref Mnil))
and type_int64x2 = newgenty (Tconstr(path_int64x2, Unapplied, ref Mnil))
and type_float32x4 = newgenty (Tconstr(path_float32x4, Unapplied, ref Mnil))
and type_float64x2 = newgenty (Tconstr(path_float64x2, Unapplied, ref Mnil))

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

and ident_null = ident_create "Null"
and ident_this = ident_create "This"

let predef_jkind_annotation primitive =
  Option.map
    (fun (primitive : Jkind.Const.Builtin.t) ->
       (* This is a bit of a hack: we're trying to figure out what a user
          could have written on a predef type declaration to give it the
          right kind. But this hack is OK as its result is just used in
          printing/untypeast.
       *)
       let user_written : _ Location.loc =
         Jane_syntax.Jkind.(Abbreviation (Const.mk primitive.name Location.none))
         |> Location.mknoloc
       in
       Jkind_types.Higher_const.Type primitive.jkind, user_written)
    primitive

let option_argument_jkind = Jkind.Builtin.value ~why:(
  Type_argument {parent_path = path_option; position = 1; arity = 1})

let list_argument_jkind = Jkind.Builtin.value ~why:(
  Type_argument {parent_path = path_list; position = 1; arity = 1})

let or_null_argument_jkind = Jkind.Builtin.value ~why:(
  Type_argument {parent_path = path_or_null; position = 1; arity = 1})

let default_abstr ret_jkind = create_type_equation_noun [] ret_jkind Public None
let default_ret_jkind type_ident = Jkind.Builtin.value ~why:(Primitive type_ident)
let default_param_jkind type_ident =
  Jkind.Builtin.value ~why:(
    Type_argument {
      parent_path = Path.Pident type_ident;
      position = 1;
      arity = 1}
  )

let mk_add_type add_type
      type_ident
      ?(kind=default_abstr (default_ret_jkind type_ident))
      (* [jkind_annotation] is just used for printing. It's best to
         provide it if the jkind is not implied by the kind of the
         type, as then the type, if printed, will be clearer.
      *)
      ?jkind_annotation
      env =
  let decl =
    {type_noun = kind;
     type_jkind_annotation = predef_jkind_annotation jkind_annotation;
     type_loc = Location.none;
     type_is_newtype = false;
     type_expansion_scope = lowest_level;
     type_attributes = [];
     type_unboxed_default = false;
     type_uid = Uid.of_predef_id type_ident;
     type_has_illegal_crossings = false;
    }
  in
  add_type type_ident decl env

let mk_add_type1 add_type type_ident
      ~kind
      (* See the comment on the [jkind_annotation] argument to [mk_add_type]
      *)
      ?jkind_annotation
      ?(param_jkind=default_param_jkind type_ident)
      env =
  let param = newgenvar (Higher_jkind.wrap param_jkind) in
  let decl =
    { type_noun = kind param;
      type_jkind_annotation = predef_jkind_annotation jkind_annotation;
      type_loc = Location.none;
      type_is_newtype = false;
      type_expansion_scope = lowest_level;
      type_attributes = [];
      type_unboxed_default = false;
      type_uid = Uid.of_predef_id type_ident;
       type_has_illegal_crossings = false;
    }
  in
  add_type type_ident decl env

let mk_add_extension add_extension id args jkinds =
  Array.iter (fun jkind ->
      let raise_error () = Misc.fatal_error
          "sanity check failed: non-value jkind in predef extension \
            constructor; should this have Constructor_mixed shape?" in
      match Jkind.get jkind with
      | Const const ->
          begin
            match Jkind.Const.get_layout const with
            | Sort Value -> ()
            | Any | Sort (Void | Float32 | Float64 | Word | Bits32 | Bits64) ->
                raise_error ()
          end
      | _ -> raise_error ())
    jkinds;
  add_extension id
    { ext_type_path = path_exn;
      ext_type_params = [];
      ext_args =
        Cstr_tuple
          (List.map
            (fun x ->
              {
                ca_type=x;
                ca_modalities=Mode.Modality.Value.Const.id;
                ca_loc=Location.none
              })
            args);
      ext_arg_jkinds = jkinds;
      ext_shape = Constructor_uniform_value;
      ext_constant = args = [];
      ext_ret_type = None;
      ext_private = Asttypes.Public;
      ext_loc = Location.none;
      ext_attributes = [Ast_helper.Attr.mk
                          (Location.mknoloc "ocaml.warn_on_literal_pattern")
                          (Parsetree.PStr [])];
      ext_uid = Uid.of_predef_id id;
    }

let variant ?manifest params ret_jkind cstrs jkinds =
  Datatype {
    params; manifest; ret_jkind;
    noun = Datatype_variant { priv = Public; cstrs; rep = Variant_boxed jkinds }
  }

let open_variant params ret_jkind =
  Datatype { params; ret_jkind; manifest = None; noun = Datatype_open { priv = Public } }

let unrestricted tvar =
  {ca_type=tvar;
     ca_modalities=Mode.Modality.Value.Const.id;
     ca_loc=Location.none}

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let build_initial_env add_type add_extension empty_env =
  let add_type = mk_add_type add_type
  and add_type1 = mk_add_type1 add_type
  and add_extension = mk_add_extension add_extension in
  let add_type' ~jkind =
    add_type ~kind:(default_abstr jkind)
  in
  let add_type1' type_ident ~variance ~separability
        ?(param_jkind = default_param_jkind type_ident) =
    add_type1 type_ident
      ~kind:(
        fun param_expr ->
          create_type_equation_noun
            [{ param_expr; variance; separability }]
            (Jkind.Builtin.value ~why:(Primitive type_ident))
            Public None)
      ~param_jkind
  in
  let add_datatype1' type_ident ~variance ~separability
        ?(param_jkind = default_param_jkind type_ident) =
    add_type1 type_ident
      ~kind:(
        fun param_expr -> 
          Datatype { 
            params = [{ param_expr; variance; separability }];
            ret_jkind = Jkind.Builtin.value ~why:(Primitive type_ident);
            manifest = None;
            noun = Datatype_abstr })
      ~param_jkind
  in
  empty_env
  (* Predefined types *)
  |> add_datatype1' ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
       ~param_jkind:(Jkind.add_nullability_crossing
                      (Jkind.Builtin.any ~why:Array_type_argument))
  |> add_datatype1' ident_iarray
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~param_jkind:(Jkind.add_nullability_crossing
                      (Jkind.Builtin.any ~why:Array_type_argument))
  |> add_type ident_bool
       ~kind:(variant [] (Jkind.Builtin.immediate ~why:Enumeration)
                [ cstr ident_false []; cstr ident_true []]
                [| Constructor_uniform_value, [| |];
                   Constructor_uniform_value, [| |] |])
  |> add_type' ident_char ~jkind:(Jkind.Builtin.immediate ~why:(Primitive ident_char))
      ~jkind_annotation:Jkind.Const.Builtin.immediate
  |> add_type ident_exn
       ~kind:(open_variant [] (Jkind.Builtin.value ~why:Extensible_variant))
  |> add_type ident_extension_constructor
  |> add_type' ident_float
      ~jkind:(Jkind.of_const ~why:(Primitive ident_float)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type' ident_floatarray
      ~jkind:(Jkind.of_const ~why:(Primitive ident_floatarray)
               Jkind.Const.Builtin.mutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.mutable_data
  |> add_type' ident_int
      ~jkind:(Jkind.Builtin.immediate ~why:(Primitive ident_int))
      ~jkind_annotation:Jkind.Const.Builtin.immediate
  |> add_type' ident_int32
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int32)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type' ident_int64
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int64)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type1' ident_lazy_t
      ~variance:Variance.covariant
      ~separability:Separability.Ind
  |> add_type1 ident_list
      ~kind:(fun tvar ->
         variant
           [{ param_expr = tvar; variance = Variance.covariant; separability =  Separability.Ind }]
           (Jkind.Builtin.value ~why:Boxed_variant)
           [cstr ident_nil [];
            cstr ident_cons [unrestricted tvar;
                             type_list tvar |> unrestricted]]
           [| Constructor_uniform_value, [| |];
              Constructor_uniform_value,
                [| list_argument_jkind;
                   Jkind.Builtin.value ~why:Boxed_variant;
                |];
           |] )
  |> add_type' ident_nativeint
      ~jkind:(Jkind.of_const ~why:(Primitive ident_nativeint)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type1 ident_option
       ~kind:(fun tvar ->
         variant
           [{ param_expr = tvar; variance = Variance.covariant; separability =  Separability.Ind }]
           (Jkind.Builtin.value ~why:Boxed_variant)
           [cstr ident_none []; cstr ident_some [unrestricted tvar]]
           [| Constructor_uniform_value, [| |];
              Constructor_uniform_value, [| option_argument_jkind |];
           |])
  |> add_type ident_lexing_position
       ~kind:(
         let lbl (field, field_type, jkind) =
           let id = Ident.create_predef field in
             {
               ld_id=id;
               ld_mutable=Immutable;
               ld_modalities=Mode.Modality.Value.Const.id;
               ld_type=field_type;
               ld_jkind=jkind;
               ld_loc=Location.none;
               ld_attributes=[];
               ld_uid=Uid.of_predef_id id;
             }
         in
         let immediate = Jkind.Builtin.value ~why:(Primitive ident_int) in
         let lbls = List.map lbl [
           ("pos_fname", type_string, (Jkind.of_const ~why:(Primitive ident_string)
                                          Jkind.Const.Builtin.immutable_data.jkind));
           ("pos_lnum", type_int, immediate);
           ("pos_bol", type_int, immediate);
           ("pos_cnum", type_int, immediate) ]
         in
         Datatype {
          params = [];
          ret_jkind = (
            Jkind.of_const ~why:(Primitive ident_lexing_position)
              Jkind.Const.Builtin.immutable_data.jkind);
          manifest = None;
          noun = Datatype_record {
           priv = Public;
           lbls;
           rep = Record_boxed (List.map (fun label -> label.ld_jkind) lbls |> Array.of_list)
          }
         })
       ~jkind_annotation:Jkind.Const.Builtin.word
  |> add_type' ident_string
       ~jkind:(Jkind.of_const ~why:(Primitive ident_string)
                Jkind.Const.Builtin.immutable_data.jkind)
       ~jkind_annotation:Jkind.Const.Builtin.word
  |> add_type' ident_unboxed_float
       ~jkind:(Jkind.of_const ~why:(Primitive ident_unboxed_float) Jkind.Const.Builtin.float64.jkind)
       ~jkind_annotation:Jkind.Const.Builtin.float64
  |> add_type' ident_unboxed_nativeint
       ~jkind:
         (Jkind.add_mode_crossing
           (Jkind.of_const ~why:(Primitive ident_unboxed_nativeint)
               Jkind.Const.Builtin.word.jkind))
       ~jkind_annotation:Jkind.Const.Builtin.word
  |> add_type' ident_unboxed_int32
       ~jkind:
         (Jkind.add_mode_crossing
           (Jkind.of_const ~why:(Primitive ident_unboxed_int32)
               Jkind.Const.Builtin.bits32.jkind))
       ~jkind_annotation:Jkind.Const.Builtin.bits32
  |> add_type' ident_unboxed_int64
       ~jkind:
         (Jkind.add_mode_crossing
           (Jkind.of_const ~why:(Primitive ident_unboxed_int64)
               Jkind.Const.Builtin.bits64.jkind))
       ~jkind_annotation:Jkind.Const.Builtin.bits64
  |> add_type' ident_bytes
      ~jkind:(Jkind.of_const ~why:(Primitive ident_bytes)
               Jkind.Const.Builtin.mutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.mutable_data
  |> add_type ident_unit
       ~kind:(variant
                [] (Jkind.Builtin.immediate ~why:Enumeration)
                [cstr ident_void []]
                [| Constructor_uniform_value, [| |] |])
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.Builtin.value ~why:Tuple |]
  |> add_extension ident_division_by_zero [] [||]
  |> add_extension ident_end_of_file [] [||]
  |> add_extension ident_failure [type_string]
       [| Jkind.Builtin.value ~why:(Primitive ident_string) |]
  |> add_extension ident_invalid_argument [type_string]
       [| Jkind.Builtin.value ~why:(Primitive ident_string) |]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.Builtin.value ~why:Tuple |]
  |> add_extension ident_not_found [] [||]
  |> add_extension ident_out_of_memory [] [||]
  |> add_extension ident_stack_overflow [] [||]
  |> add_extension ident_sys_blocked_io [] [||]
  |> add_extension ident_sys_error [type_string]
       [| Jkind.Builtin.value ~why:(Primitive ident_string) |]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[None, type_string; None, type_int; None, type_int])]
       [| Jkind.Builtin.value ~why:Tuple |]

let add_simd_extension_types add_type env =
  let add_type ~jkind = mk_add_type add_type ~kind:(default_abstr jkind) in
  env
  |> add_type ident_int8x16
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int8x16)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_int16x8
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int16x8)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_int32x4
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int32x4)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_int64x2
      ~jkind:(Jkind.of_const ~why:(Primitive ident_int64x2)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_float32x4
      ~jkind:(Jkind.of_const ~why:(Primitive ident_float32x4)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_float64x2
      ~jkind:(Jkind.of_const ~why:(Primitive ident_float64x2)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data

let add_small_number_extension_types add_type env =
  let add_type ~jkind = mk_add_type add_type ~kind:(default_abstr jkind) in
  env
  |> add_type ident_float32
      ~jkind:(Jkind.of_const ~why:(Primitive ident_float32)
                Jkind.Const.Builtin.immutable_data.jkind)
      ~jkind_annotation:Jkind.Const.Builtin.immutable_data
  |> add_type ident_unboxed_float32
       ~jkind:(Jkind.of_const ~why:(Primitive ident_unboxed_float32)
          Jkind.Const.Builtin.float32.jkind)
       ~jkind_annotation:Jkind.Const.Builtin.float32

let or_null_kind ?manifest tvar =
  variant ?manifest
    [{ param_expr = tvar; variance = Variance.covariant; separability = Separability.Ind }]
    (Jkind.Builtin.value_or_null ~why:(Primitive ident_or_null))
    [cstr ident_null []; cstr ident_this [unrestricted tvar]]
    [| Constructor_uniform_value, [| |];
        Constructor_uniform_value, [| or_null_argument_jkind |];
    |]

let add_or_null add_type env =
  let add_type1 = mk_add_type1 add_type in
  env
  |> add_type1 ident_or_null
  (* CR layouts v3: [or_null] is separable only if the argument type
     is non-float. The current separability system can't track that.
     We also want to allow [float or_null] despite it being non-separable.

     For now, we mark the type argument as [Separability.Ind] to permit
     the most argument types, and forbid arrays from accepting [or_null]s.
     In the future, we will track separability in the jkind system. *)
  ~kind:or_null_kind

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
