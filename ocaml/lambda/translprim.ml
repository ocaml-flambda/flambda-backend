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

(* Translation of primitives *)

open Primitive
open Types
open Typedtree
open Typeopt
open Lambda
open Debuginfo.Scoped_location
open Translmode

module String = Misc.Stdlib.String

type error =
  | Unknown_builtin_primitive of string
  | Wrong_arity_builtin_primitive of string
  | Invalid_floatarray_glb
  | Unexpected_product_in_prim of Jkind.Sort.Const.t

exception Error of Location.t * error

(* Insertion of debugging events *)

let event_before loc exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Clflags.debug && not !Clflags.native_code
  then Levent(lam, {lev_loc = loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = exp.exp_env})
  else lam

let event_after loc exp lam =
  if !Clflags.debug && not !Clflags.native_code
  then Levent(lam, {lev_loc = loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = exp.exp_env})
  else lam

type comparison =
  | Equal
  | Not_equal
  | Less_equal
  | Less_than
  | Greater_equal
  | Greater_than
  | Compare

type comparison_kind =
  | Compare_generic
  | Compare_ints
  | Compare_floats
  | Compare_float32s
  | Compare_strings
  | Compare_bytes
  | Compare_nativeints
  | Compare_int32s
  | Compare_int64s

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS
  | Loc_FUNCTION

type prim =
  | Primitive of Lambda.primitive * int
  | External of Lambda.external_call_description
  | Sys_argv
  | Comparison of comparison * comparison_kind
  | Raise of Lambda.raise_kind
  | Raise_with_backtrace
  | Lazy_force of Lambda.region_close
  | Loc of loc_kind
  | Send of Lambda.region_close * Lambda.layout
  | Send_self of Lambda.region_close * Lambda.layout
  | Send_cache of Lambda.region_close * Lambda.layout
  | Frame_pointers
  | Identity
  | Apply of Lambda.region_close * Lambda.layout
  | Revapply of Lambda.region_close * Lambda.layout
  | Unsupported of Lambda.primitive

let units_with_used_primitives = Hashtbl.create 7
let add_used_primitive loc env path =
  match path with
    Some (Path.Pdot (path, _)) ->
      let address = Env.find_module_address path env in
      begin match Env.address_head address with
      | AHunit cu ->
          if not (Hashtbl.mem units_with_used_primitives cu)
          then Hashtbl.add units_with_used_primitives cu loc
      | AHlocal _ -> ()
      end
  | _ -> ()

let clear_used_primitives () = Hashtbl.clear units_with_used_primitives
let get_units_with_used_primitives () =
  Hashtbl.fold (fun path _ acc -> path :: acc) units_with_used_primitives []

let gen_array_kind =
  if Config.flat_float_array then Pgenarray else Paddrarray

let gen_array_ref_kind mode =
  if Config.flat_float_array then Pgenarray_ref mode else Paddrarray_ref

let gen_array_set_kind mode =
  if Config.flat_float_array then Pgenarray_set mode else Paddrarray_set mode

let prim_sys_argv =
  Lambda.simple_prim_on_values ~name:"caml_sys_argv" ~arity:1 ~alloc:true

let to_locality ~poly = function
  | Prim_global, _ -> alloc_heap
  | Prim_local, _ -> alloc_local
  | Prim_poly, _ ->
    match poly with
    | None -> assert false
    | Some locality -> transl_locality_mode_l locality

let to_modify_mode ~poly = function
  | Prim_global, _ -> modify_heap
  | Prim_local, _ -> modify_maybe_stack
  | Prim_poly, _ ->
    match poly with
    | None -> assert false
    | Some mode -> transl_modify_mode mode

let extern_repr_of_native_repr:
  loc:_ -> poly_sort:Jkind.Sort.t option -> Primitive.native_repr
  -> Lambda.extern_repr
  = fun ~loc ~poly_sort r -> match r, poly_sort with
  | Repr_poly, Some s ->
    (* XXX can all layout poly prims support products? *)
    ignore loc;
    Same_as_ocaml_repr (Jkind.Sort.default_to_value_and_get s)
  | Repr_poly, None -> Misc.fatal_error "Unexpected Repr_poly"
  | Same_as_ocaml_repr s, _ -> Same_as_ocaml_repr s
  | Unboxed_float f, _ -> Unboxed_float f
  | Unboxed_integer i, _ -> Unboxed_integer i
  | Unboxed_vector i, _ -> Unboxed_vector i
  | Untagged_immediate, _ -> Untagged_int

let sort_of_native_repr ~loc ~poly_sort repr =
  match extern_repr_of_native_repr ~loc ~poly_sort repr with
  | Same_as_ocaml_repr s -> s
  | (Unboxed_float _ | Unboxed_integer _ | Untagged_int |
      Unboxed_vector _) ->
    Jkind.Sort.Const.Base Value

let to_lambda_prim ~loc prim ~poly_sort =
  let native_repr_args =
    List.map
    (fun (m, r) -> m, extern_repr_of_native_repr ~loc ~poly_sort r)
      prim.prim_native_repr_args
  in
  let native_repr_res =
    let (m, r) = prim.prim_native_repr_res in
    m, extern_repr_of_native_repr ~loc ~poly_sort r
  in
  Primitive.make
    ~name:prim.prim_name
    ~alloc:prim.prim_alloc
    ~c_builtin:prim.prim_c_builtin
    ~effects:prim.prim_effects
    ~coeffects:prim.prim_coeffects
    ~native_name:prim.prim_native_name
    ~native_repr_args
    ~native_repr_res
    ~is_layout_poly:prim.prim_is_layout_poly

let indexing_primitives =
  let types_and_widths =
    [
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_bigstring_get16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pbigstring_load_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_bigstring_get32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbigstring_load_32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bigstring_getf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbigstring_load_f32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bigstring_get64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbigstring_load_64 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bigstring_getu128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbigstring_load_128
            { aligned = false; unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bigstring_geta128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbigstring_load_128
            { aligned = true; unsafe; index_kind; mode; boxed } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_bigstring_set16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pbigstring_set_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_bigstring_set32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbigstring_set_32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bigstring_setf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbigstring_set_f32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bigstring_set64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbigstring_set_64 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bigstring_setu128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbigstring_set_128 { aligned = false; unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bigstring_seta128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbigstring_set_128 { aligned = true; unsafe; index_kind; boxed } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_bytes_get16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pbytes_load_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_bytes_get32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbytes_load_32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bytes_getf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbytes_load_f32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_bytes_get64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pbytes_load_64 { unsafe; index_kind; mode; boxed } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_bytes_getu128%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode ->
          Pbytes_load_128 { unsafe; index_kind; mode } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_bytes_set16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pbytes_set_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_bytes_set32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bytes_setf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_f32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bytes_set64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_64 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_bytes_setu128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_128 { unsafe; index_kind; boxed } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_string_get16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pstring_load_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_string_get32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pstring_load_32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_string_getf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pstring_load_f32 { unsafe; index_kind; mode; boxed } );
      ( Printf.sprintf "%%caml_string_get64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode ->
          Pstring_load_64 { unsafe; index_kind; mode; boxed } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_string_getu128%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode ->
          Pstring_load_128 { unsafe; index_kind; mode } );
      ( (fun unsafe _boxed index_kind ->
          Printf.sprintf "%%caml_string_set16%s%s" unsafe index_kind),
        fun ~unsafe ~boxed:_ ~index_kind ~mode:_ ->
          Pbytes_set_16 { unsafe; index_kind } );
      ( Printf.sprintf "%%caml_string_set32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_string_setf32%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_f32 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_string_set64%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_64 { unsafe; index_kind; boxed } );
      ( Printf.sprintf "%%caml_string_setu128%s%s%s",
        fun ~unsafe ~boxed ~index_kind ~mode:_ ->
          Pbytes_set_128 { unsafe; index_kind; boxed } );
    ]
  in
  let index_kinds =
    [
      (Ptagged_int_index, "");
      (Punboxed_int_index Pnativeint, "_indexed_by_nativeint#");
      (Punboxed_int_index Pint32, "_indexed_by_int32#");
      (Punboxed_int_index Pint64, "_indexed_by_int64#");
    ]
  in
  (let ( let* ) x f = List.concat_map f x in
   let* string_gen, primitive_gen = types_and_widths in
   let* index_kind, index_kind_sigil = index_kinds in
   let* unsafe, unsafe_sigil = [ (true, "u"); (false, "") ] in
   let* boxed, boxed_sigil = [ (true, ""); (false, "#") ] in
   let string = string_gen unsafe_sigil boxed_sigil index_kind_sigil in
   let primitive = primitive_gen ~unsafe ~boxed ~index_kind in
   let arity = if String.is_substring string ~substring:"get" then 2 else 3 in
   [ (string, fun ~mode -> Primitive (primitive ~mode, arity)) ])
  |> List.to_seq
  |> fun seq -> String.Map.add_seq seq String.Map.empty

let lookup_primitive loc ~poly_mode ~poly_sort pos p =
  let runtime5 = Config.runtime5 in
  let mode = to_locality ~poly:poly_mode p.prim_native_repr_res in
  let arg_modes =
    List.map (to_modify_mode ~poly:poly_mode) p.prim_native_repr_args
  in
  let get_first_arg_mode () =
    match arg_modes with
    | mode :: _ -> mode
    | [] ->
        Misc.fatal_errorf "Primitive \"%s\" unexpectedly had zero arguments"
          p.prim_name
  in
  let lambda_prim = to_lambda_prim ~loc p ~poly_sort in
  let layout =
    (* Extract the result layout of the primitive.  This can be a non-value
       layout even without the use of [@layout_poly]. For example:

       {[ external id : float# -> float# = "%opaque" ]}

       We don't allow non-value layouts for most primitives. This is checked by
       [prim_has_valid_reprs] in [typing/primitive.ml].

       We don't extract the argument layouts just because it is not needed by
       the middle-end. *)
    let (_, repr) = lambda_prim.prim_native_repr_res in
    Lambda.layout_of_extern_repr repr
  in
  let get_unboxed_product_kinds layout what =
    match layout with
    | Punboxed_product layouts -> (
      let rec convert_layout layout =
        match layout with
        | Pvalue Pintval -> Pint_ignorable
        | Pvalue _ ->
          Misc.fatal_error
            "Scannable layouts not permitted for reinterpret array load/store"
        | Punboxed_float fk -> Punboxedfloat_ignorable fk
        | Punboxed_int ubi -> Punboxedint_ignorable ubi
        | Punboxed_vector _ ->
          Misc.fatal_error
            "SIMD vectors not permitted for reinterpret array load/store"
        | Punboxed_product layouts ->
          Pproduct_ignorable (List.map convert_layout layouts)
        | Ptop -> Misc.fatal_error "Unexpected Ptop"
        | Pbottom -> Misc.fatal_error "Unexpected Pbottom"
      in
      List.map convert_layout layouts
    )
    | _ ->
      Misc.fatal_errorf
        "Expected unboxed product return layout for %s but got:@ (%a)"
        what
        Printlambda.layout layout
  in
  let get_result_unboxed_product_kinds what =
    get_unboxed_product_kinds layout ("result of " ^ what)
  in
  let get_third_arg_unboxed_product_kinds what =
    match lambda_prim.prim_native_repr_args with
    | [ _; _; (_mode, repr) ] ->
      get_unboxed_product_kinds (Lambda.layout_of_extern_repr repr)
        ("third arg of " ^ what)
    | _ -> Misc.fatal_error "Expected three arguments"
  in
  let prim = match p.prim_name with
    | "%identity" -> Identity
    | "%bytes_to_string" -> Primitive (Pbytes_to_string, 1)
    | "%bytes_of_string" -> Primitive (Pbytes_of_string, 1)
    | "%ignore" -> Primitive (Pignore, 1)
    | "%revapply" -> Revapply (pos, layout)
    | "%apply" -> Apply (pos, layout)
    | "%loc_LOC" -> Loc Loc_LOC
    | "%loc_FILE" -> Loc Loc_FILE
    | "%loc_LINE" -> Loc Loc_LINE
    | "%loc_POS" -> Loc Loc_POS
    | "%loc_MODULE" -> Loc Loc_MODULE
    | "%loc_FUNCTION" -> Loc Loc_FUNCTION
    | "%field0" -> Primitive (Pfield (0, Pointer, Reads_vary), 1)
    | "%field1" -> Primitive (Pfield (1, Pointer, Reads_vary), 1)
    | "%field0_immut" -> Primitive ((Pfield (0, Pointer, Reads_agree)), 1)
    | "%field1_immut" -> Primitive ((Pfield (1, Pointer, Reads_agree)), 1)
    | "%setfield0" ->
       let mode = get_first_arg_mode () in
       Primitive ((Psetfield(0, Pointer, Assignment mode)), 2)
    | "%setfield1" ->
       let mode = get_first_arg_mode () in
       Primitive ((Psetfield(1, Pointer, Assignment mode)), 2);
    | "%makeblock" -> Primitive ((Pmakeblock(0, Immutable, None, mode)), 1)
    | "%makemutable" -> Primitive ((Pmakeblock(0, Mutable, None, mode)), 1)
    | "%raise" -> Raise Raise_regular
    | "%reraise" -> Raise Raise_reraise
    | "%raise_notrace" -> Raise Raise_notrace
    | "%raise_with_backtrace" -> Raise_with_backtrace
    | "%sequand" -> Primitive (Psequand, 2)
    | "%sequor" -> Primitive (Psequor, 2)
    | "%boolnot" -> Primitive (Pnot, 1)
    | "%big_endian" -> Primitive ((Pctconst Big_endian), 1)
    | "%backend_type" -> Primitive ((Pctconst Backend_type), 1)
    | "%word_size" -> Primitive ((Pctconst Word_size), 1)
    | "%int_size" -> Primitive ((Pctconst Int_size), 1)
    | "%max_wosize" -> Primitive ((Pctconst Max_wosize), 1)
    | "%ostype_unix" -> Primitive ((Pctconst Ostype_unix), 1)
    | "%ostype_win32" -> Primitive ((Pctconst Ostype_win32), 1)
    | "%ostype_cygwin" -> Primitive ((Pctconst Ostype_cygwin), 1)
    | "%runtime5" -> Primitive ((Pctconst Runtime5), 1)
    | "%frame_pointers" -> Frame_pointers
    | "%negint" -> Primitive (Pnegint, 1)
    | "%succint" -> Primitive ((Poffsetint 1), 1)
    | "%predint" -> Primitive ((Poffsetint(-1)), 1)
    | "%addint" -> Primitive (Paddint, 2)
    | "%subint" -> Primitive (Psubint, 2)
    | "%mulint" -> Primitive (Pmulint, 2)
    | "%divint" -> Primitive ((Pdivint Safe), 2)
    | "%modint" -> Primitive ((Pmodint Safe), 2)
    | "%andint" -> Primitive (Pandint, 2)
    | "%orint" -> Primitive (Porint, 2)
    | "%xorint" -> Primitive (Pxorint, 2)
    | "%lslint" -> Primitive (Plslint, 2)
    | "%lsrint" -> Primitive (Plsrint, 2)
    | "%asrint" -> Primitive (Pasrint, 2)
    | "%eq" -> Primitive ((Pintcomp Ceq), 2)
    | "%noteq" -> Primitive ((Pintcomp Cne), 2)
    | "%ltint" -> Primitive ((Pintcomp Clt), 2)
    | "%leint" -> Primitive ((Pintcomp Cle), 2)
    | "%gtint" -> Primitive ((Pintcomp Cgt), 2)
    | "%geint" -> Primitive ((Pintcomp Cge), 2)
    | "%incr" -> Primitive ((Poffsetref(1)), 1)
    | "%decr" -> Primitive ((Poffsetref(-1)), 1)
    | "%floatoffloat32" -> Primitive (Pfloatoffloat32 mode, 1)
    | "%float32offloat" -> Primitive (Pfloat32offloat mode, 1)
    | "%intoffloat32" -> Primitive (Pintoffloat Pfloat32, 1)
    | "%float32ofint" -> Primitive (Pfloatofint (Pfloat32, mode), 1)
    | "%negfloat32" -> Primitive (Pnegfloat (Pfloat32, mode), 1)
    | "%absfloat32" -> Primitive (Pabsfloat (Pfloat32, mode), 1)
    | "%addfloat32" -> Primitive (Paddfloat (Pfloat32, mode), 2)
    | "%subfloat32" -> Primitive (Psubfloat (Pfloat32, mode), 2)
    | "%mulfloat32" -> Primitive (Pmulfloat (Pfloat32, mode), 2)
    | "%divfloat32" -> Primitive (Pdivfloat (Pfloat32, mode), 2)
    | "%eqfloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFeq)), 2)
    | "%noteqfloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFneq)), 2)
    | "%ltfloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFlt)), 2)
    | "%lefloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFle)), 2)
    | "%gtfloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFgt)), 2)
    | "%gefloat32" -> Primitive ((Pfloatcomp (Pfloat32, CFge)), 2)
    | "%intoffloat" -> Primitive (Pintoffloat Pfloat64, 1)
    | "%floatofint" -> Primitive (Pfloatofint (Pfloat64, mode), 1)
    | "%negfloat" -> Primitive (Pnegfloat (Pfloat64, mode), 1)
    | "%absfloat" -> Primitive (Pabsfloat (Pfloat64, mode), 1)
    | "%addfloat" -> Primitive (Paddfloat (Pfloat64, mode), 2)
    | "%subfloat" -> Primitive (Psubfloat (Pfloat64, mode), 2)
    | "%mulfloat" -> Primitive (Pmulfloat (Pfloat64, mode), 2)
    | "%divfloat" -> Primitive (Pdivfloat (Pfloat64, mode), 2)
    | "%eqfloat" -> Primitive ((Pfloatcomp (Pfloat64, CFeq)), 2)
    | "%noteqfloat" -> Primitive ((Pfloatcomp (Pfloat64, CFneq)), 2)
    | "%ltfloat" -> Primitive ((Pfloatcomp (Pfloat64, CFlt)), 2)
    | "%lefloat" -> Primitive ((Pfloatcomp (Pfloat64, CFle)), 2)
    | "%gtfloat" -> Primitive ((Pfloatcomp (Pfloat64, CFgt)), 2)
    | "%gefloat" -> Primitive ((Pfloatcomp (Pfloat64, CFge)), 2)
    | "%string_length" -> Primitive (Pstringlength, 1)
    | "%string_safe_get" -> Primitive (Pstringrefs, 2)
    | "%string_safe_set" -> Primitive (Pbytessets, 3)
    | "%string_unsafe_get" -> Primitive (Pstringrefu, 2)
    | "%string_unsafe_set" -> Primitive (Pbytessetu, 3)
    | "%bytes_length" -> Primitive (Pbyteslength, 1)
    | "%bytes_safe_get" -> Primitive (Pbytesrefs, 2)
    | "%bytes_safe_set" -> Primitive (Pbytessets, 3)
    | "%bytes_unsafe_get" -> Primitive (Pbytesrefu, 2)
    | "%bytes_unsafe_set" -> Primitive (Pbytessetu, 3)
    | "%array_length" -> Primitive ((Parraylength gen_array_kind), 1)
    | "%array_safe_get" ->
      Primitive
        ((Parrayrefs (gen_array_ref_kind mode, Ptagged_int_index)), 2)
    | "%array_safe_set" ->
      Primitive
        (Parraysets (gen_array_set_kind (get_first_arg_mode ()), Ptagged_int_index),
         3)
    | "%array_unsafe_get" ->
      Primitive
        (Parrayrefu (gen_array_ref_kind mode, Ptagged_int_index), 2)
    | "%array_unsafe_set" ->
      Primitive
        ((Parraysetu (gen_array_set_kind (get_first_arg_mode ()), Ptagged_int_index)),
        3)
    | "%unboxed_int64_array_safe_get_reinterpret" ->
      Primitive
        (Parrayrefs (Punboxedint64array_reinterpret_ref
          (get_result_unboxed_product_kinds p.prim_name), Ptagged_int_index), 2)
    | "%unboxed_int64_array_safe_set_reinterpret" ->
      Primitive
        ((Parraysets (Punboxedint64array_reinterpret_set
          (get_third_arg_unboxed_product_kinds p.prim_name), Ptagged_int_index)),
         3)
    | "%unboxed_int64_array_unsafe_get_reinterpret" ->
      Primitive
        (Parrayrefu (Punboxedint64array_reinterpret_ref
          (get_result_unboxed_product_kinds p.prim_name), Ptagged_int_index), 2)
    | "%unboxed_int64_array_unsafe_set_reinterpret" ->
      Primitive
        ((Parraysetu (Punboxedint64array_reinterpret_set
          (get_third_arg_unboxed_product_kinds p.prim_name), Ptagged_int_index)),
        3)
    | "%array_safe_get_indexed_by_int64#" ->
      Primitive
        ((Parrayrefs (gen_array_ref_kind mode, Punboxed_int_index Pint64)), 2)
    | "%array_safe_set_indexed_by_int64#" ->
      Primitive
        (Parraysets
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pint64),
         3)
    | "%array_unsafe_get_indexed_by_int64#" ->
      Primitive
        (Parrayrefu (gen_array_ref_kind mode, Punboxed_int_index Pint64), 2)
    | "%array_unsafe_set_indexed_by_int64#" ->
      Primitive
        ((Parraysetu
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pint64)),
        3)
    | "%array_safe_get_indexed_by_int32#" ->
      Primitive
        ((Parrayrefs (gen_array_ref_kind mode, Punboxed_int_index Pint32)), 2)
    | "%array_safe_set_indexed_by_int32#" ->
      Primitive
        (Parraysets
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pint32),
         3)
    | "%array_unsafe_get_indexed_by_int32#" ->
      Primitive
        (Parrayrefu (gen_array_ref_kind mode, Punboxed_int_index Pint32), 2)
    | "%array_unsafe_set_indexed_by_int32#" ->
      Primitive
        ((Parraysetu
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pint32)),
        3)
    | "%array_safe_get_indexed_by_nativeint#" ->
      Primitive
        ((Parrayrefs (gen_array_ref_kind mode, Punboxed_int_index Pnativeint)), 2)
    | "%array_safe_set_indexed_by_nativeint#" ->
      Primitive
        (Parraysets
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pnativeint),
         3)
    | "%array_unsafe_get_indexed_by_nativeint#" ->
      Primitive
        (Parrayrefu (gen_array_ref_kind mode, Punboxed_int_index Pnativeint), 2)
    | "%array_unsafe_set_indexed_by_nativeint#" ->
      Primitive
        ((Parraysetu
          (gen_array_set_kind (get_first_arg_mode ()), Punboxed_int_index Pnativeint)),
        3)
    | "%make_unboxed_tuple_vect" ->
      Primitive (Pmake_unboxed_tuple_vect (gen_array_kind, mode), 2)
    | "%obj_size" -> Primitive ((Parraylength Pgenarray), 1)
    | "%obj_field" -> Primitive ((Parrayrefu (Pgenarray_ref mode, Ptagged_int_index)), 2)
    | "%obj_set_field" ->
      Primitive
        ((Parraysetu (Pgenarray_set (get_first_arg_mode ()), Ptagged_int_index)), 3)
    | "%floatarray_length" -> Primitive ((Parraylength Pfloatarray), 1)
    | "%floatarray_safe_get" ->
      Primitive ((Parrayrefs (Pfloatarray_ref mode, Ptagged_int_index)), 2)
    | "%floatarray_safe_set" ->
      Primitive (Parraysets (Pfloatarray_set, Ptagged_int_index), 3)
    | "%floatarray_unsafe_get" ->
      Primitive ((Parrayrefu (Pfloatarray_ref mode, Ptagged_int_index)), 2)
    | "%floatarray_unsafe_set" ->
      Primitive ((Parraysetu (Pfloatarray_set, Ptagged_int_index)), 3)
    | "%obj_is_int" -> Primitive (Pisint { variant_only = false }, 1)
    | "%lazy_force" -> Lazy_force pos
    | "%nativeint_of_int" -> Primitive ((Pbintofint (Pnativeint, mode)), 1)
    | "%nativeint_to_int" -> Primitive ((Pintofbint Pnativeint), 1)
    | "%nativeint_neg" -> Primitive ((Pnegbint (Pnativeint, mode)), 1)
    | "%nativeint_add" -> Primitive ((Paddbint (Pnativeint, mode)), 2)
    | "%nativeint_sub" -> Primitive ((Psubbint (Pnativeint, mode)), 2)
    | "%nativeint_mul" -> Primitive ((Pmulbint (Pnativeint, mode)), 2)
    | "%nativeint_div" ->
      Primitive ((Pdivbint { size = Pnativeint; is_safe = Safe; mode }), 2);
    | "%nativeint_mod" ->
      Primitive ((Pmodbint { size = Pnativeint; is_safe = Safe; mode }), 2);
    | "%nativeint_and" -> Primitive ((Pandbint (Pnativeint, mode)), 2)
    | "%nativeint_or" -> Primitive ( (Porbint (Pnativeint, mode)), 2)
    | "%nativeint_xor" -> Primitive ((Pxorbint (Pnativeint, mode)), 2)
    | "%nativeint_lsl" -> Primitive ((Plslbint (Pnativeint, mode)), 2)
    | "%nativeint_lsr" -> Primitive ((Plsrbint (Pnativeint, mode)), 2)
    | "%nativeint_asr" -> Primitive ((Pasrbint (Pnativeint, mode)), 2)
    | "%int32_of_int" -> Primitive ((Pbintofint (Pint32, mode)), 1)
    | "%int32_to_int" -> Primitive ((Pintofbint Pint32), 1)
    | "%int32_neg" -> Primitive ((Pnegbint (Pint32, mode)), 1)
    | "%int32_add" -> Primitive ((Paddbint (Pint32, mode)), 2)
    | "%int32_sub" -> Primitive ((Psubbint (Pint32, mode)), 2)
    | "%int32_mul" -> Primitive ((Pmulbint (Pint32, mode)), 2)
    | "%int32_div" ->
       Primitive ((Pdivbint { size = Pint32; is_safe = Safe; mode }), 2)
    | "%int32_mod" ->
       Primitive ((Pmodbint { size = Pint32; is_safe = Safe; mode }), 2)
    | "%int32_and" -> Primitive ((Pandbint (Pint32, mode)), 2)
    | "%int32_or" -> Primitive ( (Porbint (Pint32, mode)), 2)
    | "%int32_xor" -> Primitive ((Pxorbint (Pint32, mode)), 2)
    | "%int32_lsl" -> Primitive ((Plslbint (Pint32, mode)), 2)
    | "%int32_lsr" -> Primitive ((Plsrbint (Pint32, mode)), 2)
    | "%int32_asr" -> Primitive ((Pasrbint (Pint32, mode)), 2)
    | "%int64_of_int" -> Primitive ((Pbintofint (Pint64, mode)), 1)
    | "%int64_to_int" -> Primitive ((Pintofbint Pint64), 1)
    | "%int64_neg" -> Primitive ((Pnegbint (Pint64, mode)), 1)
    | "%int64_add" -> Primitive ((Paddbint (Pint64, mode)), 2)
    | "%int64_sub" -> Primitive ((Psubbint (Pint64, mode)), 2)
    | "%int64_mul" -> Primitive ((Pmulbint (Pint64, mode)), 2)
    | "%int64_div" ->
       Primitive ((Pdivbint { size = Pint64; is_safe = Safe; mode }), 2)
    | "%int64_mod" ->
       Primitive ((Pmodbint { size = Pint64; is_safe = Safe; mode }), 2)
    | "%int64_and" -> Primitive ((Pandbint (Pint64, mode)), 2)
    | "%int64_or" -> Primitive ( (Porbint (Pint64, mode)), 2)
    | "%int64_xor" -> Primitive ((Pxorbint (Pint64, mode)), 2)
    | "%int64_lsl" -> Primitive ((Plslbint (Pint64, mode)), 2)
    | "%int64_lsr" -> Primitive ((Plsrbint (Pint64, mode)), 2)
    | "%int64_asr" -> Primitive ((Pasrbint (Pint64, mode)), 2)
    | "%nativeint_of_int32" -> Primitive ((Pcvtbint(Pint32, Pnativeint, mode)), 1)
    | "%nativeint_to_int32" -> Primitive ((Pcvtbint(Pnativeint, Pint32, mode)), 1)
    | "%int64_of_int32" -> Primitive ((Pcvtbint(Pint32, Pint64, mode)), 1)
    | "%int64_to_int32" -> Primitive ((Pcvtbint(Pint64, Pint32, mode)), 1)
    | "%int64_of_nativeint" -> Primitive ((Pcvtbint(Pnativeint, Pint64, mode)), 1)
    | "%int64_to_nativeint" -> Primitive ((Pcvtbint(Pint64, Pnativeint, mode)), 1)
    | "%caml_ba_ref_1" ->
      Primitive
        ((Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout)),
         2);
    | "%caml_ba_ref_2" ->
      Primitive
        ((Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_ref_3" ->
      Primitive
        ((Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_set_1" ->
      Primitive
        ((Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_set_2" ->
      Primitive
        ((Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_set_3" ->
      Primitive
        ((Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout)),
         5);
    | "%caml_ba_unsafe_ref_1" ->
      Primitive
        ((Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout)),
         2);
    | "%caml_ba_unsafe_ref_2" ->
      Primitive
        ((Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_unsafe_ref_3" ->
      Primitive
        ((Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_unsafe_set_1" ->
      Primitive
        ((Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_unsafe_set_2" ->
      Primitive
        ((Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_unsafe_set_3" ->
      Primitive
        ((Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout)),
         5);
    | "%caml_ba_float32_ref_1" ->
      Primitive
        ((Pbigarrayref(false, 1, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         2);
    | "%caml_ba_float32_ref_2" ->
      Primitive
        ((Pbigarrayref(false, 2, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_float32_ref_3" ->
      Primitive
        ((Pbigarrayref(false, 3, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_float32_set_1" ->
      Primitive
        ((Pbigarrayset(false, 1, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_float32_set_2" ->
      Primitive
        ((Pbigarrayset(false, 2, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_float32_set_3" ->
      Primitive
        ((Pbigarrayset(false, 3, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         5);
    | "%caml_ba_float32_unsafe_ref_1" ->
      Primitive
        ((Pbigarrayref(true, 1, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         2);
    | "%caml_ba_float32_unsafe_ref_2" ->
      Primitive
        ((Pbigarrayref(true, 2, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_float32_unsafe_ref_3" ->
      Primitive
        ((Pbigarrayref(true, 3, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_float32_unsafe_set_1" ->
      Primitive
        ((Pbigarrayset(true, 1, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         3);
    | "%caml_ba_float32_unsafe_set_2" ->
      Primitive
        ((Pbigarrayset(true, 2, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         4);
    | "%caml_ba_float32_unsafe_set_3" ->
      Primitive
        ((Pbigarrayset(true, 3, Pbigarray_float32_t, Pbigarray_unknown_layout)),
         5);
    | "%caml_ba_dim_1" -> Primitive ((Pbigarraydim(1)), 1)
    | "%caml_ba_dim_2" -> Primitive ((Pbigarraydim(2)), 1)
    | "%caml_ba_dim_3" -> Primitive ((Pbigarraydim(3)), 1)
    | "%caml_float_array_get128" ->
      Primitive ((Pfloat_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_float_array_get128u" ->
      Primitive ((Pfloat_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_floatarray_get128" ->
      Primitive ((Pfloatarray_load_128 {unsafe = false; mode}), 2)
    | "%caml_floatarray_get128u" ->
      Primitive ((Pfloatarray_load_128 {unsafe = true; mode}), 2)
    | "%caml_unboxed_float_array_get128" ->
      Primitive ((Punboxed_float_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_unboxed_float_array_get128u" ->
      Primitive ((Punboxed_float_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_unboxed_float32_array_get128" ->
      Primitive ((Punboxed_float32_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_unboxed_float32_array_get128u" ->
      Primitive ((Punboxed_float32_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_int_array_get128" ->
      Primitive ((Pint_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_int_array_get128u" ->
      Primitive ((Pint_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_unboxed_int64_array_get128" ->
      Primitive ((Punboxed_int64_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_unboxed_int64_array_get128u" ->
      Primitive ((Punboxed_int64_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_unboxed_int32_array_get128" ->
      Primitive ((Punboxed_int32_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_unboxed_int32_array_get128u" ->
      Primitive ((Punboxed_int32_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_unboxed_nativeint_array_get128" ->
      Primitive ((Punboxed_nativeint_array_load_128 {unsafe = false; mode}), 2)
    | "%caml_unboxed_nativeint_array_get128u" ->
      Primitive ((Punboxed_nativeint_array_load_128 {unsafe = true; mode}), 2)
    | "%caml_float_array_set128" ->
      Primitive ((Pfloat_array_set_128 {unsafe = false}), 3)
    | "%caml_float_array_set128u" ->
      Primitive ((Pfloat_array_set_128 {unsafe = true}), 3)
    | "%caml_floatarray_set128" ->
      Primitive ((Pfloatarray_set_128 {unsafe = false}), 3)
    | "%caml_floatarray_set128u" ->
      Primitive ((Pfloatarray_set_128 {unsafe = true}), 3)
    | "%caml_unboxed_float_array_set128" ->
      Primitive ((Punboxed_float_array_set_128 {unsafe = false}), 3)
    | "%caml_unboxed_float_array_set128u" ->
      Primitive ((Punboxed_float_array_set_128 {unsafe = true}), 3)
    | "%caml_unboxed_float32_array_set128" ->
      Primitive ((Punboxed_float32_array_set_128 {unsafe = false}), 3)
    | "%caml_unboxed_float32_array_set128u" ->
      Primitive ((Punboxed_float32_array_set_128 {unsafe = true}), 3)
    | "%caml_int_array_set128" ->
      Primitive ((Pint_array_set_128 {unsafe = false}), 3)
    | "%caml_int_array_set128u" ->
      Primitive ((Pint_array_set_128 {unsafe = true}), 3)
    | "%caml_unboxed_int64_array_set128" ->
      Primitive ((Punboxed_int64_array_set_128 {unsafe = false}), 3)
    | "%caml_unboxed_int64_array_set128u" ->
      Primitive ((Punboxed_int64_array_set_128 {unsafe = true}), 3)
    | "%caml_unboxed_int32_array_set128" ->
      Primitive ((Punboxed_int32_array_set_128 {unsafe = false}), 3)
    | "%caml_unboxed_int32_array_set128u" ->
      Primitive ((Punboxed_int32_array_set_128 {unsafe = true}), 3)
    | "%caml_unboxed_nativeint_array_set128" ->
      Primitive ((Punboxed_nativeint_array_set_128 {unsafe = false}), 3)
    | "%caml_unboxed_nativeint_array_set128u" ->
      Primitive ((Punboxed_nativeint_array_set_128 {unsafe = true}), 3)
    | "%bswap16" -> Primitive (Pbswap16, 1)
    | "%bswap_int32" -> Primitive ((Pbbswap(Pint32, mode)), 1)
    | "%bswap_int64" -> Primitive ((Pbbswap(Pint64, mode)), 1)
    | "%bswap_native" -> Primitive ((Pbbswap(Pnativeint, mode)), 1)
    | "%int_as_pointer" -> Primitive (Pint_as_pointer mode, 1)
    | "%opaque" -> Primitive (Popaque layout, 1)
    | "%sys_argv" -> Sys_argv
    | "%send" -> Send (pos, layout)
    | "%sendself" -> Send_self (pos, layout)
    | "%sendcache" -> Send_cache (pos, layout)
    | "%equal" -> Comparison(Equal, Compare_generic)
    | "%notequal" -> Comparison(Not_equal, Compare_generic)
    | "%lessequal" -> Comparison(Less_equal, Compare_generic)
    | "%lessthan" -> Comparison(Less_than, Compare_generic)
    | "%greaterequal" -> Comparison(Greater_equal, Compare_generic)
    | "%greaterthan" -> Comparison(Greater_than, Compare_generic)
    | "%compare" -> Comparison(Compare, Compare_generic)
    | "%obj_dup" -> Primitive(Pobj_dup, 1)
    | "%obj_magic" -> Primitive(Pobj_magic layout, 1)
    | "%array_to_iarray" -> Primitive (Parray_to_iarray, 1)
    | "%array_of_iarray" -> Primitive (Parray_of_iarray, 1)
    | "%unbox_float" -> Primitive(Punbox_float Pfloat64, 1)
    | "%box_float" -> Primitive(Pbox_float (Pfloat64, mode), 1)
    | "%unbox_float32" -> Primitive(Punbox_float Pfloat32, 1)
    | "%box_float32" -> Primitive(Pbox_float (Pfloat32, mode), 1)
    | "%get_header" -> Primitive (Pget_header mode, 1)
    | "%atomic_load" ->
        Primitive ((Patomic_load {immediate_or_pointer=Pointer}), 1)
    | "%atomic_exchange" -> Primitive (Patomic_exchange, 2)
    | "%atomic_cas" -> Primitive (Patomic_cas, 3)
    | "%atomic_fetch_add" -> Primitive (Patomic_fetch_add, 2)
    | "%runstack" ->
      if runtime5 then Primitive (Prunstack, 3) else Unsupported Prunstack
    | "%reperform" ->
      if runtime5 then Primitive (Preperform, 3) else Unsupported Preperform
    | "%perform" ->
      if runtime5 then Primitive (Pperform, 1) else Unsupported Pperform
    | "%resume" ->
      if runtime5 then Primitive (Presume, 4) else Unsupported Presume
    | "%dls_get" -> Primitive (Pdls_get, 1)
    | "%poll" -> Primitive (Ppoll, 1)
    | "%unbox_nativeint" -> Primitive(Punbox_int Pnativeint, 1)
    | "%box_nativeint" -> Primitive(Pbox_int (Pnativeint, mode), 1)
    | "%unbox_int32" -> Primitive(Punbox_int Pint32, 1)
    | "%box_int32" -> Primitive(Pbox_int (Pint32, mode), 1)
    | "%unbox_int64" -> Primitive(Punbox_int Pint64, 1)
    | "%box_int64" -> Primitive(Pbox_int (Pint64, mode), 1)
    | "%reinterpret_tagged_int63_as_unboxed_int64" ->
      Primitive(Preinterpret_tagged_int63_as_unboxed_int64, 1)
    | "%reinterpret_unboxed_int64_as_tagged_int63" ->
      Primitive(Preinterpret_unboxed_int64_as_tagged_int63, 1)
    | s when String.length s > 0 && s.[0] = '%' ->
      (match String.Map.find_opt s indexing_primitives with
       | Some prim -> prim ~mode
       | None -> raise (Error (loc, Unknown_builtin_primitive s)))
    | _ -> External lambda_prim
  in
  prim

let lookup_primitive_and_mark_used loc ~poly_mode ~poly_sort pos p env path =
  match lookup_primitive loc ~poly_mode ~poly_sort pos p with
  | External _ as e -> add_used_primitive loc env path; e
  | x -> x

let simplify_constant_constructor = function
  | Equal -> true
  | Not_equal -> true
  | Less_equal -> false
  | Less_than -> false
  | Greater_equal -> false
  | Greater_than -> false
  | Compare -> false

(* See [glb_array_type] *)
let rec glb_scannable_kinds kinds1 kinds2 =
  if List.length kinds1 = List.length kinds2 then
    Misc.Stdlib.List.map2_option glb_scannable_kind kinds1 kinds2
  else
    None

and glb_scannable_kind kind1 kind2 =
  match kind1, kind2 with
  | Pint_scannable, (Paddr_scannable | Pint_scannable)
  | Paddr_scannable, Pint_scannable -> Some Pint_scannable
  | Paddr_scannable, Paddr_scannable -> Some Paddr_scannable
  | Pproduct_scannable kinds1, Pproduct_scannable kinds2 ->
    Option.map (fun x -> Pproduct_scannable x)
      (glb_scannable_kinds kinds1 kinds2)
  | (Pint_scannable | Paddr_scannable | Pproduct_scannable _), _ -> None

(* The following function computes the greatest lower bound of array kinds:

        gen      unboxed-float  unboxed-int32  unboxed-int64  unboxed-nativeint
         |
      /------\
      |      |
    addr  float
      |
    int

   For product kinds, we take the product of this lattice.

   Note that the GLB is not guaranteed to exist.
   In case of array kinds working with layout value, we return
   our first argument instead of raising a fatal error because, although
   it cannot happen in a well-typed program, (ab)use of Obj.magic can
   probably trigger it. For other layouts, we raise an error.
*)
let glb_array_type loc t1 t2 =

  match t1, t2 with
  (* Handle unboxed array kinds which should only match with themselves.

     However, a cheat is added just for [Pgenarray] to allow the [%array_*]
     primitives to work with unboxed types.

     WARNING: This trick will stop working when [Config.flat_float_array]
     becomes [false].*)
  | Pfloatarray, (Punboxedfloatarray _ | Punboxedintarray _) ->
    (* Have a nice error message for a case reachable. *)
    raise(Error(loc, Invalid_floatarray_glb))
  | (Pgenarray | Punboxedfloatarray Pfloat64), Punboxedfloatarray Pfloat64 ->
    Punboxedfloatarray Pfloat64
  | (Pgenarray | Punboxedfloatarray Pfloat32), Punboxedfloatarray Pfloat32 ->
    Punboxedfloatarray Pfloat32
  | Punboxedfloatarray _, _ | _, Punboxedfloatarray _ ->
    Misc.fatal_error "unexpected array kind in glb"
  | (Pgenarray | Punboxedintarray Pint32), Punboxedintarray Pint32 ->
    Punboxedintarray Pint32
  | (Pgenarray | Punboxedintarray Pint64), Punboxedintarray Pint64 ->
    Punboxedintarray Pint64
  | (Pgenarray | Punboxedintarray Pnativeint), Punboxedintarray Pnativeint ->
    Punboxedintarray Pnativeint
  | Punboxedintarray _, _ | _, Punboxedintarray _ ->
    Misc.fatal_error "unexpected array kind in glb"

  (* Unboxed product arrays. Same cheat as above for Pgenarray. *)
  (* XXX: I think these are too conservative for Obj.magics we want to work.
     Write tests. *)
  | Pgenarray,
    ((Pgcignorableproductarray _ | Pgcscannableproductarray _) as k) -> k
  | (Pgcignorableproductarray kinds1) as k, Pgcignorableproductarray kinds2 ->
    if kinds1 = kinds2 then k else
      Misc.fatal_error "mismatched ignorableproductarray kinds in glb"
  | Pgcscannableproductarray kinds1, Pgcscannableproductarray kinds2 ->
    begin match glb_scannable_kinds kinds1 kinds2 with
    | Some kinds -> Pgcscannableproductarray kinds
    | None -> Misc.fatal_error "mismatched scannableproductarray kinds in glb"
    end
  | Pgcignorableproductarray _, _ | _, Pgcignorableproductarray _ ->
    Misc.fatal_error "unexpected Pgcignorableproductarray kind in glb"
  | Pgcscannableproductarray _, _ | _, Pgcscannableproductarray _ ->
    Misc.fatal_error "unexpected Pgcscannableproductarray kind in glb"

  (* No GLB; only used in the [Obj.magic] case *)
  | Pfloatarray, (Paddrarray | Pintarray)
  | (Paddrarray | Pintarray), Pfloatarray -> t1

  (* Compute the correct GLB *)
  | Pgenarray, ((Pgenarray | Paddrarray | Pintarray | Pfloatarray) as x)
  | ((Paddrarray | Pintarray | Pfloatarray) as x), Pgenarray -> x
  | Paddrarray, Paddrarray -> Paddrarray
  | Paddrarray, Pintarray | Pintarray, Paddrarray -> Pintarray
  | Pintarray, Pintarray -> Pintarray
  | Pfloatarray, Pfloatarray -> Pfloatarray

let glb_array_ref_type loc t1 t2 =
  match t1, t2 with
  (* Handle unboxed array kinds which should only match with themselves.

     However, a cheat is added just for [Pgenarray_ref] to allow the [%array_*]
     primitives to work with unboxed types.

     WARNING: This trick will stop working when [Config.flat_float_array]
     becomes [false].*)
  | Pfloatarray_ref _, (Punboxedfloatarray _ | Punboxedintarray _) ->
    (* Have a nice error message for a case reachable. *)
    raise(Error(loc, Invalid_floatarray_glb))
  | (Pgenarray_ref _ | Punboxedfloatarray_ref Pfloat64), Punboxedfloatarray Pfloat64 ->
    Punboxedfloatarray_ref Pfloat64
  | (Pgenarray_ref _ | Punboxedfloatarray_ref Pfloat32), Punboxedfloatarray Pfloat32 ->
    Punboxedfloatarray_ref Pfloat32
  | Punboxedfloatarray_ref _, _
  | _, Punboxedfloatarray _ ->
    Misc.fatal_error "unexpected array kind in glb"
  | Punboxedint64array_reinterpret_ref kinds, Punboxedintarray Pint64 ->
    Punboxedint64array_reinterpret_ref kinds
  | Punboxedint64array_reinterpret_ref _, _ ->
    Misc.fatal_error "Punboxedint64array_reinterpret_ref is only compatible \
      with Punboxedintarray Pint64 arrays"
  | (Pgenarray_ref _ | Punboxedintarray_ref Pint32), Punboxedintarray Pint32 ->
    Punboxedintarray_ref Pint32
  | (Pgenarray_ref _ | Punboxedintarray_ref Pint64), Punboxedintarray Pint64 ->
    Punboxedintarray_ref Pint64
  | (Pgenarray_ref _ | Punboxedintarray_ref Pnativeint), Punboxedintarray Pnativeint ->
    Punboxedintarray_ref Pnativeint
  | Punboxedintarray_ref _, _ | _, Punboxedintarray _ ->
    Misc.fatal_error "unexpected array kind in glb"

  (* Unboxed product arrays. Same cheat as above for Pgenarray. *)
  (* XXX: I think these are too conservative for Obj.magics we want to work.
     Write tests. *)
  | Pgenarray_ref _, Pgcignorableproductarray kinds ->
    Pgcignorableproductarray_ref kinds
  | Pgenarray_ref _, Pgcscannableproductarray kinds ->
    Pgcscannableproductarray_ref kinds
  | (Pgcignorableproductarray_ref kinds1) as k,
    Pgcignorableproductarray kinds2 ->
    if kinds1 = kinds2 then k else
      Misc.fatal_error "mismatched ignorableproductarray kinds in glb"
  | Pgcscannableproductarray_ref kinds1, Pgcscannableproductarray kinds2 ->
    begin match glb_scannable_kinds kinds1 kinds2 with
    | Some kinds -> Pgcscannableproductarray_ref kinds
    | None -> Misc.fatal_error "mismatched scannableproductarray kinds in glb"
    end
  | Pgcignorableproductarray_ref _, _ | _, Pgcignorableproductarray _ ->
    Misc.fatal_error "unexpected Pgcignorableproductarray kind in glb"
  | Pgcscannableproductarray_ref _, _ | _, Pgcscannableproductarray _ ->
    Misc.fatal_error "unexpected Pgcscannableproductarray kind in glb"

  (* No GLB; only used in the [Obj.magic] case *)
  | Pfloatarray_ref _, (Paddrarray | Pintarray)
  | (Paddrarray_ref | Pintarray_ref), Pfloatarray -> t1

  (* Compute the correct GLB *)

  (* Pgenarray >= _ *)
  | (Pgenarray_ref _ as x), Pgenarray -> x
  | Pgenarray_ref _, Pintarray -> Pintarray_ref
  | Pgenarray_ref _, Paddrarray -> Paddrarray_ref
  | Pgenarray_ref mode, Pfloatarray -> Pfloatarray_ref mode
  | (Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _ as x), Pgenarray -> x

  (* Paddrarray > Pintarray *)
  | Paddrarray_ref, Paddrarray -> Paddrarray_ref
  | Paddrarray_ref, Pintarray -> Pintarray_ref
  | Pintarray_ref, Paddrarray -> Pintarray_ref

  (* Pintarray is a minimum *)
  | Pintarray_ref, Pintarray -> Pintarray_ref

  (* Pfloatarray is a minimum *)
  | (Pfloatarray_ref _ as x), Pfloatarray -> x

let glb_array_set_type loc t1 t2 =
  match t1, t2 with
  (* Handle unboxed array kinds which can only match with themselves.

     However, a cheat is added just for [Pgenarray_set] to allow the [%array_*]
     primitives to work with unboxed types.

     WARNING: This trick will stop working when [Config.flat_float_array]
     becomes [false].*)
  | Pfloatarray_set, (Punboxedfloatarray _ | Punboxedintarray _) ->
    (* Have a nice error message for a case reachable. *)
    raise(Error(loc, Invalid_floatarray_glb))
  | (Pgenarray_set _ | Punboxedfloatarray_set Pfloat64), Punboxedfloatarray Pfloat64 ->
    Punboxedfloatarray_set Pfloat64
  | (Pgenarray_set _ | Punboxedfloatarray_set Pfloat32), Punboxedfloatarray Pfloat32 ->
    Punboxedfloatarray_set Pfloat32
  | Punboxedfloatarray_set _, _
  | _, Punboxedfloatarray _ ->
    Misc.fatal_error "unexpected array kind in glb"
  | Punboxedint64array_reinterpret_set kinds, Punboxedintarray Pint64 ->
    Punboxedint64array_reinterpret_set kinds
  | Punboxedint64array_reinterpret_set _, _ ->
    Misc.fatal_error "Punboxedint64array_reinterpret_set is only compatible \
      with Punboxedintarray Pint64 arrays"
  | (Pgenarray_set _ | Punboxedintarray_set Pint32), Punboxedintarray Pint32 ->
    Punboxedintarray_set Pint32
  | (Pgenarray_set _ | Punboxedintarray_set Pint64), Punboxedintarray Pint64 ->
    Punboxedintarray_set Pint64
  | (Pgenarray_set _ | Punboxedintarray_set Pnativeint), Punboxedintarray Pnativeint ->
    Punboxedintarray_set Pnativeint
  | Punboxedintarray_set _, _ | _, Punboxedintarray _ ->
    Misc.fatal_error "unexpected array kind in glb"

  (* Unboxed product arrays. Same cheat as above for Pgenarray. *)
  (* XXX: I think these are too conservative for Obj.magics we want to work.
     Write tests. *)
  | Pgenarray_set _, Pgcignorableproductarray kinds ->
    Pgcignorableproductarray_set kinds
  | Pgenarray_set m, Pgcscannableproductarray kinds ->
    Pgcscannableproductarray_set (m, kinds)
  | (Pgcignorableproductarray_set kinds1) as k,
    Pgcignorableproductarray kinds2 ->
    if kinds1 = kinds2 then k else
      Misc.fatal_error "mismatched ignorableproductarray kinds in glb"
  | Pgcscannableproductarray_set (mode, kinds1),
    Pgcscannableproductarray kinds2 ->
    begin match glb_scannable_kinds kinds1 kinds2 with
    | Some kinds -> Pgcscannableproductarray_set (mode, kinds)
    | None -> Misc.fatal_error "mismatched scannableproductarray kinds in glb"
    end
  | Pgcignorableproductarray_set _, _ | _, Pgcignorableproductarray _ ->
    Misc.fatal_error "unexpected Pgcignorableproductarray_set kind in glb"
  | Pgcscannableproductarray_set _, _ | _, Pgcscannableproductarray _ ->
    Misc.fatal_error "unexpected Pgcscannableproductarray_set kind in glb"

  (* No GLB; only used in the [Obj.magic] case *)
  | Pfloatarray_set, (Paddrarray | Pintarray)
  | (Paddrarray_set _ | Pintarray_set), Pfloatarray -> t1

  (* Compute the correct GLB *)

  (* Pgenarray >= _ *)
  | (Pgenarray_set _ as x), Pgenarray -> x
  | Pgenarray_set _, Pintarray -> Pintarray_set
  | Pgenarray_set mode, Paddrarray -> Paddrarray_set mode
  | Pgenarray_set _, Pfloatarray -> Pfloatarray_set
  | (Paddrarray_set _ | Pintarray_set | Pfloatarray_set as x), Pgenarray -> x

  (* Paddrarray > Pintarray *)
  | (Paddrarray_set _ as x), Paddrarray -> x
  | Paddrarray_set _, Pintarray -> Pintarray_set
  | Pintarray_set, Paddrarray -> Pintarray_set

  (* Pintarray is a minimum *)
  | Pintarray_set, Pintarray -> Pintarray_set

  (* Pfloatarray is a minimum *)
  | Pfloatarray_set, Pfloatarray -> Pfloatarray_set

(* Specialize a primitive from available type information. *)
(* CR layouts v7: This function had a loc argument added just to support the void
   check error message.  Take it out when we remove that. *)
let specialize_primitive env loc ty ~has_constant_constructor prim =
  let param_tys =
    match is_function_type env ty with
    | None -> []
    | Some (p1, rhs) ->
      match is_function_type env rhs with
      | None -> [p1]
      | Some (p2, _) -> [p1;p2]
  in
  match prim, param_tys with
  | Primitive (Psetfield(n, Pointer, init), arity), [_; p2] -> begin
      match maybe_pointer_type env p2 with
      | Pointer -> None
      | Immediate -> Some (Primitive (Psetfield(n, Immediate, init), arity))
    end
  | Primitive (Pfield (n, Pointer, mut), arity), _ ->
      (* try strength reduction based on the *result type* *)
      let is_int = match is_function_type env ty with
        | None -> Pointer
        | Some (_p1, rhs) -> maybe_pointer_type env rhs in
      Some (Primitive (Pfield (n, is_int, mut), arity))
  | Primitive (Parraylength t, arity), [p] -> begin
      let loc = to_location loc in
      (* CR layouts: [~elt_sort:None] here is not ideal and should be
         fixed. To do that, we will need more checking of primitives
         in the front end. *)
      let array_type =
        glb_array_type loc t (array_type_kind ~elt_sort:None env loc p)
      in
      if t = array_type then None
      else Some (Primitive (Parraylength array_type, arity))
    end
  | Primitive (Parrayrefu (rt, index_kind), arity), p1 :: _ -> begin
      let loc = to_location loc in
      let array_ref_type =
        glb_array_ref_type loc rt (array_type_kind ~elt_sort:None env loc p1)
      in
      if rt = array_ref_type then None
      else Some (Primitive (Parrayrefu (array_ref_type, index_kind), arity))
    end
  | Primitive (Parraysetu (st, index_kind), arity), p1 :: _ -> begin
      let loc = to_location loc in
      let array_set_type =
        glb_array_set_type loc st (array_type_kind ~elt_sort:None env loc p1)
      in
      if st = array_set_type then None
      else Some (Primitive (Parraysetu (array_set_type, index_kind), arity))
    end
  | Primitive (Parrayrefs (rt, index_kind), arity), p1 :: _ -> begin
      let loc = to_location loc in
      let array_ref_type =
        glb_array_ref_type loc rt (array_type_kind ~elt_sort:None env loc p1)
      in
      if rt = array_ref_type then None
      else Some (Primitive (Parrayrefs (array_ref_type, index_kind), arity))
    end
  | Primitive (Parraysets (st, index_kind), arity), p1 :: _ -> begin
      let loc = to_location loc in
      let array_set_type =
        glb_array_set_type loc st (array_type_kind ~elt_sort:None env loc p1)
      in
      if st = array_set_type then None
      else Some (Primitive (Parraysets (array_set_type, index_kind), arity))
    end
  | Primitive (Pmake_unboxed_tuple_vect (at, mode), arity),
    _ :: p2 :: _ -> begin
      let loc = to_location loc in
      let array_type =
        glb_array_type loc at
          (array_kind_of_elt ~elt_sort:None env loc p2)
      in
      if at = array_type then None
      else Some (Primitive (Pmake_unboxed_tuple_vect (array_type, mode), arity))
    end
  | Primitive (Pbigarrayref(unsafe, n, kind, layout), arity), p1 :: _ -> begin
      let (k, l) = bigarray_specialize_kind_and_layout env ~kind ~layout p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayref(unsafe, n, k, l), arity))
    end
  | Primitive (Pbigarrayset(unsafe, n, kind, layout), arity), p1 :: _ -> begin
      let (k, l) = bigarray_specialize_kind_and_layout env ~kind ~layout p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayset(unsafe, n, k, l), arity))
    end
  | Primitive (Pmakeblock(tag, mut, None, mode), arity), fields -> begin
      let shape =
        List.map (fun typ ->
          Lambda.must_be_value (Typeopt.layout env (to_location loc)
                                  Jkind.Sort.for_block_element typ))
          fields
      in
      let useful = List.exists (fun knd -> knd <> Pgenval) shape in
      if useful then
        Some (Primitive (Pmakeblock(tag, mut, Some shape, mode),arity))
      else None
    end
  | Primitive (Patomic_load { immediate_or_pointer = Pointer },
               arity), _ ->begin
      let is_int = match is_function_type env ty with
        | None -> Pointer
        | Some (_p1, rhs) -> maybe_pointer_type env rhs in
      Some (Primitive (Patomic_load {immediate_or_pointer = is_int}, arity))
    end
  | Comparison(comp, Compare_generic), p1 :: _ ->
    if (has_constant_constructor
        && simplify_constant_constructor comp) then begin
      Some (Comparison(comp, Compare_ints))
    end else if (is_base_type env p1 Predef.path_int
        || is_base_type env p1 Predef.path_char
        || (maybe_pointer_type env p1 = Immediate)) then begin
      Some (Comparison(comp, Compare_ints))
    end else if is_base_type env p1 Predef.path_float then begin
      Some (Comparison(comp, Compare_floats))
    end else if is_base_type env p1 Predef.path_float32 then begin
      Some (Comparison(comp, Compare_float32s))
    end else if is_base_type env p1 Predef.path_string then begin
      Some (Comparison(comp, Compare_strings))
    end else if is_base_type env p1 Predef.path_bytes then begin
      Some (Comparison(comp, Compare_bytes))
    end else if is_base_type env p1 Predef.path_nativeint then begin
      Some (Comparison(comp, Compare_nativeints))
    end else if is_base_type env p1 Predef.path_int32 then begin
      Some (Comparison(comp, Compare_int32s))
    end else if is_base_type env p1 Predef.path_int64 then begin
      Some (Comparison(comp, Compare_int64s))
    end else begin
      None
    end
  | _ -> None

let caml_equal =
  Lambda.simple_prim_on_values ~name:"caml_equal" ~arity:2 ~alloc:true
let caml_string_equal =
  Lambda.simple_prim_on_values ~name:"caml_string_equal" ~arity:2 ~alloc:false
let caml_bytes_equal =
  Lambda.simple_prim_on_values ~name:"caml_bytes_equal" ~arity:2 ~alloc:false
let caml_notequal =
  Lambda.simple_prim_on_values ~name:"caml_notequal" ~arity:2 ~alloc:true
let caml_string_notequal =
  Lambda.simple_prim_on_values ~name:"caml_string_notequal" ~arity:2 ~alloc:false
let caml_bytes_notequal =
  Lambda.simple_prim_on_values ~name:"caml_bytes_notequal" ~arity:2 ~alloc:false
let caml_lessequal =
  Lambda.simple_prim_on_values ~name:"caml_lessequal" ~arity:2 ~alloc:true
let caml_string_lessequal =
  Lambda.simple_prim_on_values ~name:"caml_string_lessequal" ~arity:2 ~alloc:false
let caml_bytes_lessequal =
  Lambda.simple_prim_on_values ~name:"caml_bytes_lessequal" ~arity:2 ~alloc:false
let caml_lessthan =
  Lambda.simple_prim_on_values ~name:"caml_lessthan" ~arity:2 ~alloc:true
let caml_string_lessthan =
  Lambda.simple_prim_on_values ~name:"caml_string_lessthan" ~arity:2 ~alloc:false
let caml_bytes_lessthan =
  Lambda.simple_prim_on_values ~name:"caml_bytes_lessthan" ~arity:2 ~alloc:false
let caml_greaterequal =
  Lambda.simple_prim_on_values ~name:"caml_greaterequal" ~arity:2 ~alloc:true
let caml_string_greaterequal =
  Lambda.simple_prim_on_values ~name:"caml_string_greaterequal" ~arity:2
    ~alloc:false
let caml_bytes_greaterequal =
  Lambda.simple_prim_on_values ~name:"caml_bytes_greaterequal" ~arity:2
    ~alloc:false
let caml_greaterthan =
  Lambda.simple_prim_on_values ~name:"caml_greaterthan" ~arity:2 ~alloc:true
let caml_string_greaterthan =
  Lambda.simple_prim_on_values ~name:"caml_string_greaterthan" ~arity:2
    ~alloc:false
let caml_bytes_greaterthan =
  Lambda.simple_prim_on_values ~name:"caml_bytes_greaterthan" ~arity:2
    ~alloc:false
let caml_compare =
  Lambda.simple_prim_on_values ~name:"caml_compare" ~arity:2 ~alloc:true
let caml_string_compare =
  Lambda.simple_prim_on_values ~name:"caml_string_compare" ~arity:2 ~alloc:false
let caml_bytes_compare =
  Lambda.simple_prim_on_values ~name:"caml_bytes_compare" ~arity:2 ~alloc:false

let comparison_primitive comparison comparison_kind =
  match comparison, comparison_kind with
  | Equal, Compare_generic -> Pccall caml_equal
  | Equal, Compare_ints -> Pintcomp Ceq
  | Equal, Compare_floats -> Pfloatcomp (Pfloat64, CFeq)
  | Equal, Compare_float32s -> Pfloatcomp (Pfloat32, CFeq)
  | Equal, Compare_strings -> Pccall caml_string_equal
  | Equal, Compare_bytes -> Pccall caml_bytes_equal
  | Equal, Compare_nativeints -> Pbintcomp(Pnativeint, Ceq)
  | Equal, Compare_int32s -> Pbintcomp(Pint32, Ceq)
  | Equal, Compare_int64s -> Pbintcomp(Pint64, Ceq)
  | Not_equal, Compare_generic -> Pccall caml_notequal
  | Not_equal, Compare_ints -> Pintcomp Cne
  | Not_equal, Compare_floats -> Pfloatcomp (Pfloat64, CFneq)
  | Not_equal, Compare_float32s -> Pfloatcomp (Pfloat32, CFneq)
  | Not_equal, Compare_strings -> Pccall caml_string_notequal
  | Not_equal, Compare_bytes -> Pccall caml_bytes_notequal
  | Not_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cne)
  | Not_equal, Compare_int32s -> Pbintcomp(Pint32, Cne)
  | Not_equal, Compare_int64s -> Pbintcomp(Pint64, Cne)
  | Less_equal, Compare_generic -> Pccall caml_lessequal
  | Less_equal, Compare_ints -> Pintcomp Cle
  | Less_equal, Compare_floats -> Pfloatcomp (Pfloat64, CFle)
  | Less_equal, Compare_float32s -> Pfloatcomp (Pfloat32, CFle)
  | Less_equal, Compare_strings -> Pccall caml_string_lessequal
  | Less_equal, Compare_bytes -> Pccall caml_bytes_lessequal
  | Less_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cle)
  | Less_equal, Compare_int32s -> Pbintcomp(Pint32, Cle)
  | Less_equal, Compare_int64s -> Pbintcomp(Pint64, Cle)
  | Less_than, Compare_generic -> Pccall caml_lessthan
  | Less_than, Compare_ints -> Pintcomp Clt
  | Less_than, Compare_floats -> Pfloatcomp (Pfloat64, CFlt)
  | Less_than, Compare_float32s -> Pfloatcomp (Pfloat32, CFlt)
  | Less_than, Compare_strings -> Pccall caml_string_lessthan
  | Less_than, Compare_bytes -> Pccall caml_bytes_lessthan
  | Less_than, Compare_nativeints -> Pbintcomp(Pnativeint, Clt)
  | Less_than, Compare_int32s -> Pbintcomp(Pint32, Clt)
  | Less_than, Compare_int64s -> Pbintcomp(Pint64, Clt)
  | Greater_equal, Compare_generic -> Pccall caml_greaterequal
  | Greater_equal, Compare_ints -> Pintcomp Cge
  | Greater_equal, Compare_floats -> Pfloatcomp (Pfloat64, CFge)
  | Greater_equal, Compare_float32s -> Pfloatcomp (Pfloat32, CFge)
  | Greater_equal, Compare_strings -> Pccall caml_string_greaterequal
  | Greater_equal, Compare_bytes -> Pccall caml_bytes_greaterequal
  | Greater_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cge)
  | Greater_equal, Compare_int32s -> Pbintcomp(Pint32, Cge)
  | Greater_equal, Compare_int64s -> Pbintcomp(Pint64, Cge)
  | Greater_than, Compare_generic -> Pccall caml_greaterthan
  | Greater_than, Compare_ints -> Pintcomp Cgt
  | Greater_than, Compare_floats -> Pfloatcomp (Pfloat64, CFgt)
  | Greater_than, Compare_float32s -> Pfloatcomp (Pfloat32, CFgt)
  | Greater_than, Compare_strings -> Pccall caml_string_greaterthan
  | Greater_than, Compare_bytes -> Pccall caml_bytes_greaterthan
  | Greater_than, Compare_nativeints -> Pbintcomp(Pnativeint, Cgt)
  | Greater_than, Compare_int32s -> Pbintcomp(Pint32, Cgt)
  | Greater_than, Compare_int64s -> Pbintcomp(Pint64, Cgt)
  | Compare, Compare_generic -> Pccall caml_compare
  | Compare, Compare_ints -> Pcompare_ints
  | Compare, Compare_floats -> Pcompare_floats Pfloat64
  | Compare, Compare_float32s -> Pcompare_floats Pfloat32
  | Compare, Compare_strings -> Pccall caml_string_compare
  | Compare, Compare_bytes -> Pccall caml_bytes_compare
  | Compare, Compare_nativeints -> Pcompare_bints Pnativeint
  | Compare, Compare_int32s -> Pcompare_bints Pint32
  | Compare, Compare_int64s -> Pcompare_bints Pint64

let lambda_of_loc kind sloc =
  let loc = to_location sloc in
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let file =
    if Filename.is_relative file then
      file
    else
      Location.rewrite_absolute_path file in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
    Lconst (Const_block (0, [
          Const_immstring file;
          Const_base (Const_int lnum);
          Const_base (Const_int cnum);
          Const_base (Const_int enum);
        ]))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Compilation_unit.get_current () in
    let module_name =
      match name with
      | None -> "//"^filename^"//"
      | Some comp_unit ->
        Compilation_unit.name_as_string comp_unit
    in
    Lconst (Const_immstring module_name)
  | Loc_LOC ->
    let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
        file lnum cnum enum in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum))
  | Loc_FUNCTION ->
    let scope_name = Debuginfo.Scoped_location.string_of_scoped_location sloc in
    Lconst (Const_immstring scope_name)

let caml_restore_raw_backtrace =
  Lambda.simple_prim_on_values ~name:"caml_restore_raw_backtrace" ~arity:2
    ~alloc:false

let try_ids = Hashtbl.create 8

let add_exception_ident id =
  Hashtbl.replace try_ids id ()

let remove_exception_ident id =
  Hashtbl.remove try_ids id

let lambda_of_prim prim_name prim loc args arg_exps =
  match prim, args with
  | Primitive (prim, arity), args when arity = List.length args ->
      Lprim(prim, args, loc)
  | Sys_argv, [] ->
      Lprim(Pccall prim_sys_argv, [Lconst (const_int 0)], loc)
  | External prim, args ->
      Lprim(Pccall prim, args, loc)
  | Comparison(comp, knd), ([_;_] as args) ->
      let prim = comparison_primitive comp knd in
      Lprim(prim, args, loc)
  | Raise kind, [arg] ->
      let kind =
        match kind, arg with
        | Raise_regular, Lvar argv when Hashtbl.mem try_ids argv ->
            Raise_reraise
        | _, _ ->
            kind
      in
      let arg =
        match arg_exps with
        | None -> arg
        | Some [arg_exp] -> event_after loc arg_exp arg
        | Some _ -> assert false
      in
      Lprim(Praise kind, [arg], loc)
  | Raise_with_backtrace, [exn; bt] ->
      let vexn = Ident.create_local "exn" in
      let raise_arg =
        match arg_exps with
        | None -> Lvar vexn
        | Some [exn_exp; _] -> event_after loc exn_exp (Lvar vexn)
        | Some _ -> assert false
      in
      Llet(Strict, Lambda.layout_block, vexn, exn,
           Lsequence(Lprim(Pccall caml_restore_raw_backtrace,
                           [Lvar vexn; bt],
                           loc),
                     Lprim(Praise Raise_reraise, [raise_arg], loc)))
  | Lazy_force pos, [arg] ->
      Matching.inline_lazy_force arg pos loc
  | Loc kind, [] ->
      lambda_of_loc kind loc
  | Loc kind, [arg] ->
      let lam = lambda_of_loc kind loc in
      Lprim(Pmakeblock(0, Immutable, None, alloc_heap), [lam; arg], loc)
  | Send (pos, layout), [obj; meth] ->
      Lsend(Public, meth, obj, [], pos, alloc_heap, loc, layout)
  | Send_self (pos, layout), [obj; meth] ->
      Lsend(Self, meth, obj, [], pos, alloc_heap, loc, layout)
  | Send_cache (apos, layout), [obj; meth; cache; pos] ->
      (* Cached mode only works in the native backend *)
      if !Clflags.native_code then
        Lsend(Cached, meth, obj, [cache; pos], apos, alloc_heap, loc, layout)
      else
        Lsend(Public, meth, obj, [], apos, alloc_heap, loc, layout)
  | Frame_pointers, [] ->
      let frame_pointers =
        if !Clflags.native_code && Config.with_frame_pointers then 1 else 0
      in
      Lconst (const_int frame_pointers)
  | Identity, [arg] -> arg
  | Apply (pos, layout), [func; arg]
  | Revapply (pos, layout), [arg; func] ->
      Lapply {
        ap_func = func;
        ap_args = [arg];
        ap_result_layout = layout;
        ap_loc = loc;
        (* CR-someday lwhite: it would be nice to be able to give
           application attributes to functions applied with the application
           operators. *)
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inlined;
        ap_specialised = Default_specialise;
        ap_probe = None;
        ap_region_close = pos;
        ap_mode = alloc_heap;
      }
  | Unsupported prim, _ ->
      let exn =
        transl_extension_path loc (Lazy.force Env.initial)
          Predef.path_invalid_argument
      in
      let msg =
        Format.asprintf "Unsupported primitive %a" Printlambda.primitive prim
      in
      Lprim (
        Praise Raise_regular,
        [Lprim (
          Pmakeblock (0, Immutable, None, alloc_heap),
          [exn; Lconst (Const_immstring msg)],
          loc)],
        loc)
  | (Raise _ | Raise_with_backtrace
    | Lazy_force _ | Loc _ | Primitive _ | Sys_argv | Comparison _
    | Send _ | Send_self _ | Send_cache _ | Frame_pointers | Identity
    | Apply _ | Revapply _), _ ->
      raise(Error(to_location loc, Wrong_arity_builtin_primitive prim_name))

let check_primitive_arity loc p =
  let mode =
    match p.prim_native_repr_res with
    | Prim_global, _ | Prim_poly, _ ->
      (* We assume all primitives are compiled to have the same arity for
         different modes and types, so just pick one of the modes in the
         [Prim_poly] case. *)
      Some Mode.Locality.global
    | Prim_local, _ -> Some Mode.Locality.local
  in
  (* By a similar assumption, the sort shouldn't change the arity.  So it's ok
     to lie here. *)
  let sort = Some (Jkind.Sort.of_base Value) in
  let prim =
    lookup_primitive loc ~poly_mode:mode ~poly_sort:sort Rc_normal p
  in
  let ok =
    match prim with
    | Primitive (_,arity) -> arity = p.prim_arity
    | External _ -> true
    | Sys_argv -> p.prim_arity = 0
    | Comparison _ -> p.prim_arity = 2
    | Raise _ -> p.prim_arity = 1
    | Raise_with_backtrace -> p.prim_arity = 2
    | Lazy_force _ -> p.prim_arity = 1
    | Loc _ -> p.prim_arity = 1 || p.prim_arity = 0
    | Send _ | Send_self _ -> p.prim_arity = 2
    | Send_cache _ -> p.prim_arity = 4
    | Frame_pointers -> p.prim_arity = 0
    | Identity -> p.prim_arity = 1
    | Apply _ | Revapply _ -> p.prim_arity = 2
    | Unsupported _ -> true
  in
  if not ok then raise(Error(loc, Wrong_arity_builtin_primitive p.prim_name))

(* Eta-expand a primitive *)

let transl_primitive loc p env ty ~poly_mode ~poly_sort path =
  let prim =
    lookup_primitive_and_mark_used
      (to_location loc) ~poly_mode ~poly_sort Rc_normal p env path
  in
  let has_constant_constructor = false in
  let prim =
    match specialize_primitive env loc ty ~has_constant_constructor prim with
    | None -> prim
    | Some prim -> prim
  in
  let to_locality = to_locality ~poly:poly_mode in
  let error_loc = to_location loc in
  let rec make_params ty repr_args repr_res =
    match repr_args, repr_res with
    | [], (_, res_repr) ->
      let res_sort =
        Jkind.Sort.of_const
          (sort_of_native_repr ~loc:error_loc res_repr ~poly_sort)
      in
      [], Typeopt.layout env error_loc res_sort ty
    | (((_, arg_repr) as arg) :: repr_args), _ ->
      match Typeopt.is_function_type env ty with
      | None ->
          Misc.fatal_errorf "Primitive %s type does not correspond to arity"
            (Primitive.byte_name p)
      | Some (arg_ty, ret_ty) ->
          let arg_sort =
            Jkind.Sort.of_const
              (sort_of_native_repr ~loc:error_loc arg_repr ~poly_sort)
          in
          let arg_layout =
            Typeopt.layout env error_loc arg_sort arg_ty
          in
          let arg_mode = to_locality arg in
          let params, return = make_params ret_ty repr_args repr_res in
          { name = Ident.create_local "prim";
            layout = arg_layout;
            attributes = Lambda.default_param_attribute;
            mode = arg_mode }
          :: params, return
  in
  let params, return =
    make_params ty p.prim_native_repr_args p.prim_native_repr_res
  in
  let args = List.map (fun p -> Lvar p.name) params in
  match params with
  | [] -> lambda_of_prim p.prim_name prim loc args None
  | _ ->
     let loc =
       Debuginfo.Scoped_location.map_scopes (fun ~scopes ->
         Debuginfo.Scoped_location.enter_partial_or_eta_wrapper ~scopes)
         loc
     in
     let body = lambda_of_prim p.prim_name prim loc args None in
     let alloc_mode = to_locality p.prim_native_repr_res in
     let () =
       (* CR mshinwell: Write a version of [primitive_may_allocate] that
          works on the [prim] type. *)
       match body with
       | Lprim (prim, _, _) ->
         (match Lambda.primitive_may_allocate prim with
          | None ->
            (* We don't check anything in this case; if the primitive doesn't
               allocate, then after [Lambda] it will be translated to a term
               not involving any region variables, meaning there would be
               no concern about potentially unbound region variables. *)
            ()
          | Some lambda_alloc_mode ->
            (* In this case we add a check to ensure the middle end has
               the correct information as to whether a region was inserted
               at this point. *)
            match alloc_mode, lambda_alloc_mode with
            | Alloc_heap, Alloc_heap
            | Alloc_local, Alloc_local -> ()
            | Alloc_local, Alloc_heap ->
              (* This case is ok: the Lambda-derived information is more
                 precise.  A region will be inserted, likely unused, and
                 deleted by the middle end. *)
              ()
            | Alloc_heap, Alloc_local ->
              Misc.fatal_errorf "Alloc mode incompatibility for:@ %a@ \
                  (from to_locality, %a; from primitive_may_allocate, %a)"
                Printlambda.lambda body
                Printlambda.alloc_mode alloc_mode
                Printlambda.alloc_mode lambda_alloc_mode
         )
       | _ -> ()
     in
     let region =
       match alloc_mode with
       | Alloc_heap -> true
       | Alloc_local -> false
     in
     let rec count_nlocal = function
       | [] -> assert false
       | [_] -> if region then 0 else 1
       | Alloc_heap :: args -> count_nlocal args
       | (Alloc_local :: _) as args -> List.length args
     in
     let nlocal = count_nlocal (List.map to_locality p.prim_native_repr_args) in
     lfunction
       ~kind:(Curried {nlocal})
       ~params
       ~return
       ~attr:default_stub_attribute
       ~loc
       ~body
       ~mode:alloc_heap
       ~ret_mode:(to_locality p.prim_native_repr_res)
       ~region

let lambda_primitive_needs_event_after = function
  (* We add an event after any primitive resulting in a C call that
     may raise an exception or allocate. These are places where we may
     collect the call stack. *)
  | Pduprecord _ | Pccall _
  | Pfloatofint (_, _)
  | Pfloatoffloat32 _
  | Pfloat32offloat _
  | Pnegfloat (_, _) | Pabsfloat (_, _)
  | Paddfloat (_, _) | Psubfloat (_, _)
  | Pmulfloat (_, _) | Pdivfloat (_, _)
  | Pstringrefs | Pbytesrefs
  | Pbytessets | Pmakearray (Pgenarray, _, _) | Pduparray _
  | Parrayrefu ((Pgenarray_ref _ | Pfloatarray_ref _), _)
  | Parrayrefs _ | Parraysets _ | Pbintofint _ | Pcvtbint _ | Pnegbint _
  | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _
  | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
  | Pbintcomp _ | Punboxed_int_comp _ | Pcompare_bints _
  | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _ | Pstring_load_16 _
  | Pstring_load_32 _ | Pstring_load_f32 _ | Pstring_load_64 _ | Pstring_load_128 _
  | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_f32 _ | Pbytes_load_64 _
  | Pbytes_load_128 _ | Pbytes_set_16 _ | Pbytes_set_32 _  | Pbytes_set_f32 _
  | Pbytes_set_64 _ | Pbytes_set_128 _ | Pbigstring_load_16 _
  | Pbigstring_load_32 _ | Pbigstring_load_f32 _ | Pbigstring_load_64 _
  | Pbigstring_load_128 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pbigstring_set_f32 _ | Pbigstring_set_64 _ | Pbigstring_set_128 _
  | Pfloatarray_load_128 _ | Pfloat_array_load_128 _ | Pint_array_load_128 _
  | Punboxed_float_array_load_128 _| Punboxed_float32_array_load_128 _
  | Punboxed_int32_array_load_128 _ | Punboxed_int64_array_load_128 _
  | Punboxed_nativeint_array_load_128 _
  | Pfloatarray_set_128 _ | Pfloat_array_set_128 _ | Pint_array_set_128 _
  | Punboxed_float_array_set_128 _| Punboxed_float32_array_set_128 _
  | Punboxed_int32_array_set_128 _ | Punboxed_int64_array_set_128 _
  | Punboxed_nativeint_array_set_128 _
  | Prunstack | Pperform | Preperform | Presume
  | Pbbswap _ | Ppoll | Pobj_dup | Pget_header _ -> true
  (* [Preinterpret_tagged_int63_as_unboxed_int64] has to allocate in
     bytecode, because int64# is actually represented as a boxed value. *)
  | Preinterpret_tagged_int63_as_unboxed_int64 -> true

  | Pbytes_to_string | Pbytes_of_string
  | Parray_to_iarray | Parray_of_iarray
  | Pignore | Psetglobal _
  | Pgetglobal _ | Pgetpredef _ | Pmakeblock _ | Pmakefloatblock _
  | Pmakeufloatblock _ | Pmakemixedblock _
  | Pmake_unboxed_product _ | Punboxed_product_field _
  | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Praise _
  | Pufloatfield _ | Psetufloatfield _ | Pmixedfield _ | Psetmixedfield _
  | Psequor | Psequand | Pnot | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _ | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Poffsetint _ | Poffsetref _ | Pintoffloat _
  | Pcompare_ints | Pcompare_floats _
  | Pfloatcomp (_, _) | Punboxed_float_comp (_, _)
  | Pstringlength | Pstringrefu | Pbyteslength | Pbytesrefu
  | Pbytessetu
  | Pmakearray ((Pintarray | Paddrarray | Pfloatarray | Punboxedfloatarray _
                | Punboxedintarray _ | Pgcscannableproductarray _
                | Pgcignorableproductarray _), _, _)
  | Pmake_unboxed_tuple_vect _
  | Parraylength _ | Parrayrefu _ | Parraysetu _ | Pisint _ | Pisout
  | Pprobe_is_enabled _
  | Patomic_exchange | Patomic_cas | Patomic_fetch_add | Patomic_load _
  | Pintofbint _ | Pctconst _ | Pbswap16 | Pint_as_pointer _ | Popaque _
  | Pdls_get
  | Pobj_magic _ | Punbox_float _ | Punbox_int _
  | Preinterpret_unboxed_int64_as_tagged_int63
  (* These don't allocate in bytecode; they're just identity functions: *)
  | Pbox_float (_, _) | Pbox_int _
    -> false

(* Determine if a primitive should be surrounded by an "after" debug event *)
let primitive_needs_event_after = function
  | Primitive (prim,_) -> lambda_primitive_needs_event_after prim
  | External _ | Sys_argv -> true
  | Comparison(comp, knd) ->
      lambda_primitive_needs_event_after (comparison_primitive comp knd)
  | Lazy_force _ | Send _ | Send_self _ | Send_cache _
  | Apply _ | Revapply _ -> true
  | Raise _ | Raise_with_backtrace | Loc _ | Frame_pointers | Identity
  | Unsupported _ -> false

let transl_primitive_application loc p env ty ~poly_mode ~poly_sort
    path exp args arg_exps pos =
  let prim =
    lookup_primitive_and_mark_used
      (to_location loc) ~poly_mode ~poly_sort pos p env (Some path)
  in
  let has_constant_constructor =
    match arg_exps with
    | [_; {exp_desc = Texp_construct(_, {cstr_constant}, _, _)}]
    | [{exp_desc = Texp_construct(_, {cstr_constant}, _, _)}; _] -> cstr_constant
    | [_; {exp_desc = Texp_variant(_, None)}]
    | [{exp_desc = Texp_variant(_, None)}; _] -> true
    | _ -> false
  in
  let prim =
    match specialize_primitive env loc ty ~has_constant_constructor prim with
    | None -> prim
    | Some prim -> prim
  in
  let lam = lambda_of_prim p.prim_name prim loc args (Some arg_exps) in
  let lam =
    if primitive_needs_event_after prim then begin
      match exp with
      | None -> lam
      | Some exp -> event_after loc exp lam
    end else begin
      lam
    end
  in
  lam

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
  | Unknown_builtin_primitive prim_name ->
      fprintf ppf "Unknown builtin primitive %a" Style.inline_code prim_name
  | Wrong_arity_builtin_primitive prim_name ->
      fprintf ppf "Wrong arity for builtin primitive %a"
        Style.inline_code prim_name
  | Invalid_floatarray_glb ->
      fprintf ppf
        "@[Floatarray primitives can't be used on arrays containing@ \
         unboxed types.@]"

  | Unexpected_product_in_prim c ->
      fprintf ppf
        "@[Unboxed product layouts are not yet supported as arguments to@ \
         layout polymorphic externals.@ The layout of this argument is %a.@]"
        Jkind.Sort.Const.format c
let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
