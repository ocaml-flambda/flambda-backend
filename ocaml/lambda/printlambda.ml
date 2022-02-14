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

open Format
open Asttypes
open Primitive
open Types
open Lambda


let rec struct_const ppf = function
  | Const_base(Const_int n) -> fprintf ppf "%i" n
  | Const_base(Const_char c) -> fprintf ppf "%C" c
  | Const_base(Const_string (s, _, _)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base(Const_float f) -> fprintf ppf "%s" f
  | Const_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Const_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl in
      fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_block [] ->
      fprintf ppf "[|b |]"
  | Const_float_block (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|b@[%s%a@]|]@]" f1 floats fl
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let array_kind = function
  | Pgenarray -> "gen"
  | Paddrarray -> "addr"
  | Pintarray -> "int"
  | Pfloatarray -> "float"

let alloc_mode = function
  | Alloc_heap -> ""
  | Alloc_local -> "local"

let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let rec value_kind ppf = function
  | Pgenval -> ()
  | Pintval -> fprintf ppf "[int]"
  | Pfloatval -> fprintf ppf "[float]"
  | Parrayval elt_kind -> fprintf ppf "[%sarray]" (array_kind elt_kind)
  | Pboxedintval bi -> fprintf ppf "[%s]" (boxed_integer_name bi)
  | Pblock { tag; fields } ->
    fprintf ppf "[%d: %a]" tag
      (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
        value_kind') fields

and value_kind' ppf = function
  | Pgenval -> fprintf ppf "*"
  | Pintval -> fprintf ppf "[int]"
  | Pfloatval -> fprintf ppf "[float]"
  | Parrayval elt_kind -> fprintf ppf "[%sarray]" (array_kind elt_kind)
  | Pboxedintval bi -> fprintf ppf "[%s]" (boxed_integer_name bi)
  | Pblock { tag; fields } ->
    fprintf ppf "[%d: %a]" tag
      (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
        value_kind') fields

let return_kind ppf (mode, kind) =
  let smode = alloc_mode mode in
  match kind with
  | Pgenval when mode = Alloc_heap -> ()
  | Pgenval -> fprintf ppf ": %s@ " smode
  | Pintval -> fprintf ppf ": int@ "
  | Pfloatval -> fprintf ppf ": %sfloat@ " smode
  | Parrayval elt_kind ->
     fprintf ppf ": %s%sarray@ " smode (array_kind elt_kind)
  | Pboxedintval bi -> fprintf ppf ": %s%s@ " smode (boxed_integer_name bi)
  | Pblock { tag; fields } ->
    fprintf ppf ": %s[%d: %a]@ " smode tag
      (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
        value_kind') fields

let field_kind ppf = function
  | Pgenval -> pp_print_string ppf "*"
  | Pintval -> pp_print_string ppf "int"
  | Pfloatval -> pp_print_string ppf "float"
  | Parrayval elt_kind -> fprintf ppf "%s-array" (array_kind elt_kind)
  | Pboxedintval bi -> pp_print_string ppf (boxed_integer_name bi)
  | Pblock { tag; fields } ->
    fprintf ppf "[%d: %a]" tag
      (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
        value_kind') fields

let alloc_kind = function
  | Alloc_heap -> ""
  | Alloc_local -> "[L]"

let print_boxed_integer_conversion ppf bi1 bi2 m =
  fprintf ppf "%s_of_%s%s" (boxed_integer_name bi2) (boxed_integer_name bi1)
    (alloc_kind m)

let boxed_integer_mark name bi m =
  match bi with
  | Pnativeint -> Printf.sprintf "Nativeint.%s%s" name (alloc_kind m)
  | Pint32 -> Printf.sprintf "Int32.%s%s" name (alloc_kind m)
  | Pint64 -> Printf.sprintf "Int64.%s%s" name (alloc_kind m)

let print_boxed_integer name ppf bi m =
  fprintf ppf "%s" (boxed_integer_mark name bi m);;

let print_bigarray name unsafe kind ppf layout =
  fprintf ppf "Bigarray.%s[%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    (match kind with
     | Pbigarray_unknown -> "generic"
     | Pbigarray_float32 -> "float32"
     | Pbigarray_float64 -> "float64"
     | Pbigarray_sint8 -> "sint8"
     | Pbigarray_uint8 -> "uint8"
     | Pbigarray_sint16 -> "sint16"
     | Pbigarray_uint16 -> "uint16"
     | Pbigarray_int32 -> "int32"
     | Pbigarray_int64 -> "int64"
     | Pbigarray_caml_int -> "camlint"
     | Pbigarray_native_int -> "nativeint"
     | Pbigarray_complex32 -> "complex32"
     | Pbigarray_complex64 -> "complex64")
    (match layout with
    |  Pbigarray_unknown_layout -> "unknown"
     | Pbigarray_c_layout -> "C"
     | Pbigarray_fortran_layout -> "Fortran")

let record_rep ppf r =
  match r with
  | Record_regular -> fprintf ppf "regular"
  | Record_inlined i -> fprintf ppf "inlined(%i)" i
  | Record_unboxed false -> fprintf ppf "unboxed"
  | Record_unboxed true -> fprintf ppf "inlined(unboxed)"
  | Record_float -> fprintf ppf "float"
  | Record_extension path -> fprintf ppf "ext(%a)" Printtyp.path path
;;

let block_shape ppf shape = match shape with
  | None | Some [] -> ()
  | Some l when List.for_all ((=) Pgenval) l -> ()
  | Some [elt] ->
      Format.fprintf ppf " (%a)" field_kind elt
  | Some (h :: t) ->
      Format.fprintf ppf " (%a" field_kind h;
      List.iter (fun elt ->
          Format.fprintf ppf ",%a" field_kind elt)
        t;
      Format.fprintf ppf ")"

let integer_comparison ppf = function
  | Ceq -> fprintf ppf "=="
  | Cne -> fprintf ppf "!="
  | Clt -> fprintf ppf "<"
  | Cle -> fprintf ppf "<="
  | Cgt -> fprintf ppf ">"
  | Cge -> fprintf ppf ">="

let float_comparison ppf = function
  | CFeq -> fprintf ppf "==."
  | CFneq -> fprintf ppf "!=."
  | CFlt -> fprintf ppf "<."
  | CFnlt -> fprintf ppf "!<."
  | CFle -> fprintf ppf "<=."
  | CFnle -> fprintf ppf "!<=."
  | CFgt -> fprintf ppf ">."
  | CFngt -> fprintf ppf "!>."
  | CFge -> fprintf ppf ">=."
  | CFnge -> fprintf ppf "!>=."

let field_read_semantics ppf sem =
  match sem with
  | Reads_agree -> ()
  | Reads_vary -> fprintf ppf "_mut"

let primitive ppf = function
  | Pidentity -> fprintf ppf "id"
  | Pbytes_to_string -> fprintf ppf "bytes_to_string"
  | Pbytes_of_string -> fprintf ppf "bytes_of_string"
  | Pignore -> fprintf ppf "ignore"
  | Prevapply _ -> fprintf ppf "revapply"
  | Pdirapply _ -> fprintf ppf "dirapply"
  | Pgetglobal id -> fprintf ppf "global %a" Ident.print id
  | Psetglobal id -> fprintf ppf "setglobal %a" Ident.print id
  | Pmakeblock(tag, Immutable, shape, mode) ->
      fprintf ppf "make%sblock %i%a"
        (alloc_mode mode) tag block_shape shape
  | Pmakeblock(tag, Immutable_unique, shape, mode) ->
      fprintf ppf "make%sblock_unique %i%a"
        (alloc_mode mode) tag block_shape shape
  | Pmakeblock(tag, Mutable, shape, mode) ->
      fprintf ppf "make%smutable %i%a"
        (alloc_mode mode) tag block_shape shape
  | Pmakefloatblock (Immutable, mode) ->
      fprintf ppf "make%sfloatblock Immutable"
        (alloc_mode mode)
  | Pmakefloatblock (Immutable_unique, mode) ->
     fprintf ppf "make%sfloatblock Immutable_unique"
        (alloc_mode mode)
  | Pmakefloatblock (Mutable, mode) ->
     fprintf ppf "make%sfloatblock Mutable"
        (alloc_mode mode)
  | Pfield (n, sem) ->
      fprintf ppf "field%a %i" field_read_semantics sem n
  | Pfield_computed sem ->
      fprintf ppf "field_computed%a" field_read_semantics sem
  | Psetfield(n, ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
        | Local_assignment -> "(local)"
      in
      fprintf ppf "setfield_%s%s %i" instr init n
  | Psetfield_computed (ptr, init) ->
      let instr =
        match ptr with
        | Pointer -> "ptr"
        | Immediate -> "imm"
      in
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
        | Local_assignment -> "(local)"
      in
      fprintf ppf "setfield_%s%s_computed" instr init
  | Pfloatfield (n, sem, mode) ->
      fprintf ppf "floatfield%a%s %i"
        field_read_semantics sem (alloc_mode mode) n
  | Psetfloatfield (n, init) ->
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment -> ""
        | Local_assignment -> "(local)"
      in
      fprintf ppf "setfloatfield%s %i" init n
  | Pduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size
  | Pccall p -> fprintf ppf "%s" p.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint Safe -> fprintf ppf "/"
  | Pdivint Unsafe -> fprintf ppf "/u"
  | Pmodint Safe -> fprintf ppf "mod"
  | Pmodint Unsafe -> fprintf ppf "mod_unsafe"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(cmp) -> integer_comparison ppf cmp
  | Pcompare_ints -> fprintf ppf "compare_ints"
  | Pcompare_floats -> fprintf ppf "compare_floats"
  | Pcompare_bints bi -> fprintf ppf "compare_bints %s" (boxed_integer_name bi)
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i"n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint m -> fprintf ppf "float_of_int%s" (alloc_kind m)
  | Pnegfloat m -> fprintf ppf "~.%s" (alloc_kind m)
  | Pabsfloat m -> fprintf ppf "abs.%s" (alloc_kind m)
  | Paddfloat m -> fprintf ppf "+.%s" (alloc_kind m)
  | Psubfloat m -> fprintf ppf "-.%s" (alloc_kind m)
  | Pmulfloat m -> fprintf ppf "*.%s" (alloc_kind m)
  | Pdivfloat m -> fprintf ppf "/.%s" (alloc_kind m)
  | Pfloatcomp(cmp) -> float_comparison ppf cmp
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pbyteslength -> fprintf ppf "bytes.length"
  | Pbytesrefu -> fprintf ppf "bytes.unsafe_get"
  | Pbytessetu -> fprintf ppf "bytes.unsafe_set"
  | Pbytesrefs -> fprintf ppf "bytes.get"
  | Pbytessets -> fprintf ppf "bytes.set"

  | Parraylength k -> fprintf ppf "array.length[%s]" (array_kind k)
  | Pmakearray (k, Mutable, mode) ->
     fprintf ppf "make%sarray[%s]" (alloc_mode mode) (array_kind k)
  | Pmakearray (k, Immutable, mode) ->
     fprintf ppf "make%sarray_imm[%s]" (alloc_mode mode) (array_kind k)
  | Pmakearray (k, Immutable_unique, mode) ->
      fprintf ppf "make%sarray_unique[%s]" (alloc_mode mode) (array_kind k)
  | Pduparray (k, Mutable) -> fprintf ppf "duparray[%s]" (array_kind k)
  | Pduparray (k, Immutable) -> fprintf ppf "duparray_imm[%s]" (array_kind k)
  | Pduparray (k, Immutable_unique) ->
      fprintf ppf "duparray_unique[%s]" (array_kind k)
  | Parrayrefu k -> fprintf ppf "array.unsafe_get[%s]" (array_kind k)
  | Parraysetu k -> fprintf ppf "array.unsafe_set[%s]" (array_kind k)
  | Parrayrefs k -> fprintf ppf "array.get[%s]" (array_kind k)
  | Parraysets k -> fprintf ppf "array.set[%s]" (array_kind k)
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     fprintf ppf "sys.constant_%s" const_name
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint (bi,m) -> print_boxed_integer "of_int" ppf bi m
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi Alloc_heap
  | Pcvtbint (bi1, bi2, m) -> print_boxed_integer_conversion ppf bi1 bi2 m
  | Pnegbint (bi,m) -> print_boxed_integer "neg" ppf bi m
  | Paddbint (bi,m) -> print_boxed_integer "add" ppf bi m
  | Psubbint (bi,m) -> print_boxed_integer "sub" ppf bi m
  | Pmulbint (bi,m) -> print_boxed_integer "mul" ppf bi m
  | Pdivbint { size = bi; is_safe = Safe; mode } ->
      print_boxed_integer "div" ppf bi mode
  | Pdivbint { size = bi; is_safe = Unsafe; mode } ->
      print_boxed_integer "div_unsafe" ppf bi mode
  | Pmodbint { size = bi; is_safe = Safe; mode } ->
      print_boxed_integer "mod" ppf bi mode
  | Pmodbint { size = bi; is_safe = Unsafe; mode } ->
      print_boxed_integer "mod_unsafe" ppf bi mode
  | Pandbint (bi,m) -> print_boxed_integer "and" ppf bi m
  | Porbint (bi,m) -> print_boxed_integer "or" ppf bi m
  | Pxorbint (bi,m) -> print_boxed_integer "xor" ppf bi m
  | Plslbint (bi,m) -> print_boxed_integer "lsl" ppf bi m
  | Plsrbint (bi,m) -> print_boxed_integer "lsr" ppf bi m
  | Pasrbint (bi,m) -> print_boxed_integer "asr" ppf bi m
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi Alloc_heap
  | Pbintcomp(bi, Cne) -> print_boxed_integer "!=" ppf bi Alloc_heap
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi Alloc_heap
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi Alloc_heap
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi Alloc_heap
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi Alloc_heap
  | Pbigarrayref(unsafe, _n, kind, layout) ->
      print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, _n, kind, layout) ->
      print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get16"
     else fprintf ppf "string.get16"
  | Pstring_load_32(unsafe, m) ->
     if unsafe then fprintf ppf "string.unsafe_get32%s" (alloc_kind m)
     else fprintf ppf "string.get32%s" (alloc_kind m)
  | Pstring_load_64(unsafe, m) ->
     if unsafe then fprintf ppf "string.unsafe_get64%s" (alloc_kind m)
     else fprintf ppf "string.get64%s" (alloc_kind m)
  | Pbytes_load_16(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_get16"
     else fprintf ppf "bytes.get16"
  | Pbytes_load_32(unsafe,m) ->
     if unsafe then fprintf ppf "bytes.unsafe_get32%s" (alloc_kind m)
     else fprintf ppf "bytes.get32%s" (alloc_kind m)
  | Pbytes_load_64(unsafe,m) ->
     if unsafe then fprintf ppf "bytes.unsafe_get64%s" (alloc_kind m)
     else fprintf ppf "bytes.get64%s" (alloc_kind m)
  | Pbytes_set_16(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set16"
     else fprintf ppf "bytes.set16"
  | Pbytes_set_32(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set32"
     else fprintf ppf "bytes.set32"
  | Pbytes_set_64(unsafe) ->
     if unsafe then fprintf ppf "bytes.unsafe_set64"
     else fprintf ppf "bytes.set64"
  | Pbigstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get16"
     else fprintf ppf "bigarray.array1.get16"
  | Pbigstring_load_32(unsafe,m) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get32%s" (alloc_kind m)
     else fprintf ppf "bigarray.array1.get32%s" (alloc_kind m)
  | Pbigstring_load_64(unsafe,m) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get64%s" (alloc_kind m)
     else fprintf ppf "bigarray.array1.get64%s" (alloc_kind m)
  | Pbigstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set16"
     else fprintf ppf "bigarray.array1.set16"
  | Pbigstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set32"
     else fprintf ppf "bigarray.array1.set32"
  | Pbigstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set64"
     else fprintf ppf "bigarray.array1.set64"
  | Pbswap16 -> fprintf ppf "bswap16"
  | Pbbswap(bi,m) -> print_boxed_integer "bswap" ppf bi m
  | Pint_as_pointer -> fprintf ppf "int_as_pointer"
  | Popaque -> fprintf ppf "opaque"
  | Pprobe_is_enabled {name} -> fprintf ppf "probe_is_enabled[%s]" name

let name_of_primitive = function
  | Pidentity -> "Pidentity"
  | Pbytes_of_string -> "Pbytes_of_string"
  | Pbytes_to_string -> "Pbytes_to_string"
  | Pignore -> "Pignore"
  | Prevapply _ -> "Prevapply"
  | Pdirapply _ -> "Pdirapply"
  | Pgetglobal _ -> "Pgetglobal"
  | Psetglobal _ -> "Psetglobal"
  | Pmakeblock _ -> "Pmakeblock"
  | Pmakefloatblock _ -> "Pmakefloatblock"
  | Pfield _ -> "Pfield"
  | Pfield_computed _ -> "Pfield_computed"
  | Psetfield _ -> "Psetfield"
  | Psetfield_computed _ -> "Psetfield_computed"
  | Pfloatfield _ -> "Pfloatfield"
  | Psetfloatfield _ -> "Psetfloatfield"
  | Pduprecord _ -> "Pduprecord"
  | Pccall _ -> "Pccall"
  | Praise _ -> "Praise"
  | Psequand -> "Psequand"
  | Psequor -> "Psequor"
  | Pnot -> "Pnot"
  | Pnegint -> "Pnegint"
  | Paddint -> "Paddint"
  | Psubint -> "Psubint"
  | Pmulint -> "Pmulint"
  | Pdivint _ -> "Pdivint"
  | Pmodint _ -> "Pmodint"
  | Pandint -> "Pandint"
  | Porint -> "Porint"
  | Pxorint -> "Pxorint"
  | Plslint -> "Plslint"
  | Plsrint -> "Plsrint"
  | Pasrint -> "Pasrint"
  | Pintcomp _ -> "Pintcomp"
  | Pcompare_ints -> "Pcompare_ints"
  | Pcompare_floats -> "Pcompare_floats"
  | Pcompare_bints _ -> "Pcompare"
  | Poffsetint _ -> "Poffsetint"
  | Poffsetref _ -> "Poffsetref"
  | Pintoffloat -> "Pintoffloat"
  | Pfloatofint _ -> "Pfloatofint"
  | Pnegfloat _ -> "Pnegfloat"
  | Pabsfloat _ -> "Pabsfloat"
  | Paddfloat _ -> "Paddfloat"
  | Psubfloat _ -> "Psubfloat"
  | Pmulfloat _ -> "Pmulfloat"
  | Pdivfloat _ -> "Pdivfloat"
  | Pfloatcomp _ -> "Pfloatcomp"
  | Pstringlength -> "Pstringlength"
  | Pstringrefu -> "Pstringrefu"
  | Pstringrefs -> "Pstringrefs"
  | Pbyteslength -> "Pbyteslength"
  | Pbytesrefu -> "Pbytesrefu"
  | Pbytessetu -> "Pbytessetu"
  | Pbytesrefs -> "Pbytesrefs"
  | Pbytessets -> "Pbytessets"
  | Parraylength _ -> "Parraylength"
  | Pmakearray _ -> "Pmakearray"
  | Pduparray _ -> "Pduparray"
  | Parrayrefu _ -> "Parrayrefu"
  | Parraysetu _ -> "Parraysetu"
  | Parrayrefs _ -> "Parrayrefs"
  | Parraysets _ -> "Parraysets"
  | Pctconst _ -> "Pctconst"
  | Pisint -> "Pisint"
  | Pisout -> "Pisout"
  | Pbintofint _ -> "Pbintofint"
  | Pintofbint _ -> "Pintofbint"
  | Pcvtbint _ -> "Pcvtbint"
  | Pnegbint _ -> "Pnegbint"
  | Paddbint _ -> "Paddbint"
  | Psubbint _ -> "Psubbint"
  | Pmulbint _ -> "Pmulbint"
  | Pdivbint _ -> "Pdivbint"
  | Pmodbint _ -> "Pmodbint"
  | Pandbint _ -> "Pandbint"
  | Porbint _ -> "Porbint"
  | Pxorbint _ -> "Pxorbint"
  | Plslbint _ -> "Plslbint"
  | Plsrbint _ -> "Plsrbint"
  | Pasrbint _ -> "Pasrbint"
  | Pbintcomp _ -> "Pbintcomp"
  | Pbigarrayref _ -> "Pbigarrayref"
  | Pbigarrayset _ -> "Pbigarrayset"
  | Pbigarraydim _ -> "Pbigarraydim"
  | Pstring_load_16 _ -> "Pstring_load_16"
  | Pstring_load_32 _ -> "Pstring_load_32"
  | Pstring_load_64 _ -> "Pstring_load_64"
  | Pbytes_load_16 _ -> "Pbytes_load_16"
  | Pbytes_load_32 _ -> "Pbytes_load_32"
  | Pbytes_load_64 _ -> "Pbytes_load_64"
  | Pbytes_set_16 _ -> "Pbytes_set_16"
  | Pbytes_set_32 _ -> "Pbytes_set_32"
  | Pbytes_set_64 _ -> "Pbytes_set_64"
  | Pbigstring_load_16 _ -> "Pbigstring_load_16"
  | Pbigstring_load_32 _ -> "Pbigstring_load_32"
  | Pbigstring_load_64 _ -> "Pbigstring_load_64"
  | Pbigstring_set_16 _ -> "Pbigstring_set_16"
  | Pbigstring_set_32 _ -> "Pbigstring_set_32"
  | Pbigstring_set_64 _ -> "Pbigstring_set_64"
  | Pbswap16 -> "Pbswap16"
  | Pbbswap _ -> "Pbbswap"
  | Pint_as_pointer -> "Pint_as_pointer"
  | Popaque -> "Popaque"
  | Pprobe_is_enabled _ -> "Pprobe_is_enabled"

let function_attribute ppf { inline; specialise; local; is_a_functor; stub } =
  if is_a_functor then
    fprintf ppf "is_a_functor@ ";
  if stub then
    fprintf ppf "stub@ ";
  begin match inline with
  | Default_inline -> ()
  | Always_inline -> fprintf ppf "always_inline@ "
  | Available_inline -> fprintf ppf "available_inline@ "
  | Never_inline -> fprintf ppf "never_inline@ "
  | Unroll i -> fprintf ppf "unroll(%i)@ " i
  end;
  begin match specialise with
  | Default_specialise -> ()
  | Always_specialise -> fprintf ppf "always_specialise@ "
  | Never_specialise -> fprintf ppf "never_specialise@ "
  end;
  begin match local with
  | Default_local -> ()
  | Always_local -> fprintf ppf "always_local@ "
  | Never_local -> fprintf ppf "never_local@ "
  end

let apply_tailcall_attribute ppf = function
  | Default_tailcall -> ()
  | Tailcall_expectation true ->
    fprintf ppf " tailcall"
  | Tailcall_expectation false ->
    fprintf ppf " tailcall(false)"

let apply_inlined_attribute ppf = function
  | Default_inlined -> ()
  | Always_inlined -> fprintf ppf " always_inline"
  | Never_inlined -> fprintf ppf " never_inline"
  | Hint_inlined -> fprintf ppf " hint_inline"
  | Unroll i -> fprintf ppf " never_inline(%i)" i

let apply_specialised_attribute ppf = function
  | Default_specialise -> ()
  | Always_specialise -> fprintf ppf " always_specialise"
  | Never_specialise -> fprintf ppf " never_specialise"

let apply_probe ppf = function
  | None -> ()
  | Some {name} -> fprintf ppf " (probe %s)" name

let apply_kind name pos mode =
  let name =
    match pos with
    | Rc_normal -> name
    | Rc_close_at_apply -> name ^ "tail"
  in
  name ^ alloc_kind mode

let rec lam ppf = function
  | Lvar id ->
      Ident.print ppf id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply ap ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let form = apply_kind "apply" ap.ap_region_close ap.ap_mode in
      fprintf ppf "@[<2>(%s@ %a%a%a%a%a%a)@]" form
        lam ap.ap_func lams ap.ap_args
        apply_tailcall_attribute ap.ap_tailcall
        apply_inlined_attribute ap.ap_inlined
        apply_specialised_attribute ap.ap_specialised
        apply_probe ap.ap_probe
  | Lfunction{kind; params; return; body; attr; mode; region} ->
      let pr_params ppf params =
        match kind with
        | Curried {nlocal} ->
            let first_local = List.length params - nlocal in
            List.iteri (fun i (param, k) ->
                fprintf ppf "@ %a%a%s" Ident.print param value_kind k
                  (if first_local <= i then "[->L]" else "")) params
        | Tupled ->
            fprintf ppf " (";
            let first = ref true in
            List.iter
              (fun (param, k) ->
                if !first then first := false else fprintf ppf ",@ ";
                Ident.print ppf param;
                value_kind ppf k)
              params;
            fprintf ppf ")" in
      let rmode = if region then Alloc_heap else Alloc_local in
      fprintf ppf "@[<2>(function%s%a@ %a%a%a)@]"
        (alloc_kind mode) pr_params params
        function_attribute attr return_kind (rmode, return) lam body
  | Llet _ as expr ->
      let kind = function
        Alias -> "a" | Strict -> "" | StrictOpt -> "o" | Variable -> "v"
      in
      let rec letbody ~sp = function
        | Llet(str, k, id, arg, body) ->
            if sp then fprintf ppf "@ ";
            fprintf ppf "@[<2>%a =%s%a@ %a@]"
              Ident.print id (kind str) value_kind k lam arg;
            letbody ~sp:true body
        | expr -> expr
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(";
      let expr = letbody ~sp:false expr in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw, _loc, _kind) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.sw_blocks ;
        begin match sw.sw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
       "@[<1>(%s %a@ @[<v 0>%a@])@]"
       (match sw.sw_failaction with None -> "switch*" | _ -> "switch")
       lam larg switch sw
  | Lstringswitch(arg, cases, default, _, _kind) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Lstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Lstaticcatch(lbody, (i, vars), lhandler, _kind) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars ->
           List.iter
             (fun (x, k) -> fprintf ppf " %a%a" Ident.print x value_kind k)
             vars
        )
        vars
        lam lhandler
  | Ltrywith(lbody, param, lhandler, _kind) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Lifthenelse(lcond, lif, lelse, _kind) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (k, met, obj, largs, pos, reg, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Self then "self" else if k = Cached then "cache" else "" in
      let form = apply_kind "send" pos reg in
      fprintf ppf "@[<2>(%s%s@ %a@ %a%a)@]" form kind lam obj lam met args largs
  | Levent(expr, ev) ->
      let kind =
       match ev.lev_kind with
       | Lev_before -> "before"
       | Lev_after _  -> "after"
       | Lev_function -> "funct-body"
       | Lev_pseudo -> "pseudo"
       | Lev_module_definition ident ->
         Format.asprintf "module-defn(%a)" Ident.print ident
      in
      (* -dno-locations also hides the placement of debug events;
         this is good for the readability of the resulting output (usually
         the end-user goal when using -dno-locations), as it strongly
         reduces the nesting level of subterms. *)
      if not !Clflags.locations then lam ppf expr
      else begin match ev.lev_loc with
      | Loc_unknown ->
        fprintf ppf "@[<2>(%s <unknown location>@ %a)@]" kind lam expr
      | Loc_known {scopes; loc} ->
        fprintf ppf "@[<2>(%s %s %s(%i)%s:%i-%i@ %a)@]" kind
                (Debuginfo.Scoped_location.string_of_scopes scopes)
                loc.Location.loc_start.Lexing.pos_fname
                loc.Location.loc_start.Lexing.pos_lnum
                (if loc.Location.loc_ghost then "<ghost>" else "")
                loc.Location.loc_start.Lexing.pos_cnum
                loc.Location.loc_end.Lexing.pos_cnum
                lam expr
      end
  | Lifused(id, expr) ->
      fprintf ppf "@[<2>(ifused@ %a@ %a)@]" Ident.print id lam expr
  | Lregion expr ->
      fprintf ppf "@[<2>(region@ %a)@]" lam expr

and sequence ppf = function
  | Lsequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

let structured_constant = struct_const

let lambda = lam

let program ppf { code } = lambda ppf code
