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

let boxed_integer_name = function
  | Lambda.Pnativeint -> "nativeint"
  | Lambda.Pint32 -> "int32"
  | Lambda.Pint64 -> "int64"

let boxed_integer_mark name = function
  | Lambda.Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Lambda.Pint32 -> Printf.sprintf "Int32.%s" name
  | Lambda.Pint64 -> Printf.sprintf "Int64.%s" name

let alloc_kind = function
  | Lambda.Alloc_heap -> ""
  | Lambda.Alloc_local -> "[L]"

let print_boxed_integer name ppf bi m =
  fprintf ppf "%s%s" (boxed_integer_mark name bi) (alloc_kind m);;

let array_kind array_kind =
  let open Lambda in
  match array_kind with
  | Pgenarray -> "gen"
  | Paddrarray -> "addr"
  | Pintarray -> "int"
  | Pfloatarray -> "float"

let access_size size =
  let open Clambda_primitives in
  match size with
  | Sixteen -> "16"
  | Thirty_two -> "32"
  | Sixty_four -> "64"

let access_safety safety =
  let open Lambda in
  match safety with
  | Safe -> ""
  | Unsafe -> "unsafe_"

let primitive ppf (prim:Clambda_primitives.primitive) =
  let open Lambda in
  let open Clambda_primitives in
  match prim with
  | Pread_symbol sym ->
      fprintf ppf "read_symbol %s" sym
  | Pmakeblock(tag, mut, shape, mode) ->
      let mode = match mode with
        | Alloc_heap -> ""
        | Alloc_local -> "local"
      in
      let mut = match mut with
        | Immutable -> "block"
        | Immutable_unique -> "block_unique"
        | Mutable -> "mutable"
      in
      let name = "make" ^ mode ^ mut in
      fprintf ppf "%s %i%a" name tag Printlambda.block_shape shape
  | Pfield n -> fprintf ppf "field %i" n
  | Pfield_computed -> fprintf ppf "field_computed"
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
        | Assignment Alloc_heap -> ""
        | Assignment Alloc_local -> "(local)"
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
        | Assignment Alloc_heap -> ""
        | Assignment Alloc_local -> "(local)"
      in
      fprintf ppf "setfield_%s%s_computed" instr init
  | Pfloatfield (n, Alloc_heap) -> fprintf ppf "floatfield %i" n
  | Pfloatfield (n, Alloc_local) -> fprintf ppf "floatfieldlocal %i" n
  | Psetfloatfield (n, init) ->
      let init =
        match init with
        | Heap_initialization -> "(heap-init)"
        | Root_initialization -> "(root-init)"
        | Assignment Alloc_heap -> ""
        | Assignment Alloc_local -> "(local)"
      in
      fprintf ppf "setfloatfield%s %i" init n
  | Pduprecord (rep, size) ->
      fprintf ppf "duprecord %a %i" Printlambda.record_rep rep size
  | Pccall p -> fprintf ppf "%s" p.Primitive.prim_name
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
  | Pintcomp(cmp) -> Printlambda.integer_comparison ppf cmp
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
  | Pfloatcomp(cmp) -> Printlambda.float_comparison ppf cmp
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pbyteslength -> fprintf ppf "bytes.length"
  | Pbytesrefu -> fprintf ppf "bytes.unsafe_get"
  | Pbytessetu -> fprintf ppf "bytes.unsafe_set"
  | Pbytesrefs -> fprintf ppf "bytes.get"
  | Pbytessets -> fprintf ppf "bytes.set"

  | Parraylength k -> fprintf ppf "array.length[%s]" (array_kind k)
  | Pmakearray (k, mut, mode) ->
     let mode = match mode with Alloc_local -> "local" | Alloc_heap -> "" in
     let mut = match mut with
       | Mutable -> ""
       | Immutable -> "_imm"
       | Immutable_unique -> "_unique"
     in
     fprintf ppf "make%sarray%s[%s]" mut mode (array_kind k)
  | Pduparray (k, Mutable) -> fprintf ppf "duparray[%s]" (array_kind k)
  | Pduparray (k, Immutable) -> fprintf ppf "duparray_imm[%s]" (array_kind k)
  | Pduparray (k, Immutable_unique) ->
    fprintf ppf "duparray_unique[%s]" (array_kind k)
  | Parrayrefu k -> fprintf ppf "array.unsafe_get[%s]" (array_kind k)
  | Parraysetu k -> fprintf ppf "array.unsafe_set[%s]" (array_kind k)
  | Parrayrefs k -> fprintf ppf "array.get[%s]" (array_kind k)
  | Parraysets k -> fprintf ppf "array.set[%s]" (array_kind k)
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbintofint (bi,m) -> print_boxed_integer "of_int" ppf bi m
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi alloc_heap
  | Pcvtbint (bi1, bi2, m) ->
      fprintf ppf "%s_of_%s%s" (boxed_integer_name bi2) (boxed_integer_name bi1)
        (alloc_kind m)
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
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi alloc_heap
  | Pbintcomp(bi, Cne) -> print_boxed_integer "!=" ppf bi alloc_heap
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi alloc_heap
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi alloc_heap
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi alloc_heap
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi alloc_heap
  | Pbigarrayref(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, _n, kind, layout) ->
      Printlambda.print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load(size, safety, mode) ->
      fprintf ppf "string.%sget%s%s" (access_safety safety) (access_size size)
        (alloc_kind mode)
  | Pbytes_load(size, safety, mode) ->
      fprintf ppf "bytes.%sget%s%s" (access_safety safety) (access_size size)
        (alloc_kind mode)
  | Pbytes_set(size, safety) ->
      fprintf ppf "bytes.%sset%s" (access_safety safety) (access_size size)
  | Pbigstring_load(size, safety, mode) ->
      fprintf ppf "bigarray.array1.%sget%s%s"
        (access_safety safety) (access_size size) (alloc_kind mode)
  | Pbigstring_set(size, safety) ->
      fprintf ppf "bigarray.array1.%sset%s"
        (access_safety safety) (access_size size)
  | Pbswap16 -> fprintf ppf "bswap16"
  | Pbbswap(bi,m) -> print_boxed_integer "bswap" ppf bi m
  | Pint_as_pointer -> fprintf ppf "int_as_pointer"
  | Popaque -> fprintf ppf "opaque"
  | Pprobe_is_enabled {name} -> fprintf ppf "probe_is_enabled[%s]" name
