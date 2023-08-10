(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let convert_unsafety is_unsafe : Clambda_primitives.is_safe =
  if is_unsafe then
    Unsafe
  else
    Safe

let convert (prim : Lambda.primitive) : Clambda_primitives.primitive =
  match prim with
  | Pmakeblock (tag, mutability, shape, mode) ->
      Pmakeblock (tag, mutability, shape, mode)
  | Pmakefloatblock (mutability, mode) ->
      Pmakearray (Pfloatarray, mutability, mode)
  | Pfield (field, _sem) -> Pfield (field, Pvalue Pgenval)
  | Pfield_computed _sem -> Pfield_computed
  | Psetfield (field, imm_or_pointer, init_or_assign) ->
      Psetfield (field, imm_or_pointer, init_or_assign)
  | Psetfield_computed (imm_or_pointer, init_or_assign) ->
      Psetfield_computed (imm_or_pointer, init_or_assign)
  | Pfloatfield (field, _sem, mode) -> Pfloatfield (field, mode)
  | Psetfloatfield (field, init_or_assign) ->
      Psetfloatfield (field, init_or_assign)
  | Pduprecord (repr, size) -> Pduprecord (repr, size)
  | Pccall prim -> Pccall prim
  | Praise kind -> Praise kind
  | Psequand -> Psequand
  | Psequor -> Psequor
  | Pnot -> Pnot
  | Pnegint -> Pnegint
  | Paddint -> Paddint
  | Psubint -> Psubint
  | Pmulint -> Pmulint
  | Pdivint is_safe -> Pdivint is_safe
  | Pmodint is_safe -> Pmodint is_safe
  | Pandint -> Pandint
  | Porint -> Porint
  | Pxorint -> Pxorint
  | Plslint -> Plslint
  | Plsrint -> Plsrint
  | Pasrint -> Pasrint
  | Pintcomp comp -> Pintcomp comp
  | Pcompare_ints -> Pcompare_ints
  | Pcompare_floats -> Pcompare_floats
  | Pcompare_bints bi -> Pcompare_bints bi
  | Poffsetint offset -> Poffsetint offset
  | Poffsetref offset -> Poffsetref offset
  | Pintoffloat -> Pintoffloat
  | Pfloatofint m -> Pfloatofint m
  | Pnegfloat m -> Pnegfloat m
  | Pabsfloat m -> Pabsfloat m
  | Paddfloat m -> Paddfloat m
  | Psubfloat m -> Psubfloat m
  | Pmulfloat m -> Pmulfloat m
  | Pdivfloat m -> Pdivfloat m
  | Pfloatcomp comp -> Pfloatcomp comp
  | Pstringlength -> Pstringlength
  | Pstringrefu -> Pstringrefu
  | Pstringrefs -> Pstringrefs
  | Pbyteslength -> Pbyteslength
  | Pbytesrefu -> Pbytesrefu
  | Pbytessetu -> Pbytessetu
  | Pbytesrefs -> Pbytesrefs
  | Pbytessets -> Pbytessets
  | Pmakearray (kind, mutability, mode) -> Pmakearray (kind, mutability, mode)
  | Pduparray (kind, mutability) -> Pduparray (kind, mutability)
  | Parraylength kind -> Parraylength kind
  | Parrayrefu rkind -> Parrayrefu rkind
  | Parraysetu skind -> Parraysetu skind
  | Parrayrefs rkind -> Parrayrefs rkind
  | Parraysets skind -> Parraysets skind
  | Pisint _ -> Pisint
  | Pisout -> Pisout
  | Pcvtbint (src, dest, m) -> Pcvtbint (src, dest, m)
  | Pnegbint (bi,m) -> Pnegbint (bi,m)
  | Paddbint (bi,m) -> Paddbint (bi,m)
  | Psubbint (bi,m) -> Psubbint (bi,m)
  | Pmulbint (bi,m) -> Pmulbint (bi,m)
  | Pbintofint (bi,m) -> Pbintofint (bi,m)
  | Pintofbint bi -> Pintofbint bi
  | Pandbint (bi,m) -> Pandbint (bi,m)
  | Porbint (bi,m) -> Porbint (bi,m)
  | Pxorbint (bi,m) -> Pxorbint (bi,m)
  | Plslbint (bi,m) -> Plslbint (bi,m)
  | Plsrbint (bi,m) -> Plsrbint (bi,m)
  | Pasrbint (bi,m) -> Pasrbint (bi,m)
  | Pbbswap (bi,m) -> Pbbswap (bi,m)
  | Pdivbint { size; is_safe; mode } -> Pdivbint { size; is_safe; mode }
  | Pmodbint { size; is_safe; mode } -> Pmodbint { size; is_safe; mode }
  | Pbintcomp (bi, comp) -> Pbintcomp (bi, comp)
  | Pbigarrayref (safe, dims, kind, layout) ->
      Pbigarrayref (safe, dims, kind, layout)
  | Pbigarrayset (safe, dims, kind, layout) ->
      Pbigarrayset (safe, dims, kind, layout)
  | Pstring_load_16 is_unsafe ->
      Pstring_load (Sixteen, convert_unsafety is_unsafe, Lambda.alloc_heap)
  | Pstring_load_32 (is_unsafe,m) ->
      Pstring_load (Thirty_two, convert_unsafety is_unsafe, m)
  | Pstring_load_64 (is_unsafe, m) ->
      Pstring_load (Sixty_four, convert_unsafety is_unsafe, m)
  | Pbytes_load_16 is_unsafe ->
      Pbytes_load (Sixteen, convert_unsafety is_unsafe, Lambda.alloc_heap)
  | Pbytes_load_32 (is_unsafe, m) ->
      Pbytes_load (Thirty_two, convert_unsafety is_unsafe, m)
  | Pbytes_load_64 (is_unsafe, m) ->
      Pbytes_load (Sixty_four, convert_unsafety is_unsafe, m)
  | Pbytes_set_16 is_unsafe ->
      Pbytes_set (Sixteen, convert_unsafety is_unsafe)
  | Pbytes_set_32 is_unsafe ->
      Pbytes_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbytes_set_64 is_unsafe ->
      Pbytes_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigstring_load_16 is_unsafe ->
      Pbigstring_load (Sixteen, convert_unsafety is_unsafe, Lambda.alloc_heap)
  | Pbigstring_load_32 (is_unsafe, m) ->
      Pbigstring_load (Thirty_two, convert_unsafety is_unsafe, m)
  | Pbigstring_load_64 (is_unsafe, m) ->
      Pbigstring_load (Sixty_four, convert_unsafety is_unsafe, m)
  | Pbigstring_set_16 is_unsafe ->
      Pbigstring_set (Sixteen, convert_unsafety is_unsafe)
  | Pbigstring_set_32 is_unsafe ->
      Pbigstring_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbigstring_set_64 is_unsafe ->
      Pbigstring_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigarraydim dim -> Pbigarraydim dim
  | Pbswap16 -> Pbswap16
  | Pint_as_pointer m -> Pint_as_pointer m
  | Popaque _ -> Popaque
  | Pprobe_is_enabled {name} -> Pprobe_is_enabled {name}
  | Pobj_dup ->
    let module P = Primitive in
    Pccall (Primitive.make
      ~name:"caml_obj_dup"
      ~alloc:true
      ~c_builtin:false
      ~effects:Only_generative_effects
      ~coeffects:Has_coeffects
      ~native_name:"caml_obj_dup"
      ~native_repr_args:[P.Prim_global, P.Same_as_ocaml_repr Layouts.Sort.Value]
      ~native_repr_res:(P.Prim_global, P.Same_as_ocaml_repr Layouts.Sort.Value))
  | Punbox_float -> Punbox_float
  | Pbox_float m -> Pbox_float m
  | Punbox_int bi -> Punbox_int bi
  | Pbox_int (bi, m) -> Pbox_int (bi, m)
  | Pobj_magic _
  | Pbytes_to_string
  | Pbytes_of_string
  | Pctconst _
  | Pignore
  | Pgetglobal _
  | Psetglobal _
  | Pgetpredef _
  | Parray_to_iarray
  | Parray_of_iarray
    ->
      Misc.fatal_errorf "lambda primitive %a can't be converted to \
                         clambda primitive"
        Printlambda.primitive prim
