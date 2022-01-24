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
  | Pmakeblock (tag, mutability, shape, _) ->
      Pmakeblock (tag, mutability, shape)
  | Pmakefloatblock (mutability, _) ->
      Pmakearray (Pfloatarray, mutability)
  | Pfield (field, _) -> Pfield field
  | Pfield_computed _sem -> Pfield_computed
  | Psetfield (field, imm_or_pointer, init_or_assign) ->
      Psetfield (field, imm_or_pointer, init_or_assign)
  | Psetfield_computed (imm_or_pointer, init_or_assign) ->
      Psetfield_computed (imm_or_pointer, init_or_assign)
  | Pfloatfield (field, _sem, _mode) -> Pfloatfield field
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
  | Pfloatofint _ -> Pfloatofint
  | Pnegfloat _ -> Pnegfloat
  | Pabsfloat _ -> Pabsfloat
  | Paddfloat _ -> Paddfloat
  | Psubfloat _ -> Psubfloat
  | Pmulfloat _ -> Pmulfloat
  | Pdivfloat _ -> Pdivfloat
  | Pfloatcomp comp -> Pfloatcomp comp
  | Pstringlength -> Pstringlength
  | Pstringrefu -> Pstringrefu
  | Pstringrefs -> Pstringrefs
  | Pbyteslength -> Pbyteslength
  | Pbytesrefu -> Pbytesrefu
  | Pbytessetu -> Pbytessetu
  | Pbytesrefs -> Pbytesrefs
  | Pbytessets -> Pbytessets
  | Pmakearray (kind, mutability, _) -> Pmakearray (kind, mutability)
  | Pduparray (kind, mutability) -> Pduparray (kind, mutability)
  | Parraylength kind -> Parraylength kind
  | Parrayrefu kind -> Parrayrefu kind
  | Parraysetu kind -> Parraysetu kind
  | Parrayrefs kind -> Parrayrefs kind
  | Parraysets kind -> Parraysets kind
  | Pisint -> Pisint
  | Pisout -> Pisout
  | Pcvtbint (src, dest, _) -> Pcvtbint (src, dest)
  | Pnegbint (bi, _) -> Pnegbint bi
  | Paddbint (bi, _) -> Paddbint bi
  | Psubbint (bi, _) -> Psubbint bi
  | Pmulbint (bi, _) -> Pmulbint bi
  | Pbintofint (bi, _) -> Pbintofint bi
  | Pintofbint bi -> Pintofbint bi
  | Pandbint (bi, _) -> Pandbint bi
  | Porbint (bi, _) -> Porbint bi
  | Pxorbint (bi, _) -> Pxorbint bi
  | Plslbint (bi, _) -> Plslbint bi
  | Plsrbint (bi, _) -> Plsrbint bi
  | Pasrbint (bi, _) -> Pasrbint bi
  | Pbbswap (bi, _) -> Pbbswap bi
  | Pdivbint { size; is_safe } -> Pdivbint { size; is_safe }
  | Pmodbint { size; is_safe } -> Pmodbint { size; is_safe }
  | Pbintcomp (bi, comp) -> Pbintcomp (bi, comp)
  | Pbigarrayref (safe, dims, kind, layout) ->
      Pbigarrayref (safe, dims, kind, layout)
  | Pbigarrayset (safe, dims, kind, layout) ->
      Pbigarrayset (safe, dims, kind, layout)
  | Pstring_load_16 is_unsafe ->
      Pstring_load (Sixteen, convert_unsafety is_unsafe)
  | Pstring_load_32 (is_unsafe, _) ->
      Pstring_load (Thirty_two, convert_unsafety is_unsafe)
  | Pstring_load_64 (is_unsafe, _) ->
      Pstring_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbytes_load_16 is_unsafe ->
      Pbytes_load (Sixteen, convert_unsafety is_unsafe)
  | Pbytes_load_32 (is_unsafe, _) ->
      Pbytes_load (Thirty_two, convert_unsafety is_unsafe)
  | Pbytes_load_64 (is_unsafe, _) ->
      Pbytes_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbytes_set_16 is_unsafe ->
      Pbytes_set (Sixteen, convert_unsafety is_unsafe)
  | Pbytes_set_32 is_unsafe ->
      Pbytes_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbytes_set_64 is_unsafe ->
      Pbytes_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigstring_load_16 is_unsafe ->
      Pbigstring_load (Sixteen, convert_unsafety is_unsafe)
  | Pbigstring_load_32 (is_unsafe, _) ->
      Pbigstring_load (Thirty_two, convert_unsafety is_unsafe)
  | Pbigstring_load_64 (is_unsafe, _) ->
      Pbigstring_load (Sixty_four, convert_unsafety is_unsafe)
  | Pbigstring_set_16 is_unsafe ->
      Pbigstring_set (Sixteen, convert_unsafety is_unsafe)
  | Pbigstring_set_32 is_unsafe ->
      Pbigstring_set (Thirty_two, convert_unsafety is_unsafe)
  | Pbigstring_set_64 is_unsafe ->
      Pbigstring_set (Sixty_four, convert_unsafety is_unsafe)
  | Pbigarraydim dim -> Pbigarraydim dim
  | Pbswap16 -> Pbswap16
  | Pint_as_pointer -> Pint_as_pointer
  | Popaque -> Popaque
  | Pprobe_is_enabled {name} -> Pprobe_is_enabled {name}

  | Pbytes_to_string
  | Pbytes_of_string
  | Pctconst _
  | Pignore
  | Prevapply _
  | Pdirapply _
  | Pidentity
  | Pgetglobal _
  | Psetglobal _
    ->
      Misc.fatal_errorf "lambda primitive %a can't be converted to \
                         clambda primitive"
        Printlambda.primitive prim
