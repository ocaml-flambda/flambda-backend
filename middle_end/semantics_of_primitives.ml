(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

let coeffects_of : Lambda.alloc_mode -> coeffects = function
  | Alloc_heap ->
     No_coeffects
  | Alloc_local ->
     (* Ensure that local allocations are not reordered wrt. regions *)
     Has_coeffects

let for_primitive (prim : Clambda_primitives.primitive) =
  match prim with
  | Pmakeblock (_, _, _, m)
  | Pmakeufloatblock (_, m)
  | Pmakearray (_, Mutable, m) -> Only_generative_effects, coeffects_of m
  | Pmakearray (_, (Immutable | Immutable_unique), m) ->
     No_effects, coeffects_of m
  | Pduparray (_, (Immutable | Immutable_unique)) ->
      No_effects, No_coeffects  (* Pduparray (_, Immutable) is allowed only on
                                   immutable arrays. *)
  | Pduparray (_, Mutable) | Pduprecord _ ->
      Only_generative_effects, Has_coeffects
  | Pccall { prim_name =
               ( "caml_format_float" | "caml_format_int" | "caml_int32_format"
               | "caml_nativeint_format" | "caml_int64_format" ) } ->
      No_effects, No_coeffects
  | Pccall _ -> Arbitrary_effects, Has_coeffects
  | Pprobe_is_enabled _ -> No_effects, Has_coeffects
  | Praise _ -> Arbitrary_effects, No_coeffects
  | Prunstack | Pperform | Presume | Preperform ->
      Arbitrary_effects, Has_coeffects
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp _ -> No_effects, No_coeffects
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
    -> No_effects, No_coeffects
  | Pdivbint { is_safe = Unsafe }
  | Pmodbint { is_safe = Unsafe }
  | Pdivint Unsafe
  | Pmodint Unsafe ->
      No_effects, No_coeffects  (* Will not raise [Division_by_zero]. *)
  | Pdivbint { is_safe = Safe }
  | Pmodbint { is_safe = Safe }
  | Pdivint Safe
  | Pmodint Safe ->
      Arbitrary_effects, No_coeffects
  | Poffsetint _ -> No_effects, No_coeffects
  | Poffsetref _ -> Arbitrary_effects, Has_coeffects
  | Punbox_float | Punbox_int _
  | Pintoffloat
  | Pfloatcomp _ -> No_effects, No_coeffects
  | Pbox_float m | Pbox_int (_, m)
  | Pfloatofint m
  | Pnegfloat m
  | Pabsfloat m
  | Paddfloat m
  | Psubfloat m
  | Pmulfloat m
  | Pdivfloat m -> No_effects, coeffects_of m
  | Pstringlength | Pbyteslength
  | Parraylength _ -> No_effects, No_coeffects
  | Pisint
  | Pisout
  | Pintofbint _
  | Pbintcomp _ -> No_effects, No_coeffects
  | Pbintofint (_,m)
  | Pcvtbint (_,_,m)
  | Pnegbint (_,m)
  | Paddbint (_,m)
  | Psubbint (_,m)
  | Pmulbint (_,m)
  | Pandbint (_,m)
  | Porbint (_,m)
  | Pxorbint (_,m)
  | Plslbint (_,m)
  | Plsrbint (_,m)
  | Pasrbint (_,m) -> No_effects, coeffects_of m
  | Pbigarraydim _ ->
      No_effects, Has_coeffects  (* Some people resize bigarrays in place. *)
  | Pread_symbol _
  | Pfield _
  | Pfield_computed
  | Pfloatfield _
  | Pufloatfield _
  | Parrayrefu _
  | Pstringrefu
  | Pbytesrefu
  | Pstring_load (_, Unsafe, _)
  | Pbytes_load (_, Unsafe, _)
  | Pbigarrayref (true, _, _, _)
  | Pbigstring_load (_, Unsafe, _) ->
      No_effects, Has_coeffects
  | Parrayrefs _
  | Pstringrefs
  | Pbytesrefs
  | Pstring_load (_, Safe, _)
  | Pbytes_load (_, Safe, _)
  | Pbigarrayref (false, _, _, _)
  | Pbigstring_load (_, Safe, _) ->
      (* May trigger a bounds check exception. *)
      Arbitrary_effects, Has_coeffects
  | Psetfield _
  | Psetfield_computed _
  | Psetfloatfield _
  | Psetufloatfield _
  | Patomic_load _
  | Patomic_exchange
  | Patomic_cas
  | Patomic_fetch_add
  | Parraysetu _
  | Parraysets _
  | Pbytessetu
  | Pbytessets
  | Pbytes_set _
  | Pbigarrayset _
  | Pbigstring_set _ ->
      (* Whether or not some of these are "unsafe" is irrelevant; they always
         have an effect. *)
      Arbitrary_effects, No_coeffects
  | Pbswap16 -> No_effects, No_coeffects
  | Pbbswap (_,m) -> No_effects, coeffects_of m
  | Pint_as_pointer m -> No_effects, coeffects_of m
  | Popaque -> Arbitrary_effects, Has_coeffects
  | Psequand
  | Psequor ->
      (* Removed by [Closure_conversion] in the flambda pipeline. *)
      No_effects, No_coeffects
  | Pget_header _ -> No_effects, No_coeffects
  | Pdls_get ->
      (* only read *)
      No_effects, No_coeffects

type return_type =
  | Float
  | Other

let return_type_of_primitive (prim:Clambda_primitives.primitive) =
  match prim with
  | Pfloatofint _
  | Pnegfloat _
  | Pabsfloat _
  | Paddfloat _
  | Psubfloat _
  | Pmulfloat _
  | Pdivfloat _
  | Pfloatfield _
  | Parrayrefu (Pfloatarray_ref _)
  | Parrayrefs (Pfloatarray_ref _) ->
      Float
  | _ ->
      Other

let is_local_alloc = function
  | Lambda.Alloc_local -> true
  | Lambda.Alloc_heap -> false

let may_locally_allocate (prim:Clambda_primitives.primitive) : bool =
  match prim with
  | Pmakeblock (_, _, _, m)
  | Pmakeufloatblock (_, m)
  | Pmakearray (_, _, m) -> is_local_alloc m
  | Pduparray (_, _)
  | Pduprecord (_,_) -> false
  | Pccall { prim_name =
               ( "caml_format_float" | "caml_format_int" | "caml_int32_format"
               | "caml_nativeint_format" | "caml_int64_format" ) } -> false
  | Pccall _ ->
     (* TODO: Track which C calls may locally allocate more precisely *)
     true
  | Praise _ -> false
  | Pnot
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pandint
  | Pdivint _
  | Pmodint _
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp _ -> false
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
    -> false
  | Poffsetint _ -> false
  | Poffsetref _ -> false
  | Punbox_float | Punbox_int _
  | Pintoffloat
  | Pfloatcomp _ -> false
  | Pbox_float m | Pbox_int (_, m)
  | Pfloatofint m
  | Pnegfloat m
  | Pabsfloat m
  | Paddfloat m
  | Psubfloat m
  | Pmulfloat m
  | Pdivfloat m -> is_local_alloc m
  | Pstringlength | Pbyteslength
  | Parraylength _ -> false
  | Pisint
  | Pisout
  | Pintofbint _
  | Pbintcomp _ -> false
  | Pdivbint { mode = m }
  | Pmodbint { mode = m }
  | Pbintofint (_,m)
  | Pcvtbint (_,_,m)
  | Pnegbint (_,m)
  | Paddbint (_,m)
  | Psubbint (_,m)
  | Pmulbint (_,m)
  | Pandbint (_,m)
  | Porbint (_,m)
  | Pxorbint (_,m)
  | Plslbint (_,m)
  | Plsrbint (_,m)
  | Pasrbint (_,m) -> is_local_alloc m
  | Pbigarraydim _ -> false
  | Pread_symbol _
  | Pfield _
  | Pfield_computed
  | Parrayrefu _
  | Pstringrefu
  | Pbytesrefu
  | Pstring_load (_, Unsafe, _)
  | Pbytes_load (_, Unsafe, _)
  | Pbigarrayref (true, _, _, _)
  | Pbigstring_load (_, Unsafe, _) ->
      false
  | Pfloatfield (_, m) -> is_local_alloc m
  | Pufloatfield _ -> false
  | Pstring_load (_, Safe, m)
  | Pbytes_load (_, Safe, m)
  | Pbigstring_load (_, Safe, m) -> is_local_alloc m
  | Parrayrefs _
  | Pstringrefs
  | Pbytesrefs
  | Pbigarrayref (false, _, _, _) -> false
  | Psetfield _
  | Psetfield_computed _
  | Psetfloatfield _
  | Psetufloatfield _
  | Parraysetu _
  | Parraysets _
  | Pbytessetu
  | Pbytessets
  | Pbytes_set _
  | Pbigarrayset _
  | Pbigstring_set _ ->
      false
  | Pbswap16 -> false
  | Pbbswap (_,m) -> is_local_alloc m
  | Pint_as_pointer m -> is_local_alloc m
  | Popaque -> false
  | Psequand
  | Psequor ->
      false
  | Pprobe_is_enabled _ -> false
  | Pget_header m -> is_local_alloc m
  | Prunstack | Pperform | Presume | Preperform -> true
  | Patomic_exchange | Patomic_cas | Patomic_fetch_add | Pdls_get
  | Patomic_load _ -> false
