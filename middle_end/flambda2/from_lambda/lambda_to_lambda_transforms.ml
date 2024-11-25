(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2024 OCamlPro SAS                                    *)
(*   Copyright 2016--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Env = Lambda_to_flambda_env
module L = Lambda
module P = Flambda_primitive

type primitive_transform_result =
  | Primitive of L.primitive * L.lambda list * L.scoped_location
  | Transformed of L.lambda

let switch_for_if_then_else ~cond ~ifso ~ifnot ~kind =
  let switch : L.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None
    }
  in
  L.Lswitch (cond, switch, L.try_to_find_location cond, kind)

let rec_catch_for_while_loop env cond body =
  let cont = L.next_raise_count () in
  let env = Env.mark_as_recursive_static_catch env cont in
  let cond_result = Ident.create_local "while_cond_result" in
  let lam : L.lambda =
    Lstaticcatch
      ( Lstaticraise (cont, []),
        (cont, []),
        Llet
          ( Strict,
            L.layout_int,
            cond_result,
            cond,
            Lifthenelse
              ( Lvar cond_result,
                Lsequence (body, Lstaticraise (cont, [])),
                Lconst (Const_base (Const_int 0)),
                L.layout_unit ) ),
        Same_region,
        L.layout_unit )
  in
  env, lam

let rec_catch_for_for_loop env loc ident start stop
    (dir : Asttypes.direction_flag) body =
  let cont = L.next_raise_count () in
  let env = Env.mark_as_recursive_static_catch env cont in
  let start_ident = Ident.create_local "for_start" in
  let stop_ident = Ident.create_local "for_stop" in
  let first_test : L.lambda =
    match dir with
    | Upto -> Lprim (Pintcomp Cle, [L.Lvar start_ident; L.Lvar stop_ident], loc)
    | Downto ->
      Lprim (Pintcomp Cge, [L.Lvar start_ident; L.Lvar stop_ident], loc)
  in
  let subsequent_test : L.lambda =
    Lprim (Pintcomp Cne, [L.Lvar ident; L.Lvar stop_ident], loc)
  in
  let one : L.lambda = Lconst (Const_base (Const_int 1)) in
  let next_value_of_counter =
    match dir with
    | Upto -> L.Lprim (Paddint, [L.Lvar ident; one], loc)
    | Downto -> L.Lprim (Psubint, [L.Lvar ident; one], loc)
  in
  let lam : L.lambda =
    (* Care needs to be taken here not to cause overflow if, for an incrementing
       for-loop, the upper bound is [max_int]; likewise, for a decrementing
       for-loop, if the lower bound is [min_int]. *)
    Llet
      ( Strict,
        L.layout_int,
        start_ident,
        start,
        Llet
          ( Strict,
            L.layout_int,
            stop_ident,
            stop,
            Lifthenelse
              ( first_test,
                Lstaticcatch
                  ( Lstaticraise (cont, [L.Lvar start_ident]),
                    (cont, [ident, L.layout_int]),
                    Lsequence
                      ( body,
                        Lifthenelse
                          ( subsequent_test,
                            Lstaticraise (cont, [next_value_of_counter]),
                            L.lambda_unit,
                            L.layout_unit ) ),
                    Same_region,
                    L.layout_unit ),
                L.lambda_unit,
                L.layout_unit ) ) )
  in
  env, lam

let transform_primitive env (prim : L.primitive) args loc =
  match prim, args with
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    Transformed
      (L.Llet
         ( Strict,
           L.layout_int,
           const_true,
           Lconst (Const_base (Const_int 1)),
           L.Llet
             ( Strict,
               L.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond)
                 ~ifso:(L.Lvar const_true) ~ifnot:arg2 ~kind:L.layout_int ) ))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    Transformed
      (L.Llet
         ( Strict,
           L.layout_int,
           const_false,
           Lconst (Const_base (Const_int 0)),
           L.Llet
             ( Strict,
               L.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond) ~ifso:arg2
                 ~ifnot:(L.Lvar const_false) ~kind:L.layout_int ) ))
  | (Psequand | Psequor), _ ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | ( (Pbytes_to_string | Pbytes_of_string | Parray_of_iarray | Parray_to_iarray),
      [arg] ) ->
    Transformed arg
  | Pignore, [arg] ->
    let result = L.Lconst (Const_base (Const_int 0)) in
    Transformed (L.Lsequence (arg, result))
  | Pfield _, [L.Lprim (Pgetglobal cu, [], _)]
    when Compilation_unit.equal cu (Env.current_unit env) ->
    Misc.fatal_error
      "[Pfield (Pgetglobal ...)] for the current compilation unit is forbidden \
       upon entry to the middle end"
  | Psetfield (_, _, _), [L.Lprim (Pgetglobal _, [], _); _] ->
    Misc.fatal_error
      "[Psetfield (Pgetglobal ...)] is forbidden upon entry to the middle end"
  | Pfield (index, _, _), _ when index < 0 ->
    Misc.fatal_error "Pfield with negative field index"
  | Pfloatfield (i, _, _), _ when i < 0 ->
    Misc.fatal_error "Pfloatfield with negative field index"
  | Psetfield (index, _, _), _ when index < 0 ->
    Misc.fatal_error "Psetfield with negative field index"
  | Pmakeblock (tag, _, _, _), _ when tag < 0 || tag >= Obj.no_scan_tag ->
    Misc.fatal_errorf "Pmakeblock with wrong or non-scannable block tag %d" tag
  | Pmakefloatblock (_mut, _mode), args when List.length args < 1 ->
    Misc.fatal_errorf "Pmakefloatblock must have at least one argument"
  | Pfloatcomp (bf, CFnlt), args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp (bf, CFlt), args, loc)], loc)
  | Pfloatcomp (bf, CFngt), args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp (bf, CFgt), args, loc)], loc)
  | Pfloatcomp (bf, CFnle), args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp (bf, CFle), args, loc)], loc)
  | Pfloatcomp (bf, CFnge), args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp (bf, CFge), args, loc)], loc)
  | Punboxed_float_comp (bf, CFnlt), args ->
    Primitive
      (L.Pnot, [L.Lprim (Punboxed_float_comp (bf, CFlt), args, loc)], loc)
  | Punboxed_float_comp (bf, CFngt), args ->
    Primitive
      (L.Pnot, [L.Lprim (Punboxed_float_comp (bf, CFgt), args, loc)], loc)
  | Punboxed_float_comp (bf, CFnle), args ->
    Primitive
      (L.Pnot, [L.Lprim (Punboxed_float_comp (bf, CFle), args, loc)], loc)
  | Punboxed_float_comp (bf, CFnge), args ->
    Primitive
      (L.Pnot, [L.Lprim (Punboxed_float_comp (bf, CFge), args, loc)], loc)
  | Pbigarrayref (_unsafe, num_dimensions, kind, layout), args -> (
    (* CR mshinwell: factor out with the [Pbigarrayset] case *)
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some _, Some _ -> Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3
      then
        let arity = 1 + num_dimensions in
        let is_float32_t =
          match kind with
          | Pbigarray_float32_t -> "float32_"
          | Pbigarray_unknown | Pbigarray_float16 | Pbigarray_float32
          | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
          | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
          | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
          | Pbigarray_complex32 | Pbigarray_complex64 ->
            ""
        in
        let name =
          "caml_ba_" ^ is_float32_t ^ "get_" ^ string_of_int num_dimensions
        in
        let desc = L.simple_prim_on_values ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      else
        Misc.fatal_errorf
          "Lambda_to_flambda.transform_primitive: Pbigarrayref with unknown \
           layout and elements should only have dimensions between 1 and 3 \
           (see translprim).")
  | Pbigarrayset (_unsafe, num_dimensions, kind, layout), args -> (
    match
      P.Bigarray_kind.from_lambda kind, P.Bigarray_layout.from_lambda layout
    with
    | Some _, Some _ -> Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3
      then
        let arity = 2 + num_dimensions in
        let is_float32_t =
          match kind with
          | Pbigarray_float32_t -> "float32_"
          | Pbigarray_unknown | Pbigarray_float16 | Pbigarray_float32
          | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
          | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
          | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
          | Pbigarray_complex32 | Pbigarray_complex64 ->
            ""
        in
        let name =
          "caml_ba_" ^ is_float32_t ^ "set_" ^ string_of_int num_dimensions
        in
        let desc = L.simple_prim_on_values ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      else
        Misc.fatal_errorf
          "Lambda_to_flambda.transform_primitive: Pbigarrayset with unknown \
           layout and elements should only have dimensions between 1 and 3 \
           (see translprim).")
  | _, _ -> Primitive (prim, args, loc)
  [@@ocaml.warning "-fragile-match"]
