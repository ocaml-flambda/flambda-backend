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

type initialize_array_element_width =
  | Thirty_two of { zero_init : L.lambda }
  | Sixty_four_or_more

let initialize_array0 env loc ~length array_set_kind width ~(init : L.lambda)
    creation_expr =
  let array = Ident.create_local "array" in
  (* If the element size is 32-bit, zero-initialize the last 64-bit word, to
     ensure reproducibility. *)
  (* CR mshinwell: why does e.g. caml_make_unboxed_int32_vect not do this? *)
  let maybe_zero_init_last_field =
    match width with
    | Sixty_four_or_more -> L.lambda_unit
    | Thirty_two { zero_init } ->
      let zero_init_last_field =
        L.Lprim
          ( Parraysetu (array_set_kind, Ptagged_int_index),
            (* [Popaque] is used to conceal the out-of-bounds write. *)
            [Lprim (Popaque L.layout_unit, [Lvar array], loc); length; zero_init],
            loc )
      in
      let length_is_greater_than_zero_and_is_one_mod_two =
        L.Lprim
          ( Psequand,
            [ Lprim (Pintcomp Cgt, [Lconst (L.const_int 0); length], loc);
              Lprim
                ( Pintcomp Cne,
                  [ Lprim (Pmodint Unsafe, [length; Lconst (L.const_int 2)], loc);
                    Lconst (L.const_int 0) ],
                  loc ) ],
            loc )
      in
      L.Lifthenelse
        ( length_is_greater_than_zero_and_is_one_mod_two,
          zero_init_last_field,
          L.lambda_unit,
          L.layout_unit )
  in
  let env, initialize =
    let index = Ident.create_local "index" in
    rec_catch_for_for_loop env loc index
      (Lconst (L.const_int 0))
      (L.Lprim (Psubint, [length; Lconst (L.const_int 1)], loc))
      Upto
      (Lprim
         ( Parraysetu (array_set_kind, Ptagged_int_index),
           [Lvar array; Lvar index; init],
           loc ))
  in
  let term =
    L.Llet
      ( Strict,
        (* XXX should this be nullable? *)
        Pvalue { raw_kind = Pgenval; nullable = Non_nullable },
        array,
        creation_expr,
        Lsequence
          (maybe_zero_init_last_field, Lsequence (initialize, Lvar array)) )
  in
  env, Transformed term

let initialize_array env loc ~length array_set_kind width ~init creation_expr =
  match init with
  | None -> env, Transformed creation_expr
  | Some init ->
    initialize_array0 env loc ~length array_set_kind width ~init creation_expr

let makearray_dynamic_singleton name (mode : L.locality_mode) ~length ~init loc
    =
  let name =
    Printf.sprintf "caml_make%s_%s%svect"
      (match mode with Alloc_heap -> "" | Alloc_local -> "_local")
      name
      (if String.length name > 0 then "_" else "")
  in
  let external_call_desc =
    Primitive.make ~name ~alloc:true (* the C stub may raise an exception *)
      ~c_builtin:false ~effects:Arbitrary_effects ~coeffects:Has_coeffects
      ~native_name:name
      ~native_repr_args:
        ([Primitive.Prim_global, L.Same_as_ocaml_repr (Base Value)]
        @
        match init with
        | None -> []
        | Some (init_extern_repr, _) -> [Primitive.Prim_global, init_extern_repr]
        )
      ~native_repr_res:
        ( (match mode with
          | Alloc_heap -> Prim_global
          | Alloc_local -> Prim_local),
          L.Same_as_ocaml_repr (Base Value) )
      ~is_layout_poly:false
  in
  L.Lprim
    ( Pccall external_call_desc,
      ([length] @ match init with None -> [] | Some (_, init) -> [init]),
      loc )

let makearray_dynamic_singleton_uninitialized name (mode : L.locality_mode)
    ~length loc =
  makearray_dynamic_singleton name
    (mode : L.locality_mode)
    ~length ~init:None loc

let makearray_dynamic_unboxed_products_only_64_bit () =
  (* To keep things simple in the C stub as regards array length, we currently
     restrict to 64-bit targets. *)
  if not (Target_system.is_64_bit ())
  then
    Misc.fatal_error
      "Cannot compile Pmakearray_dynamic at unboxed product layouts for 32-bit \
       targets"

let makearray_dynamic_unboxed_product_c_stub ~name (mode : L.locality_mode) =
  Primitive.make ~name ~alloc:true (* the C stub may raise an exception *)
    ~c_builtin:false ~effects:Arbitrary_effects ~coeffects:Has_coeffects
    ~native_name:name
    ~native_repr_args:
      [ Prim_global, L.Same_as_ocaml_repr (Base Value);
        Prim_global, L.Same_as_ocaml_repr (Base Value);
        Prim_global, L.Same_as_ocaml_repr (Base Value) ]
    ~native_repr_res:
      ( (match mode with Alloc_heap -> Prim_global | Alloc_local -> Prim_local),
        L.Same_as_ocaml_repr (Base Value) )
    ~is_layout_poly:false

let makearray_dynamic_non_scannable_unboxed_product env
    (lambda_array_kind : L.array_kind) (mode : L.locality_mode) ~length
    ~(init : L.lambda option) loc =
  makearray_dynamic_unboxed_products_only_64_bit ();
  let is_local =
    L.of_bool (match mode with Alloc_heap -> false | Alloc_local -> true)
  in
  let external_call_desc =
    makearray_dynamic_unboxed_product_c_stub
      ~name:"caml_makearray_dynamic_non_scannable_unboxed_product" mode
  in
  let num_initializers = L.count_initializers_array_kind lambda_array_kind in
  (* Note that we don't check the number of unarized arguments against the
     layout; we trust the front end. If we wanted to do this, it would have to
     be done slightly later, after unarization. *)
  let term =
    L.(
      Lprim
        ( Pccall external_call_desc,
          [Lconst (L.const_int num_initializers); is_local; length],
          loc ))
  in
  match init with
  | None -> env, Transformed term
  | Some init ->
    initialize_array0 env loc ~length
      (L.array_set_kind
         (match mode with
         | Alloc_heap -> L.modify_heap
         | Alloc_local -> L.modify_maybe_stack)
         lambda_array_kind)
      (* There is no packing in unboxed product arrays, even if the elements are
         all float32# or int32#. *)
      Sixty_four_or_more ~init term

let makearray_dynamic_scannable_unboxed_product0
    (lambda_array_kind : L.array_kind) (mode : L.locality_mode) ~length ~init
    loc =
  makearray_dynamic_unboxed_products_only_64_bit ();
  (* Trick: use the local stack as a way of getting the variable argument list
     to the C function. *)
  if not Config.stack_allocation
  then
    Misc.fatal_error
      "Cannot compile Pmakearray_dynamic at unboxed product layouts without \
       stack allocation enabled";
  let args_array = Ident.create_local "args_array" in
  let array_layout = L.layout_array lambda_array_kind in
  let is_local =
    L.of_bool (match mode with Alloc_heap -> false | Alloc_local -> true)
  in
  let external_call_desc =
    makearray_dynamic_unboxed_product_c_stub
      ~name:"caml_makearray_dynamic_scannable_unboxed_product" mode
  in
  (* Note that we don't check the number of unarized arguments against the
     layout; we trust the front end. If we wanted to do this, it would have to
     be done slightly later, after unarization. *)
  Transformed
    (Lregion
       ( Llet
           ( Strict,
             array_layout,
             args_array,
             Lprim
               ( Pmakearray (lambda_array_kind, Immutable, L.alloc_local),
                 [init] (* will be unarized when this term is CPS converted *),
                 loc ),
             Lprim
               ( Pccall external_call_desc,
                 [Lvar args_array; is_local; length],
                 loc ) ),
         array_layout ))

let makearray_dynamic_scannable_unboxed_product env
    (lambda_array_kind : L.array_kind) (mode : L.locality_mode) ~length
    ~(init : L.lambda) loc =
  let must_be_scanned =
    match lambda_array_kind with
    | Pgcignorableproductarray _ -> false
    | Pgcscannableproductarray kinds ->
      let rec must_be_scanned (kind : L.scannable_product_element_kind) =
        match kind with
        | Pint_scannable -> false
        | Paddr_scannable -> true
        | Pproduct_scannable kinds -> List.exists must_be_scanned kinds
      in
      List.exists must_be_scanned kinds
    | Pgenarray | Paddrarray | Pintarray | Pfloatarray | Punboxedfloatarray _
    | Punboxedintarray _ | Punboxedvectorarray _ ->
      Misc.fatal_errorf
        "%s: should have been sent to [makearray_dynamic_singleton]"
        (Printlambda.array_kind lambda_array_kind)
  in
  if must_be_scanned
  then
    ( env,
      makearray_dynamic_scannable_unboxed_product0 lambda_array_kind mode
        ~length ~init loc )
  else
    makearray_dynamic_non_scannable_unboxed_product env lambda_array_kind mode
      ~length ~init:(Some init) loc

let makearray_dynamic env (lambda_array_kind : L.array_kind)
    (mode : L.locality_mode) args loc : Env.t * primitive_transform_result =
  (* %makearray_dynamic is analogous to (from stdlib/array.ml):
   *   external create: int -> 'a -> 'a array = "caml_make_vect"
   * except that it works on any layout, including unboxed products, at both
   * heap and local modes.
   * Additionally, if the initializer is omitted, an uninitialized array will
   * be returned.  Initializers must however be provided when the array kind is
   * Pgenarray, Paddrarray, Pintarray, Pfloatarray or Pgcscannableproductarray;
   * or when a Pgcignorablearray involves an [int].  (See comment below.)
   *)
  let dbg = Debuginfo.from_location loc in
  let length, init =
    match args with
    | [length] -> length, None
    | [length; init] -> length, Some init
    | [] | _ :: _ :: _ ->
      Misc.fatal_error
        "%makearray_dynamic takes the (non-unarized) length and optionally an \
         initializer (the latter perhaps of unboxed product layout)"
  in
  let[@inline] must_have_initializer () =
    match init with
    | Some init -> init
    | None ->
      Misc.fatal_errorf
        "Cannot compile Pmakearray_dynamic at layout %s without an \
         initializer:@ %a"
        (Printlambda.array_kind lambda_array_kind)
        Debuginfo.print_compact dbg
  in
  match lambda_array_kind with
  | Pgenarray | Paddrarray | Pintarray | Pfloatarray ->
    let init = must_have_initializer () in
    ( env,
      Transformed
        (makearray_dynamic_singleton "" mode ~length
           ~init:(Some (Same_as_ocaml_repr (Base Value), init))
           loc) )
  | Punboxedfloatarray Pfloat32 ->
    makearray_dynamic_singleton_uninitialized "unboxed_float32" ~length mode loc
    |> initialize_array env loc ~length (Punboxedfloatarray_set Pfloat32)
         (Thirty_two
            { zero_init = Lconst (Const_base (Const_unboxed_float32 "0")) })
         ~init
  | Punboxedfloatarray Pfloat64 ->
    makearray_dynamic_singleton_uninitialized "unboxed_float64" ~length mode loc
    |> initialize_array env loc ~length (Punboxedfloatarray_set Pfloat64)
         Sixty_four_or_more ~init
  | Punboxedintarray Pint32 ->
    makearray_dynamic_singleton_uninitialized "unboxed_int32" ~length mode loc
    |> initialize_array env loc ~length (Punboxedintarray_set Pint32)
         (Thirty_two
            { zero_init = Lconst (Const_base (Const_unboxed_int32 0l)) })
         ~init
  | Punboxedintarray Pint64 ->
    makearray_dynamic_singleton_uninitialized "unboxed_int64" ~length mode loc
    |> initialize_array env loc ~length (Punboxedintarray_set Pint64)
         Sixty_four_or_more ~init
  | Punboxedintarray Pnativeint ->
    makearray_dynamic_singleton_uninitialized "unboxed_nativeint" ~length mode
      loc
    |> initialize_array env loc ~length (Punboxedintarray_set Pnativeint)
         Sixty_four_or_more ~init
  | Punboxedvectorarray Pvec128 ->
    makearray_dynamic_singleton_uninitialized "unboxed_vec128" ~length mode loc
    |> initialize_array env loc ~length (Punboxedvectorarray_set Pvec128)
         Sixty_four_or_more ~init
  | Pgcscannableproductarray _ ->
    let init = must_have_initializer () in
    makearray_dynamic_scannable_unboxed_product env lambda_array_kind mode
      ~length ~init loc
  | Pgcignorableproductarray ignorable ->
    (* Care: all (unarized) elements that are valid OCaml values, in this case
       of type [int] or equivalent, must be initialized. This is to ensure
       soundness in the event of a read occurring prior to initialization (e.g.
       by ensuring that values without the bottom bit set cannot be returned at
       type [int]). *)
    let init =
      if List.exists L.ignorable_product_element_kind_involves_int ignorable
      then Some (must_have_initializer ())
      else init
    in
    makearray_dynamic_non_scannable_unboxed_product env lambda_array_kind mode
      ~length ~init loc

let arrayblit env ~(src_mutability : L.mutable_flag)
    ~(dst_array_set_kind : L.array_set_kind) args loc =
  (* ignore src_mutability;

     let name = match dst_array_set_kind with

     | Pgenarray_set _ | Paddrarray_set _ | Pintarray_set |
     Pgcscannableproductarray_set _ ->

     "caml_array_blit" | Pfloatarray_set | Punboxedfloatarray_set Pfloat64 -> ->
     "caml_floatarray_blit" | Punboxedfloatarray_set Pfloat32 ->
     "caml_unboxed_float32_vect_blit" | Punboxedintarray_set Pint32 ->
     "caml_unboxed_int32_vect_blit" | Punboxedintarray_set Pint64 ->
     "caml_unboxed_int64_vect_blit" | Punboxedintarray_set Pnativeint ->
     "caml_unboxed_nativeint_vect_blit" | Punboxedvectorarray_set Pvec128 ->
     "caml_unboxed_vec128_vect_blit"

     in

     let extcall = Primitive.make ~name ~alloc:true (* the C stub may raise an
     exception *) ~c_builtin:false ~effects:Arbitrary_effects
     ~coeffects:Has_coeffects ~native_name:name ~native_repr_args: [
     Prim_global, L.Same_as_ocaml_repr (Base Value); Prim_global,
     L.Same_as_ocaml_repr (Base Value); Prim_global, L.Same_as_ocaml_repr (Base
     Value); Prim_global, L.Same_as_ocaml_repr (Base Value); Prim_global,
     L.Same_as_ocaml_repr (Base Value)

     ] ~native_repr_res: ( Prim_global, L.Same_as_ocaml_repr (Base Value) )
     ~is_layout_poly:false in let term = L.Lprim (Pccall extcall, [

     ], loc)

     in env, Transformed term *)
  let src_array_ref_kind =
    L.array_ref_kind_of_array_set_kind_for_unboxed_types_and_int
      dst_array_set_kind ~print_array_set_kind:Printlambda.array_set_kind
  in
  match args with
  | [src_expr; src_start_pos_expr; dst_expr; dst_start_pos_expr; length_expr] ->
    (* Care: the [args] are arbitrary Lambda expressions, so need to be
       [let]-bound *)
    let id = Ident.create_local in
    let bind = L.bind_with_layout in
    let src = id "src" in
    let src_start_pos = id "src_start_pos" in
    let dst = id "dst" in
    let dst_start_pos = id "dst_start_pos" in
    let length = id "length" in
    (* CR mshinwell: support indexing by other types apart from [int] *)
    let src_end_pos_exclusive =
      L.Lprim (Paddint, [Lvar src_start_pos; Lvar length], loc)
    in
    let src_end_pos_inclusive =
      L.Lprim (Psubint, [src_end_pos_exclusive; Lconst (L.const_int 1)], loc)
    in
    let dst_start_pos_minus_src_start_pos =
      L.Lprim (Psubint, [Lvar dst_start_pos; Lvar src_start_pos], loc)
    in
    let dst_start_pos_minus_src_start_pos_var =
      Ident.create_local "dst_start_pos_minus_src_start_pos"
    in
    let must_copy_backwards =
      L.Lprim (Pintcomp Cgt, [Lvar dst_start_pos; Lvar src_start_pos], loc)
    in
    let make_loop env (direction : Asttypes.direction_flag) =
      let src_index = Ident.create_local "index" in
      let start_pos, end_pos =
        match direction with
        | Upto -> L.Lvar src_start_pos, src_end_pos_inclusive
        | Downto -> src_end_pos_inclusive, L.Lvar src_start_pos
      in
      rec_catch_for_for_loop env loc src_index start_pos end_pos direction
        (Lprim
           ( Parraysetu (dst_array_set_kind, Ptagged_int_index),
             [ Lvar dst;
               Lprim
                 ( Paddint,
                   [Lvar src_index; dst_start_pos_minus_src_start_pos],
                   loc );
               Lprim
                 ( Parrayrefu
                     ( src_array_ref_kind,
                       Ptagged_int_index,
                       match src_mutability with
                       | Immutable | Immutable_unique -> Immutable
                       | Mutable -> Mutable ),
                   [Lvar src; Lvar src_index],
                   loc ) ],
             loc ))
    in
    let env, copy_backwards = make_loop env Downto in
    let env, copy_forwards = make_loop env Upto in
    let body =
      L.Lifthenelse
        (must_copy_backwards, copy_backwards, copy_forwards, L.layout_unit)
    in
    let expr =
      (* Preserve right-to-left evaluation order. *)
      bind Strict (length, L.layout_int) length_expr
      @@ bind Strict (dst_start_pos, L.layout_int) dst_start_pos_expr
      @@ bind Strict (dst, L.layout_any_value) dst_expr
      @@ bind Strict (src_start_pos, L.layout_int) src_start_pos_expr
      @@ bind Strict (src, L.layout_any_value) src_expr
      @@ bind Strict
           (dst_start_pos_minus_src_start_pos_var, L.layout_int)
           dst_start_pos_minus_src_start_pos body
    in
    env, Transformed expr
  | _ ->
    Misc.fatal_errorf
      "Wrong arity for Parrayblit{,_immut} (expected src, src_offset, \
       dst_offset and length):@ %a"
      Debuginfo.print_compact
      (Debuginfo.from_location loc)

let transform_primitive0 env (prim : L.primitive) args loc =
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

let transform_primitive env (prim : L.primitive) args loc =
  match prim with
  | Pmakearray_dynamic (lambda_array_kind, mode) ->
    makearray_dynamic env lambda_array_kind mode args loc
  | Parrayblit { src_mutability; dst_array_set_kind } ->
    arrayblit env ~src_mutability ~dst_array_set_kind args loc
  | _ -> env, transform_primitive0 env prim args loc
  [@@ocaml.warning "-fragile-match"]
