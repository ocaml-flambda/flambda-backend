(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2023 OCamlPro SAS                                    *)
(*   Copyright 2016--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Use CPS". -- A. Kennedy, "Compiling with Continuations Continued", ICFP
   2007. *)

module Env = Lambda_to_flambda_env
module L = Lambda
module CC = Closure_conversion
module P = Flambda_primitive
module IR = Closure_conversion.IR
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Function_decl = Closure_conversion_aux.Function_decls.Function_decl
module CCenv = Closure_conversion_aux.Env

(* CR pchambart: Replace uses by CC.Acc.t *)
module Acc = Closure_conversion_aux.Acc

type primitive_transform_result =
  | Primitive of L.primitive * L.lambda list * L.scoped_location
  | Transformed of L.lambda

let must_be_singleton_simple simples =
  match simples with
  | [simple] -> simple
  | [] | _ :: _ ->
    Misc.fatal_errorf "Expected singleton Simple but got: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space IR.print_simple)
      simples

let print_compact_location ppf (loc : Location.t) =
  if String.equal loc.loc_start.pos_fname "//toplevel//"
  then ()
  else
    let file, line, startchar = Location.get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "%a:%i" Location.print_filename file line;
    if startchar >= 0 then Format.fprintf ppf ",%i--%i" startchar endchar

let name_for_function (func : Lambda.lfunction) =
  (* Name anonymous functions by their source location, if known. *)
  match func.loc with
  | Loc_unknown -> "fn"
  | Loc_known { loc; _ } ->
    if Flambda_features.Expert.shorten_symbol_names ()
    then "fn"
    else Format.asprintf "fn[%a]" print_compact_location loc

let extra_args_for_exn_continuation env exn_handler =
  List.map
    (fun (ident, kind) -> IR.Var ident, kind)
    (Env.extra_args_for_continuation_with_kinds env exn_handler)

let _print_stack ppf stack =
  Format.fprintf ppf "%a"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
       (fun ppf (_id, cont) -> Format.fprintf ppf "%a" Continuation.print cont))
    stack

(* Uses of [Lstaticfail] that jump out of try-with handlers need special care:
   the correct number of pop trap operations must be inserted. A similar thing
   is also necessary for closing local allocation regions. *)
let compile_staticfail acc env ccenv ~(continuation : Continuation.t) ~args :
    Expr_with_acc.t =
  let try_stack_at_handler = Env.get_try_stack_at_handler env continuation in
  let try_stack_now = Env.get_try_stack env in
  let dbg =
    Debuginfo.none
    (* CR mshinwell: should probably be fixed in Lambda (on Lstaticraise) *)
  in
  if List.length try_stack_at_handler > List.length try_stack_now
  then
    Misc.fatal_errorf
      "Cannot jump to continuation %a: it would involve jumping into a \
       try-with body"
      Continuation.print continuation;
  assert (
    Continuation.Set.subset
      (Continuation.Set.of_list try_stack_at_handler)
      (Continuation.Set.of_list try_stack_now));
  let rec add_pop_traps acc ~try_stack_now =
    let add_pop cont ~try_stack_now after_pop =
      let mk_remaining_traps acc = add_pop_traps acc ~try_stack_now in
      let wrapper_cont = Continuation.create () in
      let trap_action : IR.trap_action = Pop { exn_handler = cont } in
      let handler = mk_remaining_traps acc after_pop in
      let body acc ccenv =
        CC.close_apply_cont acc ccenv ~dbg wrapper_cont (Some trap_action) []
      in
      fun acc env ->
        CC.close_let_cont acc env ~name:wrapper_cont ~is_exn_handler:false
          ~params:[] ~recursive:Nonrecursive ~body ~handler
    in
    let no_pop after_pop = after_pop in
    match try_stack_now, try_stack_at_handler with
    | [], [] -> no_pop
    | cont1 :: try_stack_now, cont2 :: _ ->
      if Continuation.equal cont1 cont2
      then no_pop
      else add_pop cont1 ~try_stack_now
    | cont :: try_stack_now, [] -> add_pop cont ~try_stack_now
    | [], _ :: _ -> assert false
    (* see above *)
  in
  let region_stack_at_handler =
    Env.region_stack_in_cont_scope env continuation
  in
  let region_stack_now = Env.region_stack env in
  if List.length region_stack_at_handler > List.length region_stack_now
  then
    Misc.fatal_errorf
      "Cannot jump to continuation %a: it would involve jumping into a local \
       allocation region"
      Continuation.print continuation;
  let rec add_end_regions acc ~region_stack_now =
    (* CR pchambart: This closes all the regions between region_stack_now and
       region_stack_at_handler, but closing only the last one should be
       sufficient. *)
    let add_end_region (region : Env.region_stack_element) ~region_stack_now
        after_everything =
      let add_remaining_end_regions acc =
        add_end_regions acc ~region_stack_now
      in
      let body = add_remaining_end_regions acc after_everything in
      fun acc ccenv ->
        CC.close_let acc ccenv
          [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
          Not_user_visible
          (End_region { is_try_region = false; region })
          ~body
    in
    let no_end_region after_everything = after_everything in
    match
      Env.pop_region region_stack_now, Env.pop_region region_stack_at_handler
    with
    | None, None -> no_end_region
    | Some (region1, region_stack_now), Some (region2, _) ->
      if Env.same_region region1 region2
      then no_end_region
      else add_end_region region1 ~region_stack_now
    | Some (region, region_stack_now), None ->
      add_end_region region ~region_stack_now
    | None, Some _ -> assert false
    (* see above *)
  in
  add_pop_traps acc ~try_stack_now
    (fun acc ccenv ->
      add_end_regions acc ~region_stack_now
        (fun acc ccenv ->
          CC.close_apply_cont acc ccenv ~dbg continuation None args)
        acc ccenv)
    acc ccenv

let rec try_to_find_location (lam : L.lambda) =
  (* This is very much best-effort and may overshoot, but will still likely be
     better than nothing. *)
  match lam with
  | Lprim (_, _, loc)
  | Lfunction { loc; _ }
  | Lletrec ({ def = { loc; _ }; _ } :: _, _)
  | Lapply { ap_loc = loc; _ }
  | Lfor { for_loc = loc; _ }
  | Lswitch (_, _, loc, _)
  | Lstringswitch (_, _, _, loc, _)
  | Lsend (_, _, _, _, _, _, loc, _)
  | Levent (_, { lev_loc = loc; _ }) ->
    loc
  | Llet (_, _, _, lam, _)
  | Lmutlet (_, _, lam, _)
  | Lifthenelse (lam, _, _, _)
  | Lstaticcatch (lam, _, _, _, _)
  | Lstaticraise (_, lam :: _)
  | Lwhile { wh_cond = lam; _ }
  | Lsequence (lam, _)
  | Lassign (_, lam)
  | Lifused (_, lam)
  | Lregion (lam, _)
  | Lexclave lam
  | Ltrywith (lam, _, _, _) ->
    try_to_find_location lam
  | Lvar _ | Lmutvar _ | Lconst _ | Lletrec _ | Lstaticraise (_, []) ->
    Debuginfo.Scoped_location.Loc_unknown

let try_to_find_debuginfo lam =
  Debuginfo.from_location (try_to_find_location lam)

let switch_for_if_then_else ~cond ~ifso ~ifnot ~kind =
  let switch : Lambda.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None
    }
  in
  L.Lswitch (cond, switch, try_to_find_location cond, kind)

let transform_primitive env (prim : L.primitive) args loc =
  match prim, args with
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    Transformed
      (L.Llet
         ( Strict,
           Lambda.layout_int,
           const_true,
           Lconst (Const_base (Const_int 1)),
           L.Llet
             ( Strict,
               Lambda.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond)
                 ~ifso:(L.Lvar const_true) ~ifnot:arg2 ~kind:Lambda.layout_int
             ) ))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    Transformed
      (L.Llet
         ( Strict,
           Lambda.layout_int,
           const_false,
           Lconst (Const_base (Const_int 0)),
           L.Llet
             ( Strict,
               Lambda.layout_int,
               cond,
               arg1,
               switch_for_if_then_else ~cond:(L.Lvar cond) ~ifso:arg2
                 ~ifnot:(L.Lvar const_false) ~kind:Lambda.layout_int ) ))
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
        let desc = Lambda.simple_prim_on_values ~name ~arity ~alloc:true in
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
        let desc = Lambda.simple_prim_on_values ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      else
        Misc.fatal_errorf
          "Lambda_to_flambda.transform_primitive: Pbigarrayset with unknown \
           layout and elements should only have dimensions between 1 and 3 \
           (see translprim).")
  | _, _ -> Primitive (prim, args, loc)
  [@@ocaml.warning "-fragile-match"]

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
            Lambda.layout_int,
            cond_result,
            cond,
            Lifthenelse
              ( Lvar cond_result,
                Lsequence (body, Lstaticraise (cont, [])),
                Lconst (Const_base (Const_int 0)),
                Lambda.layout_unit ) ),
        Same_region,
        Lambda.layout_unit )
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
        Lambda.layout_int,
        start_ident,
        start,
        Llet
          ( Strict,
            Lambda.layout_int,
            stop_ident,
            stop,
            Lifthenelse
              ( first_test,
                Lstaticcatch
                  ( Lstaticraise (cont, [L.Lvar start_ident]),
                    (cont, [ident, Lambda.layout_int]),
                    Lsequence
                      ( body,
                        Lifthenelse
                          ( subsequent_test,
                            Lstaticraise (cont, [next_value_of_counter]),
                            L.lambda_unit,
                            Lambda.layout_unit ) ),
                    Same_region,
                    Lambda.layout_unit ),
                L.lambda_unit,
                Lambda.layout_unit ) ) )
  in
  env, lam

let is_user_visible env id : IR.user_visible =
  if Ident.stamp id >= Env.ident_stamp_upon_starting env
  then Not_user_visible
  else
    let name = Ident.name id in
    if String.starts_with ~prefix:"*opt*" name
    then User_visible
    else
      let len = String.length name in
      if len > 0 && Char.equal name.[0] '*'
      then Not_user_visible
      else User_visible

let let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler
    ~params
    ~(body : Acc.t -> Env.t -> CCenv.t -> Continuation.t -> Expr_with_acc.t)
    ~(handler : Acc.t -> Env.t -> CCenv.t -> Expr_with_acc.t) : Expr_with_acc.t
    =
  let cont = Continuation.create () in
  let { Env.body_env; handler_env; extra_params } =
    Env.add_continuation env cont ~push_to_try_stack:is_exn_handler
      ~pop_region:false Nonrecursive
  in
  let handler_env, params_rev =
    List.fold_left
      (fun (handler_env, params_rev) (id, visible, layout) ->
        let arity_component =
          Flambda_arity.Component_for_creation.from_lambda layout
        in
        match arity_component with
        | Singleton kind ->
          let param = id, visible, kind in
          handler_env, param :: params_rev
        | Unboxed_product _ ->
          let arity = Flambda_arity.create [arity_component] in
          let fields =
            List.mapi
              (fun n kind ->
                let field =
                  Ident.create_local
                    (Printf.sprintf "%s_unboxed%d" (Ident.unique_name id) n)
                in
                field, kind)
              (Flambda_arity.unarize arity)
          in
          let handler_env =
            Env.register_unboxed_product handler_env ~unboxed_product:id
              ~before_unarization:arity_component ~fields
          in
          let new_params_rev =
            List.map (fun (id, kind) -> id, IR.Not_user_visible, kind) fields
            |> List.rev
          in
          handler_env, new_params_rev @ params_rev)
      (handler_env, []) params
  in
  let params = List.rev params_rev in
  let extra_params =
    List.map (fun (id, kind) -> id, is_user_visible env id, kind) extra_params
  in
  let handler acc ccenv = handler acc handler_env ccenv in
  let body acc ccenv = body acc body_env ccenv cont in
  CC.close_let_cont acc ccenv ~name:cont ~is_exn_handler
    ~params:(params @ extra_params) ~recursive:Nonrecursive ~body ~handler

let restore_continuation_context acc env ccenv cont ~close_current_region_early
    body =
  let[@inline] normal_case env acc ccenv =
    match Env.pop_regions_up_to_context env cont with
    | None -> body acc ccenv cont
    | Some region ->
      let ({ continuation_closing_region; continuation_after_closing_region }
            : Env.region_closure_continuation) =
        Env.region_closure_continuation env region
      in
      if not (Continuation.equal cont continuation_after_closing_region)
      then
        Misc.fatal_errorf
          "The continuation %a following the region closure should be the \
           current continuation %a"
          Continuation.print continuation_after_closing_region
          Continuation.print cont;
      body acc ccenv continuation_closing_region
  in
  (* If we need to close the current region early, that has to be done first.
     Then we redirect the return continuation to the one closing any further
     regions, if any exist, such that the region stack is brought in line with
     that expected by the real return continuation. See comment in [cps] on the
     [Lregion] case. *)
  if close_current_region_early
  then
    let env, region = Env.pop_one_region env in
    CC.close_let acc ccenv
      [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
      Not_user_visible
      (End_region { is_try_region = false; region })
      ~body:(fun acc ccenv -> normal_case env acc ccenv)
  else normal_case env acc ccenv

let restore_continuation_context_for_switch_arm env cont =
  match Env.pop_regions_up_to_context env cont with
  | None -> cont
  | Some region ->
    let ({ continuation_closing_region; continuation_after_closing_region }
          : Env.region_closure_continuation) =
      Env.region_closure_continuation env region
    in
    if not (Continuation.equal cont continuation_after_closing_region)
    then
      Misc.fatal_errorf
        "The continuation %a following the region closure should be the \
         current continuation %a"
        Continuation.print continuation_after_closing_region Continuation.print
        cont;
    continuation_closing_region

let apply_cont_with_extra_args acc env ccenv ~dbg cont traps args =
  let extra_args =
    List.map
      (fun var : IR.simple -> Var var)
      (Env.extra_args_for_continuation env cont)
  in
  restore_continuation_context acc env ccenv cont
    ~close_current_region_early:false (fun acc ccenv cont ->
      CC.close_apply_cont acc ~dbg ccenv cont traps (args @ extra_args))

let wrap_return_continuation acc env ccenv (apply : IR.apply) =
  let extra_args = Env.extra_args_for_continuation env apply.continuation in
  let close_current_region_early, region =
    match apply.region_close with
    | Rc_normal | Rc_nontail -> false, apply.region
    | Rc_close_at_apply ->
      (* [Rc_close_at_apply] means that the application is in tail position with
         respect to the *current region*. Only that region should be closed
         early, prior to the application, meaning that the region for the
         application itself is the one which is currently our parent. After the
         application, further regions should be closed if necessary in order to
         bring the current region stack in line with the return continuation's
         region stack. *)
      true, Env.parent_region env
  in
  let body acc ccenv continuation =
    match extra_args with
    | [] -> CC.close_apply acc ccenv { apply with continuation; region }
    | _ :: _ ->
      let wrapper_cont = Continuation.create () in
      let return_kinds = Flambda_arity.unarized_components apply.return_arity in
      let return_value_components =
        List.mapi
          (fun i _ -> Ident.create_local (Printf.sprintf "return_val%d" i))
          return_kinds
      in
      let args =
        List.map
          (fun var : IR.simple -> Var var)
          (return_value_components @ extra_args)
      in
      let dbg = Debuginfo.none in
      let handler acc ccenv =
        CC.close_apply_cont acc ccenv ~dbg continuation None args
      in
      let body acc ccenv =
        CC.close_apply acc ccenv
          { apply with continuation = wrapper_cont; region }
      in
      (* CR mshinwell: Think about DWARF support for unboxed products, here and
         elsewhere. *)
      let params =
        List.map2
          (fun return_value_component kind ->
            return_value_component, IR.Not_user_visible, kind)
          return_value_components return_kinds
      in
      CC.close_let_cont acc ccenv ~name:wrapper_cont ~is_exn_handler:false
        ~params ~recursive:Nonrecursive ~body ~handler
  in
  restore_continuation_context acc env ccenv apply.continuation
    ~close_current_region_early body

let primitive_can_raise (prim : Lambda.primitive) =
  match prim with
  | Pccall _ | Praise _ | Parrayrefs _ | Parraysets _ | Pmodint _ | Pdivint _
  | Pstringrefs | Pbytesrefs | Pbytessets
  | Pstring_load_16 false
  | Pstring_load_32 (false, _)
  | Pstring_load_f32 (false, _)
  | Pstring_load_64 (false, _)
  | Pstring_load_128 { unsafe = false; _ }
  | Pbytes_load_16 false
  | Pbytes_load_32 (false, _)
  | Pbytes_load_f32 (false, _)
  | Pbytes_load_64 (false, _)
  | Pbytes_load_128 { unsafe = false; _ }
  | Pbytes_set_16 false
  | Pbytes_set_32 false
  | Pbytes_set_f32 false
  | Pbytes_set_64 false
  | Pbytes_set_128 { unsafe = false; _ }
  | Pbigstring_load_16 { unsafe = false }
  | Pbigstring_load_32 { unsafe = false; mode = _; boxed = _ }
  | Pbigstring_load_f32 { unsafe = false; mode = _; boxed = _ }
  | Pbigstring_load_64 { unsafe = false; mode = _; boxed = _ }
  | Pbigstring_load_128 { unsafe = false; _ }
  | Pbigstring_set_16 { unsafe = false }
  | Pbigstring_set_32 { unsafe = false; boxed = _ }
  | Pbigstring_set_f32 { unsafe = false; boxed = _ }
  | Pbigstring_set_64 { unsafe = false; boxed = _ }
  | Pbigstring_set_128 { unsafe = false; _ }
  | Pfloatarray_load_128 { unsafe = false; _ }
  | Pfloat_array_load_128 { unsafe = false; _ }
  | Pint_array_load_128 { unsafe = false; _ }
  | Punboxed_float_array_load_128 { unsafe = false; _ }
  | Punboxed_float32_array_load_128 { unsafe = false; _ }
  | Punboxed_int32_array_load_128 { unsafe = false; _ }
  | Punboxed_int64_array_load_128 { unsafe = false; _ }
  | Punboxed_nativeint_array_load_128 { unsafe = false; _ }
  | Pfloatarray_set_128 { unsafe = false; _ }
  | Pfloat_array_set_128 { unsafe = false; _ }
  | Pint_array_set_128 { unsafe = false; _ }
  | Punboxed_float_array_set_128 { unsafe = false; _ }
  | Punboxed_float32_array_set_128 { unsafe = false; _ }
  | Punboxed_int32_array_set_128 { unsafe = false; _ }
  | Punboxed_int64_array_set_128 { unsafe = false; _ }
  | Punboxed_nativeint_array_set_128 { unsafe = false; _ }
  | Pdivbint { is_safe = Safe; _ }
  | Pmodbint { is_safe = Safe; _ }
  | Pbigarrayref (false, _, _, _)
  | Pbigarrayset (false, _, _, _)
  (* These bigarray primitives are translated into c-calls which may raise even
     if the unsafe flag is true *)
  | Pbigarrayref (_, _, Pbigarray_unknown, _)
  | Pbigarrayset (_, _, Pbigarray_unknown, _)
  | Pbigarrayref (_, _, _, Pbigarray_unknown_layout)
  | Pbigarrayset (_, _, _, Pbigarray_unknown_layout) ->
    true
  | Pbytes_to_string | Pbytes_of_string | Parray_of_iarray | Parray_to_iarray
  | Pignore | Pgetglobal _ | Psetglobal _ | Pgetpredef _ | Pmakeblock _
  | Pmakefloatblock _ | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Pmakeufloatblock _ | Pufloatfield _ | Psetufloatfield _ | Psequand | Psequor
  | Pmixedfield _ | Psetmixedfield _ | Pmakemixedblock _ | Pnot | Pnegint
  | Paddint | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Pcompare_ints | Pcompare_floats _ | Pcompare_bints _
  | Poffsetint _ | Poffsetref _ | Pintoffloat _
  | Pfloatofint (_, _)
  | Pfloatoffloat32 _ | Pfloat32offloat _
  | Pnegfloat (_, _)
  | Pabsfloat (_, _)
  | Paddfloat (_, _)
  | Psubfloat (_, _)
  | Pmulfloat (_, _)
  | Pdivfloat (_, _)
  | Pfloatcomp (_, _)
  | Punboxed_float_comp (_, _)
  | Pstringlength | Pstringrefu | Pbyteslength | Pbytesrefu | Pbytessetu
  | Pmakearray _ | Pduparray _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Pisint _ | Pisout | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _
  | Paddbint _ | Psubbint _ | Pmulbint _
  | Pdivbint { is_safe = Unsafe; _ }
  | Pmodbint { is_safe = Unsafe; _ }
  | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
  | Pbintcomp _ | Punboxed_int_comp _ | Pbigarraydim _
  | Pbigarrayref
      ( true,
        _,
        ( Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float32_t
        | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
        | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
        | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
        | Pbigarray_complex32 | Pbigarray_complex64 ),
        _ )
  | Pbigarrayset
      ( true,
        _,
        ( Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float32_t
        | Pbigarray_float64 | Pbigarray_sint8 | Pbigarray_uint8
        | Pbigarray_sint16 | Pbigarray_uint16 | Pbigarray_int32
        | Pbigarray_int64 | Pbigarray_caml_int | Pbigarray_native_int
        | Pbigarray_complex32 | Pbigarray_complex64 ),
        (Pbigarray_c_layout | Pbigarray_fortran_layout) )
  | Pstring_load_16 true
  | Pstring_load_32 (true, _)
  | Pstring_load_f32 (true, _)
  | Pstring_load_64 (true, _)
  | Pstring_load_128 { unsafe = true; _ }
  | Pbytes_load_16 true
  | Pbytes_load_32 (true, _)
  | Pbytes_load_f32 (true, _)
  | Pbytes_load_64 (true, _)
  | Pbytes_load_128 { unsafe = true; _ }
  | Pbytes_set_16 true
  | Pbytes_set_32 true
  | Pbytes_set_f32 true
  | Pbytes_set_64 true
  | Pbytes_set_128 { unsafe = true; _ }
  | Pbigstring_load_16 { unsafe = true }
  | Pbigstring_load_32 { unsafe = true; mode = _; boxed = _ }
  | Pbigstring_load_f32 { unsafe = true; mode = _; boxed = _ }
  | Pbigstring_load_64 { unsafe = true; mode = _; boxed = _ }
  | Pbigstring_load_128 { unsafe = true; _ }
  | Pbigstring_set_16 { unsafe = true }
  | Pbigstring_set_32 { unsafe = true; boxed = _ }
  | Pbigstring_set_f32 { unsafe = true; boxed = _ }
  | Pbigstring_set_64 { unsafe = true; boxed = _ }
  | Pbigstring_set_128 { unsafe = true; _ }
  | Pfloatarray_load_128 { unsafe = true; _ }
  | Pfloat_array_load_128 { unsafe = true; _ }
  | Pint_array_load_128 { unsafe = true; _ }
  | Punboxed_float_array_load_128 { unsafe = true; _ }
  | Punboxed_float32_array_load_128 { unsafe = true; _ }
  | Punboxed_int32_array_load_128 { unsafe = true; _ }
  | Punboxed_int64_array_load_128 { unsafe = true; _ }
  | Punboxed_nativeint_array_load_128 { unsafe = true; _ }
  | Pfloatarray_set_128 { unsafe = true; _ }
  | Pfloat_array_set_128 { unsafe = true; _ }
  | Pint_array_set_128 { unsafe = true; _ }
  | Punboxed_float_array_set_128 { unsafe = true; _ }
  | Punboxed_float32_array_set_128 { unsafe = true; _ }
  | Punboxed_int32_array_set_128 { unsafe = true; _ }
  | Punboxed_int64_array_set_128 { unsafe = true; _ }
  | Punboxed_nativeint_array_set_128 { unsafe = true; _ }
  | Pctconst _ | Pbswap16 | Pbbswap _ | Pint_as_pointer _ | Popaque _
  | Pprobe_is_enabled _ | Pobj_dup | Pobj_magic _
  | Pbox_float (_, _)
  | Punbox_float _ | Punbox_int _ | Pbox_int _ | Pmake_unboxed_product _
  | Punboxed_product_field _ | Pget_header _ ->
    false
  | Patomic_exchange | Patomic_cas | Patomic_fetch_add | Patomic_load _ -> false
  | Prunstack | Pperform | Presume | Preperform -> true (* XXX! *)
  | Pdls_get | Preinterpret_tagged_int63_as_unboxed_int64
  | Preinterpret_unboxed_int64_as_tagged_int63 ->
    false

type non_tail_continuation =
  Acc.t ->
  Env.t ->
  CCenv.t ->
  IR.simple list ->
  [`Complex] Flambda_arity.Component_for_creation.t ->
  Expr_with_acc.t

type non_tail_list_continuation =
  Acc.t ->
  Env.t ->
  CCenv.t ->
  IR.simple list list ->
  [`Complex] Flambda_arity.Component_for_creation.t list ->
  Expr_with_acc.t

type cps_continuation =
  | Tail of Continuation.t
  | Non_tail of non_tail_continuation

let apply_cps_cont_simple k ?(dbg = Debuginfo.none) acc env ccenv simples
    (arity_component : [`Complex] Flambda_arity.Component_for_creation.t) =
  match k with
  | Tail k -> apply_cont_with_extra_args acc env ccenv ~dbg k None simples
  | Non_tail k -> k acc env ccenv simples arity_component

let apply_cps_cont k ?dbg acc env ccenv id
    (arity_component : [`Complex] Flambda_arity.Component_for_creation.t) =
  apply_cps_cont_simple k ?dbg acc env ccenv [IR.Var id] arity_component

let get_unarized_vars id env =
  match Env.get_unboxed_product_fields env id with
  | None -> [IR.Var id]
  | Some (_, fields) -> List.map (fun id -> IR.Var id) fields

let maybe_insert_let_cont result_var_name layout k acc env ccenv body =
  match k with
  | Tail k -> body acc env ccenv k
  | Non_tail k ->
    let arity_component =
      Flambda_arity.Component_for_creation.from_lambda layout
    in
    let arity = Flambda_arity.create [arity_component] in
    if Flambda_arity.cardinal_unarized arity < 1
    then
      let_cont_nonrecursive_with_extra_params acc env ccenv
        ~is_exn_handler:false ~params:[]
        ~handler:(fun acc env ccenv -> k acc env ccenv [] arity_component)
        ~body
    else
      let result_var = Ident.create_local result_var_name in
      let_cont_nonrecursive_with_extra_params acc env ccenv
        ~is_exn_handler:false
        ~params:[result_var, IR.Not_user_visible, layout]
        ~handler:(fun acc env ccenv ->
          k acc env ccenv (get_unarized_vars result_var env) arity_component)
        ~body

let name_if_not_var acc ccenv name simple kind body =
  match simple with
  | IR.Var id -> body id acc ccenv
  | IR.Const _ ->
    let id = Ident.create_local name in
    CC.close_let acc ccenv
      [id, kind]
      Not_user_visible (IR.Simple simple) ~body:(body id)

let rec cps acc env ccenv (lam : L.lambda) (k : cps_continuation)
    (k_exn : Continuation.t) : Expr_with_acc.t =
  match lam with
  | Lvar id -> (
    assert (not (Env.is_mutable env id));
    match Env.get_unboxed_product_fields env id with
    | None ->
      let kind =
        match CCenv.find_simple_to_substitute_exn ccenv id with
        | exception Not_found -> snd (CCenv.find_var ccenv id)
        | _, kind -> kind
      in
      let arity_component =
        Flambda_arity.Component_for_creation.Singleton kind
      in
      apply_cps_cont k acc env ccenv id arity_component
    | Some (before_unarization, fields) ->
      let fields = List.map (fun id -> IR.Var id) fields in
      apply_cps_cont_simple k acc env ccenv fields before_unarization)
  | Lmutvar id ->
    (* CR mshinwell: note: mutable variables of non-singleton layouts are not
       supported *)
    let return_id, kind = Env.get_mutable_variable_with_kind env id in
    apply_cps_cont k acc env ccenv return_id
      (Flambda_arity.Component_for_creation.Singleton kind)
  | Lconst const ->
    apply_cps_cont_simple k acc env ccenv [IR.Const const]
      (Singleton
         (Flambda_kind.With_subkind.from_lambda_values_and_unboxed_numbers_only
            (Lambda.structured_constant_layout const)))
  | Lapply
      { ap_func;
        ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall = _;
        ap_inlined;
        ap_specialised = _;
        ap_probe
      } ->
    (* Note that we don't need kind information about [ap_args] since we already
       have it on the corresponding [Simple]s in the environment. *)
    maybe_insert_let_cont "apply_result" ap_result_layout k acc env ccenv
      (fun acc env ccenv k ->
        cps_tail_apply acc env ccenv ap_func ap_args ap_region_close ap_mode
          ap_loc ap_inlined ap_probe ap_result_layout k k_exn)
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let dbg = Debuginfo.from_location func.loc in
    let func =
      cps_function env ~fid:id ~recursive:(Non_recursive : Recursive.t) func
    in
    let body acc ccenv =
      apply_cps_cont k ~dbg acc env ccenv id
        (Singleton Flambda_kind.With_subkind.any_value)
    in
    CC.close_let_rec acc ccenv ~function_declarations:[func] ~body
      ~current_region:(Env.current_region env)
  | Lmutlet (value_kind, id, defining_expr, body) ->
    (* CR mshinwell: user-visibleness needs thinking about here *)
    let temp_id = Ident.create_local "let_mutable" in
    let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler:false
      ~params:[temp_id, IR.Not_user_visible, value_kind]
      ~body:(fun acc env ccenv after_defining_expr ->
        cps_tail acc env ccenv defining_expr after_defining_expr k_exn)
      ~handler:(fun acc env ccenv ->
        let kind =
          Flambda_kind.With_subkind.from_lambda_values_and_unboxed_numbers_only
            value_kind
        in
        let env, new_id = Env.register_mutable_variable env id kind in
        let body acc ccenv = cps acc env ccenv body k k_exn in
        CC.close_let acc ccenv
          [new_id, kind]
          User_visible (Simple (Var temp_id)) ~body)
  | Llet ((Strict | Alias | StrictOpt), _, fun_id, Lfunction func, body) ->
    (* This case is here to get function names right. *)
    let bindings = cps_function_bindings env [L.{ id = fun_id; def = func }] in
    let body acc ccenv = cps acc env ccenv body k k_exn in
    let let_expr =
      List.fold_left
        (fun body func acc ccenv ->
          CC.close_let_rec acc ccenv ~function_declarations:[func] ~body
            ~current_region:(Env.current_region env))
        body bindings
    in
    let_expr acc ccenv
  | Llet ((Strict | Alias | StrictOpt), layout, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body acc ccenv = cps acc env ccenv body k k_exn in
    let kind =
      Flambda_kind.With_subkind.from_lambda_values_and_unboxed_numbers_only
        layout
    in
    CC.close_let acc ccenv
      [id, kind]
      (is_user_visible env id) (Simple (Const const)) ~body
  | Llet
      ( ((Strict | Alias | StrictOpt) as let_kind),
        layout,
        id,
        Lprim (prim, args, loc),
        body ) -> (
    match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : IR.exn_continuation option =
        if primitive_can_raise prim
        then
          Some
            { exn_handler = k_exn;
              extra_args = extra_args_for_exn_continuation env k_exn
            }
        else None
      in
      cps_non_tail_list acc env ccenv args
        (fun acc env ccenv args _arity ->
          let env, ids_with_kinds =
            match layout with
            | Ptop | Pbottom ->
              Misc.fatal_error "Cannot bind layout [Ptop] or [Pbottom]"
            | Pvalue _ | Punboxed_int _ | Punboxed_float _ | Punboxed_vector _
              ->
              ( env,
                [ ( id,
                    Flambda_kind.With_subkind
                    .from_lambda_values_and_unboxed_numbers_only layout ) ] )
            | Punboxed_product layouts ->
              let arity_component =
                Flambda_arity.Component_for_creation.Unboxed_product
                  (List.map Flambda_arity.Component_for_creation.from_lambda
                     layouts)
              in
              let arity = Flambda_arity.create [arity_component] in
              let fields = Flambda_arity.fresh_idents_unarized ~id arity in
              let env =
                Env.register_unboxed_product env ~unboxed_product:id
                  ~before_unarization:arity_component ~fields
              in
              env, fields
          in
          let body acc ccenv = cps acc env ccenv body k k_exn in
          let region = Env.current_region env in
          CC.close_let acc ccenv ids_with_kinds (is_user_visible env id)
            (Prim { prim; args; loc; exn_continuation; region })
            ~body)
        k_exn
    | Transformed lam ->
      cps acc env ccenv (L.Llet (let_kind, layout, id, lam, body)) k k_exn)
  | Llet
      ( (Strict | Alias | StrictOpt),
        _,
        id,
        Lassign (being_assigned, new_value),
        body ) ->
    (* This case is also to avoid extraneous continuations in code that relies
       on the ref-conversion optimisation. *)
    if not (Env.is_mutable env being_assigned)
    then
      Misc.fatal_errorf "Lassign on non-mutable variable %a" Ident.print
        being_assigned;
    cps_non_tail_simple acc env ccenv new_value
      (fun acc env ccenv new_value _arity ->
        let new_value = must_be_singleton_simple new_value in
        let env, new_id = Env.update_mutable_variable env being_assigned in
        let body acc ccenv =
          let body acc ccenv = cps acc env ccenv body k k_exn in
          CC.close_let acc ccenv
            [id, Flambda_kind.With_subkind.tagged_immediate]
            Not_user_visible (Simple (Const L.const_unit)) ~body
        in
        let value_kind =
          snd (Env.get_mutable_variable_with_kind env being_assigned)
        in
        CC.close_let acc ccenv
          [new_id, value_kind]
          User_visible (Simple new_value) ~body)
      k_exn
  | Llet ((Strict | Alias | StrictOpt), _layout, id, defining_expr, Lvar id')
    when Ident.same id id' ->
    (* Simplif already simplifies such bindings, but we can generate new ones
       when translating primitives (see the Lprim case below). *)
    (* This case must not be moved above the case for let-bound primitives. *)
    cps acc env ccenv defining_expr k k_exn
  | Llet ((Strict | Alias | StrictOpt), layout, id, defining_expr, body) ->
    let_cont_nonrecursive_with_extra_params acc env ccenv ~is_exn_handler:false
      ~params:[id, is_user_visible env id, layout]
      ~body:(fun acc env ccenv after_defining_expr ->
        cps_tail acc env ccenv defining_expr after_defining_expr k_exn)
      ~handler:(fun acc env ccenv -> cps acc env ccenv body k k_exn)
  (* CR pchambart: This version would avoid one let cont, but would miss the
     value kind. It should be used when CC.close_let can propagate the
     value_kind. *)
  (* let k acc env ccenv value =
   *   let body acc ccenv = cps acc env ccenv body k k_exn in
   *   CC.close_let acc ccenv id User_visible value_kind (Simple value) ~body
   * in
   * cps_non_tail_simple acc env ccenv defining_expr k k_exn *)
  | Lletrec (bindings, body) ->
    let function_declarations = cps_function_bindings env bindings in
    let body acc ccenv = cps acc env ccenv body k k_exn in
    CC.close_let_rec acc ccenv ~function_declarations ~body
      ~current_region:(Env.current_region env)
  | Lprim (prim, args, loc) -> (
    match[@ocaml.warning "-fragile-match"] prim with
    | Praise raise_kind -> (
      match args with
      | [_] ->
        cps_non_tail_list acc env ccenv args
          (fun acc _env ccenv args _arity ->
            if List.compare_length_with (List.hd args) 1 <> 0
            then Misc.fatal_error "Lraise takes only one unarized argument";
            let exn_continuation : IR.exn_continuation =
              { exn_handler = k_exn;
                extra_args = extra_args_for_exn_continuation env k_exn
              }
            in
            let dbg = Debuginfo.from_location loc in
            CC.close_raise acc ccenv ~raise_kind
              ~arg:(List.hd (List.hd args))
              ~dbg exn_continuation)
          k_exn
      | [] | _ :: _ ->
        Misc.fatal_errorf "Wrong number of arguments for Lraise: %a"
          Printlambda.primitive prim)
    | _ ->
      (* The code for translating primitives needs a let binding, so we
         introduce such a binding explicitly. *)
      (* For primitives like [Psequand], which are transformed instead, this
         binding is useless and can move calls out of tail position, so we rely
         on a special case above that removes such bindings when the bound
         expression isn't a primitive. *)
      let name = Printlambda.name_of_primitive prim in
      let id = Ident.create_local name in
      let result_layout = L.primitive_result_layout prim in
      (match result_layout with
      | Pvalue _ | Punboxed_float _ | Punboxed_int _ | Punboxed_vector _
      | Punboxed_product _ ->
        ()
      | Ptop | Pbottom ->
        Misc.fatal_errorf "Invalid result layout %a for primitive %a"
          Printlambda.layout result_layout Printlambda.primitive prim);
      cps acc env ccenv
        (L.Llet (Strict, result_layout, id, lam, L.Lvar id))
        k k_exn)
  | Lswitch (scrutinee, switch, loc, kind) ->
    maybe_insert_let_cont "switch_result" kind k acc env ccenv
      (fun acc env ccenv k ->
        cps_switch acc env ccenv switch
          ~condition_dbg:(Debuginfo.from_location loc)
          ~scrutinee k k_exn)
  | Lstringswitch (scrutinee, cases, default, loc, kind) ->
    cps acc env ccenv
      (Matching.expand_stringswitch loc kind scrutinee cases default)
      k k_exn
  | Lstaticraise (static_exn, args) ->
    let continuation = Env.get_static_exn_continuation env static_exn in
    cps_non_tail_list acc env ccenv args
      (fun acc env ccenv args _arity ->
        let extra_args =
          List.map
            (fun var : IR.simple -> Var var)
            (Env.extra_args_for_continuation env continuation)
        in
        compile_staticfail acc env ccenv ~continuation
          ~args:(List.flatten args @ extra_args))
      k_exn
  | Lstaticcatch (body, (static_exn, args), handler, r, layout) ->
    maybe_insert_let_cont "staticcatch_result" layout k acc env ccenv
      (fun acc env ccenv k ->
        let pop_region =
          match r with Popped_region -> true | Same_region -> false
        in
        let continuation = Continuation.create () in
        let { Env.body_env; handler_env; extra_params } =
          Env.add_static_exn_continuation env static_exn ~pop_region
            continuation
        in
        let recursive : Asttypes.rec_flag =
          if Env.is_static_exn_recursive env static_exn
          then Recursive
          else Nonrecursive
        in
        let handler_env, params =
          let args_arity = Flambda_arity.from_lambda_list (List.map snd args) in
          let unarized_per_arg =
            Flambda_arity.unarize_per_parameter args_arity
          in
          let handler_env, args =
            List.fold_left_map
              (fun handler_env ((arg, layout), kinds) ->
                match kinds with
                | [] -> handler_env, []
                | [kind] -> handler_env, [arg, kind]
                | _ :: _ ->
                  let fields =
                    List.mapi
                      (fun n kind ->
                        let ident =
                          Ident.create_local
                            (Printf.sprintf "%s_unboxed%d"
                               (Ident.unique_name arg) n)
                        in
                        ident, kind)
                      kinds
                  in
                  let before_unarization =
                    Flambda_arity.Component_for_creation.from_lambda layout
                  in
                  ( Env.register_unboxed_product handler_env
                      ~unboxed_product:arg ~before_unarization ~fields,
                    fields ))
              handler_env
              (List.combine args unarized_per_arg)
          in
          ( handler_env,
            List.map
              (fun (arg, kind) -> arg, is_user_visible env arg, kind)
              (List.flatten args @ extra_params) )
        in
        let handler acc ccenv =
          let ccenv = CCenv.set_not_at_toplevel ccenv in
          cps_tail acc handler_env ccenv handler k k_exn
        in
        let body acc ccenv = cps_tail acc body_env ccenv body k k_exn in
        CC.close_let_cont acc ccenv ~name:continuation ~is_exn_handler:false
          ~params ~recursive ~body ~handler)
  | Lsend (meth_kind, meth, obj, args, pos, mode, loc, layout) ->
    cps_non_tail_simple acc env ccenv obj
      (fun acc env ccenv obj _obj_arity ->
        let obj = must_be_singleton_simple obj in
        cps_non_tail_var "meth" acc env ccenv meth
          Flambda_kind.With_subkind.any_value
          (fun acc env ccenv meth _meth_arity ->
            cps_non_tail_list acc env ccenv args
              (fun acc env ccenv args args_arity ->
                maybe_insert_let_cont "send_result" layout k acc env ccenv
                  (fun acc env ccenv k ->
                    let exn_continuation : IR.exn_continuation =
                      { exn_handler = k_exn;
                        extra_args = extra_args_for_exn_continuation env k_exn
                      }
                    in
                    let apply : IR.apply =
                      { kind = Method { kind = meth_kind; obj };
                        func = meth;
                        continuation = k;
                        exn_continuation;
                        args = List.flatten args;
                        loc;
                        region_close = pos;
                        inlined = Default_inlined;
                        probe = None;
                        mode;
                        region = Env.current_region env;
                        args_arity = Flambda_arity.create args_arity;
                        return_arity =
                          Flambda_arity.unarize_t
                            (Flambda_arity.create
                               [ Flambda_arity.Component_for_creation.from_lambda
                                   layout ])
                      }
                    in
                    wrap_return_continuation acc env ccenv apply))
              k_exn)
          k_exn)
      k_exn
  | Ltrywith (body, id, handler, kind) ->
    let dbg = Debuginfo.none (* CR mshinwell: fix [Lambda] *) in
    let body_result = Ident.create_local "body_result" in
    let region = Ident.create_local "try_region" in
    (* As for all other constructs, the OCaml type checker and the Lambda
       generation pass ensures that there will be an enclosing region around the
       whole [Ltrywith] (possibly not immediately enclosing, but maybe further
       out). The only reason we need a [Begin_region] here is to be able to
       unwind the local allocation stack if the exception handler is invoked.
       There is no corresponding [End_region] on the non-exceptional path
       because there might be a local allocation in the "try" block that needs
       to be returned. However, the inner allocations happen in the outer
       region, as we need to ensure the outer region is not deleted if we return
       normally from the "try" block. We annotate the begin and end region
       primitives to specify they correspond to try-regions, to ensure they are
       not deleted. *)
    (* Under a try-with block, any exception might introduce a branch to the
       handler. So while for static catches we could simplify the body in the
       same toplevel context, here we need to assume that all of the body could
       be behind a branch. *)
    let ccenv = CCenv.set_not_at_toplevel ccenv in
    let handler k acc env ccenv =
      CC.close_let acc ccenv
        [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
        Not_user_visible
        (End_region { is_try_region = true; region })
        ~body:(fun acc ccenv -> cps_tail acc env ccenv handler k k_exn)
    in
    let begin_try_region body =
      CC.close_let acc ccenv
        [region, Flambda_kind.With_subkind.region]
        Not_user_visible
        (Begin_region { is_try_region = true })
        ~body
    in
    begin_try_region (fun acc ccenv ->
        maybe_insert_let_cont "try_with_result" kind k acc env ccenv
          (fun acc env ccenv k ->
            let_cont_nonrecursive_with_extra_params acc env ccenv
              ~is_exn_handler:true
              ~params:[id, is_user_visible env id, Lambda.layout_block]
              ~body:(fun acc env ccenv handler_continuation ->
                let_cont_nonrecursive_with_extra_params acc env ccenv
                  ~is_exn_handler:false
                  ~params:[body_result, Not_user_visible, kind]
                  ~body:(fun acc env ccenv poptrap_continuation ->
                    let_cont_nonrecursive_with_extra_params acc env ccenv
                      ~is_exn_handler:false ~params:[]
                      ~body:(fun acc env ccenv body_continuation ->
                        apply_cont_with_extra_args acc env ccenv ~dbg
                          body_continuation
                          (Some (IR.Push { exn_handler = handler_continuation }))
                          [])
                      ~handler:(fun acc env ccenv ->
                        cps_tail acc env ccenv body poptrap_continuation
                          handler_continuation))
                  ~handler:(fun acc env ccenv ->
                    apply_cont_with_extra_args acc env ccenv ~dbg k
                      (Some (IR.Pop { exn_handler = handler_continuation }))
                      (get_unarized_vars body_result env)))
              ~handler:(handler k)))
  | Lifthenelse (cond, ifso, ifnot, kind) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot ~kind in
    cps acc env ccenv lam k k_exn
  | Lsequence (lam1, lam2) ->
    let k acc env ccenv _value _arity = cps acc env ccenv lam2 k k_exn in
    cps_non_tail_simple acc env ccenv lam1 k k_exn
  | Lwhile { wh_cond = cond; wh_body = body } ->
    (* CR-someday mshinwell: make use of wh_cond_region / wh_body_region? *)
    let env, loop = rec_catch_for_while_loop env cond body in
    cps acc env ccenv loop k k_exn
  | Lfor
      { for_id = ident;
        for_loc = loc;
        for_from = start;
        for_to = stop;
        for_dir = dir;
        for_body = body
      } ->
    let env, loop = rec_catch_for_for_loop env loc ident start stop dir body in
    cps acc env ccenv loop k k_exn
  | Lassign (being_assigned, new_value) ->
    if not (Env.is_mutable env being_assigned)
    then
      Misc.fatal_errorf "Lassign on non-mutable variable %a" Ident.print
        being_assigned;
    cps_non_tail_simple acc env ccenv new_value
      (fun acc env ccenv new_value _arity ->
        let new_value = must_be_singleton_simple new_value in
        let env, new_id = Env.update_mutable_variable env being_assigned in
        let body acc ccenv =
          apply_cps_cont_simple k acc env ccenv [Const L.const_unit]
            (Singleton Flambda_kind.With_subkind.tagged_immediate)
        in
        let _, value_kind =
          Env.get_mutable_variable_with_kind env being_assigned
        in
        CC.close_let acc ccenv
          [new_id, value_kind]
          User_visible (Simple new_value) ~body)
      k_exn
  | Levent (body, _event) -> cps acc env ccenv body k k_exn
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if an
       identifier is. Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression, or
       by completely removing it (replacing by unit). *)
    Misc.fatal_error
      "[Lifused] should have been removed by [Simplif.simplify_lets]"
  | Lregion (body, _) when not (Flambda_features.stack_allocation_enabled ()) ->
    cps acc env ccenv body k k_exn
  | Lexclave body ->
    let region = Env.current_region env in
    CC.close_let acc ccenv
      [Ident.create_local "unit", Flambda_kind.With_subkind.tagged_immediate]
      Not_user_visible
      (End_region { is_try_region = false; region })
      ~body:(fun acc ccenv ->
        let env = Env.leaving_region env in
        cps acc env ccenv body k k_exn)
  | Lregion (body, layout) ->
    (* Here we need to build the region closure continuation (see long comment
       above). Since we're not in tail position, we also need to have a new
       continuation for the code after the body. *)
    let region = Ident.create_local "region" in
    let dbg = Debuginfo.none in
    CC.close_let acc ccenv
      [region, Flambda_kind.With_subkind.region]
      Not_user_visible
      (Begin_region { is_try_region = false })
      ~body:(fun acc ccenv ->
        maybe_insert_let_cont "body_return" layout k acc env ccenv
          (fun acc env ccenv k ->
            let wrap_return = Ident.create_local "region_return" in
            let_cont_nonrecursive_with_extra_params acc env ccenv
              ~is_exn_handler:false
              ~params:[wrap_return, Not_user_visible, layout]
              ~body:(fun acc env ccenv continuation_closing_region ->
                (* We register this region to be closed by the newly-created
                   region closure continuation. When we reach a point in [body]
                   where we would normally jump to [return_continuation] (i.e.
                   leaving the body), we will instead jump to
                   [region_closure_continuation] to ensure the region is closed
                   at the right time. Exception raises and tailcall cases will
                   generate their own [End_region]s and use
                   [return_continuation] directly. (See long comment above.)

                   In the case where we jump out of the scope of several regions
                   at once, we will jump directly to the region closure
                   continuation for the outermost open region. For this to be
                   correct we rely on the fact that the code structure here,
                   which follows the block structure of the Lambda code, ensures
                   this is equivalent to going through the sequence of nested
                   [region_closure_continuation]s we generate.

                   In the event the region closure continuation isn't used (e.g.
                   the only exit is a tailcall), the [Let_cont] will be
                   discarded by [Closure_conversion]. *)
                let env =
                  Env.entering_region env region ~continuation_closing_region
                    ~continuation_after_closing_region:k
                in
                cps_tail acc env ccenv body k k_exn)
              ~handler:(fun acc env ccenv ->
                CC.close_let acc ccenv
                  [ ( Ident.create_local "unit",
                      Flambda_kind.With_subkind.tagged_immediate ) ]
                  Not_user_visible
                  (End_region { is_try_region = false; region })
                  ~body:(fun acc ccenv ->
                    (* Both body and handler will continue at
                       [return_continuation] by default.
                       [restore_region_context] will intercept the
                       [Lstaticraise] jump to this handler if needed. *)
                    apply_cont_with_extra_args acc env ccenv ~dbg k None
                      (get_unarized_vars wrap_return env)))))

and cps_non_tail_simple :
    Acc.t ->
    Env.t ->
    CCenv.t ->
    Lambda.lambda ->
    non_tail_continuation ->
    Continuation.t ->
    Expr_with_acc.t =
 fun acc env ccenv lam (k : non_tail_continuation) k_exn ->
  cps acc env ccenv lam (Non_tail k) k_exn

and cps_non_tail_var :
    string ->
    Acc.t ->
    Env.t ->
    CCenv.t ->
    Lambda.lambda ->
    Flambda_kind.With_subkind.t ->
    (Acc.t ->
    Env.t ->
    CCenv.t ->
    Ident.t ->
    [`Complex] Flambda_arity.Component_for_creation.t ->
    Expr_with_acc.t) ->
    Continuation.t ->
    Expr_with_acc.t =
 fun name acc env ccenv lam kind k k_exn ->
  cps_non_tail_simple acc env ccenv lam
    (fun acc env ccenv simple arity ->
      let simple = must_be_singleton_simple simple in
      name_if_not_var acc ccenv name simple kind (fun var acc ccenv ->
          k acc env ccenv var arity))
    k_exn

and cps_tail_apply acc env ccenv ap_func ap_args ap_region_close ap_mode ap_loc
    ap_inlined ap_probe ap_return (k : Continuation.t) (k_exn : Continuation.t)
    : Expr_with_acc.t =
  cps_non_tail_list acc env ccenv ap_args
    (fun acc env ccenv args args_arity ->
      cps_non_tail_var "func" acc env ccenv ap_func
        Flambda_kind.With_subkind.any_value
        (fun acc env ccenv func _func_arity ->
          let exn_continuation : IR.exn_continuation =
            { exn_handler = k_exn;
              extra_args = extra_args_for_exn_continuation env k_exn
            }
          in
          let apply : IR.apply =
            { kind = Function;
              func;
              continuation = k;
              exn_continuation;
              args = List.flatten args;
              loc = ap_loc;
              region_close = ap_region_close;
              inlined = ap_inlined;
              probe = ap_probe;
              mode = ap_mode;
              region = Env.current_region env;
              args_arity = Flambda_arity.create args_arity;
              return_arity =
                Flambda_arity.unarize_t
                  (Flambda_arity.create
                     [Flambda_arity.Component_for_creation.from_lambda ap_return])
            }
          in
          wrap_return_continuation acc env ccenv apply)
        k_exn)
    k_exn

and cps_tail acc env ccenv (lam : L.lambda) (k : Continuation.t)
    (k_exn : Continuation.t) : Expr_with_acc.t =
  cps acc env ccenv lam (Tail k) k_exn

and cps_non_tail_list :
    Acc.t ->
    Env.t ->
    CCenv.t ->
    Lambda.lambda list ->
    non_tail_list_continuation ->
    Continuation.t ->
    Expr_with_acc.t =
 fun acc env ccenv lams (k : non_tail_list_continuation) k_exn ->
  let lams = List.rev lams in
  (* Always evaluate right-to-left. *)
  cps_non_tail_list_core acc env ccenv lams
    (fun acc env ccenv ids
         (arity : [`Complex] Flambda_arity.Component_for_creation.t list) ->
      k acc env ccenv (List.rev ids) (List.rev arity))
    k_exn

and cps_non_tail_list_core acc env ccenv (lams : L.lambda list)
    (k : non_tail_list_continuation) (k_exn : Continuation.t) =
  match lams with
  | [] -> k acc env ccenv [] []
  | lam :: lams ->
    cps_non_tail_simple acc env ccenv lam
      (fun acc env ccenv simples arity ->
        cps_non_tail_list_core acc env ccenv lams
          (fun acc env ccenv simples' arity' ->
            k acc env ccenv (simples :: simples') (arity :: arity'))
          k_exn)
      k_exn

and cps_function_bindings env (bindings : Lambda.rec_binding list) =
  let bindings_with_wrappers =
    List.map
      (fun L.
             { id = fun_id;
               def =
                 { kind;
                   params;
                   body = fbody;
                   attr;
                   loc;
                   ret_mode;
                   mode;
                   region;
                   return;
                   _
                 }
             } ->
        match
          Simplif.split_default_wrapper ~id:fun_id ~kind ~params ~body:fbody
            ~return ~attr ~loc ~ret_mode ~mode ~region
        with
        | [{ id; def = lfun }] -> [id, lfun]
        | [{ id = id1; def = lfun1 }; { id = id2; def = lfun2 }] ->
          [id1, lfun1; id2, lfun2]
        | [] | _ :: _ :: _ :: _ ->
          Misc.fatal_errorf
            "Unexpected return value from [split_default_wrapper] when \
             translating:@ %a"
            Ident.print fun_id)
      bindings
  in
  let free_idents, directed_graph =
    let fun_ids =
      Ident.Set.of_list (List.map (fun { L.id; _ } -> id) bindings)
    in
    List.fold_left
      (fun (free_ids, graph) (fun_id, ({ body; _ } : L.lfunction)) ->
        let free_ids_of_body = Lambda.free_variables body in
        let free_ids = Ident.Map.add fun_id free_ids_of_body free_ids in
        let free_fun_ids = Ident.Set.inter fun_ids free_ids_of_body in
        let graph = Ident.Map.add fun_id free_fun_ids graph in
        free_ids, graph)
      (Ident.Map.empty, Ident.Map.empty)
      (List.flatten bindings_with_wrappers)
  in
  let recursive_functions =
    let module SCC = Strongly_connected_components.Make (Ident) in
    let connected_components =
      SCC.connected_components_sorted_from_roots_to_leaf directed_graph
    in
    Array.fold_left
      (fun rec_ids component ->
        match component with
        | SCC.No_loop _ -> rec_ids
        | SCC.Has_loop elts -> List.fold_right Ident.Set.add elts rec_ids)
      Ident.Set.empty connected_components
  in
  let recursive fun_id : Recursive.t =
    if Ident.Set.mem fun_id recursive_functions
    then Recursive
    else Non_recursive
  in
  let bindings_with_wrappers = List.flatten bindings_with_wrappers in
  List.map
    (fun (fun_id, def) ->
      cps_function env ~fid:fun_id ~recursive:(recursive fun_id)
        ~precomputed_free_idents:(Ident.Map.find fun_id free_idents)
        def)
    bindings_with_wrappers

and cps_function env ~fid ~(recursive : Recursive.t) ?precomputed_free_idents
    ({ kind; params; return; body; attr; loc; mode; ret_mode; region } :
      L.lfunction) : Function_decl.t =
  let first_complex_local_param =
    List.length params
    - match kind with Curried { nlocal } -> nlocal | Tupled -> 0
  in
  let unboxing_kind (layout : Lambda.layout) :
      Function_decl.unboxing_kind option =
    match[@warning "-fragile-match"] layout with
    | Pvalue
        (Pvariant
          { consts = []; non_consts = [(0, Constructor_uniform field_kinds)] })
      ->
      Some
        (Fields_of_block_with_tag_zero
           (List.map Flambda_kind.With_subkind.from_lambda_value_kind
              field_kinds))
    | Pvalue
        (Pvariant
          { consts = []; non_consts = [(tag, Constructor_uniform field_kinds)] })
      when tag = Obj.double_array_tag ->
      assert (
        List.for_all
          (fun (kind : Lambda.value_kind) ->
            match kind with
            | Pboxedfloatval Pfloat64 -> true
            | Pboxedfloatval Pfloat32
            | Pgenval | Pintval | Pboxedintval _ | Pvariant _ | Parrayval _
            | Pboxedvectorval _ ->
              false)
          field_kinds);
      Some (Unboxed_float_record (List.length field_kinds))
    | Pvalue (Pboxedfloatval Pfloat64) -> Some (Unboxed_number Naked_float)
    | Pvalue (Pboxedfloatval Pfloat32) -> Some (Unboxed_number Naked_float32)
    | Pvalue (Pboxedintval bi) ->
      let bn : Flambda_kind.Boxable_number.t =
        match bi with
        | Pint32 -> Naked_int32
        | Pint64 -> Naked_int64
        | Pnativeint -> Naked_nativeint
      in
      Some (Unboxed_number bn)
    | Pvalue (Pboxedvectorval bv) ->
      let bn : Flambda_kind.Boxable_number.t =
        match bv with Pvec128 _ -> Naked_vec128
      in
      Some (Unboxed_number bn)
    | Pvalue (Pgenval | Pintval | Pvariant _ | Parrayval _)
    | Ptop | Pbottom | Punboxed_float _ | Punboxed_int _ | Punboxed_vector _
    | Punboxed_product _ ->
      Location.prerr_warning
        (Debuginfo.Scoped_location.to_location loc)
        Warnings.Unboxing_impossible;
      None
  in
  let params_arity =
    Flambda_arity.from_lambda_list
      (List.map (fun (p : L.lparam) -> p.layout) params)
  in
  let unarized_per_param = Flambda_arity.unarize_per_parameter params_arity in
  assert (List.compare_lengths params unarized_per_param = 0);
  let calling_convention : Function_decl.calling_convention =
    (* CR-someday ncourant: we never unbox parameters or returns of stubs, which
       in particular affects stubs generated by [split_default_wrapper]. *)
    let is_a_param_unboxed =
      List.exists (fun (p : Lambda.lparam) -> p.attributes.unbox_param) params
    in
    if attr.stub || ((not attr.unbox_return) && not is_a_param_unboxed)
    then Normal_calling_convention
    else
      let unboxed_function_slot =
        Function_slot.create
          (Compilation_unit.get_current_exn ())
          ~name:(Ident.name fid ^ "_unboxed")
          Flambda_kind.With_subkind.any_value
      in
      let unboxed_return =
        if attr.unbox_return then unboxing_kind return else None
      in
      let unboxed_param (param : Lambda.lparam) =
        if param.attributes.unbox_param
        then unboxing_kind param.layout
        else None
      in
      let unboxed_params =
        List.concat
          (List.map2
             (fun param kinds ->
               match unboxed_param param, kinds with
               | unboxed, [_] -> [unboxed]
               | None, _ -> List.map (fun _ -> None) kinds
               | Some _, ([] | _ :: _ :: _) ->
                 Misc.fatal_error "Trying to unbox an unboxed product.")
             params unarized_per_param)
      in
      Unboxed_calling_convention
        (unboxed_params, unboxed_return, unboxed_function_slot)
  in
  let body_cont =
    match calling_convention with
    | Normal_calling_convention | Unboxed_calling_convention (_, None, _) ->
      Continuation.create ~sort:Return ()
    | Unboxed_calling_convention (_, Some _, _) ->
      Continuation.create ~sort:Normal_or_exn ~name:"boxed_return" ()
  in
  let body_exn_cont = Continuation.create () in
  let free_idents_of_body =
    match precomputed_free_idents with
    | Some ids -> ids
    | None -> Lambda.free_variables body
  in
  let my_region = Ident.create_local "my_region" in
  let new_env =
    Env.create ~current_unit:(Env.current_unit env)
      ~return_continuation:body_cont ~exn_continuation:body_exn_cont ~my_region
  in
  let exn_continuation : IR.exn_continuation =
    { exn_handler = body_exn_cont; extra_args = [] }
  in
  let function_slot =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name:(Ident.name fid) Flambda_kind.With_subkind.any_value
  in
  let unboxed_products = ref Ident.Map.empty in
  let params =
    List.concat_map
      (fun (({ name; layout; mode; attributes } : L.lparam), kinds) :
           Function_decl.param list ->
        match kinds with
        | [] -> []
        | [kind] -> [{ name; kind; mode; attributes }]
        | _ :: _ ->
          let fields =
            List.mapi
              (fun n kind ->
                let ident =
                  Ident.create_local
                    (Printf.sprintf "%s_unboxed%d" (Ident.unique_name name) n)
                in
                ident, kind)
              kinds
          in
          let before_unarization =
            Flambda_arity.Component_for_creation.from_lambda layout
          in
          unboxed_products
            := Ident.Map.add name (before_unarization, fields) !unboxed_products;
          List.map
            (fun (name, kind) : Function_decl.param ->
              { name; kind; mode; attributes })
            fields)
      (List.combine params unarized_per_param)
  in
  let unboxed_products = !unboxed_products in
  let removed_params = Ident.Map.keys unboxed_products in
  let return =
    Flambda_arity.unarize_t
      (Flambda_arity.create
         [Flambda_arity.Component_for_creation.from_lambda return])
  in
  let body acc ccenv =
    let ccenv = CCenv.set_path_to_root ccenv loc in
    let ccenv = CCenv.set_not_at_toplevel ccenv in
    let new_env =
      Ident.Map.fold
        (fun unboxed_product (before_unarization, fields) new_env ->
          Env.register_unboxed_product new_env ~unboxed_product
            ~before_unarization ~fields)
        unboxed_products new_env
    in
    cps_tail acc new_env ccenv body body_cont body_exn_cont
  in
  Function_decl.create ~let_rec_ident:(Some fid) ~function_slot ~kind ~params
    ~params_arity ~removed_params ~return ~calling_convention
    ~return_continuation:body_cont ~exn_continuation ~my_region ~body ~attr ~loc
    ~free_idents_of_body recursive ~closure_alloc_mode:mode
    ~first_complex_local_param ~contains_no_escaping_local_allocs:region
    ~result_mode:ret_mode

and cps_switch acc env ccenv (switch : L.lambda_switch) ~condition_dbg
    ~scrutinee (k : Continuation.t) (k_exn : Continuation.t) : Expr_with_acc.t =
  let block_nums, sw_blocks = List.split switch.sw_blocks in
  List.iter
    (fun sw_tag ->
      match Tag.Scannable.create sw_tag with
      | Some tag ->
        let tag' = Tag.Scannable.to_tag tag in
        if Tag.is_structured_block_but_not_data_constructor tag'
        then
          Misc.fatal_errorf
            "Bad tag %a in [Lswitch] (tag is that of a scannable block, but \
             not one treated like a variant; [Lswitch] can only be used for \
             variant matching)"
            Tag.print tag'
      | None ->
        Misc.fatal_errorf
          "Bad tag %d in [Lswitch] (not the tag of a GC-scannable block)" sw_tag)
    block_nums;
  if switch.sw_numblocks > Obj.last_non_constant_constructor_tag + 1
  then
    Misc.fatal_errorf
      "Too many blocks (%d) in [Lswitch], would overlap into tag space for \
       blocks that are not treated like variants; [Lswitch] can only be used \
       for variant matching"
      switch.sw_numblocks;
  let convert_arms_rev env cases wrappers =
    List.fold_left
      (fun (consts_rev, wrappers) (arm, (action : L.lambda)) ->
        match action with
        | Lvar var ->
          assert (not (Env.is_mutable env var));
          let extra_args =
            List.map
              (fun arg : IR.simple -> Var arg)
              (Env.extra_args_for_continuation env k)
          in
          let k = restore_continuation_context_for_switch_arm env k in
          let consts_rev =
            (arm, k, Debuginfo.none, None, IR.Var var :: extra_args)
            :: consts_rev
          in
          consts_rev, wrappers
        | Lconst cst ->
          let extra_args =
            List.map
              (fun arg : IR.simple -> Var arg)
              (Env.extra_args_for_continuation env k)
          in
          let k = restore_continuation_context_for_switch_arm env k in
          let consts_rev =
            (arm, k, Debuginfo.none, None, IR.Const cst :: extra_args)
            :: consts_rev
          in
          consts_rev, wrappers
        | Lmutvar _ | Lapply _ | Lfunction _ | Llet _ | Lmutlet _ | Lletrec _
        | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
        | Lstaticcatch _ | Ltrywith _ | Lifthenelse _ | Lsequence _ | Lwhile _
        | Lfor _ | Lassign _ | Lsend _ | Levent _ | Lifused _ | Lregion _
        | Lexclave _ ->
          (* The continuations created here (and for failactions) are local. The
             bodies of the let_conts will not modify mutable variables. Hence,
             it is safe to exclude them from passing along the extra arguments
             for mutable values. *)
          let cont = Continuation.create () in
          let dbg = try_to_find_debuginfo action in
          let action acc ccenv = cps_tail acc env ccenv action k k_exn in
          let consts_rev = (arm, cont, dbg, None, []) :: consts_rev in
          let wrappers = (cont, action) :: wrappers in
          consts_rev, wrappers)
      ([], wrappers) cases
  in
  cps_non_tail_var "scrutinee" acc env ccenv scrutinee
    Flambda_kind.With_subkind.any_value
    (fun acc env ccenv scrutinee _arity ->
      let ccenv = CCenv.set_not_at_toplevel ccenv in
      let consts_rev, wrappers = convert_arms_rev env switch.sw_consts [] in
      let blocks_rev, wrappers =
        convert_arms_rev env (List.combine block_nums sw_blocks) wrappers
      in
      let consts = List.rev consts_rev in
      let blocks = List.rev blocks_rev in
      let failaction, wrappers =
        match switch.sw_failaction with
        | None -> None, wrappers
        | Some action ->
          let cont = Continuation.create () in
          let dbg = try_to_find_debuginfo action in
          let action acc ccenv = cps_tail acc env ccenv action k k_exn in
          let wrappers = (cont, action) :: wrappers in
          Some (cont, dbg, None, []), wrappers
      in
      let const_switch : IR.switch =
        { numconsts = switch.sw_numconsts; consts; failaction }
      in
      let block_switch : IR.switch =
        { numconsts = switch.sw_numblocks; consts = blocks; failaction }
      in
      let build_switch scrutinee wrappers =
        let const_switch acc ccenv =
          CC.close_switch acc ccenv ~condition_dbg scrutinee const_switch
        in
        let scrutinee_tag = Ident.create_local "scrutinee_tag" in
        let block_switch acc ccenv =
          let body acc ccenv =
            CC.close_switch acc ccenv ~condition_dbg scrutinee_tag block_switch
          in
          CC.close_let acc ccenv
            [scrutinee_tag, Flambda_kind.With_subkind.tagged_immediate]
            Not_user_visible (Get_tag scrutinee) ~body
        in
        if switch.sw_numblocks = 0
        then const_switch, wrappers
        else if switch.sw_numconsts = 0
        then block_switch, wrappers
        else
          let const_cont = Continuation.create () in
          let block_cont = Continuation.create () in
          let isint_switch : IR.switch =
            { numconsts = 2;
              consts =
                [ 0, block_cont, condition_dbg, None, [];
                  1, const_cont, condition_dbg, None, [] ];
              failaction = None
            }
          in
          let is_scrutinee_int = Ident.create_local "is_scrutinee_int" in
          let isint_switch acc ccenv =
            let body acc ccenv =
              CC.close_switch acc ccenv ~condition_dbg is_scrutinee_int
                isint_switch
            in
            let region = Env.current_region env in
            CC.close_let acc ccenv
              [is_scrutinee_int, Flambda_kind.With_subkind.tagged_immediate]
              Not_user_visible
              (Prim
                 { prim = Pisint { variant_only = true };
                   args = [[Var scrutinee]];
                   loc = Loc_unknown;
                   exn_continuation = None;
                   region
                 })
              ~body
          in
          ( isint_switch,
            (const_cont, const_switch) :: (block_cont, block_switch) :: wrappers
          )
      in
      let switch, wrappers = build_switch scrutinee wrappers in
      let switch_expr =
        List.fold_left
          (fun body (cont, action) acc ccenv ->
            CC.close_let_cont acc ccenv ~name:cont ~is_exn_handler:false
              ~params:[] ~recursive:Nonrecursive ~body ~handler:action)
          switch wrappers
      in
      switch_expr acc ccenv)
    k_exn

(* CR pchambart: define a record `target_config` to hold things like
   `big_endian` *)
let lambda_to_flambda ~mode ~big_endian ~cmx_loader ~compilation_unit
    ~module_block_size_in_words (lam : Lambda.lambda) =
  let return_continuation = Continuation.create ~sort:Define_root_symbol () in
  let exn_continuation = Continuation.create () in
  let toplevel_my_region = Ident.create_local "toplevel_my_region" in
  let env =
    Env.create ~current_unit:compilation_unit ~return_continuation
      ~exn_continuation ~my_region:toplevel_my_region
  in
  let program acc ccenv =
    cps_tail acc env ccenv lam return_continuation exn_continuation
  in
  CC.close_program ~mode ~big_endian ~cmx_loader ~compilation_unit
    ~module_block_size_in_words ~program ~prog_return_cont:return_continuation
    ~exn_continuation ~toplevel_my_region
