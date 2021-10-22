(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module K = Flambda_kind
module BP = Bound_parameter
module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

let fprintf = Format.fprintf

(* This signature ensures absolutely that the insides of an expression cannot be
   accessed before any necessary delayed renaming has been applied. *)
module With_delayed_renaming : sig
  type 'descr t

  val create : 'descr -> 'descr t

  val apply_renaming : 'descr t -> Renaming.t -> 'descr t

  val descr :
    'descr t -> apply_renaming_descr:('descr -> Renaming.t -> 'descr) -> 'descr
end = struct
  type 'descr t =
    { mutable descr : 'descr;
      mutable delayed_renaming : Renaming.t
    }

  let create descr = { descr; delayed_renaming = Renaming.empty }

  let apply_renaming t renaming =
    let delayed_renaming =
      Renaming.compose ~second:renaming ~first:t.delayed_renaming
    in
    { t with delayed_renaming }

  let[@inline always] descr t ~apply_renaming_descr =
    if Renaming.is_empty t.delayed_renaming
    then t.descr
    else
      let descr = apply_renaming_descr t.descr t.delayed_renaming in
      t.descr <- descr;
      t.delayed_renaming <- Renaming.empty;
      descr
end

type expr = expr_descr With_delayed_renaming.t

and expr_descr =
  | Let of let_expr
  | Let_cont of let_cont_expr
  | Apply of Apply.t
  | Apply_cont of Apply_cont.t
  | Switch of Switch.t
  | Invalid of Invalid_term_semantics.t

and let_expr_t0 =
  { num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t;
    body : expr
  }

and let_expr =
  { let_abst : (Bound_pattern.t, let_expr_t0) Name_abstraction.t;
    defining_expr : named
  }

and named =
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t
  | Static_consts of static_const_group
  | Rec_info of Rec_info_expr.t

and let_cont_expr =
  | Non_recursive of
      { handler : non_recursive_let_cont_handler;
        num_free_occurrences : Num_occurrences.t Or_unknown.t;
        is_applied_with_traps : bool
      }
  | Recursive of recursive_let_cont_handlers

and non_recursive_let_cont_handler =
  { continuation_and_body : (Bound_continuation.t, expr) Name_abstraction.t;
    handler : continuation_handler
  }

and recursive_let_cont_handlers_t0 =
  { handlers : continuation_handlers;
    body : expr
  }

and recursive_let_cont_handlers =
  (Bound_continuations.t, recursive_let_cont_handlers_t0) Name_abstraction.t

and continuation_handler_t0 =
  { num_normal_occurrences_of_params : Num_occurrences.t Variable.Map.t;
    handler : expr
  }

and continuation_handler =
  { cont_handler_abst :
      (Bound_parameters.t, continuation_handler_t0) Name_abstraction.t;
    is_exn_handler : bool
  }

and continuation_handlers = continuation_handler Continuation.Map.t

and function_params_and_body_base =
  { expr : expr;
    free_names : Name_occurrences.t Or_unknown.t
  }

and function_params_and_body =
  { abst :
      (Bound_for_function.t, function_params_and_body_base) Name_abstraction.t;
    dbg : Debuginfo.t;
    params_arity : Flambda_arity.t;
    is_my_closure_used : bool Or_unknown.t
  }

and static_const_or_code =
  | Code of function_params_and_body Code0.t
  | Static_const of Static_const.t

and static_const_group = static_const_or_code list

let rec descr expr =
  With_delayed_renaming.descr expr
    ~apply_renaming_descr:apply_renaming_expr_descr

and apply_renaming = With_delayed_renaming.apply_renaming

and apply_renaming_expr_descr t renaming =
  match t with
  | Let let_expr ->
    let let_expr' = apply_renaming_let_expr let_expr renaming in
    if let_expr == let_expr' then t else Let let_expr'
  | Let_cont let_cont ->
    let let_cont' = apply_renaming_let_cont_expr let_cont renaming in
    if let_cont == let_cont' then t else Let_cont let_cont'
  | Apply apply ->
    let apply' = Apply.apply_renaming apply renaming in
    if apply == apply' then t else Apply apply'
  | Apply_cont apply_cont ->
    let apply_cont' = Apply_cont.apply_renaming apply_cont renaming in
    if apply_cont == apply_cont' then t else Apply_cont apply_cont'
  | Switch switch ->
    let switch' = Switch.apply_renaming switch renaming in
    if switch == switch' then t else Switch switch'
  | Invalid _ -> t

and apply_renaming_named (named : named) renaming : named =
  match named with
  | Simple simple ->
    let simple' = Simple.apply_renaming simple renaming in
    if simple == simple' then named else Simple simple'
  | Prim (prim, dbg) ->
    let prim' = Flambda_primitive.apply_renaming prim renaming in
    if prim == prim' then named else Prim (prim', dbg)
  | Set_of_closures set ->
    let set' = Set_of_closures.apply_renaming set renaming in
    if set == set' then named else Set_of_closures set'
  | Static_consts consts ->
    let consts' = apply_renaming_static_const_group consts renaming in
    if consts == consts' then named else Static_consts consts'
  | Rec_info rec_info_expr ->
    let rec_info_expr' = Rec_info_expr.apply_renaming rec_info_expr renaming in
    if rec_info_expr == rec_info_expr' then named else Rec_info rec_info_expr'

and apply_renaming_let_expr_t0
    ({ body; num_normal_occurrences_of_bound_vars } as t) renaming =
  let body' = apply_renaming body renaming in
  let changed = ref (body != body') in
  let num_normal_occurrences_of_bound_vars =
    Variable.Map.fold
      (fun var num result ->
        let var' = Renaming.apply_variable renaming var in
        changed := !changed || var != var';
        Variable.Map.add var' num result)
      num_normal_occurrences_of_bound_vars Variable.Map.empty
  in
  if not !changed
  then t
  else { body = body'; num_normal_occurrences_of_bound_vars }

and apply_renaming_let_expr ({ let_abst; defining_expr } as t) renaming =
  let module A =
    Name_abstraction.Make_matching_and_renaming
      (Bound_pattern)
      (struct
        type t = let_expr_t0

        let apply_renaming = apply_renaming_let_expr_t0
      end)
  in
  let let_abst' = A.apply_renaming let_abst renaming in
  let defining_expr' = apply_renaming_named defining_expr renaming in
  if let_abst == let_abst' && defining_expr == defining_expr'
  then t
  else { let_abst = let_abst'; defining_expr = defining_expr' }

and apply_renaming_let_cont_expr let_cont renaming =
  match let_cont with
  | Non_recursive { handler; num_free_occurrences; is_applied_with_traps } ->
    let handler' =
      apply_renaming_non_recursive_let_cont_handler handler renaming
    in
    if handler == handler'
    then let_cont
    else
      Non_recursive
        { handler = handler'; num_free_occurrences; is_applied_with_traps }
  | Recursive handlers ->
    let handlers' =
      apply_renaming_recursive_let_cont_handlers handlers renaming
    in
    if handlers == handlers' then let_cont else Recursive handlers'

and apply_renaming_non_recursive_let_cont_handler
    { continuation_and_body; handler } renaming =
  let module A =
    Name_abstraction.Make_matching_and_renaming
      (Bound_continuation)
      (struct
        type t = expr

        let apply_renaming = apply_renaming
      end)
  in
  let continuation_and_body' =
    A.apply_renaming continuation_and_body renaming
  in
  let handler' = apply_renaming_continuation_handler handler renaming in
  { handler = handler'; continuation_and_body = continuation_and_body' }

and apply_renaming_recursive_let_cont_handlers_t0 { handlers; body } renaming =
  let handlers' = apply_renaming_continuation_handlers handlers renaming in
  let body' = apply_renaming body renaming in
  { handlers = handlers'; body = body' }

and apply_renaming_recursive_let_cont_handlers t renaming =
  let module A =
    Name_abstraction.Make_matching_and_renaming
      (Bound_continuations)
      (struct
        type t = recursive_let_cont_handlers_t0

        let apply_renaming = apply_renaming_recursive_let_cont_handlers_t0
      end)
  in
  A.apply_renaming t renaming

and apply_renaming_continuation_handler_t0
    ({ handler; num_normal_occurrences_of_params } as t) renaming =
  let handler' = apply_renaming handler renaming in
  if handler == handler'
  then t
  else { handler = handler'; num_normal_occurrences_of_params }

and apply_renaming_continuation_handler
    ({ cont_handler_abst; is_exn_handler } as t) renaming =
  let module A =
    Name_abstraction.Make_matching_and_renaming
      (Bound_parameters)
      (struct
        type t = continuation_handler_t0

        let apply_renaming = apply_renaming_continuation_handler_t0
      end)
  in
  let cont_handler_abst' = A.apply_renaming cont_handler_abst renaming in
  if cont_handler_abst == cont_handler_abst'
  then t
  else { cont_handler_abst = cont_handler_abst'; is_exn_handler }

and apply_renaming_continuation_handlers t renaming =
  Continuation.Map.fold
    (fun k handler result ->
      let k = Renaming.apply_continuation renaming k in
      let handler = apply_renaming_continuation_handler handler renaming in
      Continuation.Map.add k handler result)
    t Continuation.Map.empty

and apply_renaming_function_params_and_body_base { expr; free_names } renaming =
  let expr = apply_renaming expr renaming in
  let free_names =
    Or_unknown.map free_names ~f:(fun free_names ->
        Name_occurrences.apply_renaming free_names renaming)
  in
  { expr; free_names }

and apply_renaming_function_params_and_body
    ({ abst; dbg; params_arity; is_my_closure_used } as t) renaming =
  let module A =
    Name_abstraction.Make_matching_and_renaming
      (Bound_for_function)
      (struct
        type t = function_params_and_body_base

        let apply_renaming = apply_renaming_function_params_and_body_base
      end)
  in
  let abst' = A.apply_renaming abst renaming in
  if abst == abst'
  then t
  else { abst = abst'; dbg; params_arity; is_my_closure_used }

and apply_renaming_static_const_or_code
    (static_const_or_code : static_const_or_code) renaming :
    static_const_or_code =
  if Renaming.is_empty renaming
  then static_const_or_code
  else
    match static_const_or_code with
    | Code code ->
      let code' =
        Code0.apply_renaming ~apply_renaming_function_params_and_body code
          renaming
      in
      if code == code' then static_const_or_code else Code code'
    | Static_const const ->
      let const' = Static_const.apply_renaming const renaming in
      if const == const' then static_const_or_code else Static_const const'

and apply_renaming_static_const_group t renaming =
  List.map
    (fun static_const ->
      apply_renaming_static_const_or_code static_const renaming)
    t

let rec all_ids_for_export_continuation_handler_t0
    { handler; num_normal_occurrences_of_params = _ } =
  all_ids_for_export handler

and all_ids_for_export_continuation_handler
    { cont_handler_abst; is_exn_handler = _ } =
  let module A =
    Name_abstraction.Make_ids_for_export
      (Bound_parameters)
      (struct
        type t = continuation_handler_t0

        let all_ids_for_export = all_ids_for_export_continuation_handler_t0
      end)
  in
  A.all_ids_for_export cont_handler_abst

and all_ids_for_export_continuation_handlers t =
  Continuation.Map.fold
    (fun k handler ids ->
      Ids_for_export.union ids
        (Ids_for_export.add_continuation
           (all_ids_for_export_continuation_handler handler)
           k))
    t Ids_for_export.empty

and all_ids_for_export t =
  match descr t with
  | Let let_expr -> all_ids_for_export_let_expr let_expr
  | Let_cont let_cont -> all_ids_for_export_let_cont_expr let_cont
  | Apply apply -> Apply.all_ids_for_export apply
  | Apply_cont apply_cont -> Apply_cont.all_ids_for_export apply_cont
  | Switch switch -> Switch.all_ids_for_export switch
  | Invalid _ -> Ids_for_export.empty

and all_ids_for_export_let_expr_t0
    { body; num_normal_occurrences_of_bound_vars = _ } =
  all_ids_for_export body

and all_ids_for_export_let_expr { let_abst; defining_expr } =
  let module A =
    Name_abstraction.Make_ids_for_export
      (Bound_pattern)
      (struct
        type t = let_expr_t0

        let all_ids_for_export = all_ids_for_export_let_expr_t0
      end)
  in
  let defining_expr_ids = all_ids_for_export_named defining_expr in
  let let_abst_ids = A.all_ids_for_export let_abst in
  Ids_for_export.union defining_expr_ids let_abst_ids

and all_ids_for_export_named t =
  match t with
  | Simple simple -> Ids_for_export.from_simple simple
  | Prim (prim, _dbg) -> Flambda_primitive.all_ids_for_export prim
  | Set_of_closures set -> Set_of_closures.all_ids_for_export set
  | Static_consts consts -> all_ids_for_export_static_const_group consts
  | Rec_info rec_info_expr -> Rec_info_expr.all_ids_for_export rec_info_expr

and all_ids_for_export_let_cont_expr t =
  match t with
  | Non_recursive
      { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
    all_ids_for_export_non_recursive_let_cont_handler handler
  | Recursive handlers ->
    all_ids_for_export_recursive_let_cont_handlers handlers

and all_ids_for_export_non_recursive_let_cont_handler
    { continuation_and_body; handler } =
  let module A =
    Name_abstraction.Make_ids_for_export
      (Bound_continuation)
      (struct
        type t = expr

        let all_ids_for_export = all_ids_for_export
      end)
  in
  let handler_ids = all_ids_for_export_continuation_handler handler in
  let continuation_and_body_ids = A.all_ids_for_export continuation_and_body in
  Ids_for_export.union handler_ids continuation_and_body_ids

and all_ids_for_export_recursive_let_cont_handlers_t0 { handlers; body } =
  let body_ids = all_ids_for_export body in
  let handlers_ids = all_ids_for_export_continuation_handlers handlers in
  Ids_for_export.union body_ids handlers_ids

and all_ids_for_export_recursive_let_cont_handlers t =
  let module A =
    Name_abstraction.Make_ids_for_export
      (Bound_continuations)
      (struct
        type t = recursive_let_cont_handlers_t0

        let all_ids_for_export =
          all_ids_for_export_recursive_let_cont_handlers_t0
      end)
  in
  A.all_ids_for_export t

and all_ids_for_export_function_params_and_body_base { expr; free_names = _ } =
  all_ids_for_export expr

and all_ids_for_export_function_params_and_body
    { abst; params_arity = _; dbg = _; is_my_closure_used = _ } =
  let module A =
    Name_abstraction.Make_ids_for_export
      (Bound_for_function)
      (struct
        type t = function_params_and_body_base

        let all_ids_for_export =
          all_ids_for_export_function_params_and_body_base
      end)
  in
  A.all_ids_for_export abst

and all_ids_for_export_static_const_or_code t =
  match t with
  | Code code ->
    Code0.all_ids_for_export ~all_ids_for_export_function_params_and_body code
  | Static_const const -> Static_const.all_ids_for_export const

and all_ids_for_export_static_const_group t =
  List.map all_ids_for_export_static_const_or_code t
  |> Ids_for_export.union_list

type flattened_for_printing_descr =
  | Flat_code of Code_id.t * function_params_and_body Code0.t
  | Flat_set_of_closures of Symbol.t Closure_id.Lmap.t * Set_of_closures.t
  | Flat_block_like of Symbol.t * Static_const.t

type flattened_for_printing =
  { second_or_later_binding_within_one_set : bool;
    second_or_later_rec_binding : bool;
    descr : flattened_for_printing_descr
  }

let _shape_colour descr =
  match descr with
  | Flat_code _ -> Flambda_colours.code_id ()
  | Flat_set_of_closures _ | Flat_block_like _ -> Flambda_colours.symbol ()

let rec named_must_be_static_consts (named : named) =
  match named with
  | Static_consts consts -> consts
  | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ ->
    Misc.fatal_errorf "Must be [Static_consts], but is not: %a" print_named
      named

and match_against_bound_symbols_pattern_static_const_or_code :
      'a.
      static_const_or_code ->
      Bound_symbols.Pattern.t ->
      code:(Code_id.t -> function_params_and_body Code0.t -> 'a) ->
      set_of_closures:
        (closure_symbols:Symbol.t Closure_id.Lmap.t -> Set_of_closures.t -> 'a) ->
      block_like:(Symbol.t -> Static_const.t -> 'a) ->
      'a =
 fun static_const_or_code (pat : Bound_symbols.Pattern.t) ~code:code_callback
     ~set_of_closures ~block_like ->
  match static_const_or_code, pat with
  | Code code, Code code_id ->
    if not (Code_id.equal (Code0.code_id code) code_id)
    then
      Misc.fatal_errorf "Mismatch on declared code IDs:@ %a@ =@ %a"
        Bound_symbols.Pattern.print pat print_static_const_or_code
        static_const_or_code;
    code_callback code_id code
  | Static_const const, (Set_of_closures _ | Block_like _) ->
    Static_const.match_against_bound_symbols_pattern const pat ~set_of_closures
      ~block_like
  | Static_const _, Code _ | Code _, (Set_of_closures _ | Block_like _) ->
    Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
      Bound_symbols.Pattern.print pat print_static_const_or_code
      static_const_or_code

and match_against_bound_symbols_static_const_group :
      'a.
      static_const_group ->
      Bound_symbols.t ->
      init:'a ->
      code:('a -> Code_id.t -> function_params_and_body Code0.t -> 'a) ->
      set_of_closures:
        ('a ->
        closure_symbols:Symbol.t Closure_id.Lmap.t ->
        Set_of_closures.t ->
        'a) ->
      block_like:('a -> Symbol.t -> Static_const.t -> 'a) ->
      'a =
 fun t bound_symbols ~init ~code:code_callback
     ~set_of_closures:set_of_closures_callback ~block_like:block_like_callback ->
  let bound_symbol_pats = Bound_symbols.to_list bound_symbols in
  if List.compare_lengths t bound_symbol_pats <> 0
  then
    Misc.fatal_errorf
      "Mismatch between length of [Bound_symbols.t] and [Static_const.t \
       list]:@ %a@ =@ %a"
      Bound_symbols.print bound_symbols print_static_const_group t;
  ListLabels.fold_left2 t bound_symbol_pats ~init
    ~f:(fun acc static_const bound_symbols_pat ->
      match_against_bound_symbols_pattern_static_const_or_code static_const
        bound_symbols_pat
        ~code:(fun code_id code -> code_callback acc code_id code)
        ~set_of_closures:(fun ~closure_symbols set_of_closures ->
          set_of_closures_callback acc ~closure_symbols set_of_closures)
        ~block_like:(fun symbol static_const ->
          block_like_callback acc symbol static_const))

and print ppf (t : expr) =
  match descr t with
  | Let let_expr -> print_let_expr ppf let_expr
  | Let_cont let_cont -> print_let_cont_expr ppf let_cont
  | Apply apply ->
    Format.fprintf ppf "@[<hov 1>(@<0>%sapply@<0>%s@ %a)@]"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      Apply.print apply
  | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
  | Switch switch -> Switch.print ppf switch
  | Invalid semantics ->
    fprintf ppf "@[@<0>%sInvalid %a@<0>%s@]"
      (Flambda_colours.expr_keyword ())
      Invalid_term_semantics.print semantics
      (Flambda_colours.normal ())

and print_continuation_handler (recursive : Recursive.t) ppf k
    ({ cont_handler_abst = _; is_exn_handler } as t) occurrences ~first =
  let fprintf = Format.fprintf in
  if not first then fprintf ppf "@ ";
  let print params ~handler =
    begin
      match descr handler with
      | Apply_cont _ | Invalid _ -> fprintf ppf "@[<hov 0>"
      | Let _ | Let_cont _ | Apply _ | Switch _ -> fprintf ppf "@[<v 0>"
    end;
    fprintf ppf "@[<hov 1>@<0>%s%a@<0>%s%s@<0>%s%s@<0>%s"
      (Flambda_colours.continuation_definition ())
      Continuation.print k
      (Flambda_colours.expr_keyword ())
      (match recursive with Non_recursive -> "" | Recursive -> " (rec)")
      (Flambda_colours.continuation_annotation ())
      (if is_exn_handler then "[eh]" else "")
      (Flambda_colours.normal ());
    if List.length params > 0
    then fprintf ppf " %a" Bound_parameter.List.print params;
    fprintf ppf "@<0>%s #%a:@<0>%s@]@ @[<hov 0>%a@]" (Flambda_colours.elide ())
      (Or_unknown.print Num_occurrences.print)
      occurrences
      (Flambda_colours.normal ())
      print handler;
    fprintf ppf "@]"
  in
  if Flambda_features.freshen_when_printing ()
  then
    let open
      Name_abstraction.Make_matching_and_renaming
        (Bound_parameters)
        (struct
          type t = continuation_handler_t0

          let apply_renaming = apply_renaming_continuation_handler_t0
        end) in
    let<> params, { handler; _ } = t.cont_handler_abst in
    print (Bound_parameters.to_list params) ~handler
  else
    let params, { handler; num_normal_occurrences_of_params = _ } =
      Name_abstraction.peek_for_printing t.cont_handler_abst
    in
    print (Bound_parameters.to_list params) ~handler

and print_function_params_and_body ppf t =
  let print ~return_continuation ~exn_continuation params ~body ~my_closure
      ~is_my_closure_used:_ ~my_depth ~free_names_of_body:_ =
    let my_closure =
      Bound_parameter.create my_closure (K.With_subkind.create K.value Anything)
    in
    fprintf ppf
      "@[<hov 1>(@<0>%s@<1>\u{03bb}@<0>%s@[<hov \
       1>@<1>\u{3008}%a@<1>\u{3009}@<1>\u{300a}%a@<1>\u{300b}%a %a @<0>%s%a \
       @<0>%s.@<0>%s@]@ %a))@]"
      (Flambda_colours.lambda ())
      (Flambda_colours.normal ())
      Continuation.print return_continuation Continuation.print exn_continuation
      Bound_parameter.List.print params Bound_parameter.print my_closure
      (Flambda_colours.depth_variable ())
      Variable.print my_depth (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      print body
  in
  if Flambda_features.freshen_when_printing ()
  then
    let module BFF = Bound_for_function in
    let open
      Name_abstraction.Make_matching_and_renaming
        (BFF)
        (struct
          type t = function_params_and_body_base

          let apply_renaming = apply_renaming_function_params_and_body_base
        end) in
    let<> bff, { expr; free_names } = t.abst in
    print
      ~return_continuation:(BFF.return_continuation bff)
      ~exn_continuation:(BFF.exn_continuation bff) (BFF.params bff) ~body:expr
      ~my_closure:(BFF.my_closure bff) ~is_my_closure_used:t.is_my_closure_used
      ~my_depth:(BFF.my_depth bff) ~free_names_of_body:free_names
  else
    let bff, { expr = body; _ } = Name_abstraction.peek_for_printing t.abst in
    let module BFF = Bound_for_function in
    print
      ~return_continuation:(BFF.return_continuation bff)
      ~exn_continuation:(BFF.exn_continuation bff) (BFF.params bff) ~body
      ~my_closure:(BFF.my_closure bff) ~is_my_closure_used:false
      ~my_depth:(BFF.my_depth bff) ~free_names_of_body:Name_occurrences.empty

and print_let_cont_expr ppf t =
  let rec gather_let_conts let_conts let_cont =
    match let_cont with
    | Non_recursive { handler; num_free_occurrences; is_applied_with_traps = _ }
      ->
      let print k ~body =
        let let_conts, body =
          match descr body with
          | Let_cont let_cont -> gather_let_conts let_conts let_cont
          | Let _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
            let_conts, body
        in
        ( (k, Recursive.Non_recursive, handler.handler, num_free_occurrences)
          :: let_conts,
          body )
      in
      if Flambda_features.freshen_when_printing ()
      then
        let open
          Name_abstraction.Make_matching_and_renaming
            (Bound_continuation)
            (struct
              type t = expr

              let apply_renaming = apply_renaming
            end) in
        let<> k, body = handler.continuation_and_body in
        print k ~body
      else
        let k, body =
          Name_abstraction.peek_for_printing handler.continuation_and_body
        in
        print k ~body
    | Recursive handlers ->
      let print ~body handlers =
        let let_conts, body =
          match descr body with
          | Let_cont let_cont -> gather_let_conts let_conts let_cont
          | Let _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
            let_conts, body
        in
        let new_let_conts =
          List.map
            (fun (k, handler) ->
              k, Recursive.Recursive, handler, Or_unknown.Unknown)
            (Continuation.Map.bindings handlers)
        in
        new_let_conts @ let_conts, body
      in
      if Flambda_features.freshen_when_printing ()
      then
        let open
          Name_abstraction.Make_matching_and_renaming
            (Bound_continuations)
            (struct
              type t = recursive_let_cont_handlers_t0

              let apply_renaming = apply_renaming_recursive_let_cont_handlers_t0
            end) in
        let<> _, { body; handlers } = handlers in
        print ~body handlers
      else
        let _, { body; handlers } =
          Name_abstraction.peek_for_printing handlers
        in
        print ~body handlers
  in
  let let_conts, body = gather_let_conts [] t in
  fprintf ppf "@[<v 1>(%a@;" print body;
  let first = ref true in
  List.iter
    (fun (cont, recursive, handler, occurrences) ->
      print_continuation_handler recursive ppf cont handler occurrences
        ~first:!first;
      first := false)
    (List.rev let_conts);
  fprintf ppf ")@]"

(* CR mshinwell: Remove [second_or_later_binding_within_one_set] if it doesn't
   become used soon. *)
and flatten_for_printing0 bound_symbols defining_exprs =
  match_against_bound_symbols_static_const_group defining_exprs bound_symbols
    ~init:([], false)
    ~code:(fun (flattened_acc, second_or_later_rec_binding) code_id code ->
      let flattened =
        { second_or_later_binding_within_one_set = false;
          second_or_later_rec_binding;
          descr = Flat_code (code_id, code)
        }
      in
      flattened_acc @ [flattened], true)
    ~set_of_closures:
      (fun (flattened_acc, second_or_later_rec_binding) ~closure_symbols
           set_of_closures ->
      let flattened =
        if Set_of_closures.is_empty set_of_closures
        then []
        else
          let second_or_later_binding_within_one_set = false in
          [ { second_or_later_binding_within_one_set;
              second_or_later_rec_binding;
              descr = Flat_set_of_closures (closure_symbols, set_of_closures)
            } ]
      in
      flattened_acc @ flattened, true)
    ~block_like:
      (fun (flattened_acc, second_or_later_rec_binding) symbol defining_expr ->
      let flattened =
        { second_or_later_binding_within_one_set = false;
          second_or_later_rec_binding;
          descr = Flat_block_like (symbol, defining_expr)
        }
      in
      flattened_acc @ [flattened], true)

and flatten_for_printing t =
  let print (bound_pattern : Bound_pattern.t) ~body =
    match bound_pattern with
    | Symbols { bound_symbols } ->
      let flattened, _ =
        flatten_for_printing0 bound_symbols
          (named_must_be_static_consts t.defining_expr)
      in
      Some (flattened, body)
    | Singleton _ | Set_of_closures _ -> None
  in
  if Flambda_features.freshen_when_printing ()
  then
    let open
      Name_abstraction.Make_matching_and_renaming
        (Bound_pattern)
        (struct
          type t = let_expr_t0

          let apply_renaming = apply_renaming_let_expr_t0
        end) in
    let<> bound_pattern, { body; _ } = t.let_abst in
    print bound_pattern ~body
  else
    let bound_pattern, { body; _ } =
      Name_abstraction.peek_for_printing t.let_abst
    in
    print bound_pattern ~body

and print_closure_binding ppf (closure_id, sym) =
  Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s@ %a@]" Symbol.print sym
    (Flambda_colours.elide ()) (Flambda_colours.elide ()) Closure_id.print
    closure_id

and print_flattened_descr_lhs ppf descr =
  match descr with
  | Flat_code (code_id, _) -> Code_id.print ppf code_id
  | Flat_set_of_closures (closure_symbols, _) ->
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () ->
           Format.fprintf ppf "@<0>%s,@ @<0>%s" (Flambda_colours.elide ())
             (Flambda_colours.normal ()))
         print_closure_binding)
      (Closure_id.Lmap.bindings closure_symbols)
  | Flat_block_like (symbol, _) -> Symbol.print ppf symbol

and print_flattened_descr_rhs ppf descr =
  match descr with
  | Flat_code (_, code) -> Code0.print ~print_function_params_and_body ppf code
  | Flat_set_of_closures (_, set) -> Set_of_closures.print ppf set
  | Flat_block_like (_, static_const) -> Static_const.print ppf static_const

and print_flattened ppf
    { second_or_later_binding_within_one_set = _;
      second_or_later_rec_binding;
      descr
    } =
  fprintf ppf "@[<hov 1>";
  if second_or_later_rec_binding
  then
    fprintf ppf "@<0>%sand @<0>%s"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ());
  (* else let shape = "\u{25b7}" (* unfilled triangle *) in fprintf ppf
     "@<0>%s@<1>%s @<0>%s" (shape_colour descr) shape (Flambda_colours.normal
     ())); *)
  fprintf ppf "%a@<0>%s =@<0>%s@ %a@]" print_flattened_descr_lhs descr
    (Flambda_colours.elide ())
    (Flambda_colours.normal ())
    print_flattened_descr_rhs descr

and flatten_let_symbol t : _ * expr =
  let rec flatten (expr : expr) : _ * expr =
    match descr expr with
    | Let t -> begin
      match flatten_for_printing t with
      | Some (flattened, body) ->
        let flattened', body = flatten body in
        flattened @ flattened', body
      | None -> [], expr
    end
    | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> [], expr
  in
  match flatten_for_printing t with
  | Some (flattened, body) ->
    let flattened', body = flatten body in
    flattened @ flattened', body
  | None -> assert false
(* see below *)

(* CR mshinwell: Merge the "let symbol" and "normal let" cases to use the same
   flattened type? *)
and print_let_symbol ppf t =
  let rec print_more flattened =
    match flattened with
    | [] -> ()
    | flat :: flattened ->
      fprintf ppf "@ ";
      print_flattened ppf flat;
      print_more flattened
  in
  let flattened, body = flatten_let_symbol t in
  match flattened with
  | [] -> assert false
  | flat :: flattened ->
    fprintf ppf "@[<v 0>@[<v 0>%a" print_flattened flat;
    print_more flattened;
    fprintf ppf "@]@ %a@]" print body

(* For printing all kinds of let-expressions: *)
and print_let_expr ppf ({ let_abst = _; defining_expr } as t) : unit =
  let let_bound_var_colour bound_pattern defining_expr =
    let name_mode = Bound_pattern.name_mode bound_pattern in
    if Name_mode.is_phantom name_mode
    then Flambda_colours.elide ()
    else
      match (defining_expr : named) with
      | Rec_info _ -> Flambda_colours.depth_variable ()
      | Simple _ | Prim _ | Set_of_closures _ | Static_consts _ ->
        Flambda_colours.variable ()
  in

  let open
    Name_abstraction.Make_matching_and_renaming
      (Bound_pattern)
      (struct
        type t = let_expr_t0

        let apply_renaming = apply_renaming_let_expr_t0
      end) in
  let rec let_body (expr : expr) =
    match descr expr with
    | Let ({ let_abst = _; defining_expr } as t) ->
      let print (bound_pattern : Bound_pattern.t) ~body =
        match bound_pattern with
        | Singleton _ | Set_of_closures _ ->
          fprintf ppf "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
            (let_bound_var_colour bound_pattern defining_expr)
            Bound_pattern.print bound_pattern (Flambda_colours.elide ())
            (Flambda_colours.normal ())
            print_named defining_expr;
          let_body body
        | Symbols _ -> expr
      in
      if Flambda_features.freshen_when_printing ()
      then
        let<> bound_pattern, { body; _ } = t.let_abst in
        print bound_pattern ~body
      else
        let bound_pattern, { body; _ } =
          Name_abstraction.peek_for_printing t.let_abst
        in
        print bound_pattern ~body
    | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> expr
  in
  let print (bound_pattern : Bound_pattern.t) ~body =
    match bound_pattern with
    | Symbols _ -> print_let_symbol ppf t
    | Singleton _ | Set_of_closures _ ->
      fprintf ppf "@[<v 0>@[<v 0>@[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
        (let_bound_var_colour bound_pattern defining_expr)
        Bound_pattern.print bound_pattern (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        print_named defining_expr;
      let expr = let_body body in
      fprintf ppf "@]@ %a@]" print expr
  in
  if Flambda_features.freshen_when_printing ()
  then
    let<> bound_pattern, { body; _ } = t.let_abst in
    print bound_pattern ~body
  else
    let bound_pattern, { body; _ } =
      Name_abstraction.peek_for_printing t.let_abst
    in
    print bound_pattern ~body

and print_named ppf (t : named) =
  let print_or_elide_debuginfo ppf dbg =
    if Debuginfo.is_none dbg
    then Format.pp_print_string ppf ""
    else Format.fprintf ppf "@ %a" Debuginfo.print_compact dbg
  in
  match t with
  | Simple simple -> Simple.print ppf simple
  | Prim (prim, dbg) ->
    fprintf ppf "@[<hov 1>(%a@<0>%s%a@<0>%s)@]" Flambda_primitive.print prim
      (Flambda_colours.debuginfo ())
      print_or_elide_debuginfo dbg
      (Flambda_colours.normal ())
  | Set_of_closures set_of_closures -> Set_of_closures.print ppf set_of_closures
  | Static_consts consts -> print_static_const_group ppf consts
  | Rec_info rec_info_expr -> Rec_info_expr.print ppf rec_info_expr

and print_static_const_group ppf static_const_group =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       print_static_const_or_code)
    static_const_group

and print_static_const_or_code ppf static_const_or_code =
  match static_const_or_code with
  | Code code ->
    fprintf ppf "@[<hov 1>(@<0>%sCode@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Code0.print ~print_function_params_and_body)
      code
  | Static_const const -> Static_const.print ppf const

module Continuation_handler = struct
  module T0 = struct
    type t = continuation_handler_t0

    let all_ids_for_export = all_ids_for_export_continuation_handler_t0

    let apply_renaming = apply_renaming_continuation_handler_t0
  end

  type t = continuation_handler

  module A = Name_abstraction.Make (Bound_parameters) (T0)

  let create params ~handler ~(free_names_of_handler : _ Or_unknown.t)
      ~is_exn_handler =
    let num_normal_occurrences_of_params =
      match free_names_of_handler with
      | Unknown -> Variable.Map.empty
      | Known free_names_of_handler ->
        ListLabels.fold_left params ~init:Variable.Map.empty
          ~f:(fun num_occurrences param ->
            let var = Bound_parameter.var param in
            let num =
              Name_occurrences.count_variable_normal_mode free_names_of_handler
                var
            in
            Variable.Map.add var num num_occurrences)
    in
    let t0 : T0.t = { num_normal_occurrences_of_params; handler } in
    let cont_handler_abst = A.create (Bound_parameters.create params) t0 in
    { cont_handler_abst; is_exn_handler }

  let pattern_match t ~f =
    let open A in
    let<> params, { handler; _ } = t.cont_handler_abst in
    f (Bound_parameters.to_list params) ~handler

  let pattern_match' t ~f =
    A.pattern_match t.cont_handler_abst
      ~f:(fun params { handler; num_normal_occurrences_of_params } ->
        f
          (Bound_parameters.to_list params)
          ~num_normal_occurrences_of_params ~handler)

  module Pattern_match_pair_error = struct
    type t = Parameter_lists_have_different_lengths

    let to_string = function
      | Parameter_lists_have_different_lengths ->
        "Parameter lists have different lengths"
  end

  let pattern_match_pair t1 t2 ~f =
    pattern_match t1 ~f:(fun params1 ~handler:_ ->
        pattern_match t2 ~f:(fun params2 ~handler:_ ->
            if List.compare_lengths params1 params2 = 0
            then
              A.pattern_match_pair t1.cont_handler_abst t2.cont_handler_abst
                ~f:(fun
                     params
                     ({ handler = handler1; _ } : T0.t)
                     ({ handler = handler2; _ } : T0.t)
                   ->
                  Ok (f (Bound_parameters.to_list params) ~handler1 ~handler2))
            else
              Error
                Pattern_match_pair_error.Parameter_lists_have_different_lengths))

  let is_exn_handler t = t.is_exn_handler

  let apply_renaming = apply_renaming_continuation_handler
end

module Continuation_handlers = struct
  type t = continuation_handlers

  let to_map t = t

  let domain t = Continuation.Map.keys t

  let contains_exn_handler t =
    Continuation.Map.exists
      (fun _cont handler -> Continuation_handler.is_exn_handler handler)
      t
end

module Function_params_and_body = struct
  module Base = struct
    type t = function_params_and_body_base

    let apply_renaming = apply_renaming_function_params_and_body_base

    let all_ids_for_export = all_ids_for_export_function_params_and_body_base
  end

  module A = Name_abstraction.Make (Bound_for_function) (Base)

  type t = function_params_and_body

  let create ~return_continuation ~exn_continuation params ~dbg ~body
      ~free_names_of_body ~my_closure ~my_depth =
    let is_my_closure_used =
      Or_unknown.map free_names_of_body ~f:(fun free_names_of_body ->
          Name_occurrences.mem_var free_names_of_body my_closure)
    in
    let base : Base.t = { expr = body; free_names = free_names_of_body } in
    let bound_for_function =
      Bound_for_function.create ~return_continuation ~exn_continuation ~params
        ~my_closure ~my_depth
    in
    let abst = A.create bound_for_function base in
    { abst;
      dbg;
      params_arity = Bound_parameter.List.arity params;
      is_my_closure_used
    }

  let print = print_function_params_and_body

  let pattern_match t ~f =
    let module BFF = Bound_for_function in
    let open A in
    let<> bff, { expr; free_names } = t.abst in
    f
      ~return_continuation:(BFF.return_continuation bff)
      ~exn_continuation:(BFF.exn_continuation bff) (BFF.params bff) ~body:expr
      ~my_closure:(BFF.my_closure bff) ~is_my_closure_used:t.is_my_closure_used
      ~my_depth:(BFF.my_depth bff) ~free_names_of_body:free_names

  let pattern_match_pair t1 t2 ~f =
    A.pattern_match_pair t1.abst t2.abst
      ~f:(fun
           bound_for_function
           { expr = body1; free_names = _ }
           { expr = body2; free_names = _ }
         ->
        f
          ~return_continuation:
            (Bound_for_function.return_continuation bound_for_function)
          ~exn_continuation:
            (Bound_for_function.exn_continuation bound_for_function)
          (Bound_for_function.params bound_for_function)
          ~body1 ~body2
          ~my_closure:(Bound_for_function.my_closure bound_for_function)
          ~my_depth:(Bound_for_function.my_depth bound_for_function))

  let params_arity t = t.params_arity

  let apply_renaming = apply_renaming_function_params_and_body

  let debuginfo { dbg; _ } = dbg

  let all_ids_for_export = all_ids_for_export_function_params_and_body
end

module Let_expr = struct
  module T0 = struct
    type t = let_expr_t0

    let apply_renaming = apply_renaming_let_expr_t0

    let all_ids_for_export = all_ids_for_export_let_expr_t0
  end

  module A = Name_abstraction.Make (Bound_pattern) (T0)

  type t = let_expr

  let pattern_match t ~f =
    let open A in
    let<> bound_pattern, { body; _ } = t.let_abst in
    f bound_pattern ~body

  let pattern_match' t ~f =
    A.pattern_match t.let_abst ~f:(fun bound_pattern t0 ->
        let num_normal_occurrences_of_bound_vars =
          t0.num_normal_occurrences_of_bound_vars
        in
        f bound_pattern ~num_normal_occurrences_of_bound_vars ~body:t0.body)

  module Pattern_match_pair_error = struct
    type t = Mismatched_let_bindings

    let to_string = function
      | Mismatched_let_bindings -> "Mismatched let bindings"
  end

  let pattern_match_pair t1 t2 ~dynamic ~static =
    A.pattern_match t1.let_abst ~f:(fun bound_pattern1 t0_1 ->
        let body1 = t0_1.body in
        A.pattern_match t2.let_abst ~f:(fun bound_pattern2 t0_2 ->
            let body2 = t0_2.body in
            let dynamic_case () =
              let ans =
                A.pattern_match_pair t1.let_abst t2.let_abst
                  ~f:(fun bound_pattern t0_1 t0_2 ->
                    dynamic bound_pattern ~body1:t0_1.body ~body2:t0_2.body)
              in
              Ok ans
            in
            match bound_pattern1, bound_pattern2 with
            | Bound_pattern.Singleton _, Bound_pattern.Singleton _ ->
              dynamic_case ()
            | ( Set_of_closures { closure_vars = vars1; _ },
                Set_of_closures { closure_vars = vars2; _ } ) ->
              if List.compare_lengths vars1 vars2 = 0
              then dynamic_case ()
              else Error Pattern_match_pair_error.Mismatched_let_bindings
            | Symbols bound_symbols1, Symbols bound_symbols2 ->
              let patterns1 =
                bound_symbols1.bound_symbols |> Bound_symbols.to_list
              in
              let patterns2 =
                bound_symbols2.bound_symbols |> Bound_symbols.to_list
              in
              if List.compare_lengths patterns1 patterns2 = 0
              then
                let ans =
                  static ~bound_symbols1 ~bound_symbols2 ~body1 ~body2
                in
                Ok ans
              else Error Pattern_match_pair_error.Mismatched_let_bindings
            | (Singleton _ | Set_of_closures _ | Symbols _), _ ->
              Error Pattern_match_pair_error.Mismatched_let_bindings))

  let print = print_let_expr

  let create (bound_pattern : Bound_pattern.t) (defining_expr : named) ~body
      ~(free_names_of_body : _ Or_unknown.t) =
    begin
      match defining_expr, bound_pattern with
      | Prim _, Singleton _
      | Simple _, Singleton _
      | Rec_info _, Singleton _
      | Set_of_closures _, Set_of_closures _ ->
        ()
      | Set_of_closures _, Singleton _ ->
        Misc.fatal_errorf
          "Cannot bind a [Set_of_closures] to a [Singleton]:@ %a =@ %a"
          Bound_pattern.print bound_pattern print_named defining_expr
      | _, Set_of_closures _ ->
        Misc.fatal_errorf
          "Cannot bind a non-[Set_of_closures] to a [Set_of_closures]:@ %a =@ \
           %a"
          Bound_pattern.print bound_pattern print_named defining_expr
      | Static_consts _, Symbols _ -> ()
      | Static_consts _, Singleton _ ->
        Misc.fatal_errorf
          "Cannot bind a [Static_const] to a [Singleton]:@ %a =@ %a"
          Bound_pattern.print bound_pattern print_named defining_expr
      | (Simple _ | Prim _ | Set_of_closures _ | Rec_info _), Symbols _ ->
        Misc.fatal_errorf
          "Cannot bind a non-[Static_const] to [Symbols]:@ %a =@ %a"
          Bound_pattern.print bound_pattern print_named defining_expr
    end;
    let num_normal_occurrences_of_bound_vars =
      match free_names_of_body with
      | Unknown -> Variable.Map.empty
      | Known free_names_of_body ->
        let free_names_of_bindable = Bound_pattern.free_names bound_pattern in
        Name_occurrences.fold_variables free_names_of_bindable
          ~init:Variable.Map.empty ~f:(fun num_occurrences var ->
            let num =
              Name_occurrences.count_variable_normal_mode free_names_of_body var
            in
            Variable.Map.add var num num_occurrences)
    in
    let t0 : T0.t = { num_normal_occurrences_of_bound_vars; body } in
    { let_abst = A.create bound_pattern t0; defining_expr }

  let defining_expr t = t.defining_expr

  let apply_renaming = apply_renaming_let_expr
end

module Non_recursive_let_cont_handler = struct
  module Continuation_and_body =
    Name_abstraction.Make
      (Bound_continuation)
      (struct
        type t = expr

        let apply_renaming = apply_renaming

        let all_ids_for_export = all_ids_for_export
      end)

  type t = non_recursive_let_cont_handler

  let create continuation ~body handler =
    let continuation_and_body =
      Continuation_and_body.create continuation body
    in
    { continuation_and_body; handler }

  let pattern_match t ~f =
    let open
      Name_abstraction.Make_matching_and_renaming
        (Bound_continuation)
        (struct
          type t = expr

          let apply_renaming = apply_renaming
        end) in
    let<> continuation, body = t.continuation_and_body in
    f continuation ~body

  let pattern_match_pair t1 t2 ~f =
    Continuation_and_body.pattern_match_pair t1.continuation_and_body
      t2.continuation_and_body ~f:(fun continuation body1 body2 ->
        f continuation ~body1 ~body2)

  let handler t = t.handler

  let apply_renaming = apply_renaming_non_recursive_let_cont_handler
end

module Recursive_let_cont_handlers = struct
  module T0 = struct
    type t = recursive_let_cont_handlers_t0

    let create ~body handlers = { handlers; body }

    let apply_renaming = apply_renaming_recursive_let_cont_handlers_t0

    let all_ids_for_export = all_ids_for_export_recursive_let_cont_handlers_t0
  end

  module A = Name_abstraction.Make (Bound_continuations) (T0)

  type t = recursive_let_cont_handlers

  let create ~body handlers =
    let bound = Continuation_handlers.domain handlers in
    let handlers0 = T0.create ~body handlers in
    A.create
      (Bound_continuations.create (Continuation.Set.elements bound))
      handlers0

  let pattern_match t ~f =
    let open A in
    let<> _, { body; handlers } = t in
    f ~body handlers

  let pattern_match_pair t1 t2 ~f =
    A.pattern_match_pair t1 t2
      ~f:(fun
           _bound
           (handlers0_1 : recursive_let_cont_handlers_t0)
           (handlers0_2 : recursive_let_cont_handlers_t0)
         ->
        let body1 = handlers0_1.body in
        let body2 = handlers0_2.body in
        let handlers1 = handlers0_1.handlers in
        let handlers2 = handlers0_2.handlers in
        f ~body1 ~body2 handlers1 handlers2)

  let apply_renaming = apply_renaming_recursive_let_cont_handlers
end

module Static_const_or_code = struct
  type t = static_const_or_code

  let print = print_static_const_or_code

  include Container_types.Make (struct
    type nonrec t = t

    let print = print

    let compare t1 t2 =
      match t1, t2 with
      | Code code1, Code code2 -> Code0.compare code1 code2
      | Static_const const1, Static_const const2 ->
        Static_const.compare const1 const2
      | Code _, Static_const _ -> -1
      | Static_const _, Code _ -> 1

    let equal t1 t2 = compare t1 t2 = 0

    let hash _t = Misc.fatal_error "Not yet implemented"

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)

  let free_names t =
    match t with
    | Code code -> Code0.free_names code
    | Static_const const -> Static_const.free_names const

  let apply_renaming = apply_renaming_static_const_or_code

  let all_ids_for_export = all_ids_for_export_static_const_or_code

  let create_code code = Code code

  let create_static_const const = Static_const const

  let is_fully_static t =
    match t with
    | Code _ -> true
    | Static_const const -> Static_const.is_fully_static const

  let to_code t = match t with Code code -> Some code | Static_const _ -> None
end

module Static_const_group = struct
  type t = static_const_group

  let create static_consts = static_consts

  let to_list t = t

  let empty = []

  let print = print_static_const_group

  let free_names t =
    List.map Static_const_or_code.free_names t |> Name_occurrences.union_list

  let apply_renaming = apply_renaming_static_const_group

  let all_ids_for_export = all_ids_for_export_static_const_group

  let match_against_bound_symbols =
    match_against_bound_symbols_static_const_group

  let pieces_of_code t =
    List.filter_map Static_const_or_code.to_code t
    |> List.filter_map (fun code ->
           if Code0.is_deleted code
           then None
           else Some (Code0.code_id code, code))
    |> Code_id.Map.of_list

  let pieces_of_code' t = pieces_of_code t |> Code_id.Map.data

  let is_fully_static t = List.for_all Static_const_or_code.is_fully_static t

  let concat t1 t2 = t1 @ t2

  let map t ~f = List.map f t
end

module Named = struct
  type t = named

  let create_simple simple = Simple simple

  let create_prim prim dbg = Prim (prim, dbg)

  let create_set_of_closures set_of_closures = Set_of_closures set_of_closures

  let create_static_consts consts = Static_consts consts

  let create_rec_info rec_info_expr = Rec_info rec_info_expr

  let free_names t =
    match t with
    | Simple simple -> Simple.free_names simple
    | Prim (prim, _dbg) -> Flambda_primitive.free_names prim
    | Set_of_closures set -> Set_of_closures.free_names set
    | Static_consts consts -> Static_const_group.free_names consts
    | Rec_info rec_info_expr -> Rec_info_expr.free_names rec_info_expr

  let print = print_named

  let apply_renaming = apply_renaming_named

  let box_value name (kind : Flambda_kind.t) dbg : t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate -> Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Box_number Naked_float, simple), dbg), K.value
    | Naked_number Naked_int32 ->
      Prim (Unary (Box_number Naked_int32, simple), dbg), K.value
    | Naked_number Naked_int64 ->
      Prim (Unary (Box_number Naked_int64, simple), dbg), K.value
    | Naked_number Naked_nativeint ->
      Prim (Unary (Box_number Naked_nativeint, simple), dbg), K.value
    | Fabricated -> Misc.fatal_error "Cannot box values of [Fabricated] kind"
    | Rec_info -> Misc.fatal_error "Cannot box values of [Rec_info] kind"

  let unbox_value name (kind : Flambda_kind.t) dbg : t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate -> Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Unbox_number Naked_float, simple), dbg), K.naked_float
    | Naked_number Naked_int32 ->
      Prim (Unary (Unbox_number Naked_int32, simple), dbg), K.naked_int32
    | Naked_number Naked_int64 ->
      Prim (Unary (Unbox_number Naked_int64, simple), dbg), K.naked_int64
    | Naked_number Naked_nativeint ->
      ( Prim (Unary (Unbox_number Naked_nativeint, simple), dbg),
        K.naked_nativeint )
    | Fabricated -> Misc.fatal_error "Cannot unbox values of [Fabricated] kind"
    | Rec_info -> Misc.fatal_error "Cannot unbox values of [Rec_info] kind"

  let at_most_generative_effects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
    | Set_of_closures _ -> true
    | Static_consts _ -> true
    | Rec_info _ -> true

  let dummy_value (kind : K.t) : t =
    let simple =
      match kind with
      | Value -> Simple.const_zero
      | Naked_number Naked_immediate ->
        Simple.const (Reg_width_const.naked_immediate Targetint_31_63.zero)
      | Naked_number Naked_float ->
        Simple.const
          (Reg_width_const.naked_float Numeric_types.Float_by_bit_pattern.zero)
      | Naked_number Naked_int32 ->
        Simple.const (Reg_width_const.naked_int32 Int32.zero)
      | Naked_number Naked_int64 ->
        Simple.const (Reg_width_const.naked_int64 Int64.zero)
      | Naked_number Naked_nativeint ->
        Simple.const (Reg_width_const.naked_nativeint Targetint_32_64.zero)
      | Fabricated -> Misc.fatal_error "[Fabricated] kind not expected here"
      | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"
    in
    Simple simple

  let is_dynamically_allocated_set_of_closures t =
    match t with
    | Set_of_closures _ -> true
    | Simple _ | Prim _ | Static_consts _ | Rec_info _ -> false

  let is_static_consts t =
    match t with
    | Static_consts _ -> true
    | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ -> false

  let must_be_static_consts = named_must_be_static_consts
end

module Expr = struct
  type t = expr

  type descr = expr_descr

  let create = With_delayed_renaming.create

  let descr = descr

  let apply_renaming = apply_renaming

  let all_ids_for_export = all_ids_for_export

  let print = print

  let create_let let_expr = create (Let let_expr)

  let create_let_cont let_cont = create (Let_cont let_cont)

  let create_apply apply = create (Apply apply)

  let create_apply_cont apply_cont = create (Apply_cont apply_cont)

  let create_switch switch = create (Switch switch)

  let create_invalid ?semantics () =
    let semantics : Invalid_term_semantics.t =
      match semantics with
      | Some semantics -> semantics
      | None ->
        if Flambda_features.treat_invalid_code_as_unreachable ()
        then Treat_as_unreachable
        else Halt_and_catch_fire
    in
    create (Invalid semantics)

  let bind_parameters_to_args_no_simplification ~params ~args ~body =
    if List.compare_lengths params args <> 0
    then
      Misc.fatal_errorf "Mismatching parameters and arguments: %a and %a"
        BP.List.print params Simple.List.print args;
    ListLabels.fold_left2 (List.rev params) (List.rev args) ~init:body
      ~f:(fun expr param arg ->
        let var = Bound_var.create (BP.var param) Name_mode.normal in
        Let_expr.create
          (Bound_pattern.singleton var)
          (Named.create_simple arg) ~body:expr ~free_names_of_body:Unknown
        |> create_let)
end

module Let_cont_expr = struct
  type t = let_cont_expr

  let print = print_let_cont_expr

  let create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body:num_free_occurrences
      ~is_applied_with_traps =
    let handler = Non_recursive_let_cont_handler.create cont handler ~body in
    Expr.create_let_cont
      (Non_recursive { handler; num_free_occurrences; is_applied_with_traps })

  let create_non_recursive cont handler ~body ~free_names_of_body =
    let num_free_occurrences_of_cont_in_body, is_applied_with_traps =
      (* Only the continuations of [free_names_of_body] are used.
         [Closure_conversion_aux] relies on this property. *)
      match (free_names_of_body : _ Or_unknown.t) with
      | Unknown -> Or_unknown.Unknown, true
      | Known free_names_of_body ->
        ( Or_unknown.Known
            (Name_occurrences.count_continuation free_names_of_body cont),
          Name_occurrences.continuation_is_applied_with_traps free_names_of_body
            cont )
    in
    create_non_recursive' ~cont handler ~body
      ~num_free_occurrences_of_cont_in_body ~is_applied_with_traps

  let create_recursive handlers ~body =
    if Continuation_handlers.contains_exn_handler handlers
    then Misc.fatal_error "Exception-handling continuations cannot be recursive";
    Expr.create_let_cont
      (Recursive (Recursive_let_cont_handlers.create handlers ~body))

  let apply_renaming = apply_renaming_let_cont_expr
end

(* CR mshinwell: Consider counting numbers of names in Name_occurrences *)
(* CR mshinwell: Check that apply_cont is well-formed when there is a trap
   installation or removal. *)
(* CR-someday pchambart: for sum types, we should probably add an exhaustive
   pattern in ignores functions to be reminded if a type change *)
(* CR-someday mshinwell: We should make "direct applications should not have
   overapplication" be an invariant throughout. At the moment I think this is
   only true after [Simplify] has split overapplications. *)

module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures

module Import = struct
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Continuation_handler = Continuation_handler
  module Continuation_handlers = Continuation_handlers
  module Expr = Expr
  module Function_declarations = Function_declarations
  module Function_params_and_body = Function_params_and_body
  module Let = Let
  module Let_cont = Let_cont
  module Named = Named
  module Non_recursive_let_cont_handler = Non_recursive_let_cont_handler
  module Recursive_let_cont_handlers = Recursive_let_cont_handlers
  module Set_of_closures = Set_of_closures
  module Static_const = Static_const
  module Static_const_group = Static_const_group
  module Static_const_or_code = Static_const_or_code
  module Switch = Switch
end
