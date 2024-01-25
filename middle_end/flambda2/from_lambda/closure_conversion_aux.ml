(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module IR = struct
  type simple =
    | Var of Ident.t
    | Const of Lambda.structured_constant

  type exn_continuation =
    { exn_handler : Continuation.t;
      extra_args : (simple * Flambda_kind.With_subkind.t) list
    }

  type trap_action =
    | Push of { exn_handler : Continuation.t }
    | Pop of { exn_handler : Continuation.t }

  type user_visible =
    | User_visible
    | Not_user_visible

  type named =
    | Simple of simple
    | Get_tag of Ident.t
    | Begin_region of { is_try_region : bool }
    | End_region of
        { is_try_region : bool;
          region : Ident.t
        }
    | Prim of
        { prim : Lambda.primitive;
          args : simple list list;
          loc : Lambda.scoped_location;
          exn_continuation : exn_continuation option;
          region : Ident.t
        }

  type apply_kind =
    | Function
    | Method of
        { kind : Lambda.meth_kind;
          obj : simple
        }

  type apply =
    { kind : apply_kind;
      func : Ident.t;
      args : simple list;
      continuation : Continuation.t;
      exn_continuation : exn_continuation;
      loc : Lambda.scoped_location;
      region_close : Lambda.region_close;
      inlined : Lambda.inlined_attribute;
      probe : Lambda.probe;
      mode : Lambda.alloc_mode;
      region : Ident.t;
      args_arity : [`Complex] Flambda_arity.t;
      return_arity : [`Unarized] Flambda_arity.t
    }

  type switch =
    { numconsts : int;
      consts :
        (int * Continuation.t * Debuginfo.t * trap_action option * simple list)
        list;
      failaction :
        (Continuation.t * Debuginfo.t * trap_action option * simple list) option
    }

  let fprintf = Format.fprintf

  let print_simple ppf simple =
    match simple with
    | Var id -> Ident.print ppf id
    | Const cst -> Printlambda.structured_constant ppf cst

  let print_named ppf (named : named) =
    match named with
    | Simple (Var id) -> Ident.print ppf id
    | Simple (Const cst) -> Printlambda.structured_constant ppf cst
    | Get_tag id -> fprintf ppf "@[<2>(Gettag %a)@]" Ident.print id
    | Begin_region { is_try_region } ->
      if is_try_region
      then fprintf ppf "Begin_try_region"
      else fprintf ppf "Begin_region"
    | End_region { is_try_region; region } ->
      if is_try_region
      then fprintf ppf "@[<2>(End_try_region@ %a)@]" Ident.print region
      else fprintf ppf "@[<2>(End_region@ %a)@]" Ident.print region
    | Prim { prim; args; _ } ->
      fprintf ppf "@[<2>(%a %a)@]" Printlambda.primitive prim
        (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf arg ->
             fprintf ppf "@[<2>(%a)@]"
               (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple)
               arg))
        args
end

module Inlining = struct
  type inlinable_result =
    | Not_inlinable
    | Inlinable of Code.t

  let threshold () =
    let inline_threshold =
      Clflags.Float_arg_helper.get ~key:0 !Clflags.inline_threshold
    in
    let magic_scale_constant = 20. in
    int_of_float (inline_threshold *. magic_scale_constant)

  let definition_inlining_decision inline cost_metrics =
    let inline_threshold = threshold () in
    let code_size = Cost_metrics.size cost_metrics in
    match (inline : Inline_attribute.t) with
    | Never_inline ->
      Function_decl_inlining_decision_type.Never_inline_attribute
    | Always_inline | Available_inline ->
      Function_decl_inlining_decision_type.Attribute_inline
    | Unroll _ | Default_inline ->
      if Code_size.to_int code_size <= inline_threshold
      then
        Function_decl_inlining_decision_type.Small_function
          { size = code_size;
            small_function_size = Code_size.of_int inline_threshold
          }
      else
        Function_decl_inlining_decision_type.Function_body_too_large
          (Code_size.of_int inline_threshold)
end

module Env = struct
  type value_approximation = Code_or_metadata.t Value_approximation.t

  type t =
    { variables : (Variable.t * Flambda_kind.With_subkind.t) Ident.Map.t;
      globals : Symbol.t Numeric_types.Int.Map.t;
      simples_to_substitute :
        (Simple.t * Flambda_kind.With_subkind.t) Ident.Map.t;
      current_unit : Compilation_unit.t;
      current_depth : Variable.t option;
      value_approximations : value_approximation Variable.Map.t;
      big_endian : bool;
      path_to_root : Debuginfo.Scoped_location.t;
      inlining_history_tracker : Inlining_history.Tracker.t;
      at_toplevel : bool
    }

  let current_unit t = t.current_unit

  let big_endian t = t.big_endian

  let current_depth t = t.current_depth

  let create ~big_endian =
    let current_unit = Compilation_unit.get_current_exn () in
    { variables = Ident.Map.empty;
      globals = Numeric_types.Int.Map.empty;
      simples_to_substitute = Ident.Map.empty;
      current_unit;
      current_depth = None;
      value_approximations = Variable.Map.empty;
      big_endian;
      path_to_root = Debuginfo.Scoped_location.Loc_unknown;
      inlining_history_tracker = Inlining_history.Tracker.empty current_unit;
      at_toplevel = true
    }

  let set_not_at_toplevel t = { t with at_toplevel = false }

  let at_toplevel t = t.at_toplevel

  let clear_local_bindings
      { variables = _;
        globals;
        simples_to_substitute;
        current_unit;
        current_depth;
        value_approximations;
        big_endian;
        path_to_root;
        inlining_history_tracker;
        at_toplevel
      } =
    let simples_to_substitute =
      Ident.Map.filter
        (fun _ (simple, _kind) -> not (Simple.is_var simple))
        simples_to_substitute
    in
    { variables = Ident.Map.empty;
      globals;
      simples_to_substitute;
      current_unit;
      current_depth;
      value_approximations;
      big_endian;
      path_to_root;
      inlining_history_tracker;
      at_toplevel
    }

  let with_depth t depth_var = { t with current_depth = Some depth_var }

  let add_var t id var kind =
    { t with variables = Ident.Map.add id (var, kind) t.variables }

  let add_vars t ids vars =
    List.fold_left2 (fun t id (var, kind) -> add_var t id var kind) t ids vars

  let add_var_map t map =
    { t with variables = Ident.Map.union_right t.variables map }

  let add_var_like t id (user_visible : IR.user_visible) kind =
    let user_visible =
      match user_visible with
      | Not_user_visible -> None
      | User_visible -> Some ()
    in
    let var = Variable.create_with_same_name_as_ident ?user_visible id in
    add_var t id var kind, var

  let add_vars_like t ids =
    let vars =
      List.map
        (fun (id, (user_visible : IR.user_visible), kind) ->
          let user_visible =
            match user_visible with
            | Not_user_visible -> None
            | User_visible -> Some ()
          in
          Variable.create_with_same_name_as_ident ?user_visible id, kind)
        ids
    in
    add_vars t (List.map (fun (id, _, _) -> id) ids) vars, List.map fst vars

  let find_var t id =
    try Ident.Map.find id t.variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let find_var_exn t id = Ident.Map.find id t.variables

  let find_name t id = Name.var (fst (find_var t id))

  let find_name_exn t id = Name.var (fst (find_var_exn t id))

  let find_vars t ids = List.map (fun id -> find_var t id) ids

  let add_global t pos symbol =
    { t with globals = Numeric_types.Int.Map.add pos symbol t.globals }

  let find_global t pos =
    try Numeric_types.Int.Map.find pos t.globals
    with Not_found ->
      Misc.fatal_error
        ("Closure_conversion.Env.find_global: global " ^ string_of_int pos)

  let add_simple_to_substitute t id simple kind =
    if Ident.Map.mem id t.simples_to_substitute
    then
      Misc.fatal_errorf "Cannot redefine [Simple] associated with %a"
        Ident.print id;
    { t with
      simples_to_substitute =
        Ident.Map.add id (simple, kind) t.simples_to_substitute
    }

  let add_simple_to_substitute_map t map =
    { t with
      simples_to_substitute =
        Ident.Map.disjoint_union map t.simples_to_substitute
    }

  let find_simple_to_substitute_exn t id =
    Ident.Map.find id t.simples_to_substitute

  let add_var_approximation t var approx =
    if Value_approximation.is_unknown approx
    then t
    else
      { t with
        value_approximations =
          Variable.Map.add var approx t.value_approximations
      }

  let add_block_approximation t var tag approxs alloc_mode =
    if Array.for_all Value_approximation.is_unknown approxs
    then t
    else
      add_var_approximation t var
        (Block_approximation (tag, approxs, alloc_mode))

  let find_var_approximation t var =
    try Variable.Map.find var t.value_approximations
    with Not_found -> Value_approximation.Value_unknown

  let set_path_to_root t path_to_root =
    if path_to_root = Debuginfo.Scoped_location.Loc_unknown
    then t
    else { t with path_to_root }

  let path_to_root { path_to_root; _ } = path_to_root

  let use_inlining_history_tracker t inlining_history_tracker =
    { t with inlining_history_tracker }

  let inlining_history_tracker { inlining_history_tracker; _ } =
    inlining_history_tracker

  let relative_history_from_scoped ~loc { path_to_root; _ } =
    Inlining_history.Relative.between_scoped_locations ~parent:path_to_root
      ~child:loc
end

module Acc = struct
  type continuation_application =
    | Trackable_arguments of Env.value_approximation list
    | Untrackable

  type closure_info =
    { code_id : Code_id.t;
      return_continuation : Continuation.t;
      exn_continuation : Exn_continuation.t;
      my_closure : Variable.t;
      is_purely_tailrec : bool;
      slot_offsets_at_definition : Slot_offsets.t
          (* note: this last field is not a property of the current closure, but
             rather a property of its point of definition (i.e. the state of the
             slot_offsets right before we entered the current closure). It's
             mainly stored here for efficiency reasons. *)
    }

  type t =
    { declared_symbols : (Symbol.t * Static_const.t) list;
      lifted_sets_of_closures :
        (Symbol.t Function_slot.Lmap.t * Flambda.Set_of_closures.t) list;
      shareable_constants : Symbol.t Static_const.Map.t;
      symbol_approximations : Env.value_approximation Symbol.Map.t;
      approximation_for_external_symbol : Symbol.t -> Env.value_approximation;
      code_in_reverse_order : Code.t list;
      code_map : Code.t Code_id.Map.t;
      free_names : Name_occurrences.t;
      continuation_applications : continuation_application Continuation.Map.t;
      cost_metrics : Cost_metrics.t;
      seen_a_function : bool;
      slot_offsets : Slot_offsets.t;
      code_slot_offsets : Slot_offsets.t Code_id.Map.t;
      regions_closed_early : Ident.Set.t;
      closure_infos : closure_info list;
      symbol_short_name_counter : int
    }

  let manufacture_symbol_short_name t =
    let counter = t.symbol_short_name_counter in
    let t = { t with symbol_short_name_counter = counter + 1 } in
    let name = Linkage_name.of_string ("s" ^ string_of_int counter) in
    t, name

  let cost_metrics t = t.cost_metrics

  let increment_metrics metrics t =
    { t with cost_metrics = Cost_metrics.( + ) t.cost_metrics metrics }

  let with_cost_metrics cost_metrics t = { t with cost_metrics }

  let seen_a_function t = t.seen_a_function

  let with_seen_a_function t seen_a_function = { t with seen_a_function }

  let approximation_loader loader =
    let externals = ref Symbol.Map.empty in
    fun symbol ->
      match Symbol.Map.find symbol !externals with
      | approx -> approx
      | exception Not_found ->
        let approx = Flambda_cmx.load_symbol_approx loader symbol in
        (if Flambda_features.check_invariants ()
        then
          match approx with
          | Value_symbol sym ->
            Misc.fatal_errorf
              "Closure_conversion: approximation loader returned a Symbol \
               approximation (%a) for symbol %a"
              Symbol.print sym Symbol.print symbol
          | Value_unknown | Value_int _ | Closure_approximation _
          | Block_approximation _ ->
            ());
        let rec filter_inlinable approx =
          match (approx : Env.value_approximation) with
          | Value_unknown | Value_symbol _ | Value_int _ -> approx
          | Block_approximation (tag, approxs, alloc_mode) ->
            let approxs = Array.map filter_inlinable approxs in
            Value_approximation.Block_approximation (tag, approxs, alloc_mode)
          | Closure_approximation
              { code_id;
                function_slot;
                all_function_slots;
                all_value_slots;
                code;
                _
              } -> (
            let metadata = Code_or_metadata.code_metadata code in
            if not (Code_or_metadata.code_present code)
            then approx
            else
              match
                Inlining.definition_inlining_decision
                  (Code_metadata.inline metadata)
                  (Code_metadata.cost_metrics metadata)
              with
              | Attribute_inline | Small_function _ -> approx
              | Not_yet_decided | Never_inline_attribute | Stub | Recursive
              | Function_body_too_large _ | Speculatively_inlinable _
              | Functor _ ->
                Value_approximation.Closure_approximation
                  { code_id;
                    function_slot;
                    all_function_slots;
                    all_value_slots;
                    code = Code_or_metadata.create_metadata_only metadata;
                    symbol = None
                  })
        in
        let approx = filter_inlinable approx in
        externals := Symbol.Map.add symbol approx !externals;
        approx

  let create ~cmx_loader =
    { declared_symbols = [];
      lifted_sets_of_closures = [];
      shareable_constants = Static_const.Map.empty;
      symbol_approximations = Symbol.Map.empty;
      approximation_for_external_symbol =
        (if Flambda_features.classic_mode ()
        then approximation_loader cmx_loader
        else fun _symbol -> Value_approximation.Value_unknown);
      code_in_reverse_order = [];
      code_map = Code_id.Map.empty;
      free_names = Name_occurrences.empty;
      continuation_applications = Continuation.Map.empty;
      cost_metrics = Cost_metrics.zero;
      seen_a_function = false;
      slot_offsets = Slot_offsets.empty;
      code_slot_offsets = Code_id.Map.empty;
      regions_closed_early = Ident.Set.empty;
      closure_infos = [];
      symbol_short_name_counter = 0
    }

  let declared_symbols t = t.declared_symbols

  let lifted_sets_of_closures t = t.lifted_sets_of_closures

  let shareable_constants t = t.shareable_constants

  let code t =
    (* This only gets called once *)
    List.rev t.code_in_reverse_order
    |> List.map (fun code -> Code.code_id code, code)
    |> Code_id.Lmap.of_list

  let code_map t = t.code_map

  let free_names t = t.free_names

  let slot_offsets t = t.slot_offsets

  let code_slot_offsets t = t.code_slot_offsets

  let add_declared_symbol ~symbol ~constant t =
    let declared_symbols = (symbol, constant) :: t.declared_symbols in
    let approx : _ Value_approximation.t =
      match (constant : Static_const.t) with
      | Block (tag, mut, fields) ->
        if not (Mutability.is_mutable mut)
        then
          let approx_of_field :
              Field_of_static_block.t -> _ Value_approximation.t = function
            | Symbol sym -> Value_symbol sym
            | Tagged_immediate i -> Value_int i
            | Dynamically_computed _ -> Value_unknown
          in
          let tag = Tag.Scannable.to_tag tag in
          let fields = List.map approx_of_field fields |> Array.of_list in
          Block_approximation (tag, fields, Alloc_mode.For_types.unknown ())
        else Value_unknown
      | Set_of_closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_vec128 _ | Boxed_nativeint _ | Immutable_float_block _
      | Immutable_float_array _ | Immutable_value_array _ | Empty_array _
      | Immutable_int32_array _ | Immutable_int64_array _
      | Immutable_nativeint_array _ | Mutable_string _ | Immutable_string _ ->
        Value_unknown
    in
    let symbol_approximations =
      Symbol.Map.add symbol approx t.symbol_approximations
    in
    { t with declared_symbols; symbol_approximations }

  let add_lifted_set_of_closures ~symbols ~set_of_closures t =
    { t with
      lifted_sets_of_closures =
        (symbols, set_of_closures) :: t.lifted_sets_of_closures
    }

  let add_shareable_constant ~symbol ~constant t =
    let shareable_constants =
      Static_const.Map.add constant symbol t.shareable_constants
    in
    { t with shareable_constants }

  let find_symbol_approximation t symbol =
    try Symbol.Map.find symbol t.symbol_approximations
    with Not_found -> t.approximation_for_external_symbol symbol

  let add_symbol_approximation t symbol approx =
    match (approx : _ Value_approximation.t) with
    | Value_symbol s ->
      (* This should not happen. But in case it does, we don't want to add an
         indirection *)
      Misc.fatal_errorf "Symbol %a approximated to symbol %a" Symbol.print
        symbol Symbol.print s
    | Value_unknown | Closure_approximation _ | Block_approximation _
    | Value_int _ ->
      (* We need all defined symbols to be present in [symbol_approximations],
         even when their approximation is [Value_unknown] *)
      { t with
        symbol_approximations =
          Symbol.Map.add symbol approx t.symbol_approximations
      }

  let symbol_approximations t = t.symbol_approximations

  let add_code ~code_id ~code ?slot_offsets t =
    { t with
      code_map = Code_id.Map.add code_id code t.code_map;
      code_in_reverse_order = code :: t.code_in_reverse_order;
      code_slot_offsets =
        (match slot_offsets with
        | None -> t.code_slot_offsets
        | Some offsets -> Code_id.Map.add code_id offsets t.code_slot_offsets)
    }

  let add_free_names free_names t =
    { t with free_names = Name_occurrences.union free_names t.free_names }

  let add_free_names_and_check_my_closure_use free_names t =
    let t =
      match t.closure_infos with
      | [] -> t
      | closure_info :: closure_infos ->
        if closure_info.is_purely_tailrec
           && Name_occurrences.mem_var free_names closure_info.my_closure
        then
          { t with
            closure_infos =
              { closure_info with is_purely_tailrec = false } :: closure_infos
          }
        else t
    in
    add_free_names free_names t

  let add_name_to_free_names ~is_tail_call ~name t =
    let closure_infos =
      match is_tail_call, t.closure_infos with
      | true, closure_infos -> closure_infos
      | false, [] -> []
      | false, closure_info :: closure_infos ->
        if closure_info.is_purely_tailrec
           && Name.equal (Name.var closure_info.my_closure) name
        then { closure_info with is_purely_tailrec = false } :: closure_infos
        else t.closure_infos
    in
    { t with
      closure_infos;
      free_names = Name_occurrences.add_name t.free_names name Name_mode.normal
    }

  let add_simple_to_free_names_maybe_tail_call ~is_tail_call acc simple =
    Simple.pattern_match simple
      ~const:(fun _ -> acc)
      ~name:(fun name ~coercion ->
        let acc = add_name_to_free_names ~is_tail_call ~name acc in
        add_free_names (Coercion.free_names coercion) acc)

  let add_simple_to_free_names acc simple =
    add_simple_to_free_names_maybe_tail_call ~is_tail_call:false acc simple

  let remove_code_id_or_symbol_from_free_names code_id_or_symbol t =
    { t with
      free_names =
        Name_occurrences.remove_code_id_or_symbol t.free_names
          ~code_id_or_symbol
    }

  let remove_symbol_from_free_names symbol t =
    remove_code_id_or_symbol_from_free_names
      (Code_id_or_symbol.create_symbol symbol)
      t

  let remove_var_from_free_names var t =
    { t with free_names = Name_occurrences.remove_var t.free_names ~var }

  let add_continuation_application ~cont args_approx t =
    let continuation_application =
      match args_approx with
      | None -> Untrackable
      | Some args ->
        if Continuation.Map.mem cont t.continuation_applications
        then Untrackable
        else Trackable_arguments args
    in
    { t with
      continuation_applications =
        Continuation.Map.add cont continuation_application
          t.continuation_applications
    }

  let mark_continuation_as_untrackable cont t =
    { t with
      continuation_applications =
        Continuation.Map.add cont Untrackable t.continuation_applications
    }

  let remove_continuation_from_free_names continuation t =
    { t with
      free_names =
        Name_occurrences.remove_continuation t.free_names ~continuation
        (* We don't remove the continuation from [t.continuation_applications]
           here because we need this information of the module block to escape
           its scope to build the .cmx in [Closure_conversion.close_program]. *)
    }

  let remove_code_id_from_free_names code_id t =
    remove_code_id_or_symbol_from_free_names
      (Code_id_or_symbol.create_code_id code_id)
      t

  let continuation_known_arguments ~cont t =
    match Continuation.Map.find cont t.continuation_applications with
    | (exception Not_found) | Untrackable -> None
    | Trackable_arguments args -> Some args

  let with_free_names free_names t = { t with free_names }

  let eval_branch_free_names t ~f =
    let base_free_names = t.free_names in
    let t, res = f { t with free_names = Name_occurrences.empty } in
    t.free_names, { t with free_names = base_free_names }, res

  let measure_cost_metrics acc ~f =
    let saved_cost_metrics = cost_metrics acc in
    let acc = with_cost_metrics Cost_metrics.zero acc in
    let free_names, acc, return = eval_branch_free_names acc ~f in
    let cost_metrics = cost_metrics acc in
    cost_metrics, free_names, with_cost_metrics saved_cost_metrics acc, return

  let add_offsets_from_code t code_id =
    match Code_id.Map.find code_id t.code_slot_offsets with
    | exception Not_found ->
      Misc.fatal_errorf "No slot offsets constraints found for code id %a"
        Code_id.print code_id
    | from_function ->
      let slot_offsets =
        Slot_offsets.add_offsets_from_function t.slot_offsets ~from_function
      in
      { t with slot_offsets }

  let add_set_of_closures_offsets ~is_phantom t set_of_closures =
    let slot_offsets =
      Slot_offsets.add_set_of_closures t.slot_offsets ~is_phantom
        set_of_closures
    in
    { t with slot_offsets }

  let top_closure_info t =
    match t.closure_infos with
    | [] -> None
    | closure_info :: _ -> Some closure_info

  let push_closure_info t ~return_continuation ~exn_continuation ~my_closure
      ~is_purely_tailrec ~code_id =
    { t with
      slot_offsets = Slot_offsets.empty;
      closure_infos =
        { code_id;
          return_continuation;
          exn_continuation;
          my_closure;
          is_purely_tailrec;
          slot_offsets_at_definition = t.slot_offsets
        }
        :: t.closure_infos
    }

  let pop_closure_info t =
    let closure_info, closure_infos =
      match t.closure_infos with
      | [] -> Misc.fatal_error "pop_closure_info called on empty stack"
      | closure_info :: closure_infos -> closure_info, closure_infos
    in
    let code_slot_offsets =
      Code_id.Map.add closure_info.code_id t.slot_offsets t.code_slot_offsets
    in
    let closure_infos =
      match closure_infos with
      | [] -> []
      | closure_info2 :: closure_infos2 ->
        if closure_info2.is_purely_tailrec
           && Name_occurrences.mem_var t.free_names closure_info2.my_closure
        then { closure_info2 with is_purely_tailrec = false } :: closure_infos2
        else closure_infos
    in
    ( closure_info,
      { t with
        closure_infos;
        code_slot_offsets;
        slot_offsets = closure_info.slot_offsets_at_definition
      } )
end

module Function_decls = struct
  module Function_decl = struct
    type param =
      { name : Ident.t;
        kind : Flambda_kind.With_subkind.t;
        attributes : Lambda.parameter_attribute;
        mode : Lambda.alloc_mode
      }

    type unboxing_kind =
      | Multiple_values of Flambda_kind.With_subkind.t list
      | Unboxed_number of Flambda_kind.Boxable_number.t
      | Unboxed_float_record of int

    type calling_convention =
      | Normal_calling_convention
      | Unboxed_calling_convention of
          unboxing_kind option list * unboxing_kind option * Function_slot.t

    type t =
      { let_rec_ident : Ident.t;
        function_slot : Function_slot.t;
        kind : Lambda.function_kind;
        params : param list;
        removed_params : Ident.Set.t;
        params_arity : [`Complex] Flambda_arity.t;
        return : [`Unarized] Flambda_arity.t;
        calling_convention : calling_convention;
        return_continuation : Continuation.t;
        exn_continuation : IR.exn_continuation;
        my_region : Ident.t;
        body : Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t;
        free_idents_of_body : Ident.Set.t;
        attr : Lambda.function_attribute;
        loc : Lambda.scoped_location;
        recursive : Recursive.t;
        closure_alloc_mode : Lambda.alloc_mode;
        first_complex_local_param : int;
        result_mode : Lambda.alloc_mode;
        contains_no_escaping_local_allocs : bool
      }

    let create ~let_rec_ident ~function_slot ~kind ~params ~params_arity
        ~removed_params ~return ~calling_convention ~return_continuation
        ~exn_continuation ~my_region ~body ~(attr : Lambda.function_attribute)
        ~loc ~free_idents_of_body recursive ~closure_alloc_mode
        ~first_complex_local_param ~result_mode
        ~contains_no_escaping_local_allocs =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create_local "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        function_slot;
        kind;
        params;
        params_arity;
        removed_params;
        return;
        calling_convention;
        return_continuation;
        exn_continuation;
        my_region;
        body;
        free_idents_of_body;
        attr;
        loc;
        recursive;
        closure_alloc_mode;
        first_complex_local_param;
        result_mode;
        contains_no_escaping_local_allocs
      }

    let let_rec_ident t = t.let_rec_ident

    let function_slot t = t.function_slot

    let kind t = t.kind

    let params t = t.params

    let params_arity t = t.params_arity

    let return t = t.return

    let calling_convention t = t.calling_convention

    let return_continuation t = t.return_continuation

    let exn_continuation t = t.exn_continuation

    let my_region t = t.my_region

    let body t = t.body

    let free_idents t = Ident.Set.diff t.free_idents_of_body t.removed_params

    let inline t = t.attr.inline

    let specialise t = t.attr.specialise

    let poll_attribute t = t.attr.poll

    let loop t = t.attr.loop

    let is_a_functor t = t.attr.is_a_functor

    let is_opaque t = t.attr.is_opaque

    let check_attribute t = t.attr.check

    let stub t = t.attr.stub

    let loc t = t.loc

    let recursive t = t.recursive

    let closure_alloc_mode t = t.closure_alloc_mode

    let first_complex_local_param t = t.first_complex_local_param

    let result_mode t = t.result_mode

    let contains_no_escaping_local_allocs t =
      t.contains_no_escaping_local_allocs
  end

  type t =
    { function_decls : Function_decl.t list;
      all_free_idents : Ident.Set.t;
      alloc_mode : Lambda.alloc_mode
    }

  let alloc_mode t = t.alloc_mode

  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let free_idents_by_function function_decls =
    List.fold_right
      (fun decl map ->
        Function_slot.Map.add
          (Function_decl.function_slot decl)
          (Function_decl.free_idents decl)
          map)
      function_decls Function_slot.Map.empty

  let all_free_idents function_decls =
    Function_slot.Map.fold
      (fun _ -> Ident.Set.union)
      (free_idents_by_function function_decls)
      Ident.Set.empty

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents function_decls =
    List.map Function_decl.let_rec_ident function_decls

  (* All parameters of functions in [ts]. *)
  let all_params function_decls =
    List.concat (List.map Function_decl.params function_decls)

  let set_diff (from : Ident.Set.t) (idents : Ident.t list) =
    List.fold_right Ident.Set.remove idents from

  (* CR-someday lwhite: use a different name from above or explain the
     difference *)
  let all_free_idents function_decls =
    set_diff
      (set_diff
         (all_free_idents function_decls)
         (List.map (fun p -> p.Function_decl.name) (all_params function_decls)))
      (let_rec_idents function_decls)

  let create function_decls alloc_mode =
    { function_decls;
      all_free_idents = all_free_idents function_decls;
      alloc_mode
    }

  let to_list t = t.function_decls

  let all_free_idents t = t.all_free_idents
end

open Flambda.Import

module Expr_with_acc = struct
  type t = Acc.t * Expr.t

  let create_apply_cont acc apply_cont =
    let acc =
      Acc.increment_metrics
        (Code_size.apply_cont apply_cont |> Cost_metrics.from_size)
        acc
    in
    acc, Expr.create_apply_cont apply_cont

  let create_apply acc apply =
    let acc =
      Acc.increment_metrics
        (Code_size.apply apply |> Cost_metrics.from_size)
        acc
    in
    let is_tail_call =
      match Acc.top_closure_info acc with
      | None -> false
      | Some { return_continuation; exn_continuation; _ } -> (
        (match Apply_expr.continuation apply with
        | Never_returns -> true
        | Return cont -> Continuation.equal cont return_continuation)
        && Exn_continuation.equal
             (Apply_expr.exn_continuation apply)
             exn_continuation
        (* If the return and exn continuation match, the call is in tail
           position, but could still be an under- or over-application. By
           checking that it is a direct call, we are sure it has the correct
           arity. *)
        &&
        match Apply.call_kind apply with
        | Function { function_call = Direct _; _ } -> true
        | Function
            { function_call = Indirect_unknown_arity | Indirect_known_arity; _ }
          ->
          false
        | Method _ -> false
        | C_call _ -> false)
    in
    let acc =
      match Apply.callee apply with
      | None ->
        (* Since [is_my_closure_used] is initialized to [true] by default for
           recursive functions, this can't affect the result of the loopify
           attribute, because the recursive calls will keep the callee. Besides,
           if we are in this case, we are compiling in classic mode, and loopify
           won't run anyway. *)
        acc
      | Some callee ->
        Acc.add_simple_to_free_names_maybe_tail_call ~is_tail_call acc callee
    in
    let acc =
      Acc.add_free_names_and_check_my_closure_use
        (Apply_expr.free_names_except_callee apply)
        acc
    in
    let acc =
      match Apply_expr.continuation apply with
      | Never_returns -> acc
      | Return cont -> Acc.mark_continuation_as_untrackable cont acc
    in
    let acc =
      Acc.mark_continuation_as_untrackable
        (Exn_continuation.exn_handler (Apply_expr.exn_continuation apply))
        acc
    in
    acc, Expr.create_apply apply

  let create_switch acc switch =
    let acc =
      Acc.increment_metrics
        (Code_size.switch switch |> Cost_metrics.from_size)
        acc
    in
    let acc = Acc.add_simple_to_free_names acc (Switch_expr.scrutinee switch) in
    acc, Expr.create_switch switch

  let create_invalid acc reason =
    let acc =
      Acc.increment_metrics (Code_size.invalid |> Cost_metrics.from_size) acc
    in
    acc, Expr.create_invalid reason
end

module Apply_cont_with_acc = struct
  let create acc ?trap_action ?args_approx cont ~args ~dbg =
    let apply_cont = Apply_cont.create ?trap_action cont ~args ~dbg in
    let acc = Acc.add_continuation_application ~cont args_approx acc in
    let acc =
      Acc.add_free_names_and_check_my_closure_use
        (Apply_cont.free_names apply_cont)
        acc
    in
    acc, apply_cont

  let goto acc cont =
    create acc cont ~args:[] ?args_approx:None ~dbg:Debuginfo.none
end

module Let_with_acc = struct
  let create acc let_bound named ~body =
    let is_unused_singleton =
      match Bound_pattern.must_be_singleton_opt let_bound with
      | Some var ->
        not (Name_occurrences.mem_var (Acc.free_names acc) (Bound_var.var var))
      | None -> false
    in
    let has_no_effects =
      match (named : Named.t) with
      | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
      | Simple _ | Static_consts _ | Set_of_closures _ | Rec_info _ -> true
    in
    if is_unused_singleton && has_no_effects
    then acc, body
    else
      let cost_metrics_of_defining_expr =
        match (named : Named.t) with
        | Prim (prim, _) -> Code_size.prim prim |> Cost_metrics.from_size
        | Simple simple -> Code_size.simple simple |> Cost_metrics.from_size
        | Static_consts _consts -> Cost_metrics.zero
        | Set_of_closures set_of_closures ->
          let code_mapping = Acc.code_map acc in
          Cost_metrics.set_of_closures
            ~find_code_characteristics:(fun code_id ->
              let code = Code_id.Map.find code_id code_mapping in
              { cost_metrics = Code.cost_metrics code;
                params_arity = Flambda_arity.num_params (Code.params_arity code)
              })
            set_of_closures
        | Rec_info _ -> Cost_metrics.zero
      in
      let acc =
        Acc.increment_metrics
          (Cost_metrics.increase_due_to_let_expr ~is_phantom:false
             ~cost_metrics_of_defining_expr)
          acc
      in
      let free_names_of_body = Or_unknown.Known (Acc.free_names acc) in
      let acc =
        Bound_pattern.fold_all_bound_names let_bound ~init:acc
          ~var:(fun acc var ->
            Acc.remove_var_from_free_names (Bound_var.var var) acc)
          ~symbol:(fun acc s -> Acc.remove_symbol_from_free_names s acc)
          ~code_id:(fun acc cid -> Acc.remove_code_id_from_free_names cid acc)
      in
      let let_expr = Let.create let_bound named ~body ~free_names_of_body in
      let is_project_value_slot =
        match[@ocaml.warning "-4"] (named : Named.t) with
        | Prim (Unary (Project_value_slot _, _), _) -> true
        | _ -> false
      in
      let acc =
        if is_project_value_slot
        then Acc.add_free_names (Named.free_names named) acc
        else
          Acc.add_free_names_and_check_my_closure_use (Named.free_names named)
            acc
      in
      acc, Expr.create_let let_expr
end

module Continuation_handler_with_acc = struct
  let create acc parameters ~handler ~is_exn_handler ~is_cold =
    let free_names_of_handler = Or_unknown.Known (Acc.free_names acc) in
    let acc =
      List.fold_left
        (fun acc param ->
          Acc.remove_var_from_free_names (Bound_parameter.var param) acc)
        acc
        (Bound_parameters.to_list parameters)
    in
    ( acc,
      Continuation_handler.create parameters ~handler ~free_names_of_handler
        ~is_exn_handler ~is_cold )
end

module Let_cont_with_acc = struct
  let create_non_recursive acc cont handler ~body ~free_names_of_body
      ~cost_metrics_of_handler =
    let acc =
      Acc.increment_metrics
        (Cost_metrics.increase_due_to_let_cont_non_recursive
           ~cost_metrics_of_handler)
        acc
    in
    let expr =
      (* This function only uses continuations of [free_names_of_body] *)
      Let_cont.create_non_recursive cont handler ~body
        ~free_names_of_body:(Known free_names_of_body)
    in
    let acc = Acc.remove_continuation_from_free_names cont acc in
    acc, expr

  let create_recursive acc ~invariant_params handlers ~body
      ~cost_metrics_of_handlers =
    let acc =
      Acc.increment_metrics
        (Cost_metrics.increase_due_to_let_cont_recursive
           ~cost_metrics_of_handlers)
        acc
    in
    let expr = Let_cont.create_recursive ~invariant_params handlers ~body in
    let acc =
      Continuation.Map.fold
        (fun cont _ acc -> Acc.remove_continuation_from_free_names cont acc)
        handlers acc
    in
    acc, expr

  let build_recursive acc ~invariant_params ~handlers ~body =
    let handlers_free_names, cost_metrics_of_handlers, acc, handlers =
      Continuation.Map.fold
        (fun cont (handler, params, is_exn_handler, is_cold)
             (free_names, costs, acc, handlers) ->
          let cost_metrics_of_handler, handler_free_names, acc, handler =
            Acc.measure_cost_metrics acc ~f:(fun acc ->
                let acc, handler = handler acc in
                Continuation_handler_with_acc.create acc params ~handler
                  ~is_exn_handler ~is_cold)
          in
          ( Name_occurrences.union free_names handler_free_names,
            Cost_metrics.( + ) costs cost_metrics_of_handler,
            acc,
            Continuation.Map.add cont handler handlers ))
        handlers
        (Name_occurrences.empty, Cost_metrics.zero, acc, Continuation.Map.empty)
    in
    let body_free_names, acc, body = Acc.eval_branch_free_names acc ~f:body in
    let acc =
      Acc.with_free_names
        (Name_occurrences.union body_free_names
           (Name_occurrences.increase_counts handlers_free_names))
        acc
    in
    create_recursive acc ~invariant_params handlers ~body
      ~cost_metrics_of_handlers

  let build_non_recursive acc cont ~handler_params ~handler ~body
      ~is_exn_handler ~is_cold =
    (* We need to evaluate the body before the handler to pass along information
       on the argument for inlining *)
    let free_names_of_body, acc, body =
      Acc.eval_branch_free_names acc ~f:body
    in
    let body_acc = acc in
    let cost_metrics_of_handler, handler_free_names, acc, handler =
      Acc.measure_cost_metrics acc ~f:(fun acc ->
          let acc, handler = handler acc in
          Continuation_handler_with_acc.create acc handler_params ~handler
            ~is_exn_handler ~is_cold)
    in
    match Name_occurrences.count_continuation free_names_of_body cont with
    | Zero when not (Continuation_handler.is_exn_handler handler) ->
      Acc.with_free_names free_names_of_body body_acc, body
    | Zero | One | More_than_one ->
      (* [create_non_recursive] assumes [acc] contains free names of the body *)
      let acc, expr =
        create_non_recursive
          (Acc.with_free_names free_names_of_body acc)
          cont handler ~body ~free_names_of_body ~cost_metrics_of_handler
      in
      Acc.add_free_names handler_free_names acc, expr
end
