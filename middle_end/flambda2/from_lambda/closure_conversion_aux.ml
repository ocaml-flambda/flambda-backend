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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module IR = struct
  type simple =
    | Var of Ident.t
    | Const of Lambda.structured_constant

  type exn_continuation =
    { exn_handler : Continuation.t;
      extra_args : (simple * Lambda.value_kind) list
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
    | Prim of
        { prim : Lambda.primitive;
          args : simple list;
          loc : Lambda.scoped_location;
          exn_continuation : exn_continuation option
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
      tailcall : Lambda.tailcall_attribute;
      inlined : Lambda.inline_attribute;
      specialised : Lambda.specialise_attribute;
      probe : Lambda.probe
    }

  type switch =
    { numconsts : int;
      consts : (int * Continuation.t * trap_action option * simple list) list;
      failaction : (Continuation.t * trap_action option * simple list) option
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
    | Prim { prim; args; _ } ->
      fprintf ppf "@[<2>(%a %a)@]" Printlambda.primitive prim
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple)
        args
end

module Env = struct
  type t =
    { variables : Variable.t Ident.Map.t;
      globals : Symbol.t Numeric_types.Int.Map.t;
      simples_to_substitute : Simple.t Ident.Map.t;
      backend : (module Flambda_backend_intf.S);
      current_unit_id : Ident.t;
      symbol_for_global' : Ident.t -> Symbol.t
    }

  let backend t = t.backend

  let current_unit_id t = t.current_unit_id

  let symbol_for_global' t = t.symbol_for_global'

  let empty ~backend =
    let module Backend = (val backend : Flambda_backend_intf.S) in
    let compilation_unit = Compilation_unit.get_current_exn () in
    { variables = Ident.Map.empty;
      globals = Numeric_types.Int.Map.empty;
      simples_to_substitute = Ident.Map.empty;
      backend;
      current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global'
    }

  let clear_local_bindings
      { variables = _;
        globals;
        simples_to_substitute;
        backend;
        current_unit_id;
        symbol_for_global'
      } =
    let simples_to_substitute =
      Ident.Map.filter
        (fun _ simple -> not (Simple.is_var simple))
        simples_to_substitute
    in
    { variables = Ident.Map.empty;
      globals;
      simples_to_substitute;
      backend;
      current_unit_id;
      symbol_for_global'
    }

  let add_var t id var = { t with variables = Ident.Map.add id var t.variables }

  let add_vars t ids vars = List.fold_left2 add_var t ids vars

  let add_var_map t map =
    { t with variables = Ident.Map.union_right t.variables map }

  let add_var_like t id (user_visible : IR.user_visible) =
    let user_visible =
      match user_visible with
      | Not_user_visible -> None
      | User_visible -> Some ()
    in
    let var = Variable.create_with_same_name_as_ident ?user_visible id in
    add_var t id var, var

  let add_vars_like t ids =
    let vars =
      List.map
        (fun (id, (user_visible : IR.user_visible)) ->
          let user_visible =
            match user_visible with
            | Not_user_visible -> None
            | User_visible -> Some ()
          in
          Variable.create_with_same_name_as_ident ?user_visible id)
        ids
    in
    add_vars t (List.map fst ids) vars, vars

  (* CR mshinwell: Rethink the semantics of these re. fatal errors etc *)

  let find_var t id =
    try Ident.Map.find id t.variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let find_var_exn t id = Ident.Map.find id t.variables

  let find_name t id = Name.var (find_var t id)

  let find_name_exn t id = Name.var (find_var_exn t id)

  let find_vars t ids = List.map (fun id -> find_var t id) ids

  let add_global t pos symbol =
    { t with globals = Numeric_types.Int.Map.add pos symbol t.globals }

  let find_global t pos =
    try Numeric_types.Int.Map.find pos t.globals
    with Not_found ->
      Misc.fatal_error
        ("Closure_conversion.Env.find_global: global " ^ string_of_int pos)

  let add_simple_to_substitute t id simple =
    if Ident.Map.mem id t.simples_to_substitute
    then
      Misc.fatal_errorf "Cannot redefine [Simple] associated with %a"
        Ident.print id;
    { t with
      simples_to_substitute = Ident.Map.add id simple t.simples_to_substitute
    }

  let add_simple_to_substitute_map t map =
    { t with
      simples_to_substitute =
        Ident.Map.disjoint_union map t.simples_to_substitute
    }

  let find_simple_to_substitute_exn t id =
    Ident.Map.find id t.simples_to_substitute
end

module Acc = struct
  type t =
    { declared_symbols : (Symbol.t * Flambda.Static_const.t) list;
      shareable_constants : Symbol.t Flambda.Static_const.Map.t;
      code : Flambda.Code.t Code_id.Map.t;
      free_names : Name_occurrences.t;
      cost_metrics : Flambda.Cost_metrics.t;
      seen_a_function : bool
    }

  let cost_metrics t = t.cost_metrics

  let increment_metrics metrics t =
    { t with cost_metrics = Flambda.Cost_metrics.( + ) t.cost_metrics metrics }

  let with_cost_metrics cost_metrics t = { t with cost_metrics }

  let seen_a_function t = t.seen_a_function

  let with_seen_a_function t seen_a_function = { t with seen_a_function }

  let empty =
    { declared_symbols = [];
      shareable_constants = Flambda.Static_const.Map.empty;
      code = Code_id.Map.empty;
      free_names = Name_occurrences.empty;
      cost_metrics = Flambda.Cost_metrics.zero;
      seen_a_function = false
    }

  let declared_symbols t = t.declared_symbols

  let shareable_constants t = t.shareable_constants

  let code t = t.code

  let free_names t = t.free_names

  let add_declared_symbol ~symbol ~constant t =
    let declared_symbols = (symbol, constant) :: t.declared_symbols in
    { t with declared_symbols }

  let add_shareable_constant ~symbol ~constant t =
    let shareable_constants =
      Flambda.Static_const.Map.add constant symbol t.shareable_constants
    in
    { t with shareable_constants }

  let add_code ~code_id ~code t =
    { t with code = Code_id.Map.add code_id code t.code }

  let add_free_names free_names t =
    { t with free_names = Name_occurrences.union free_names t.free_names }

  let add_name_to_free_names ~name t =
    { t with
      free_names = Name_occurrences.add_name t.free_names name Name_mode.normal
    }

  let add_simple_to_free_names acc simple =
    Simple.pattern_match simple
      ~const:(fun _ -> acc)
      ~name:(fun name ~coercion:_ -> add_name_to_free_names ~name acc)

  let remove_code_id_or_symbol_from_free_names cis t =
    { t with
      free_names = Name_occurrences.remove_code_id_or_symbol t.free_names cis
    }

  let remove_symbol_from_free_names symbol t =
    remove_code_id_or_symbol_from_free_names (Symbol symbol) t

  let remove_var_from_free_names var t =
    { t with free_names = Name_occurrences.remove_var t.free_names var }

  let remove_continuation_from_free_names cont t =
    { t with
      free_names = Name_occurrences.remove_continuation t.free_names cont
    }

  let remove_code_id_from_free_names code_id t =
    remove_code_id_or_symbol_from_free_names (Code_id code_id) t

  let with_free_names free_names t = { t with free_names }

  let eval_branch_free_names t ~f =
    let base_free_names = t.free_names in
    let t, res = f { t with free_names = Name_occurrences.empty } in
    t.free_names, { t with free_names = base_free_names }, res

  let measure_cost_metrics acc ~f =
    let saved_cost_metrics = cost_metrics acc in
    let acc = with_cost_metrics Flambda.Cost_metrics.zero acc in
    let free_names, acc, return = eval_branch_free_names acc ~f in
    let cost_metrics = cost_metrics acc in
    cost_metrics, free_names, with_cost_metrics saved_cost_metrics acc, return
end

module Function_decls = struct
  module Function_decl = struct
    type t =
      { let_rec_ident : Ident.t;
        closure_id : Closure_id.t;
        kind : Lambda.function_kind;
        params : (Ident.t * Lambda.value_kind) list;
        return : Lambda.value_kind;
        return_continuation : Continuation.t;
        exn_continuation : IR.exn_continuation;
        body : Acc.t -> Env.t -> Acc.t * Flambda.Import.Expr.t;
        free_idents_of_body : Ident.Set.t;
        attr : Lambda.function_attribute;
        loc : Lambda.scoped_location;
        stub : bool;
        recursive : Recursive.t
      }

    let create ~let_rec_ident ~closure_id ~kind ~params ~return
        ~return_continuation ~exn_continuation ~body ~attr ~loc
        ~free_idents_of_body ~stub recursive =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create_local "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        closure_id;
        kind;
        params;
        return;
        return_continuation;
        exn_continuation;
        body;
        free_idents_of_body;
        attr;
        loc;
        stub;
        recursive
      }

    let let_rec_ident t = t.let_rec_ident

    let closure_id t = t.closure_id

    let kind t = t.kind

    let params t = t.params

    let return t = t.return

    let return_continuation t = t.return_continuation

    let exn_continuation t = t.exn_continuation

    let body t = t.body

    let free_idents t = t.free_idents_of_body

    let inline t = t.attr.inline

    let specialise t = t.attr.specialise

    let is_a_functor t = t.attr.is_a_functor

    let stub t = t.attr.stub

    let loc t = t.loc

    let recursive t = t.recursive
  end

  type t =
    { function_decls : Function_decl.t list;
      all_free_idents : Ident.Set.t
    }

  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let free_idents_by_function function_decls =
    List.fold_right
      (fun decl map ->
        Closure_id.Map.add
          (Function_decl.closure_id decl)
          (Function_decl.free_idents decl)
          map)
      function_decls Closure_id.Map.empty

  let all_free_idents function_decls =
    Closure_id.Map.fold
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
         (List.map fst (all_params function_decls)))
      (let_rec_idents function_decls)

  let create function_decls =
    { function_decls; all_free_idents = all_free_idents function_decls }

  let to_list t = t.function_decls

  let all_free_idents t = t.all_free_idents
end

open Flambda.Import

module Expr_with_acc = struct
  type t = Expr.t

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
    let acc = Acc.add_free_names (Apply_expr.free_names apply) acc in
    acc, Expr.create_apply apply

  let create_let (acc, let_expr) =
    (* The signature for create_let is a bit different. It is mainly used to
       materialize expressions coming from Let_cont_with_acc where the cost
       metrics were already computed. The signature is such that results from
       Let_cont_with_acc can be directly piped through [create_let].*)
    acc, Expr.create_let let_expr

  let create_switch acc switch =
    let acc =
      Acc.increment_metrics
        (Code_size.switch switch |> Cost_metrics.from_size)
        acc
    in
    let acc = Acc.add_simple_to_free_names acc (Switch_expr.scrutinee switch) in
    acc, Expr.create_switch switch

  let create_invalid acc ?semantics () =
    let acc =
      Acc.increment_metrics (Code_size.invalid |> Cost_metrics.from_size) acc
    in
    acc, Expr.create_invalid ?semantics ()
end

module Apply_cont_with_acc = struct
  let create acc ?trap_action cont ~args ~dbg =
    let apply_cont = Apply_cont.create ?trap_action cont ~args ~dbg in
    let acc = Acc.add_free_names (Apply_cont.free_names apply_cont) acc in
    acc, apply_cont

  let goto acc cont = create acc cont ~args:[] ~dbg:Debuginfo.none
end

module Let_with_acc = struct
  let create acc let_bound named ~body =
    let cost_metrics_of_defining_expr =
      match named with
      | Named.Prim (prim, _) -> Code_size.prim prim |> Cost_metrics.from_size
      | Named.Simple simple -> Code_size.simple simple |> Cost_metrics.from_size
      | Named.Static_consts _consts -> Cost_metrics.zero
      | Named.Set_of_closures set_of_closures ->
        let code_mapping = Acc.code acc in
        Cost_metrics.set_of_closures
          ~find_code_characteristics:(fun code_id ->
            let code = Code_id.Map.find code_id code_mapping in
            { cost_metrics = Code.cost_metrics code;
              params_arity = List.length (Code.params_arity code)
            })
          set_of_closures
      | Named.Rec_info _ -> Cost_metrics.zero
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
    let expr = Let.create let_bound named ~body ~free_names_of_body in
    let acc = Acc.add_free_names (Named.free_names named) acc in
    acc, expr
end

module Continuation_handler_with_acc = struct
  let create acc parameters ~handler ~is_exn_handler =
    let free_names_of_handler = Or_unknown.Known (Acc.free_names acc) in
    let acc =
      List.fold_left
        (fun acc param ->
          Acc.remove_var_from_free_names (Kinded_parameter.var param) acc)
        acc parameters
    in
    ( acc,
      Continuation_handler.create parameters ~handler ~free_names_of_handler
        ~is_exn_handler )
end

module Let_cont_with_acc = struct
  let create_non_recursive acc cont handler ~body ~free_names_of_body
      ~cost_metrics_of_handler =
    match Name_occurrences.count_continuation free_names_of_body cont with
    | Zero when not (Continuation_handler.is_exn_handler handler) -> acc, body
    | _ ->
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

  let create_recursive acc handlers ~body ~cost_metrics_of_handlers =
    let acc =
      Acc.increment_metrics
        (Cost_metrics.increase_due_to_let_cont_recursive
           ~cost_metrics_of_handlers)
        acc
    in
    let expr = Let_cont.create_recursive handlers ~body in
    let acc =
      Continuation.Map.fold
        (fun cont _ acc -> Acc.remove_continuation_from_free_names cont acc)
        handlers acc
    in
    acc, expr

  let build_recursive acc ~handlers ~body =
    let handlers_free_names, cost_metrics_of_handlers, acc, handlers =
      Continuation.Map.fold
        (fun cont (handler, params, is_exn_handler)
             (free_names, costs, acc, handlers) ->
          let cost_metrics_of_handler, handler_free_names, acc, handler =
            Acc.measure_cost_metrics acc ~f:(fun acc ->
                let acc, handler = handler acc in
                Continuation_handler_with_acc.create acc params ~handler
                  ~is_exn_handler)
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
        (Name_occurrences.union body_free_names handlers_free_names)
        acc
    in
    create_recursive acc handlers ~body ~cost_metrics_of_handlers

  let build_non_recursive acc cont ~handler_params ~handler ~body
      ~is_exn_handler =
    let cost_metrics_of_handler, handler_free_names, acc, handler =
      Acc.measure_cost_metrics acc ~f:(fun acc ->
          let acc, handler = handler acc in
          Continuation_handler_with_acc.create acc handler_params ~handler
            ~is_exn_handler)
    in
    let free_names_of_body, acc, body =
      Acc.eval_branch_free_names acc ~f:body
    in
    let acc, expr =
      create_non_recursive
        (Acc.with_free_names free_names_of_body acc)
        cont handler ~body ~free_names_of_body ~cost_metrics_of_handler
    in
    Acc.add_free_names handler_free_names acc, expr
end
