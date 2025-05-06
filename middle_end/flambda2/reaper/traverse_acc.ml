(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Graph = Global_flow_graph

type continuation_info =
  { is_exn_handler : bool;
    params : Variable.t list;
    arity : Flambda_kind.With_subkind.t list
  }

module Env = struct
  type cont_kind = Normal of Variable.t list

  type t =
    { parent : Rev_expr.rev_expr_holed;
      conts : cont_kind Continuation.Map.t;
      current_code_id : Code_id.t option;
      le_monde_exterieur : Name.t;
      all_constants : Name.t
    }
end

type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t; (* Dummy variable representing exn return value *)
    is_tupled : bool;
    call_witnesses :
      Code_id_or_name.t list (* One element for each (complex) parameter *)
  }

type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_args : Simple.t list;
    apply_closure : Simple.t option;
    params_of_apply_return_cont : Variable.t list option;
    param_of_apply_exn_cont : Variable.t;
    not_pure_call_witness : Variable.t
  }

type closure_dep =
  { let_bound_name_of_the_closure : Name.t;
    closure_code_id : Code_id.t
  }

type t =
  { mutable code : code_dep Code_id.Map.t;
    mutable apply_deps : apply_dep list;
    mutable set_of_closures_dep : closure_dep list;
    deps : Graph.graph;
    mutable kinds : Flambda_kind.t Name.Map.t;
    mutable fixed_arity_conts : Continuation.Set.t;
    mutable continuation_info : continuation_info Continuation.Map.t
  }

let code_deps t = t.code

let create () =
  { code = Code_id.Map.empty;
    apply_deps = [];
    set_of_closures_dep = [];
    deps = Graph.create ();
    kinds = Name.Map.empty;
    fixed_arity_conts = Continuation.Set.empty;
    continuation_info = Continuation.Map.empty
  }

let kinds t = t.kinds

let kind name k t = t.kinds <- Name.Map.add name k t.kinds

let bound_parameter_kind (bp : Bound_parameter.t) t =
  let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind bp) in
  let name = Name.var (Bound_parameter.var bp) in
  t.kinds <- Name.Map.add name kind t.kinds

let simple_to_name ~all_constants simple =
  Simple.pattern_match' simple
    ~const:(fun _ -> all_constants)
    ~var:(fun v ~coercion:_ -> Name.var v)
    ~symbol:(fun s ~coercion:_ ->
      if Compilation_unit.is_current (Symbol.compilation_unit s)
      then Name.symbol s
      else all_constants)

let alias_kind name simple t =
  let kind =
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        (* Symbols are always values and might not be in t.kinds *)
        if Name.is_symbol name
        then Flambda_kind.value
        else
          match Name.Map.find_opt name t.kinds with
          | Some k -> k
          | None -> Misc.fatal_errorf "Unbound name %a" Name.print name)
      ~const:(fun const ->
        match Int_ids.Const.descr const with
        | Naked_immediate _ -> Flambda_kind.naked_immediate
        | Tagged_immediate _ | Null -> Flambda_kind.value
        | Naked_float _ -> Flambda_kind.naked_float
        | Naked_float32 _ -> Flambda_kind.naked_float32
        | Naked_int32 _ -> Flambda_kind.naked_int32
        | Naked_int64 _ -> Flambda_kind.naked_int64
        | Naked_nativeint _ -> Flambda_kind.naked_nativeint
        | Naked_vec128 _ -> Flambda_kind.naked_vec128)
  in
  t.kinds <- Name.Map.add name kind t.kinds

let add_code code_id dep t = t.code <- Code_id.Map.add code_id dep t.code

let find_code t code_id = Code_id.Map.find code_id t.code

let alias_dep ~(denv : Env.t) pat dep t =
  Graph.add_alias t.deps ~to_:(Code_id_or_name.var pat)
    ~from:
      (Code_id_or_name.name
         (simple_to_name ~all_constants:denv.all_constants dep))

let root v t = Graph.add_use t.deps (Code_id_or_name.var v)

let used ~(denv : Env.t) dep t =
  let name = simple_to_name ~all_constants:denv.all_constants dep in
  match denv.current_code_id with
  | None -> Graph.add_use t.deps (Code_id_or_name.name name)
  | Some code_id ->
    Graph.add_use_dep t.deps
      ~to_:(Code_id_or_name.code_id code_id)
      ~from:(Code_id_or_name.name name)

let used_code_id code_id t =
  Graph.add_use t.deps (Code_id_or_name.code_id code_id)

let called ~(denv : Env.t) code_id t =
  match denv.current_code_id with
  | None -> used_code_id code_id t
  | Some code_id2 ->
    Graph.add_use_dep t.deps
      ~to_:(Code_id_or_name.code_id code_id2)
      ~from:(Code_id_or_name.code_id code_id)

let fixed_arity_continuation t k =
  t.fixed_arity_conts <- Continuation.Set.add k t.fixed_arity_conts

let fixed_arity_continuations t = t.fixed_arity_conts

let continuation_info t k info =
  t.continuation_info <- Continuation.Map.add k info t.continuation_info

let get_continuation_info t = t.continuation_info

let add_apply apply t = t.apply_deps <- apply :: t.apply_deps

let add_set_of_closures_dep let_bound_name_of_the_closure closure_code_id t =
  t.set_of_closures_dep
    <- { let_bound_name_of_the_closure; closure_code_id }
       :: t.set_of_closures_dep

let record_set_of_closure_deps ~get_code_metadata ~le_monde_exterieur t =
  List.iter
    (fun { let_bound_name_of_the_closure = name; closure_code_id = code_id } ->
      match find_code t code_id with
      | exception Not_found ->
        assert (
          not
            (Compilation_unit.is_current (Code_id.get_compilation_unit code_id)));
        (* The code comes from another compilation unit; so we don't know what
           happens once it is applied. As such, it must escape the whole block.
           Besides, return values can be anything. *)
        Graph.add_constructor_dep t.deps
          ~base:(Code_id_or_name.name name)
          Code_of_closure
          ~from:(Code_id_or_name.name name);
        let code_metadata = get_code_metadata code_id in
        let num_returns =
          Flambda_arity.cardinal_unarized
            (Code_metadata.result_arity code_metadata)
        in
        for i = 0 to num_returns - 1 do
          Graph.add_constructor_dep t.deps
            ~base:(Code_id_or_name.name name)
            (Apply (Direct_code_pointer, Normal i))
            ~from:(Code_id_or_name.name le_monde_exterieur);
          Graph.add_constructor_dep t.deps
            ~base:(Code_id_or_name.name name)
            (Apply (Indirect_code_pointer, Normal i))
            ~from:(Code_id_or_name.name le_monde_exterieur)
        done;
        Graph.add_constructor_dep t.deps
          ~base:(Code_id_or_name.name name)
          (Apply (Direct_code_pointer, Exn))
          ~from:(Code_id_or_name.name le_monde_exterieur);
        Graph.add_constructor_dep t.deps
          ~base:(Code_id_or_name.name name)
          (Apply (Indirect_code_pointer, Exn))
          ~from:(Code_id_or_name.name le_monde_exterieur);
        let params_unarized =
          Flambda_arity.unarize_per_parameter
            (Code_metadata.params_arity code_metadata)
        in
        let is_tupled = Code_metadata.is_tupled code_metadata in
        let num_indirect_params =
          if is_tupled then 1 else List.length (List.hd params_unarized)
        in
        let num_direct_params =
          List.length
            (Flambda_arity.unarize (Code_metadata.params_arity code_metadata))
        in
        let always_used = Variable.create "always_used" in
        Graph.add_use t.deps (Code_id_or_name.var always_used);
        for i = 0 to num_direct_params - 1 do
          Graph.add_coconstructor_dep t.deps
            ~base:(Code_id_or_name.name name)
            (Param (Direct_code_pointer, i))
            ~from:(Code_id_or_name.var always_used)
        done;
        for i = 0 to num_indirect_params - 1 do
          Graph.add_coconstructor_dep t.deps
            ~base:(Code_id_or_name.name name)
            (Param (Indirect_code_pointer, i))
            ~from:(Code_id_or_name.var always_used)
        done
      | code_dep ->
        Graph.add_alias t.deps
          ~to_:(Code_id_or_name.var code_dep.my_closure)
          ~from:(Code_id_or_name.name name);
        List.iteri
          (fun i v ->
            Graph.add_constructor_dep t.deps
              ~base:(Code_id_or_name.name name)
              (Apply (Direct_code_pointer, Normal i))
              ~from:(Code_id_or_name.var v))
          code_dep.return;
        List.iteri
          (fun i v ->
            Graph.add_coconstructor_dep t.deps
              ~base:(Code_id_or_name.name name)
              (Param (Direct_code_pointer, i))
              ~from:(Code_id_or_name.var v))
          code_dep.params;
        Graph.add_constructor_dep t.deps
          ~base:(Code_id_or_name.name name)
          (Apply (Direct_code_pointer, Exn))
          ~from:(Code_id_or_name.var code_dep.exn);
        let call_witnesses = code_dep.call_witnesses in
        if code_dep.is_tupled
        then (
          assert (List.compare_length_with call_witnesses 1 = 0);
          let call_witness = List.hd call_witnesses in
          Graph.add_constructor_dep t.deps
            ~base:(Code_id_or_name.name name)
            (Apply (Indirect_code_pointer, Exn))
            ~from:(Code_id_or_name.var code_dep.exn);
          List.iteri
            (fun i return_arg ->
              Graph.add_constructor_dep t.deps
                ~from:(Code_id_or_name.var return_arg)
                (Apply (Indirect_code_pointer, Normal i))
                ~base:(Code_id_or_name.name name))
            code_dep.return;
          Graph.add_constructor_dep t.deps ~from:call_witness Code_of_closure
            ~base:(Code_id_or_name.name name);
          let untuple_var = Variable.create "untuple_var" in
          Graph.add_coconstructor_dep t.deps
            ~from:(Code_id_or_name.var untuple_var)
            (Param (Indirect_code_pointer, 0))
            ~base:(Code_id_or_name.name name);
          List.iteri
            (fun i v ->
              Graph.add_accessor_dep t.deps ~to_:(Code_id_or_name.var v)
                (Block (i, Flambda_kind.value))
                ~base:(Code_id_or_name.var untuple_var))
            code_dep.params)
        else
          let rec add_deps func params_and_witnesses =
            match params_and_witnesses with
            | [] -> Misc.fatal_error "add_deps: no params"
            | (first, witness) :: rest -> (
              List.iteri
                (fun i arg ->
                  Graph.add_coconstructor_dep t.deps
                    ~from:(Code_id_or_name.var arg)
                    (Param (Indirect_code_pointer, i))
                    ~base:func)
                first;
              Graph.add_constructor_dep t.deps ~from:witness Code_of_closure
                ~base:func;
              match rest with
              | [] ->
                Graph.add_constructor_dep t.deps ~base:func
                  (Apply (Indirect_code_pointer, Exn))
                  ~from:(Code_id_or_name.var code_dep.exn);
                List.iteri
                  (fun i return_arg ->
                    Graph.add_constructor_dep t.deps
                      ~from:(Code_id_or_name.var return_arg)
                      (Apply (Indirect_code_pointer, Normal i))
                      ~base:func)
                  code_dep.return
              | _ :: _ ->
                let v = Variable.create "partial_apply" in
                Graph.add_constructor_dep t.deps ~from:(Code_id_or_name.var v)
                  (Apply (Indirect_code_pointer, Normal 0))
                  ~base:func;
                add_deps (Code_id_or_name.var v) rest)
          in
          let params =
            Flambda_arity.group_by_parameter code_dep.arity code_dep.params
          in
          assert (List.compare_lengths call_witnesses params = 0);
          add_deps
            (Code_id_or_name.name name)
            (List.combine params call_witnesses))
    t.set_of_closures_dep

let graph t = t.deps

let deps t ~get_code_metadata ~le_monde_exterieur ~all_constants =
  List.iter
    (fun { function_containing_apply_expr;
           apply_code_id;
           apply_args;
           apply_closure;
           params_of_apply_return_cont;
           param_of_apply_exn_cont;
           not_pure_call_witness
         } ->
      let code_dep = find_code t apply_code_id in
      Graph.add_alias t.deps
        ~from:(List.hd code_dep.call_witnesses)
        ~to_:(Code_id_or_name.var not_pure_call_witness);
      let add_cond_dep param name =
        let param = Name.var param in
        match function_containing_apply_expr with
        | None ->
          Graph.add_alias t.deps
            ~to_:(Code_id_or_name.name param)
            ~from:(Code_id_or_name.name name)
        | Some code_id ->
          Graph.add_propagate_dep t.deps
            ~if_used:(Code_id_or_name.code_id code_id)
            ~from:(Code_id_or_name.name name)
            ~to_:(Code_id_or_name.name param)
      in
      List.iter2
        (fun param arg ->
          add_cond_dep param (simple_to_name ~all_constants arg))
        code_dep.params apply_args;
      (match apply_closure with
      | None -> add_cond_dep code_dep.my_closure le_monde_exterieur
      | Some apply_closure ->
        add_cond_dep code_dep.my_closure
          (simple_to_name ~all_constants apply_closure));
      (match params_of_apply_return_cont with
      | None -> ()
      | Some apply_return ->
        List.iter2
          (fun arg param -> add_cond_dep param (Name.var arg))
          code_dep.return apply_return);
      add_cond_dep param_of_apply_exn_cont (Name.var code_dep.exn))
    t.apply_deps;
  record_set_of_closure_deps ~get_code_metadata ~le_monde_exterieur t;
  t.deps
