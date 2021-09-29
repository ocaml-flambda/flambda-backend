[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR lmaurer: Get rid of -4 and fix the fragile matches. *)

let map_accum_left f env l =
  let next (acc, env) x =
    let y, env = f env x in
    y :: acc, env
  in
  let acc, env = List.fold_left next ([], env) l in
  List.rev acc, env

(* Continuation variables *)
module C = struct
  type t = string

  let compare = String.compare
end

module CM = Map.Make (C)

(* Variables *)
module V = struct
  type t = string

  let compare = String.compare
end

module VM = Map.Make (V)

(* Symbols *)
module S = struct
  type t = string (* only storing local symbols so only need the name *)

  let compare = String.compare
end

module SM = Map.Make (S)

(* Code ids *)
module D = struct
  type t = string

  let compare = String.compare
end

module DM = Map.Make (D)

(* Closure ids (globally scoped, so updates are in-place) *)
module U = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module UT = Hashtbl.Make (U)

(* Variables within closures (globally scoped, so updates are in-place) *)
module W = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module WT = Hashtbl.Make (W)

type env =
  { done_continuation : Continuation.t;
    error_continuation : Exn_continuation.t;
    continuations : (Continuation.t * int) CM.t;
    exn_continuations : Exn_continuation.t CM.t;
    variables : Variable.t VM.t;
    symbols : Symbol.t SM.t;
    code_ids : Code_id.t DM.t;
    closure_ids : Closure_id.t UT.t;
    vars_within_closures : Var_within_closure.t WT.t
  }

let init_env () =
  let done_continuation =
    Continuation.create ~sort:Toplevel_return ~name:"done" ()
  in
  let exn_handler = Continuation.create ~name:"error" () in
  let error_continuation =
    Exn_continuation.create ~exn_handler ~extra_args:[]
  in
  { done_continuation;
    error_continuation;
    continuations = CM.empty;
    exn_continuations = CM.empty;
    variables = VM.empty;
    symbols = SM.empty;
    code_ids = DM.empty;
    closure_ids = UT.create 10;
    vars_within_closures = WT.create 10
  }

let enter_code env =
  { continuations = CM.empty;
    exn_continuations = CM.empty;
    variables = env.variables;
    done_continuation = env.done_continuation;
    error_continuation = env.error_continuation;
    symbols = env.symbols;
    code_ids = env.code_ids;
    closure_ids = env.closure_ids;
    vars_within_closures = env.vars_within_closures
  }

let fresh_cont env { Fexpr.txt = name; loc = _ } ~sort ~arity =
  let c = Continuation.create ~sort ~name () in
  c, { env with continuations = CM.add name (c, arity) env.continuations }

let fresh_exn_cont env { Fexpr.txt = name; loc = _ } =
  let c = Continuation.create ~name () in
  let e = Exn_continuation.create ~exn_handler:c ~extra_args:[] in
  ( e,
    { env with
      continuations = CM.add name (c, 1) env.continuations;
      exn_continuations = CM.add name e env.exn_continuations
    } )

let fresh_var env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name ~user_visible:() in
  v, { env with variables = VM.add name v env.variables }

let fresh_code_id env { Fexpr.txt = name; loc = _ } =
  let c = Code_id.create ~name (Compilation_unit.get_current_exn ()) in
  c, { env with code_ids = DM.add name c env.code_ids }

let fresh_closure_id env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Closure_id.wrap (Compilation_unit.get_current_exn ()) v in
  UT.add env.closure_ids name c;
  c

let fresh_or_existing_closure_id env ({ Fexpr.txt = name; loc = _ } as id) =
  match UT.find_opt env.closure_ids name with
  | None -> fresh_closure_id env id
  | Some closure_id -> closure_id

let fresh_var_within_closure env { Fexpr.txt = name; loc = _ } =
  let v = Variable.create name in
  let c = Var_within_closure.wrap (Compilation_unit.get_current_exn ()) v in
  WT.add env.vars_within_closures name c;
  c

let fresh_or_existing_var_within_closure env ({ Fexpr.txt = name; _ } as id) =
  match WT.find_opt env.vars_within_closures name with
  | None -> fresh_var_within_closure env id
  | Some var_within_closure -> var_within_closure

let print_scoped_location ppf loc =
  match (loc : Lambda.scoped_location) with
  | Loc_unknown -> Format.pp_print_string ppf "Unknown"
  | Loc_known { loc; _ } -> Location.print_loc ppf loc

let declare_symbol (env : env) ({ Fexpr.txt = cu, name; loc } as symbol) =
  if Option.is_some cu
  then
    Misc.fatal_errorf "Cannot declare non-local symbol %a: %a"
      Print_fexpr.symbol symbol print_scoped_location loc
  else if SM.mem name env.symbols
  then
    Misc.fatal_errorf "Redefinition of symbol %a: %a" Print_fexpr.symbol symbol
      print_scoped_location loc
  else
    let cunit =
      match cu with
      | None -> Compilation_unit.get_current_exn ()
      | Some { Fexpr.ident; linkage_name } ->
        let linkage_name = linkage_name |> Option.value ~default:ident in
        Compilation_unit.create
          (Ident.create_persistent ident)
          (Linkage_name.create linkage_name)
    in
    let symbol = Symbol.unsafe_create cunit (Linkage_name.create name) in
    symbol, { env with symbols = SM.add name symbol env.symbols }

let find_with ~descr ~find map { Fexpr.txt = name; loc } =
  match find name map with
  | None ->
    Misc.fatal_errorf "Unbound %s %s: %a" descr name print_scoped_location loc
  | Some a -> a

let get_symbol (env : env) sym =
  match sym with
  | { Fexpr.txt = Some cunit, name; loc = _ } ->
    let cunit =
      let { Fexpr.ident; linkage_name } = cunit in
      Compilation_unit.create
        (Ident.create_persistent ident)
        (Linkage_name.create (linkage_name |> Option.value ~default:ident))
    in
    Symbol.unsafe_create cunit (name |> Linkage_name.create)
  | { Fexpr.txt = None, txt; loc } ->
    find_with ~descr:"symbol" ~find:SM.find_opt env.symbols { txt; loc }

let find_cont_id env c =
  find_with ~descr:"continuation id" ~find:CM.find_opt env.continuations c

let find_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> env.done_continuation, 1
  | Special Error -> Exn_continuation.exn_handler env.error_continuation, 1
  | Named cont_id -> find_cont_id env cont_id

let find_result_cont env (c : Fexpr.result_continuation) :
    Apply_expr.Result_continuation.t =
  match c with
  | Return c -> Return (fst (find_cont env c))
  | Never_returns -> Never_returns

let find_exn_cont_id env c =
  find_with ~descr:"exn_continuation" ~find:CM.find_opt env.exn_continuations c

let find_exn_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> Misc.fatal_error "done is not an exception continuation"
  | Special Error -> env.error_continuation
  | Named cont_id -> find_exn_cont_id env cont_id

let find_var env v =
  find_with ~descr:"variable" ~find:VM.find_opt env.variables v

let find_code_id env code_id =
  find_with ~descr:"code id" ~find:DM.find_opt env.code_ids code_id

let targetint (i : Fexpr.targetint) : Targetint_32_64.t =
  Targetint_32_64.of_int64 i

let immediate i =
  i |> Targetint_32_64.of_string |> Targetint_31_63.Imm.of_targetint
  |> Targetint_31_63.int

let float f = f |> Numeric_types.Float_by_bit_pattern.create

let rec value_kind_with_subkind (k : Fexpr.kind_with_subkind) :
    Flambda_kind.With_subkind.t =
  let module KWS = Flambda_kind.With_subkind in
  match k with
  | Any_value -> KWS.any_value
  | Block { tag; fields } ->
    KWS.block tag (List.map value_kind_with_subkind fields)
  | Float_block { num_fields } -> KWS.float_block ~num_fields
  | Naked_number naked_number_kind -> begin
    match naked_number_kind with
    | Naked_immediate -> KWS.naked_immediate
    | Naked_float -> KWS.naked_float
    | Naked_int32 -> KWS.naked_int32
    | Naked_int64 -> KWS.naked_int64
    | Naked_nativeint -> KWS.naked_nativeint
  end
  | Boxed_float -> KWS.boxed_float
  | Boxed_int32 -> KWS.boxed_int32
  | Boxed_int64 -> KWS.boxed_int64
  | Boxed_nativeint -> KWS.boxed_nativeint
  | Tagged_immediate -> KWS.tagged_immediate
  | Rec_info -> KWS.rec_info

let value_kind : Fexpr.kind -> Flambda_kind.t = function
  | Value -> Flambda_kind.value
  | Naked_number naked_number_kind -> begin
    match naked_number_kind with
    | Naked_immediate -> Flambda_kind.naked_immediate
    | Naked_float -> Flambda_kind.naked_float
    | Naked_int32 -> Flambda_kind.naked_int32
    | Naked_int64 -> Flambda_kind.naked_int64
    | Naked_nativeint -> Flambda_kind.naked_nativeint
  end
  | Fabricated -> Misc.fatal_error "Fabricated should not be used"
  | Rec_info -> Flambda_kind.rec_info

let value_kind_with_subkind_opt :
    Fexpr.kind_with_subkind option -> Flambda_kind.With_subkind.t = function
  | Some kind -> value_kind_with_subkind kind
  | None -> Flambda_kind.With_subkind.any_value

let arity a =
  Flambda_arity.With_subkinds.create (List.map value_kind_with_subkind a)

let const (c : Fexpr.const) : Reg_width_const.t =
  match c with
  | Tagged_immediate i -> Reg_width_const.tagged_immediate (i |> immediate)
  | Naked_immediate i -> Reg_width_const.naked_immediate (i |> immediate)
  | Naked_float f -> Reg_width_const.naked_float (f |> float)
  | Naked_int32 i -> Reg_width_const.naked_int32 i
  | Naked_int64 i -> Reg_width_const.naked_int64 i
  | Naked_nativeint i -> Reg_width_const.naked_nativeint (i |> targetint)

let rec rec_info env (ri : Fexpr.rec_info) : Rec_info_expr.t =
  let module US = Rec_info_expr.Unrolling_state in
  match ri with
  | Depth d -> Rec_info_expr.const ~depth:(Finite d) ~unrolling:US.not_unrolling
  | Infinity -> Rec_info_expr.const ~depth:Infinity ~unrolling:US.not_unrolling
  | Do_not_inline -> Rec_info_expr.do_not_inline
  | Var dv -> Rec_info_expr.var (find_var env dv)
  | Succ ri -> Rec_info_expr.succ (rec_info env ri)
  | Unroll (u, Depth d) ->
    Rec_info_expr.const ~depth:(Finite d)
      ~unrolling:(US.unrolling ~remaining_depth:u)
  | Unroll (u, Infinity) ->
    Rec_info_expr.const ~depth:Infinity
      ~unrolling:(US.unrolling ~remaining_depth:u)
  | Unroll (d, ri) -> Rec_info_expr.unroll_to d (rec_info env ri)

let coercion env (co : Fexpr.coercion) : Coercion.t =
  match co with
  | Id -> Coercion.id
  | Change_depth { from; to_ } ->
    Coercion.change_depth ~from:(rec_info env from) ~to_:(rec_info env to_)

let rec simple env (s : Fexpr.simple) : Simple.t =
  match s with
  | Var { txt = v; loc } -> begin
    match VM.find_opt v env.variables with
    | None ->
      Misc.fatal_errorf "Unbound variable %s : %a" v print_scoped_location loc
    | Some var -> Simple.var var
  end
  | Const c -> Simple.const (const c)
  | Symbol sym -> Simple.symbol (get_symbol env sym)
  | Coerce (s, co) -> Simple.apply_coercion_exn (simple env s) (coercion env co)

let name env (s : Fexpr.name) : Name.t =
  match s with
  | Var { txt = v; loc } -> begin
    match VM.find_opt v env.variables with
    | None ->
      Misc.fatal_errorf "Unbound variable %s : %a" v print_scoped_location loc
    | Some var -> Name.var var
  end
  | Symbol sym -> Name.symbol (get_symbol env sym)

let field_of_block env (v : Fexpr.field_of_block) :
    Flambda.Static_const.Field_of_block.t =
  match v with
  | Symbol s -> Symbol (get_symbol env s)
  | Tagged_immediate i ->
    let i = Targetint_32_64.of_string i in
    Tagged_immediate (Targetint_31_63.int (Targetint_31_63.Imm.of_targetint i))
  | Dynamically_computed var ->
    let var = find_var env var in
    Dynamically_computed var

let or_variable f env (ov : _ Fexpr.or_variable) : _ Or_variable.t =
  match ov with Const c -> Const (f c) | Var v -> Var (find_var env v)

let unop env (unop : Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Array_length ak -> Array_length ak
  | Box_number bk -> Box_number bk
  | Unbox_number bk -> Unbox_number bk
  | Get_tag -> Get_tag
  | Is_int -> Is_int
  | Num_conv { src; dst } -> Num_conv { src; dst }
  | Opaque_identity -> Opaque_identity
  | Project_var { project_from; var } ->
    let var = fresh_or_existing_var_within_closure env var in
    let project_from = fresh_or_existing_closure_id env project_from in
    Project_var { project_from; var }
  | Select_closure { move_from; move_to } ->
    let move_from = fresh_or_existing_closure_id env move_from in
    let move_to = fresh_or_existing_closure_id env move_to in
    Select_closure { move_from; move_to }
  | String_length string_or_bytes -> String_length string_or_bytes

let infix_binop (binop : Fexpr.infix_binop) : Flambda_primitive.binary_primitive
    =
  match binop with
  | Int_arith o -> Int_arith (Tagged_immediate, o)
  | Int_comp c -> Int_comp (Tagged_immediate, Signed, c)
  | Int_shift s -> Int_shift (Tagged_immediate, s)
  | Float_arith o -> Float_arith o
  | Float_comp c -> Float_comp c

let binop (binop : Fexpr.binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Array_load (ak, mut) -> Array_load (ak, mut)
  | Block_load (access_kind, mutability) ->
    let size s : _ Or_unknown.t =
      match s with
      | None -> Unknown
      | Some s -> Known (s |> Targetint_31_63.Imm.of_int64)
    in
    let access_kind : Flambda_primitive.Block_access_kind.t =
      match access_kind with
      | Values { field_kind; tag; size = s } ->
        let tag = tag |> Tag.Scannable.create_exn in
        let size = size s in
        (* CR mshinwell: add support for "Unknown" tags *)
        Values { field_kind; tag = Known tag; size }
      | Naked_floats { size = s } ->
        let size = size s in
        Naked_floats { size }
    in
    Block_load (access_kind, mutability)
  | Phys_equal (kind, op) ->
    let kind =
      value_kind (kind |> Option.value ~default:(Value : Fexpr.kind))
    in
    Phys_equal (kind, op)
  | Infix op -> infix_binop op
  | Int_arith (i, o) -> Int_arith (i, o)
  | Int_comp (i, s, c) -> Int_comp (i, s, c)
  | Int_shift (i, s) -> Int_shift (i, s)

let ternop (ternop : Fexpr.ternop) : Flambda_primitive.ternary_primitive =
  match ternop with Array_set (ak, ia) -> Array_set (ak, ia)

let convert_block_shape ~num_fields =
  List.init num_fields
    (fun _field : Flambda_primitive.Block_of_values_field.t -> Any_value)

let varop (varop : Fexpr.varop) n : Flambda_primitive.variadic_primitive =
  match varop with
  | Make_block (tag, mutability) ->
    let shape = convert_block_shape ~num_fields:n in
    let kind : Flambda_primitive.Block_kind.t =
      Values (Tag.Scannable.create_exn tag, shape)
    in
    Make_block (kind, mutability)

let prim env (p : Fexpr.prim) : Flambda_primitive.t =
  match p with
  | Unary (op, arg) -> Unary (unop env op, simple env arg)
  | Binary (op, a1, a2) -> Binary (binop op, simple env a1, simple env a2)
  | Ternary (op, a1, a2, a3) ->
    Ternary (ternop op, simple env a1, simple env a2, simple env a3)
  | Variadic (op, args) ->
    Variadic (varop op (List.length args), List.map (simple env) args)

let convert_recursive_flag (flag : Fexpr.is_recursive) : Recursive.t =
  match flag with Recursive -> Recursive | Nonrecursive -> Non_recursive

let defining_expr env (named : Fexpr.named) : Flambda.Named.t =
  match named with
  | Simple s -> Flambda.Named.create_simple (simple env s)
  | Prim p ->
    let p = prim env p in
    Flambda.Named.create_prim p Debuginfo.none
  | Rec_info ri ->
    let ri = rec_info env ri in
    Flambda.Named.create_rec_info ri
  | _ -> assert false

let set_of_closures env fun_decls closure_elements =
  let fun_decls : Function_declarations.t =
    let translate_fun_decl (fun_decl : Fexpr.fun_decl) :
        Closure_id.t * Code_id.t =
      let code_id = find_code_id env fun_decl.code_id in
      let closure_id =
        (* By default, pun the code id as the closure id *)
        fun_decl.closure_id |> Option.value ~default:fun_decl.code_id
      in
      let closure_id = fresh_or_existing_closure_id env closure_id in
      closure_id, code_id
    in
    List.map translate_fun_decl fun_decls
    |> Closure_id.Lmap.of_list |> Function_declarations.create
  in
  let closure_elements = Option.value closure_elements ~default:[] in
  let closure_elements : Simple.t Var_within_closure.Map.t =
    let convert ({ var; value } : Fexpr.closure_element) =
      fresh_or_existing_var_within_closure env var, simple env value
    in
    List.map convert closure_elements |> Var_within_closure.Map.of_list
  in
  Set_of_closures.create fun_decls ~closure_elements

let apply_cont env ({ cont; args; trap_action } : Fexpr.apply_cont) =
  let trap_action : Trap_action.t option =
    trap_action
    |> Option.map (fun (ta : Fexpr.trap_action) : Trap_action.t ->
           match ta with
           | Push { exn_handler } ->
             let exn_handler, _ = find_cont env exn_handler in
             Push { exn_handler }
           | Pop { exn_handler; raise_kind } ->
             let exn_handler, _ = find_cont env exn_handler in
             Pop { exn_handler; raise_kind })
  in
  let c, arity = find_cont env cont in
  (if List.length args <> arity
  then
    let cont_str =
      match cont with
      | Special Done -> "done"
      | Special Error -> "error"
      | Named { txt = cont_id; _ } -> cont_id
    in
    Misc.fatal_errorf "wrong continuation arity %s" cont_str);
  let args = List.map (simple env) args in
  Flambda.Apply_cont.create c ~args ~dbg:Debuginfo.none ?trap_action

let continuation_sort (sort : Fexpr.continuation_sort) : Continuation.Sort.t =
  match sort with
  | Normal -> Normal_or_exn
  | Exn -> Normal_or_exn
  | Define_root_symbol -> Define_root_symbol

let rec expr env (e : Fexpr.expr) : Flambda.Expr.t =
  match e with
  | Let { bindings = []; _ } -> assert false (* should not be possible *)
  | Let
      { bindings = { defining_expr = Closure _; _ } :: _ as bindings;
        closure_elements;
        body
      } ->
    let binding_to_var_and_closure_binding : Fexpr.let_binding -> _ = function
      | { var; defining_expr = Closure binding; _ } -> var, binding
      | { var = { txt = _; loc }; _ } ->
        Misc.fatal_errorf "Cannot use 'and' with non-closure: %a"
          print_scoped_location loc
    in
    let vars_and_closure_bindings =
      List.map binding_to_var_and_closure_binding bindings
    in
    let closure_vars, env =
      let convert_binding env (var, _) : Bound_var.t * env =
        let var, env = fresh_var env var in
        let var = Bound_var.create var Name_mode.normal in
        var, env
      in
      map_accum_left convert_binding env vars_and_closure_bindings
    in
    let bound = Bound_pattern.set_of_closures ~closure_vars in
    let named =
      let closure_bindings = List.map snd vars_and_closure_bindings in
      set_of_closures env closure_bindings closure_elements
      |> Flambda.Named.create_set_of_closures
    in
    let body = expr env body in
    Flambda.Let.create bound named ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Let { bindings = _ :: _ :: _; _ } ->
    Misc.fatal_errorf
      "Multiple let bindings only allowed when defining closures"
  | Let { closure_elements = Some _; _ } ->
    Misc.fatal_errorf "'with' clause only allowed when defining closures"
  | Let
      { bindings = [{ var; defining_expr = d }]; body; closure_elements = None }
    ->
    let named = defining_expr env d in
    let id, env = fresh_var env var in
    let body = expr env body in
    let var = Bound_var.create id Name_mode.normal in
    let bound = Bound_pattern.singleton var in
    Flambda.Let.create bound named ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Let_cont { recursive; body; bindings = [{ name; params; sort; handler }] }
    -> begin
    let sort =
      sort |> Option.value ~default:(Normal : Fexpr.continuation_sort)
    in
    let is_exn_handler = match sort with Exn -> true | _ -> false in
    let sort = continuation_sort sort in
    let name, body_env =
      fresh_cont env name ~sort ~arity:(List.length params)
    in
    let body = expr body_env body in
    let env =
      match recursive with Nonrecursive -> env | Recursive -> body_env
    in
    let handler_env, params =
      List.fold_right
        (fun ({ param; kind } : Fexpr.kinded_parameter) (env, args) ->
          let var, env = fresh_var env param in
          let param =
            Kinded_parameter.create var (value_kind_with_subkind_opt kind)
          in
          env, param :: args)
        params (env, [])
    in
    let handler = expr handler_env handler in
    let handler =
      Flambda.Continuation_handler.create params ~handler
        ~free_names_of_handler:Unknown ~is_exn_handler
    in
    match recursive with
    | Nonrecursive ->
      Flambda.Let_cont.create_non_recursive name handler ~body
        ~free_names_of_body:Unknown
    | Recursive ->
      let handlers = Continuation.Map.singleton name handler in
      Flambda.Let_cont.create_recursive handlers ~body
  end
  | Let_cont _ -> failwith "TODO andwhere"
  | Apply_cont ac -> Flambda.Expr.create_apply_cont (apply_cont env ac)
  | Switch { scrutinee; cases } ->
    let arms =
      List.map
        (fun (case, apply) ->
          ( Targetint_31_63.int (Targetint_31_63.Imm.of_int case),
            apply_cont env apply ))
        cases
      |> Targetint_31_63.Map.of_list
    in
    Flambda.Expr.create_switch
      (Flambda.Switch.create ~scrutinee:(simple env scrutinee) ~arms)
  | Let_symbol { bindings; closure_elements; body } ->
    (* Desugar the abbreviated form for a single set of closures *)
    let found_explicit_set = ref false in
    let closures_in_implicit_set =
      List.filter_map
        (fun (binding : Fexpr.symbol_binding) ->
          match binding with
          | Closure clo -> Some clo
          | Set_of_closures _ ->
            found_explicit_set := true;
            None
          | _ -> None)
        bindings
    in
    let bindings =
      match closures_in_implicit_set, closure_elements with
      | _ :: _, _ when !found_explicit_set ->
        Misc.fatal_error "Cannot mix implicit and explicit sets of closures"
      | [], Some _ -> Misc.fatal_error "Found closure elements but no closures"
      | [], None -> bindings
      | _, _ ->
        let implicit_set : Fexpr.static_set_of_closures =
          { bindings = closures_in_implicit_set; elements = closure_elements }
        in
        (* Will replace the first closure found with the set and the rest with
         * nothing *)
        let found_the_first_closure = ref false in
        List.filter_map
          (fun (binding : Fexpr.symbol_binding) ->
            match binding with
            | Closure _ ->
              if !found_the_first_closure
              then None
              else begin
                found_the_first_closure := true;
                Some (Set_of_closures implicit_set : Fexpr.symbol_binding)
              end
            | _ -> Some binding)
          bindings
    in
    let bound_symbols, env =
      let process_binding env (b : Fexpr.symbol_binding) :
          Bound_symbols.Pattern.t * env =
        match b with
        | Code { id; _ } ->
          (* All code ids were bound at the beginning; see
             [bind_all_code_ids] *)
          let code_id = find_code_id env id in
          Bound_symbols.Pattern.code code_id, env
        | Data { symbol; _ } ->
          let symbol, env = declare_symbol env symbol in
          Bound_symbols.Pattern.block_like symbol, env
        | Set_of_closures soc ->
          let closure_binding env
              ({ symbol; fun_decl = { closure_id; code_id; _ } } :
                Fexpr.static_closure_binding) =
            let symbol, env = declare_symbol env symbol in
            let closure_id = closure_id |> Option.value ~default:code_id in
            let closure_id = fresh_or_existing_closure_id env closure_id in
            (closure_id, symbol), env
          in
          let closure_symbols, env =
            map_accum_left closure_binding env soc.bindings
          in
          ( Bound_symbols.Pattern.set_of_closures
              (closure_symbols |> Closure_id.Lmap.of_list),
            env )
        | Closure _ -> assert false
        (* should have been filtered out above *)
      in
      map_accum_left process_binding env bindings
    in
    let bound_symbols = bound_symbols |> Bound_symbols.create in
    let static_const env (b : Fexpr.symbol_binding) : Flambda.Static_const.t =
      match b with
      | Data { symbol = _; defining_expr = def } -> begin
        match def with
        | Block { tag; mutability; elements = args } ->
          let tag = Tag.Scannable.create_exn tag in
          Flambda.Static_const.Block
            (tag, mutability, List.map (field_of_block env) args)
        | Boxed_float f -> Boxed_float (or_variable float env f)
        | Boxed_int32 i -> Boxed_int32 (or_variable Fun.id env i)
        | Boxed_int64 i -> Boxed_int64 (or_variable Fun.id env i)
        | Boxed_nativeint i -> Boxed_nativeint (or_variable targetint env i)
        | Immutable_float_block elements ->
          Immutable_float_block (List.map (or_variable float env) elements)
        | Immutable_float_array elements ->
          Immutable_float_array (List.map (or_variable float env) elements)
        | Mutable_string { initial_value = s } ->
          Mutable_string { initial_value = s }
        | Immutable_string s -> Immutable_string s
      end
      | Set_of_closures { bindings; elements } ->
        let fun_decls =
          List.map
            (fun (b : Fexpr.static_closure_binding) -> b.fun_decl)
            bindings
        in
        let set = set_of_closures env fun_decls elements in
        Set_of_closures set
      | Closure _ -> assert false (* should have been filtered out above *)
      | Code
          { id;
            newer_version_of;
            param_arity;
            ret_arity;
            recursive;
            inline;
            params_and_body;
            code_size;
            is_tupled
          } ->
        let code_id = find_code_id env id in
        let newer_version_of = Option.map (find_code_id env) newer_version_of in
        let env = enter_code env in
        let params_arity =
          match param_arity with
          | Some ar -> arity ar
          | None -> (
            match params_and_body with
            | Deleted ->
              Misc.fatal_errorf "Param arity required for deleted code %a"
                Code_id.print code_id
            | Present { params; _ } ->
              List.map
                (fun ({ kind; _ } : Fexpr.kinded_parameter) ->
                  value_kind_with_subkind_opt kind)
                params)
        in
        let result_arity =
          match ret_arity with
          | None -> [Flambda_kind.With_subkind.any_value]
          | Some ar -> arity ar
        in
        let params_and_body : _ Or_deleted.t =
          match params_and_body with
          | Deleted -> Deleted
          | Present { params; closure_var; depth_var; ret_cont; exn_cont; body }
            ->
            let params, env =
              map_accum_left
                (fun env ({ param; kind } : Fexpr.kinded_parameter) ->
                  let var, env = fresh_var env param in
                  let param =
                    Kinded_parameter.create var
                      (value_kind_with_subkind_opt kind)
                  in
                  param, env)
                env params
            in
            let my_closure, env = fresh_var env closure_var in
            let my_depth, env = fresh_var env depth_var in
            let return_continuation, env =
              fresh_cont env ret_cont ~sort:Return
                ~arity:(List.length result_arity)
            in
            let exn_continuation, env = fresh_exn_cont env exn_cont in
            let body = expr env body in
            let dbg = Debuginfo.none in
            let params_and_body =
              Flambda.Function_params_and_body.create ~return_continuation
                exn_continuation params ~body ~my_closure ~my_depth ~dbg
                ~free_names_of_body:Unknown
            in
            (* CR lmaurer: Add
             * [Name_occurrences.with_only_names_and_closure_vars] *)
            let names_and_closure_vars names =
              Name_occurrences.(
                union
                  (restrict_to_closure_vars names)
                  (with_only_names_and_code_ids names |> without_code_ids))
            in
            let free_names =
              Flambda.Function_params_and_body.free_names params_and_body
              |> names_and_closure_vars
            in
            Present (params_and_body, free_names)
        in
        let recursive = convert_recursive_flag recursive in
        let inline =
          inline |> Option.value ~default:Inline_attribute.Default_inline
        in
        let cost_metrics =
          Flambda.Cost_metrics.from_size (Code_size.of_int code_size)
        in
        let code =
          Flambda.Code.create code_id ~params_and_body ~newer_version_of
            ~params_arity ~result_arity ~stub:false ~inline ~is_a_functor:false
            ~recursive
            ~cost_metrics (* CR poechsel: grab inlining arguments from fexpr. *)
            ~inlining_arguments:(Inlining_arguments.create ~round:0)
            ~dbg:Debuginfo.none ~is_tupled
        in
        Code code
    in
    let static_consts =
      List.map (static_const env) bindings |> Flambda.Static_const.Group.create
    in
    let body = expr env body in
    Flambda.Let.create
      (Bound_pattern.symbols bound_symbols)
      (Flambda.Named.create_static_consts static_consts)
      ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Apply
      { func;
        call_kind;
        inline;
        inlining_state;
        continuation;
        exn_continuation;
        args;
        arities
      } ->
    let continuation = find_result_cont env continuation in
    let call_kind =
      match call_kind with
      | Function (Direct { code_id; closure_id }) ->
        let closure_id = closure_id |> Option.value ~default:code_id in
        let code_id = find_code_id env code_id in
        let closure_id = fresh_or_existing_closure_id env closure_id in
        let return_arity =
          match arities with
          | None -> [Flambda_kind.With_subkind.any_value]
          | Some { ret_arity; _ } -> arity ret_arity
        in
        Call_kind.direct_function_call code_id closure_id ~return_arity
      | Function Indirect -> begin
        match arities with
        | Some { params_arity = Some params_arity; ret_arity } ->
          let param_arity = arity params_arity in
          let return_arity = arity ret_arity in
          Call_kind.indirect_function_call_known_arity ~param_arity
            ~return_arity
        | None | Some { params_arity = None; ret_arity = _ } ->
          Call_kind.indirect_function_call_unknown_arity ()
      end
      | C_call { alloc } -> begin
        match arities with
        | Some { params_arity = Some params_arity; ret_arity } ->
          let param_arity =
            arity params_arity |> Flambda_arity.With_subkinds.to_arity
          in
          let return_arity =
            arity ret_arity |> Flambda_arity.With_subkinds.to_arity
          in
          Call_kind.c_call ~alloc ~param_arity ~return_arity ~is_c_builtin:false
        | None | Some { params_arity = None; ret_arity = _ } ->
          Misc.fatal_errorf "Must specify arities for C call"
      end
    in
    let inline =
      inline |> Option.value ~default:Inline_attribute.Default_inline
    in
    let inlining_state =
      match inlining_state with
      | Some { depth } ->
        (* TODO inlining arguments *)
        Inlining_state.create
          ~arguments:(Inlining_arguments.create ~round:0)
          ~depth
      | None -> Inlining_state.default ~round:0
    in
    let exn_continuation = find_exn_cont env exn_continuation in
    let apply =
      Flambda.Apply.create
        ~callee:(Simple.name (name env func))
        ~continuation exn_continuation
        ~args:((List.map (simple env)) args)
        ~call_kind Debuginfo.none ~inline ~inlining_state ~probe_name:None
    in
    Flambda.Expr.create_apply apply
  | Invalid invalid -> Flambda.Expr.create_invalid ~semantics:invalid ()

let bind_all_code_ids env (unit : Fexpr.flambda_unit) =
  let rec go env (e : Fexpr.expr) =
    match e with
    | Let_symbol { bindings; body; _ } ->
      let env =
        List.fold_left
          (fun env (binding : Fexpr.symbol_binding) ->
            match binding with
            | Code { id; _ } ->
              let _, env = fresh_code_id env id in
              env
            | Data _ | Closure _ | Set_of_closures _ -> env)
          env bindings
      in
      go env body
    | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> env
  in
  go env unit.body

let conv ~backend ~module_ident (fexpr : Fexpr.flambda_unit) : Flambda_unit.t =
  let module Backend = (val backend : Flambda_backend_intf.S) in
  let module_symbol =
    Backend.symbol_for_global'
      (Ident.create_persistent (Ident.name module_ident))
  in
  let env = init_env () in
  let { done_continuation = return_continuation; error_continuation; _ } =
    env
  in
  let exn_continuation = Exn_continuation.exn_handler error_continuation in
  let env = bind_all_code_ids env fexpr in
  let body = expr env fexpr.body in
  Flambda_unit.create ~return_continuation ~exn_continuation ~body
    ~module_symbol ~used_closure_vars:Unknown
