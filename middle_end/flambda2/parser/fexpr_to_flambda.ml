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

(* Function slots (globally scoped, so updates are in-place) *)
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
    toplevel_region : Variable.t;
    variables : Variable.t VM.t;
    symbols : Symbol.t SM.t;
    code_ids : Code_id.t DM.t;
    function_slots : Function_slot.t UT.t;
    vars_within_closures : Value_slot.t WT.t
  }

let init_env () =
  let done_continuation =
    Continuation.create ~sort:Toplevel_return ~name:"done" ()
  in
  let exn_handler = Continuation.create ~name:"error" () in
  let error_continuation =
    Exn_continuation.create ~exn_handler ~extra_args:[]
  in
  let toplevel_region = Variable.create "toplevel" in
  { done_continuation;
    error_continuation;
    continuations = CM.empty;
    exn_continuations = CM.empty;
    toplevel_region;
    variables = VM.empty;
    symbols = SM.empty;
    code_ids = DM.empty;
    function_slots = UT.create 10;
    vars_within_closures = WT.create 10
  }

let enter_code env =
  { continuations = CM.empty;
    exn_continuations = CM.empty;
    toplevel_region = env.toplevel_region;
    variables = env.variables;
    done_continuation = env.done_continuation;
    error_continuation = env.error_continuation;
    symbols = env.symbols;
    code_ids = env.code_ids;
    function_slots = env.function_slots;
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

let fresh_function_slot env { Fexpr.txt = name; loc = _ } =
  let c =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name Flambda_kind.With_subkind.any_value
  in
  UT.add env.function_slots name c;
  c

let fresh_or_existing_function_slot env ({ Fexpr.txt = name; loc = _ } as id) =
  match UT.find_opt env.function_slots name with
  | None -> fresh_function_slot env id
  | Some function_slot -> function_slot

let fresh_value_slot env { Fexpr.txt = name; loc = _ } kind =
  let c = Value_slot.create (Compilation_unit.get_current_exn ()) ~name kind in
  WT.add env.vars_within_closures name c;
  c

let fresh_or_existing_value_slot env ({ Fexpr.txt = name; _ } as id) kind =
  match WT.find_opt env.vars_within_closures name with
  | None -> fresh_value_slot env id kind
  | Some value_slot -> value_slot

let print_scoped_location ppf loc =
  match (loc : Lambda.scoped_location) with
  | Loc_unknown -> Format.pp_print_string ppf "Unknown"
  | Loc_known { loc; _ } -> Location.print_loc ppf loc

let compilation_unit { Fexpr.ident; linkage_name } =
  (* CR lmaurer: This ignores the ident when the linkage name is given; is that
     what we want? Why did we have the ability to specify both? *)
  let linkage_name = linkage_name |> Option.value ~default:ident in
  Compilation_unit.of_string linkage_name

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
      | Some cu -> compilation_unit cu
    in
    let symbol = Symbol.unsafe_create cunit (Linkage_name.of_string name) in
    symbol, { env with symbols = SM.add name symbol env.symbols }

let find_with ~descr ~find map { Fexpr.txt = name; loc } =
  match find name map with
  | None ->
    Misc.fatal_errorf "Unbound %s %s: %a" descr name print_scoped_location loc
  | Some a -> a

let get_symbol (env : env) sym =
  match sym with
  | { Fexpr.txt = Some cunit, name; loc = _ } ->
    let cunit = compilation_unit cunit in
    Symbol.unsafe_create cunit (name |> Linkage_name.of_string)
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

let find_region env (r : Fexpr.region) =
  match r with Toplevel -> env.toplevel_region | Named v -> find_var env v

let find_code_id env code_id =
  find_with ~descr:"code id" ~find:DM.find_opt env.code_ids code_id

let targetint (i : Fexpr.targetint) : Targetint_32_64.t =
  Targetint_32_64.of_int64 i

let targetint_31_63 (i : Fexpr.targetint) : Targetint_31_63.t =
  Targetint_31_63.of_int64 i

let vec128 bits : Vector_types.Vec128.Bit_pattern.t =
  Vector_types.Vec128.Bit_pattern.of_bits bits

let tag_scannable (tag : Fexpr.tag_scannable) : Tag.Scannable.t =
  Tag.Scannable.create_exn tag

let immediate i = i |> Targetint_32_64.of_string |> Targetint_31_63.of_targetint

let float f = f |> Numeric_types.Float_by_bit_pattern.create

let rec subkind : Fexpr.subkind -> Flambda_kind.With_subkind.Subkind.t =
  function
  | Anything -> Anything
  | Boxed_float -> Boxed_float
  | Boxed_int32 -> Boxed_int32
  | Boxed_int64 -> Boxed_int64
  | Boxed_nativeint -> Boxed_nativeint
  | Boxed_vec128 -> Boxed_vec128
  | Tagged_immediate -> Tagged_immediate
  | Variant { consts; non_consts } ->
    let consts =
      consts |> List.map targetint_31_63 |> Targetint_31_63.Set.of_list
    in
    let non_consts =
      non_consts
      |> List.map (fun (tag, sk) ->
             tag_scannable tag, List.map value_kind_with_subkind sk)
      |> Tag.Scannable.Map.of_list
    in
    Variant { consts; non_consts }
  | Float_block { num_fields } -> Float_block { num_fields }
  | Float_array -> Float_array
  | Immediate_array -> Immediate_array
  | Value_array -> Value_array
  | Generic_array -> Generic_array

and value_kind_with_subkind :
    Fexpr.kind_with_subkind -> Flambda_kind.With_subkind.t = function
  | Value sk ->
    Flambda_kind.With_subkind.create Flambda_kind.value (sk |> subkind)
  | Naked_number nnk -> Flambda_kind.With_subkind.of_naked_number_kind nnk
  | Region -> Flambda_kind.With_subkind.region
  | Rec_info -> Flambda_kind.With_subkind.rec_info

let value_kind_with_subkind_opt :
    Fexpr.kind_with_subkind option -> Flambda_kind.With_subkind.t = function
  | Some kind -> value_kind_with_subkind kind
  | None -> Flambda_kind.With_subkind.any_value

let arity a =
  Flambda_arity.create_singletons (List.map value_kind_with_subkind a)

let const (c : Fexpr.const) : Reg_width_const.t =
  match c with
  | Tagged_immediate i -> Reg_width_const.tagged_immediate (i |> immediate)
  | Naked_immediate i -> Reg_width_const.naked_immediate (i |> immediate)
  | Naked_float f -> Reg_width_const.naked_float (f |> float)
  | Naked_int32 i -> Reg_width_const.naked_int32 i
  | Naked_int64 i -> Reg_width_const.naked_int64 i
  | Naked_nativeint i -> Reg_width_const.naked_nativeint (i |> targetint)
  | Naked_vec128 bits -> Reg_width_const.naked_vec128 (bits |> vec128)

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
  | Var { txt = v; loc } -> (
    match VM.find_opt v env.variables with
    | None ->
      Misc.fatal_errorf "Unbound variable %s : %a" v print_scoped_location loc
    | Some var -> Simple.var var)
  | Const c -> Simple.const (const c)
  | Symbol sym -> Simple.symbol (get_symbol env sym)
  | Coerce (s, co) -> Simple.apply_coercion_exn (simple env s) (coercion env co)

let field_of_block env (v : Fexpr.field_of_block) : Field_of_static_block.t =
  match v with
  | Symbol s -> Symbol (get_symbol env s)
  | Tagged_immediate i ->
    let i = Targetint_32_64.of_string i in
    Tagged_immediate (Targetint_31_63.of_targetint i)
  | Dynamically_computed var ->
    let var = find_var env var in
    Dynamically_computed (var, Debuginfo.none)

let or_variable f env (ov : _ Fexpr.or_variable) : _ Or_variable.t =
  match ov with
  | Const c -> Const (f c)
  | Var v -> Var (find_var env v, Debuginfo.none)

let alloc_mode_for_allocations env (alloc : Fexpr.alloc_mode_for_allocations) =
  match alloc with
  | Heap -> Alloc_mode.For_allocations.heap
  | Local { region = r } ->
    let r = find_region env r in
    Alloc_mode.For_allocations.local ~region:r

let alloc_mode_for_assignments (alloc : Fexpr.alloc_mode_for_assignments) =
  match alloc with
  | Heap -> Alloc_mode.For_assignments.heap
  | Local -> Alloc_mode.For_assignments.local ()

let init_or_assign _env (ia : Fexpr.init_or_assign) :
    Flambda_primitive.Init_or_assign.t =
  match ia with
  | Initialization -> Initialization
  | Assignment alloc -> Assignment (alloc_mode_for_assignments alloc)

let nullop (nullop : Fexpr.nullop) : Flambda_primitive.nullary_primitive =
  match nullop with
  | Begin_region -> Begin_region
  | Begin_try_region -> Begin_try_region

let unop env (unop : Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Array_length -> Array_length
  | Boolean_not -> Boolean_not
  | Box_number (bk, alloc) ->
    Box_number (bk, alloc_mode_for_allocations env alloc)
  | Unbox_number bk -> Unbox_number bk
  | Tag_immediate -> Tag_immediate
  | Untag_immediate -> Untag_immediate
  | End_region -> End_region
  | End_try_region -> End_try_region
  | Get_tag -> Get_tag
  | Int_arith (i, o) -> Int_arith (i, o)
  | Is_flat_float_array -> Is_flat_float_array
  | Is_int -> Is_int { variant_only = true } (* CR vlaviron: discuss *)
  | Num_conv { src; dst } -> Num_conv { src; dst }
  | Opaque_identity ->
    Opaque_identity { middle_end_only = false; kind = Flambda_kind.value }
  | Project_value_slot { project_from; value_slot } ->
    (* CR mshinwell: support non-value kinds *)
    let kind = Flambda_kind.With_subkind.any_value in
    let value_slot = fresh_or_existing_value_slot env value_slot kind in
    let project_from = fresh_or_existing_function_slot env project_from in
    Project_value_slot { project_from; value_slot; kind }
  | Project_function_slot { move_from; move_to } ->
    let move_from = fresh_or_existing_function_slot env move_from in
    let move_to = fresh_or_existing_function_slot env move_to in
    Project_function_slot { move_from; move_to }
  | String_length string_or_bytes -> String_length string_or_bytes

let infix_binop (binop : Fexpr.infix_binop) : Flambda_primitive.binary_primitive
    =
  match binop with
  | Int_arith o -> Int_arith (Tagged_immediate, o)
  | Int_comp c -> Int_comp (Tagged_immediate, c)
  | Int_shift s -> Int_shift (Tagged_immediate, s)
  | Float_arith o -> Float_arith o
  | Float_comp c -> Float_comp c

let block_access_kind (ak : Fexpr.block_access_kind) :
    Flambda_primitive.Block_access_kind.t =
  let size s : _ Or_unknown.t =
    match s with
    | None -> Unknown
    | Some s -> Known (s |> Targetint_31_63.of_int64)
  in
  match ak with
  | Values { field_kind; tag; size = s } ->
    let tag : Tag.Scannable.t Or_unknown.t =
      match tag with
      | Some tag -> Known (tag |> tag_scannable)
      | None -> Unknown
    in
    let size = size s in
    Values { field_kind; tag; size }
  | Naked_floats { size = s } ->
    let size = size s in
    Naked_floats { size }

let binop (binop : Fexpr.binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Array_load (ak, mut) -> Array_load (ak, mut)
  | Block_load (ak, mutability) -> Block_load (block_access_kind ak, mutability)
  | Phys_equal op -> Phys_equal op
  | Infix op -> infix_binop op
  | Int_arith (i, o) -> Int_arith (i, o)
  | Int_comp (i, c) -> Int_comp (i, c)
  | Int_shift (i, s) -> Int_shift (i, s)
  | String_or_bigstring_load (slv, saw) -> String_or_bigstring_load (slv, saw)
  | Bigarray_get_alignment align -> Bigarray_get_alignment align

let ternop env (ternop : Fexpr.ternop) : Flambda_primitive.ternary_primitive =
  match ternop with
  | Array_set (ak, ia) ->
    let ask : Flambda_primitive.Array_set_kind.t =
      match ak, ia with
      | Immediates, _ -> Immediates
      | Naked_floats, _ -> Naked_floats
      | Values, ia -> Values (init_or_assign env ia)
    in
    Array_set ask
  | Block_set (bk, ia) -> Block_set (block_access_kind bk, init_or_assign env ia)
  | Bytes_or_bigstring_set (blv, saw) -> Bytes_or_bigstring_set (blv, saw)

let convert_block_shape ~num_fields =
  List.init num_fields (fun _field -> Flambda_kind.With_subkind.any_value)

let varop env (varop : Fexpr.varop) n : Flambda_primitive.variadic_primitive =
  match varop with
  | Make_block (tag, mutability, alloc) ->
    let shape = convert_block_shape ~num_fields:n in
    let kind : Flambda_primitive.Block_kind.t =
      Values (tag_scannable tag, shape)
    in
    let alloc = alloc_mode_for_allocations env alloc in
    Make_block (kind, mutability, alloc)

let prim env (p : Fexpr.prim) : Flambda_primitive.t =
  match p with
  | Nullary op -> Nullary (nullop op)
  | Unary (op, arg) -> Unary (unop env op, simple env arg)
  | Binary (op, a1, a2) -> Binary (binop op, simple env a1, simple env a2)
  | Ternary (op, a1, a2, a3) ->
    Ternary (ternop env op, simple env a1, simple env a2, simple env a3)
  | Variadic (op, args) ->
    Variadic (varop env op (List.length args), List.map (simple env) args)

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
  | Closure _ -> assert false

let set_of_closures env fun_decls value_slots alloc =
  let fun_decls : Function_declarations.t =
    let translate_fun_decl (fun_decl : Fexpr.fun_decl) :
        Function_slot.t * Code_id.t =
      let code_id = find_code_id env fun_decl.code_id in
      let function_slot =
        (* By default, pun the code id as the function slot *)
        fun_decl.function_slot |> Option.value ~default:fun_decl.code_id
      in
      let function_slot = fresh_or_existing_function_slot env function_slot in
      function_slot, code_id
    in
    List.map translate_fun_decl fun_decls
    |> Function_slot.Lmap.of_list |> Function_declarations.create
  in
  let value_slots = Option.value value_slots ~default:[] in
  let value_slots : Simple.t Value_slot.Map.t =
    let convert ({ var; value } : Fexpr.one_value_slot) =
      (* CR mshinwell: support non-value kinds *)
      ( fresh_or_existing_value_slot env var Flambda_kind.With_subkind.any_value,
        simple env value )
    in
    List.map convert value_slots |> Value_slot.Map.of_list
  in
  let alloc = alloc_mode_for_allocations env alloc in
  Set_of_closures.create ~value_slots alloc fun_decls

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
      { bindings = { defining_expr = Closure { alloc; _ }; _ } :: _ as bindings;
        value_slots;
        body
      } ->
    let binding_to_var_and_closure_binding : Fexpr.let_binding -> _ = function
      | { var; defining_expr = Closure binding; _ } -> var, binding
      | { var = { txt = _; loc };
          defining_expr = Simple _ | Prim _ | Rec_info _;
          _
        } ->
        Misc.fatal_errorf "Cannot use 'and' with non-closure: %a"
          print_scoped_location loc
    in
    let vars_and_closure_bindings =
      List.map binding_to_var_and_closure_binding bindings
    in
    let bound_vars, env =
      let convert_binding env (var, _) : Bound_var.t * env =
        let var, env = fresh_var env var in
        let var = Bound_var.create var Name_mode.normal in
        var, env
      in
      map_accum_left convert_binding env vars_and_closure_bindings
    in
    let bound = Bound_pattern.set_of_closures bound_vars in
    let named =
      let closure_bindings = List.map snd vars_and_closure_bindings in
      set_of_closures env closure_bindings value_slots alloc
      |> Flambda.Named.create_set_of_closures
    in
    let body = expr env body in
    Flambda.Let.create bound named ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Let
      { bindings =
          { defining_expr = Simple _ | Prim _ | Rec_info _; _ } :: _ :: _;
        _
      } ->
    Misc.fatal_errorf
      "Multiple let bindings only allowed when defining closures"
  | Let { value_slots = Some _; _ } ->
    Misc.fatal_errorf "'with' clause only allowed when defining closures"
  | Let { bindings = [{ var; defining_expr = d }]; body; value_slots = None } ->
    let named = defining_expr env d in
    let id, env = fresh_var env var in
    let body = expr env body in
    let var = Bound_var.create id Name_mode.normal in
    let bound = Bound_pattern.singleton var in
    Flambda.Let.create bound named ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Let_cont { recursive; body; bindings = [{ name; params; sort; handler }] }
    -> (
    let sort =
      sort |> Option.value ~default:(Normal : Fexpr.continuation_sort)
    in
    let is_exn_handler =
      match sort with Exn -> true | Normal | Define_root_symbol -> false
    in
    let sort = continuation_sort sort in
    let name, body_env =
      if is_exn_handler
      then
        let e, env = fresh_exn_cont env name in
        Exn_continuation.exn_handler e, env
      else fresh_cont env name ~sort ~arity:(List.length params)
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
            Bound_parameter.create var (value_kind_with_subkind_opt kind)
          in
          env, param :: args)
        params (env, [])
    in
    let handler = expr handler_env handler in
    let handler =
      Flambda.Continuation_handler.create
        (Bound_parameters.create params)
        ~handler ~free_names_of_handler:Unknown ~is_exn_handler ~is_cold:false
    in
    match recursive with
    | Nonrecursive ->
      Flambda.Let_cont.create_non_recursive name handler ~body
        ~free_names_of_body:Unknown
    | Recursive ->
      let handlers = Continuation.Map.singleton name handler in
      Flambda.Let_cont.create_recursive ~invariant_params:Bound_parameters.empty
        handlers ~body)
  | Let_cont _ -> failwith "TODO andwhere"
  | Apply_cont ac -> Flambda.Expr.create_apply_cont (apply_cont env ac)
  | Switch { scrutinee; cases } ->
    let arms =
      List.map
        (fun (case, apply) -> Targetint_31_63.of_int case, apply_cont env apply)
        cases
      |> Targetint_31_63.Map.of_list
    in
    Flambda.Expr.create_switch
      (Flambda.Switch.create ~condition_dbg:Debuginfo.none
         ~scrutinee:(simple env scrutinee) ~arms)
  | Let_symbol { bindings; value_slots; body } ->
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
          | Data _ | Code _ | Deleted_code _ -> None)
        bindings
    in
    let bindings =
      match closures_in_implicit_set, value_slots with
      | _ :: _, _ when !found_explicit_set ->
        Misc.fatal_error "Cannot mix implicit and explicit sets of closures"
      | [], Some _ -> Misc.fatal_error "Found closure elements but no closures"
      | [], None -> bindings
      | _, _ ->
        let implicit_set : Fexpr.static_set_of_closures =
          { bindings = closures_in_implicit_set; elements = value_slots }
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
              else (
                found_the_first_closure := true;
                Some (Set_of_closures implicit_set : Fexpr.symbol_binding))
            | Data _ | Code _ | Deleted_code _ | Set_of_closures _ ->
              Some binding)
          bindings
    in
    let bound_static, env =
      let process_binding env (b : Fexpr.symbol_binding) :
          Bound_static.Pattern.t * env =
        match b with
        | Code { id; _ } ->
          (* All code ids were bound at the beginning; see
             [bind_all_code_ids] *)
          let code_id = find_code_id env id in
          Bound_static.Pattern.code code_id, env
        | Deleted_code id ->
          let code_id = find_code_id env id in
          Bound_static.Pattern.code code_id, env
        | Data { symbol; _ } ->
          let symbol, env = declare_symbol env symbol in
          Bound_static.Pattern.block_like symbol, env
        | Set_of_closures soc ->
          let closure_binding env
              ({ symbol; fun_decl = { function_slot; code_id; _ } } :
                Fexpr.static_closure_binding) =
            let symbol, env = declare_symbol env symbol in
            let function_slot =
              function_slot |> Option.value ~default:code_id
            in
            let function_slot =
              fresh_or_existing_function_slot env function_slot
            in
            (function_slot, symbol), env
          in
          let closure_symbols, env =
            map_accum_left closure_binding env soc.bindings
          in
          ( Bound_static.Pattern.set_of_closures
              (closure_symbols |> Function_slot.Lmap.of_list),
            env )
        | Closure _ -> assert false
        (* should have been filtered out above *)
      in
      map_accum_left process_binding env bindings
    in
    let bound_static = bound_static |> Bound_static.create in
    let static_const env (b : Fexpr.symbol_binding) :
        Flambda.Static_const_or_code.t =
      let static_const const =
        Flambda.Static_const_or_code.create_static_const const
      in
      let module SC = Static_const in
      match b with
      | Data { symbol = _; defining_expr = def } -> (
        match def with
        | Block { tag; mutability; elements = args } ->
          let tag = tag_scannable tag in
          static_const
            (SC.block tag mutability (List.map (field_of_block env) args))
        | Boxed_float f ->
          static_const (SC.boxed_float (or_variable float env f))
        | Boxed_int32 i ->
          static_const (SC.boxed_int32 (or_variable Fun.id env i))
        | Boxed_int64 i ->
          static_const (SC.boxed_int64 (or_variable Fun.id env i))
        | Boxed_nativeint i ->
          static_const (SC.boxed_nativeint (or_variable targetint env i))
        | Boxed_vec128 i ->
          static_const (SC.boxed_vec128 (or_variable vec128 env i))
        | Immutable_float_block elements ->
          static_const
            (SC.immutable_float_block
               (List.map (or_variable float env) elements))
        | Immutable_float_array elements ->
          static_const
            (SC.immutable_float_array
               (List.map (or_variable float env) elements))
        | Immutable_value_array elements ->
          static_const
            (SC.immutable_value_array (List.map (field_of_block env) elements))
        | Empty_array -> static_const SC.empty_array
        | Mutable_string { initial_value = s } ->
          static_const (SC.mutable_string ~initial_value:s)
        | Immutable_string s -> static_const (SC.immutable_string s))
      | Set_of_closures { bindings; elements } ->
        let fun_decls =
          List.map
            (fun (b : Fexpr.static_closure_binding) -> b.fun_decl)
            bindings
        in
        let set = set_of_closures env fun_decls elements Heap in
        static_const (SC.set_of_closures set)
      | Closure _ -> assert false (* should have been filtered out above *)
      | Deleted_code _ -> Flambda.Static_const_or_code.deleted_code
      | Code
          { id;
            newer_version_of;
            param_arity;
            ret_arity;
            recursive;
            inline;
            params_and_body;
            code_size;
            is_tupled;
            loopify;
            result_mode
          } ->
        let code_id = find_code_id env id in
        let newer_version_of = Option.map (find_code_id env) newer_version_of in
        let env = enter_code env in
        let params_arity =
          match param_arity with
          | Some ar -> arity ar
          | None ->
            List.map
              (fun ({ kind; _ } : Fexpr.kinded_parameter) ->
                value_kind_with_subkind_opt kind)
              params_and_body.params
            |> Flambda_arity.create_singletons
        in
        let result_arity =
          match ret_arity with
          | None ->
            Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]
          | Some ar -> arity ar
        in
        let ( _params,
              params_and_body,
              free_names_of_params_and_body,
              is_my_closure_used ) =
          let { Fexpr.params;
                closure_var;
                region_var;
                depth_var;
                ret_cont;
                exn_cont;
                body
              } =
            params_and_body
          in
          let params, env =
            map_accum_left
              (fun env ({ param; kind } : Fexpr.kinded_parameter) ->
                let var, env = fresh_var env param in
                let param =
                  Bound_parameter.create var (value_kind_with_subkind_opt kind)
                in
                param, env)
              env params
          in
          let my_closure, env = fresh_var env closure_var in
          let my_region, env = fresh_var env region_var in
          let my_depth, env = fresh_var env depth_var in
          let return_continuation, env =
            fresh_cont env ret_cont ~sort:Return
              ~arity:(Flambda_arity.cardinal_unarized result_arity)
          in
          let exn_continuation, env = fresh_exn_cont env exn_cont in
          assert (
            match Exn_continuation.extra_args exn_continuation with
            | [] -> true
            | _ :: _ -> false);
          let body = expr env body in
          let params_and_body =
            Flambda.Function_params_and_body.create ~return_continuation
              ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
              (Bound_parameters.create params)
              ~body ~my_closure ~my_region ~my_depth ~free_names_of_body:Unknown
          in
          let free_names =
            (* CR mshinwell: This needs fixing XXX *)
            Name_occurrences.empty
          in
          ( params,
            params_and_body,
            free_names,
            Flambda.Function_params_and_body.is_my_closure_used params_and_body
          )
        in
        let recursive = convert_recursive_flag recursive in
        let inline =
          inline |> Option.value ~default:Inline_attribute.Default_inline
        in
        let loopify =
          loopify
          |> Option.value
               ~default:Loopify_attribute.Default_loopify_and_not_tailrec
        in
        let cost_metrics =
          Cost_metrics.from_size (Code_size.of_int code_size)
        in
        (* CR ncourant: allow fexpr to specify modes? *)
        let param_modes =
          List.map
            (fun _ -> Alloc_mode.For_types.heap)
            (Flambda_arity.unarize params_arity)
        in
        let result_mode =
          match result_mode with
          | Heap -> Lambda.alloc_heap
          | Local -> Lambda.alloc_local
        in
        let code =
          (* CR mshinwell: [inlining_decision] should maybe be set properly *)
          Code.create code_id ~params_and_body ~free_names_of_params_and_body
            ~newer_version_of ~params_arity ~param_modes
            ~first_complex_local_param:(Flambda_arity.num_params params_arity)
            ~result_arity ~result_types:Unknown ~result_mode
            ~contains_no_escaping_local_allocs:false ~stub:false ~inline
            ~check:Default_check
              (* CR gyorsh: should [check] be set properly? *)
            ~is_a_functor:false ~is_opaque:false ~recursive
            ~cost_metrics (* CR poechsel: grab inlining arguments from fexpr. *)
            ~inlining_arguments:(Inlining_arguments.create ~round:0)
            ~poll_attribute:Default ~dbg:Debuginfo.none ~is_tupled
            ~is_my_closure_used ~inlining_decision:Never_inline_attribute
            ~absolute_history:
              (Inlining_history.Absolute.empty
                 (Compilation_unit.get_current_exn ()))
            ~relative_history:Inlining_history.Relative.empty ~loopify
        in
        Flambda.Static_const_or_code.create_code code
    in
    let static_consts =
      List.map (static_const env) bindings |> Flambda.Static_const_group.create
    in
    let body = expr env body in
    Flambda.Let.create
      (Bound_pattern.static bound_static)
      (Flambda.Named.create_static_consts static_consts)
      ~body ~free_names_of_body:Unknown
    |> Flambda.Expr.create_let
  | Apply
      { func;
        call_kind;
        inlined;
        inlining_state;
        continuation;
        exn_continuation;
        args;
        arities
      } ->
    let continuation = find_result_cont env continuation in
    let call_kind, args_arity, return_arity =
      match call_kind with
      | Function (Direct { code_id; function_slot = _; alloc }) ->
        let code_id = find_code_id env code_id in
        let params_arity =
          (* CR mshinwell: This needs fixing to cope with the fact that the
             arities have moved onto [Apply_expr] *)
          Flambda_arity.create_singletons
            (List.map (fun _ -> Flambda_kind.With_subkind.any_value) args)
        in
        let return_arity =
          match arities with
          | None ->
            Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]
          | Some { ret_arity; _ } -> arity ret_arity
        in
        let alloc = alloc_mode_for_allocations env alloc in
        Call_kind.direct_function_call code_id alloc, params_arity, return_arity
      | Function (Indirect alloc) -> (
        let alloc = alloc_mode_for_allocations env alloc in
        match arities with
        | Some { params_arity = Some params_arity; ret_arity } ->
          let params_arity = arity params_arity in
          let return_arity = arity ret_arity in
          ( Call_kind.indirect_function_call_known_arity alloc,
            params_arity,
            return_arity )
        | None | Some { params_arity = None; ret_arity = _ } ->
          let params_arity =
            (* CR mshinwell: This needs fixing to cope with the fact that the
               arities have moved onto [Apply_expr] *)
            Flambda_arity.create_singletons
              (List.map (fun _ -> Flambda_kind.With_subkind.any_value) args)
          in
          let return_arity =
            (* CR mshinwell: This needs fixing to cope with the fact that the
               arities have moved onto [Apply_expr] *)
            Flambda_arity.create_singletons [Flambda_kind.With_subkind.any_value]
          in
          ( Call_kind.indirect_function_call_unknown_arity alloc,
            params_arity,
            return_arity ))
      | C_call { alloc } -> (
        match arities with
        | Some { params_arity = Some params_arity; ret_arity } ->
          let params_arity = arity params_arity in
          let return_arity = arity ret_arity in
          ( Call_kind.c_call ~alloc ~is_c_builtin:false,
            params_arity,
            return_arity )
        | None | Some { params_arity = None; ret_arity = _ } ->
          Misc.fatal_errorf "Must specify arities for C call")
    in
    let inlined : Inlined_attribute.t =
      match inlined with
      | None | Some Default_inlined -> Default_inlined
      | Some Hint_inlined -> Hint_inlined
      | Some Always_inlined -> Always_inlined Expected_to_be_used
      | Some (Unroll n) -> Unroll (n, Expected_to_be_used)
      | Some Never_inlined -> Never_inlined
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
        ~callee:(Some (simple env func))
        ~continuation exn_continuation
        ~args:((List.map (simple env)) args)
        ~args_arity ~return_arity ~call_kind Debuginfo.none ~inlined
        ~inlining_state ~probe:None ~position:Normal
        ~relative_history:Inlining_history.Relative.empty
    in
    Flambda.Expr.create_apply apply
  | Invalid { message } -> Flambda.Expr.create_invalid (Message message)

let bind_all_code_ids env (unit : Fexpr.flambda_unit) =
  let rec go env (e : Fexpr.expr) =
    match e with
    | Let_symbol { bindings; body; _ } ->
      let env =
        List.fold_left
          (fun env (binding : Fexpr.symbol_binding) ->
            match binding with
            | Code { id; _ } | Deleted_code id ->
              let _, env = fresh_code_id env id in
              env
            | Data _ | Closure _ | Set_of_closures _ -> env)
          env bindings
      in
      go env body
    | Let { body; _ } -> go env body
    | Let_cont { body; bindings; _ } ->
      let env =
        List.fold_left
          (fun env (binding : Fexpr.continuation_binding) ->
            go env binding.handler)
          env bindings
      in
      go env body
    | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> env
  in
  go env unit.body

let conv comp_unit (fexpr : Fexpr.flambda_unit) : Flambda_unit.t =
  let module_symbol =
    Flambda2_import.Symbol.for_compilation_unit comp_unit
    |> Symbol.create_wrapped
  in
  let env = init_env () in
  let { done_continuation = return_continuation;
        error_continuation;
        toplevel_region;
        _
      } =
    env
  in
  let exn_continuation = Exn_continuation.exn_handler error_continuation in
  let env = bind_all_code_ids env fexpr in
  let body = expr env fexpr.body in
  Flambda_unit.create ~return_continuation ~exn_continuation
    ~toplevel_my_region:toplevel_region ~body ~module_symbol
    ~used_value_slots:Unknown
