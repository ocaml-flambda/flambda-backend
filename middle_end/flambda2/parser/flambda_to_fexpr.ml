open! Flambda.Import

(* CR-someday mshinwell: share with Fexpr_to_flambda / move to Stdlib *)
let map_accum_left f env l =
  let next (acc, env) x =
    let y, env = f env x in
    y :: acc, env
  in
  let acc, env = List.fold_left next ([], env) l in
  List.rev acc, env

module type Convertible_id = sig
  type t

  type fexpr_id

  include Container_types.S with type t := t

  val desc : string

  val name : t -> string

  val add_tag : string -> int -> string

  val mk_fexpr_id : string -> fexpr_id
end

let default_add_tag name tag = Printf.sprintf "%s_%d" name tag

module Name_map (I : Convertible_id) : sig
  type t

  val empty : t

  val bind : t -> I.t -> I.fexpr_id * t

  val bind_to : t -> I.t -> I.fexpr_id -> t

  val find_exn : t -> I.t -> I.fexpr_id
end = struct
  module String_map = Map.Make (String)

  type t =
    { id_map : I.fexpr_id I.Map.t;
      names : int String_map.t
    }

  let empty = { id_map = I.Map.empty; names = String_map.empty }

  let bind { id_map; names } id =
    let name = I.name id in
    let rec try_name name names =
      match String_map.find_opt name names with
      | None ->
        let fexpr_id = I.mk_fexpr_id name in
        let names = String_map.add name 1 names in
        fexpr_id, names
      | Some count ->
        let names = String_map.add name (count + 1) names in
        let name = I.add_tag name count in
        (* Unlikely but possible that, say, both x and x_1 are used; in this
         * case we'll end up with x_1_1 *)
        try_name name names
    in
    let fexpr_id, names = try_name name names in
    let id_map = I.Map.add id fexpr_id id_map in
    fexpr_id, { id_map; names }

  let bind_to { id_map; names } id fexpr_id =
    let id_map = I.Map.add id fexpr_id id_map in
    { id_map; names }

  let find t id = I.Map.find_opt id t.id_map

  let find_exn t id =
    match find t id with
    | Some fexpr_id -> fexpr_id
    | None ->
      Misc.fatal_errorf "missing %s %a (known names: %a)" I.desc I.print id
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (String_map.bindings t.names |> List.map fst)
end

module Global_name_map (I : Convertible_id) : sig
  type t

  val create : unit -> t

  val translate : t -> I.t -> I.fexpr_id
end = struct
  module String_tbl = Hashtbl.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  type t =
    { mutable id_tbl : I.fexpr_id I.Map.t;
      names : int String_tbl.t
    }

  let create () = { id_tbl = I.Map.empty; names = String_tbl.create 10 }

  let translate t id =
    match I.Map.find_opt id t.id_tbl with
    | Some fexpr_id -> fexpr_id
    | None ->
      (* CR-soon lmaurer: Too much duplication with Name_map.bind *)
      let rec try_name name =
        match String_tbl.find_opt t.names name with
        | None ->
          let fexpr_id = I.mk_fexpr_id name in
          String_tbl.add t.names name 1;
          fexpr_id
        | Some count ->
          String_tbl.replace t.names name (count + 1);
          let name = Printf.sprintf "%s_%d" name count in
          (* Unlikely but possible that, say, both x and x_1 are used; in this
           * case we'll end up with x_1_1 *)
          try_name name
      in
      let fexpr_id = try_name (I.name id) in
      t.id_tbl <- I.Map.add id fexpr_id t.id_tbl;
      fexpr_id
end

let nowhere a = { Fexpr.txt = a; loc = Loc_unknown }

module Env : sig
  type t

  val create : unit -> t

  val bind_var : t -> Variable.t -> Fexpr.variable * t

  val bind_bound_var : t -> Bound_var.t -> Fexpr.variable * t

  val bind_symbol : t -> Symbol.t -> Fexpr.symbol * t

  val bind_code_id : t -> Code_id.t -> Fexpr.code_id * t

  val bind_named_continuation : t -> Continuation.t -> Fexpr.continuation_id * t

  val bind_special_continuation :
    t -> Continuation.t -> to_:Fexpr.special_continuation -> t

  val bind_toplevel_region : t -> Variable.t -> t

  val find_var_exn : t -> Variable.t -> Fexpr.variable

  val find_symbol_exn : t -> Symbol.t -> Fexpr.symbol

  val find_code_id_exn : t -> Code_id.t -> Fexpr.code_id

  val find_continuation_exn : t -> Continuation.t -> Fexpr.continuation

  val find_region_exn : t -> Variable.t -> Fexpr.region

  val translate_function_slot : t -> Function_slot.t -> Fexpr.function_slot

  val translate_value_slot : t -> Value_slot.t -> Fexpr.value_slot
end = struct
  module Variable_name_map = Name_map (struct
    include Variable

    type fexpr_id = Fexpr.variable

    let desc = "variable"

    let name v = raw_name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Symbol_name_map = Name_map (struct
    include Symbol

    (* We don't need the name map for non-local symbols, so only bother with
     * the ident part of the symbol here *)
    type fexpr_id = string

    let desc = "symbol"

    let name v = linkage_name v |> Linkage_name.to_string

    let add_tag = default_add_tag

    let mk_fexpr_id name = name
  end)

  module Code_id_name_map = Name_map (struct
    include Code_id

    type fexpr_id = Fexpr.code_id

    let desc = "code id"

    let name v = Code_id.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Function_slot_name_map = Global_name_map (struct
    include Function_slot

    type fexpr_id = Fexpr.function_slot

    let desc = "function slot"

    let name v = Function_slot.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Value_slot_name_map = Global_name_map (struct
    include Value_slot

    type fexpr_id = Fexpr.value_slot

    let desc = "var within closure"

    let name v = Value_slot.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Continuation_name_map = Name_map (struct
    include Continuation

    type fexpr_id = Fexpr.continuation

    let desc = "continuation"

    let name c = Continuation.name c

    let add_tag name tag =
      match name with
      | "k" -> Printf.sprintf "k%d" tag
      | _ -> default_add_tag name tag

    let mk_fexpr_id name : Fexpr.continuation = Named (name |> nowhere)
  end)

  type t =
    { variables : Variable_name_map.t;
      symbols : Symbol_name_map.t;
      code_ids : Code_id_name_map.t;
      function_slots : Function_slot_name_map.t;
      vars_within_closures : Value_slot_name_map.t;
      continuations : Continuation_name_map.t;
      toplevel_region : Variable.t option
    }

  let create () =
    { variables = Variable_name_map.empty;
      symbols = Symbol_name_map.empty;
      code_ids = Code_id_name_map.empty;
      function_slots = Function_slot_name_map.create ();
      vars_within_closures = Value_slot_name_map.create ();
      continuations = Continuation_name_map.empty;
      toplevel_region = None
    }

  let bind_var t v =
    let v, variables = Variable_name_map.bind t.variables v in
    v, { t with variables }

  let bind_bound_var t v = bind_var t (v |> Bound_var.var)

  let bind_symbol t s =
    let is_local =
      Compilation_unit.equal
        (Symbol.compilation_unit s)
        (Compilation_unit.get_current_exn ())
    in
    if not is_local
    then
      Misc.fatal_errorf "Cannot bind non-local symbol %a@ Current unit is %a"
        Symbol.print s Compilation_unit.print
        (Compilation_unit.get_current_exn ());
    let s, symbols = Symbol_name_map.bind t.symbols s in
    (None, s) |> nowhere, { t with symbols }

  let bind_code_id t c =
    let c, code_ids = Code_id_name_map.bind t.code_ids c in
    c, { t with code_ids }

  let bind_named_continuation t c =
    let c, continuations = Continuation_name_map.bind t.continuations c in
    let c_id = match c with Named c_id -> c_id | Special _ -> assert false in
    c_id, { t with continuations }

  let bind_special_continuation t c ~to_:s =
    let continuations =
      Continuation_name_map.bind_to t.continuations c (Special s)
    in
    { t with continuations }

  let bind_toplevel_region t v = { t with toplevel_region = Some v }

  let find_var_exn t v = Variable_name_map.find_exn t.variables v

  let find_symbol_exn t s =
    let cunit = Symbol.compilation_unit s in
    let is_local =
      Compilation_unit.equal cunit (Compilation_unit.get_current_exn ())
    in
    if is_local
    then (None, Symbol_name_map.find_exn t.symbols s) |> nowhere
    else
      let cunit =
        let ident =
          Compilation_unit.name cunit |> Compilation_unit.Name.to_string
        in
        let linkage_name = Compilation_unit.full_path_as_string cunit in
        let linkage_name =
          if String.equal ident linkage_name then None else Some linkage_name
        in
        { Fexpr.ident; linkage_name }
      in
      let linkage_name = Symbol.linkage_name s |> Linkage_name.to_string in
      (Some cunit, linkage_name) |> nowhere

  let find_code_id_exn t c = Code_id_name_map.find_exn t.code_ids c

  let find_continuation_exn t c =
    Continuation_name_map.find_exn t.continuations c

  let find_region_exn t r : Fexpr.region =
    match t.toplevel_region with
    | Some toplevel_region when Variable.equal toplevel_region r -> Toplevel
    | _ -> Named (find_var_exn t r)

  let translate_function_slot t c =
    Function_slot_name_map.translate t.function_slots c

  let translate_value_slot t v =
    Value_slot_name_map.translate t.vars_within_closures v
end

let name env n =
  Name.pattern_match n
    ~var:(fun v : Fexpr.name -> Var (Env.find_var_exn env v))
    ~symbol:(fun s : Fexpr.name -> Symbol (Env.find_symbol_exn env s))

let float f = f |> Numeric_types.Float_by_bit_pattern.to_float

let vec128 v = v |> Vector_types.Vec128.Bit_pattern.to_bits

let targetint i = i |> Targetint_32_64.to_int64

let const c : Fexpr.const =
  match Reg_width_const.descr c with
  | Naked_immediate imm ->
    Naked_immediate
      (imm |> Targetint_31_63.to_targetint |> Targetint_32_64.to_string)
  | Tagged_immediate imm ->
    Tagged_immediate
      (imm |> Targetint_31_63.to_targetint |> Targetint_32_64.to_string)
  | Naked_float f -> Naked_float (f |> float)
  | Naked_int32 i -> Naked_int32 i
  | Naked_int64 i -> Naked_int64 i
  | Naked_vec128 bits ->
    Naked_vec128 (Vector_types.Vec128.Bit_pattern.to_bits bits)
  | Naked_nativeint i -> Naked_nativeint (i |> targetint)

let depth_or_infinity (d : int Or_infinity.t) : Fexpr.rec_info =
  match d with Finite d -> Depth d | Infinity -> Infinity

let rec rec_info env (ri : Rec_info_expr.t) : Fexpr.rec_info =
  match ri with
  | Const { depth; unrolling } -> (
    match unrolling with
    | Not_unrolling -> depth_or_infinity depth
    | Unrolling { remaining_depth } ->
      Unroll (remaining_depth, depth_or_infinity depth)
    | Do_not_unroll -> (
      match depth with
      | Infinity -> Do_not_inline
      | Finite _ ->
        Misc.fatal_errorf "unexpected finite depth with Do_not_unroll:@ %a"
          Rec_info_expr.print ri))
  | Var dv -> Var (Env.find_var_exn env dv)
  | Succ ri -> Succ (rec_info env ri)
  | Unroll_to (d, ri) -> Unroll (d, rec_info env ri)

let coercion env (co : Coercion.t) : Fexpr.coercion =
  match co with
  | Id -> Id
  | Change_depth { from; to_ } ->
    let from = rec_info env from in
    let to_ = rec_info env to_ in
    Change_depth { from; to_ }

let simple env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:co : Fexpr.simple ->
      let s : Fexpr.simple =
        match name env n with Var v -> Var v | Symbol s -> Symbol s
      in
      if Coercion.is_id co
      then s
      else
        let co = coercion env co in
        Coerce (s, co))
    ~const:(fun c -> Fexpr.Const (const c))

let is_default_kind_with_subkind (k : Flambda_kind.With_subkind.t) =
  Flambda_kind.is_value (Flambda_kind.With_subkind.kind k)
  && not (Flambda_kind.With_subkind.has_useful_subkind_info k)

let rec subkind (k : Flambda_kind.With_subkind.Subkind.t) : Fexpr.subkind =
  match k with
  | Anything -> Anything
  | Boxed_float -> Boxed_float
  | Boxed_int32 -> Boxed_int32
  | Boxed_int64 -> Boxed_int64
  | Boxed_nativeint -> Boxed_nativeint
  | Boxed_vec128 -> Boxed_vec128
  | Tagged_immediate -> Tagged_immediate
  | Variant { consts; non_consts } -> variant_subkind consts non_consts
  | Float_array -> Float_array
  | Immediate_array -> Immediate_array
  | Value_array -> Value_array
  | Generic_array -> Generic_array
  | Float_block { num_fields } -> Float_block { num_fields }
  | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array ->
    Misc.fatal_error
      "fexpr support for unboxed int32/64/nativeint arrays not yet implemented"

and variant_subkind consts non_consts : Fexpr.subkind =
  let consts =
    consts |> Targetint_31_63.Set.elements |> List.map Targetint_31_63.to_int64
  in
  let non_consts =
    non_consts |> Tag.Scannable.Map.bindings
    |> List.map (fun (tag, sk) ->
           Tag.Scannable.to_int tag, List.map kind_with_subkind sk)
  in
  Variant { consts; non_consts }

and kind_with_subkind (k : Flambda_kind.With_subkind.t) :
    Fexpr.kind_with_subkind =
  match Flambda_kind.With_subkind.kind k with
  | Value -> Value (subkind (Flambda_kind.With_subkind.subkind k))
  | Naked_number nnk -> Naked_number nnk
  | Region -> Region
  | Rec_info -> Rec_info

let kind_with_subkind_opt (k : Flambda_kind.With_subkind.t) :
    Fexpr.kind_with_subkind option =
  if is_default_kind_with_subkind k then None else Some (k |> kind_with_subkind)

let is_default_arity (a : [`Unarized] Flambda_arity.t) =
  match Flambda_arity.unarized_components a with
  | [k] -> is_default_kind_with_subkind k
  | _ -> false

let complex_arity (a : [`Complex] Flambda_arity.t) : Fexpr.arity =
  (* CR mshinwell: add unboxed arities to Fexpr *)
  Flambda_arity.unarize a |> List.map kind_with_subkind

let arity (a : [`Unarized] Flambda_arity.t) : Fexpr.arity =
  (* CR mshinwell: add unboxed arities to Fexpr *)
  Flambda_arity.unarized_components a |> List.map kind_with_subkind

let arity_opt (a : [`Unarized] Flambda_arity.t) : Fexpr.arity option =
  if is_default_arity a then None else Some (arity a)

let kinded_parameter env (kp : Bound_parameter.t) :
    Fexpr.kinded_parameter * Env.t =
  let k = Bound_parameter.kind kp |> kind_with_subkind_opt in
  let param, env = Env.bind_var env (Bound_parameter.var kp) in
  { param; kind = k }, env

let targetint_ocaml (i : Targetint_31_63.t) : Fexpr.targetint =
  i |> Targetint_31_63.to_int64

let recursive_flag (r : Recursive.t) : Fexpr.is_recursive =
  match r with Recursive -> Recursive | Non_recursive -> Nonrecursive

let alloc_mode_for_allocations env (alloc : Alloc_mode.For_allocations.t) :
    Fexpr.alloc_mode_for_allocations =
  match alloc with
  | Heap -> Heap
  | Local { region = r } ->
    let r = Env.find_region_exn env r in
    Local { region = r }

let alloc_mode_for_assignments _env (alloc : Alloc_mode.For_assignments.t) :
    Fexpr.alloc_mode_for_assignments =
  match alloc with Heap -> Heap | Local -> Local

let init_or_assign env (ia : Flambda_primitive.Init_or_assign.t) :
    Fexpr.init_or_assign =
  match ia with
  | Initialization -> Initialization
  | Assignment alloc -> Assignment (alloc_mode_for_assignments env alloc)

let nullop _env (op : Flambda_primitive.nullary_primitive) : Fexpr.nullop =
  match op with
  | Begin_region -> Begin_region
  | Begin_try_region -> Begin_try_region
  | Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _ ->
    Misc.fatal_errorf "TODO: Nullary primitive: %a" Flambda_primitive.print
      (Flambda_primitive.Nullary op)

let unop env (op : Flambda_primitive.unary_primitive) : Fexpr.unop =
  match op with
  | Array_length ak -> Array_length ak
  | Box_number (bk, alloc) ->
    Box_number (bk, alloc_mode_for_allocations env alloc)
  | Tag_immediate -> Tag_immediate
  | Get_tag -> Get_tag
  | End_region -> End_region
  | End_try_region -> End_try_region
  | Int_arith (i, o) -> Int_arith (i, o)
  | Is_flat_float_array -> Is_flat_float_array
  | Is_int _ -> Is_int (* CR vlaviron: discuss *)
  | Num_conv { src; dst } -> Num_conv { src; dst }
  | Opaque_identity _ -> Opaque_identity
  | Unbox_number bk -> Unbox_number bk
  | Untag_immediate -> Untag_immediate
  | Project_value_slot { project_from; value_slot } ->
    let project_from = Env.translate_function_slot env project_from in
    let value_slot = Env.translate_value_slot env value_slot in
    Project_value_slot { project_from; value_slot }
  | Project_function_slot { move_from; move_to } ->
    let move_from = Env.translate_function_slot env move_from in
    let move_to = Env.translate_function_slot env move_to in
    Project_function_slot { move_from; move_to }
  | String_length string_or_bytes -> String_length string_or_bytes
  | Boolean_not -> Boolean_not
  | Int_as_pointer _ | Duplicate_block _ | Duplicate_array _ | Bigarray_length _
  | Float_arith _ | Reinterpret_int64_as_float | Is_boxed_float | Obj_dup
  | Get_header | Atomic_load _ ->
    Misc.fatal_errorf "TODO: Unary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Unary op)

let block_access_kind (bk : Flambda_primitive.Block_access_kind.t) :
    Fexpr.block_access_kind =
  let size (s : _ Or_unknown.t) =
    match s with Known s -> Some (s |> targetint_ocaml) | Unknown -> None
  in
  match bk with
  | Values { field_kind; size = s; tag } ->
    let size = s |> size in
    let tag =
      match tag with
      | Unknown -> None
      | Known tag -> Some (tag |> Tag.Scannable.to_int)
    in
    Values { field_kind; size; tag }
  | Naked_floats { size = s } ->
    let size = s |> size in
    Naked_floats { size }
  | Mixed { tag; size = s; field_kind } ->
    let size = s |> size in
    let tag =
      match tag with
      | Unknown -> None
      | Known tag -> Some (tag |> Tag.Scannable.to_int)
    in
    Mixed { tag; size; field_kind }

let binop (op : Flambda_primitive.binary_primitive) : Fexpr.binop =
  match op with
  | Array_load (ak, width, mut) -> Array_load (ak, width, mut)
  | Block_load (access_kind, mutability) ->
    let access_kind = block_access_kind access_kind in
    Block_load (access_kind, mutability)
  | Phys_equal op -> Phys_equal op
  | Int_arith (Tagged_immediate, o) -> Infix (Int_arith o)
  | Int_arith
      (((Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) as i), o)
    ->
    Int_arith (i, o)
  | Int_comp (i, c) -> Int_comp (i, c)
  | Int_shift (Tagged_immediate, s) -> Infix (Int_shift s)
  | Int_shift (i, s) -> Int_shift (i, s)
  | Float_arith o -> Infix (Float_arith o)
  | Float_comp c -> Infix (Float_comp c)
  | String_or_bigstring_load (slv, saw) -> String_or_bigstring_load (slv, saw)
  | Bigarray_get_alignment align -> Bigarray_get_alignment align
  | Bigarray_load _ | Atomic_exchange | Atomic_fetch_and_add ->
    Misc.fatal_errorf "TODO: Binary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Binary op)

let ternop env (op : Flambda_primitive.ternary_primitive) : Fexpr.ternop =
  match op with
  | Array_set (ak, width) ->
    let ia = Flambda_primitive.Array_set_kind.init_or_assign ak in
    let ak = Flambda_primitive.Array_set_kind.array_kind ak in
    Array_set (ak, width, init_or_assign env ia)
  | Block_set (bk, ia) -> Block_set (block_access_kind bk, init_or_assign env ia)
  | Bytes_or_bigstring_set (blv, saw) -> Bytes_or_bigstring_set (blv, saw)
  | Bigarray_set _ | Atomic_compare_and_set ->
    Misc.fatal_errorf "TODO: Ternary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Ternary op)

let varop env (op : Flambda_primitive.variadic_primitive) : Fexpr.varop =
  match op with
  | Make_block (Values (tag, _), mutability, alloc) ->
    let tag = tag |> Tag.Scannable.to_int in
    let alloc = alloc_mode_for_allocations env alloc in
    Make_block (tag, mutability, alloc)
  | Make_block (Naked_floats, _, _) | Make_array _ | Make_mixed_block _ ->
    Misc.fatal_errorf "TODO: Variadic primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Variadic op)

let prim env (p : Flambda_primitive.t) : Fexpr.prim =
  match p with
  | Nullary op -> Nullary (nullop env op)
  | Unary (op, arg) -> Unary (unop env op, simple env arg)
  | Binary (op, arg1, arg2) ->
    Binary (binop op, simple env arg1, simple env arg2)
  | Ternary (op, arg1, arg2, arg3) ->
    Ternary (ternop env op, simple env arg1, simple env arg2, simple env arg3)
  | Variadic (op, args) -> Variadic (varop env op, List.map (simple env) args)

let value_slots env map =
  List.map
    (fun (var, value) ->
      let kind = Value_slot.kind var in
      if not
           (Flambda_kind.equal
              (Flambda_kind.With_subkind.kind kind)
              Flambda_kind.value)
      then
        Misc.fatal_errorf "Value slot %a not of kind Value" Simple.print value;
      let var = Env.translate_value_slot env var in
      let value = simple env value in
      { Fexpr.var; value })
    (map |> Value_slot.Map.bindings)

let function_declaration env code_id function_slot alloc : Fexpr.fun_decl =
  let code_id = Env.find_code_id_exn env code_id in
  let function_slot = Env.translate_function_slot env function_slot in
  (* Omit the function slot when possible *)
  let function_slot =
    if String.equal code_id.txt function_slot.txt
    then None
    else Some function_slot
  in
  let alloc = alloc |> alloc_mode_for_allocations env in
  { code_id; function_slot; alloc }

let set_of_closures env sc =
  let alloc = Set_of_closures.alloc_mode sc in
  let fun_decls =
    List.map
      (fun (function_slot, fun_decl) ->
        function_declaration env fun_decl function_slot alloc)
      (Set_of_closures.function_decls sc
      |> Function_declarations.funs_in_order |> Function_slot.Lmap.bindings)
  in
  let elts = value_slots env (Set_of_closures.value_slots sc) in
  let elts = match elts with [] -> None | _ -> Some elts in
  fun_decls, elts

let field_of_block env (field : Field_of_static_block.t) : Fexpr.field_of_block
    =
  match field with
  | Symbol symbol -> Symbol (Env.find_symbol_exn env symbol)
  | Tagged_immediate imm ->
    Tagged_immediate
      (imm |> Targetint_31_63.to_targetint |> Targetint_32_64.to_string)
  | Dynamically_computed (var, _dbg) ->
    Dynamically_computed (Env.find_var_exn env var)

let or_variable f env (ov : _ Or_variable.t) : _ Fexpr.or_variable =
  match ov with
  | Const c -> Const (f c)
  | Var (v, _dbg) -> Var (Env.find_var_exn env v)

let static_const env (sc : Static_const.t) : Fexpr.static_data =
  match sc with
  | Block (tag, mutability, fields) ->
    let tag = tag |> Tag.Scannable.to_int in
    let elements = List.map (field_of_block env) fields in
    Block { tag; mutability; elements }
  | Set_of_closures _ -> assert false
  | Boxed_float f -> Boxed_float (or_variable float env f)
  | Boxed_int32 i -> Boxed_int32 (or_variable Fun.id env i)
  | Boxed_int64 i -> Boxed_int64 (or_variable Fun.id env i)
  | Boxed_nativeint i -> Boxed_nativeint (or_variable targetint env i)
  | Boxed_vec128 i -> Boxed_vec128 (or_variable vec128 env i)
  | Immutable_float_block elements ->
    Immutable_float_block (List.map (or_variable float env) elements)
  | Immutable_float_array elements ->
    Immutable_float_array (List.map (or_variable float env) elements)
  | Immutable_value_array elements ->
    Immutable_value_array (List.map (field_of_block env) elements)
  | Immutable_int32_array _ | Immutable_int64_array _
  | Immutable_nativeint_array _ ->
    Misc.fatal_error
      "fexpr support for unboxed int32/64/nativeint arrays not yet implemented"
  | Empty_array array_kind -> Empty_array array_kind
  | Mutable_string { initial_value } -> Mutable_string { initial_value }
  | Immutable_string s -> Immutable_string s

let inlining_state (is : Inlining_state.t) : Fexpr.inlining_state option =
  if Inlining_state.equal is (Inlining_state.default ~round:0)
  then None
  else
    let depth = Inlining_state.depth is in
    (* TODO: inlining arguments *)
    Some { depth }

let rec expr env e =
  match Flambda.Expr.descr e with
  | Let l -> let_expr env l
  | Let_cont lc -> let_cont_expr env lc
  | Apply app -> apply_expr env app
  | Apply_cont app_cont -> apply_cont_expr env app_cont
  | Switch switch -> switch_expr env switch
  | Invalid { message } -> invalid_expr env ~message

and let_expr env le =
  Flambda.Let_expr.pattern_match le ~f:(fun bound ~body : Fexpr.expr ->
      let defining_expr = Flambda.Let_expr.defining_expr le in
      match bound with
      | Singleton var -> dynamic_let_expr env [var] defining_expr body
      | Set_of_closures value_slots ->
        dynamic_let_expr env value_slots defining_expr body
      | Static bound_static ->
        static_let_expr env bound_static defining_expr body)

and dynamic_let_expr env vars (defining_expr : Flambda.Named.t) body :
    Fexpr.expr =
  let vars, body_env = map_accum_left Env.bind_bound_var env vars in
  let body = expr body_env body in
  let defining_exprs, value_slots =
    match defining_expr with
    | Simple s -> ([Simple (simple env s)] : Fexpr.named list), None
    | Prim (p, _dbg) -> ([Prim (prim env p)] : Fexpr.named list), None
    | Set_of_closures sc ->
      let fun_decls, value_slots = set_of_closures env sc in
      let defining_exprs =
        List.map (fun decl : Fexpr.named -> Fexpr.Closure decl) fun_decls
      in
      defining_exprs, value_slots
    | Rec_info ri -> ([Rec_info (rec_info env ri)] : Fexpr.named list), None
    | Static_consts _ -> assert false
  in
  if List.compare_lengths vars defining_exprs <> 0
  then Misc.fatal_error "Mismatched vars vs. values";
  let bindings =
    List.map2
      (fun var defining_expr -> { Fexpr.var; defining_expr })
      vars defining_exprs
  in
  Let { bindings; value_slots; body }

and static_let_expr env bound_static defining_expr body : Fexpr.expr =
  let static_consts =
    Named.must_be_static_consts defining_expr |> Static_const_group.to_list
  in
  let bound_static = bound_static |> Bound_static.to_list in
  let env =
    let bind_names env (pat : Bound_static.Pattern.t) =
      match pat with
      | Code _code_id ->
        (* Already bound at the beginning; see [bind_all_code_ids] *)
        env
      | Block_like symbol ->
        let _, env = Env.bind_symbol env symbol in
        env
      | Set_of_closures closure_symbols ->
        Function_slot.Lmap.fold
          (fun _function_slot symbol env ->
            let _, env = Env.bind_symbol env symbol in
            env)
          closure_symbols env
    in
    List.fold_left bind_names env bound_static
  in
  let translate_const (pat : Bound_static.Pattern.t)
      (const : Static_const_or_code.t) : Fexpr.symbol_binding =
    match pat, const with
    | Block_like symbol, Static_const const ->
      (* This is a binding occurrence, but it should have been added
       * already during the first pass *)
      let symbol = Env.find_symbol_exn env symbol in
      let defining_expr = static_const env const in
      Data { symbol; defining_expr }
    | Set_of_closures closure_symbols, Static_const const ->
      let set = Static_const.must_be_set_of_closures const in
      let fun_decls, elements = set_of_closures env set in
      let symbols_by_function_slot =
        closure_symbols |> Function_slot.Lmap.bindings
        |> Function_slot.Map.of_list
      in
      let function_slots =
        Set_of_closures.function_decls set
        |> Function_declarations.funs_in_order |> Function_slot.Lmap.keys
      in
      let bindings =
        List.map2
          (fun fun_decl function_slot : Fexpr.static_closure_binding ->
            let symbol =
              Function_slot.Map.find function_slot symbols_by_function_slot
            in
            let symbol = Env.find_symbol_exn env symbol in
            { symbol; fun_decl })
          fun_decls function_slots
      in
      Set_of_closures { bindings; elements }
    | Code code_id, Code code ->
      let code_id = Env.find_code_id_exn env code_id in
      let newer_version_of =
        Option.map (Env.find_code_id_exn env) (Code.newer_version_of code)
      in
      let param_arity = Some (complex_arity (Code.params_arity code)) in
      let ret_arity = Code.result_arity code |> arity_opt in
      let recursive = recursive_flag (Code.recursive code) in
      let inline =
        if Flambda2_terms.Inline_attribute.is_default (Code.inline code)
        then None
        else Some (Code.inline code)
      in
      let loopify =
        if Flambda2_terms.Loopify_attribute.equal (Code.loopify code)
             Default_loopify_and_not_tailrec
        then None
        else Some (Code.loopify code)
      in
      let is_tupled = Code.is_tupled code in
      let params_and_body =
        Flambda.Function_params_and_body.pattern_match
          (Code.params_and_body code)
          ~f:(fun
               ~return_continuation
               ~exn_continuation
               params
               ~body
               ~my_closure
               ~is_my_closure_used:_
               ~my_region
               ~my_depth
               ~free_names_of_body:_
               :
               Fexpr.params_and_body
             ->
            let ret_cont, env =
              Env.bind_named_continuation env return_continuation
            in
            let exn_cont, env =
              Env.bind_named_continuation env exn_continuation
            in
            let params, env =
              map_accum_left kinded_parameter env
                (Bound_parameters.to_list params)
            in
            let closure_var, env = Env.bind_var env my_closure in
            let region_var, env = Env.bind_var env my_region in
            let depth_var, env = Env.bind_var env my_depth in
            let body = expr env body in
            (* CR-someday lmaurer: Omit exn_cont, closure_var if not used *)
            { params;
              ret_cont;
              exn_cont;
              closure_var;
              region_var;
              depth_var;
              body
            })
      in
      let code_size =
        Code.cost_metrics code |> Cost_metrics.size |> Code_size.to_int
      in
      let result_mode : Fexpr.alloc_mode_for_assignments =
        match Code.result_mode code with
        | Alloc_heap -> Heap
        | Alloc_local -> Local
      in
      Code
        { id = code_id;
          newer_version_of;
          param_arity;
          ret_arity;
          recursive;
          inline;
          loopify;
          params_and_body;
          code_size;
          is_tupled;
          result_mode
        }
    | Code code_id, Deleted_code ->
      Deleted_code (code_id |> Env.find_code_id_exn env)
    | (Code _ | Block_like _), _ | Set_of_closures _, (Code _ | Deleted_code) ->
      Misc.fatal_errorf "Mismatched pattern and constant: %a vs. %a"
        Bound_static.Pattern.print pat Static_const_or_code.print const
  in
  let bindings = List.map2 translate_const bound_static static_consts in
  let body = expr env body in
  (* If there's exactly one set of closures, make it implicit *)
  let only_set_of_closures =
    let rec loop only_set (bindings : Fexpr.symbol_binding list) =
      match bindings with
      | [] -> only_set
      | Set_of_closures set :: bindings -> (
        match only_set with None -> loop (Some set) bindings | Some _ -> None)
      | (Data _ | Code _ | Deleted_code _ | Closure _) :: bindings ->
        loop only_set bindings
    in
    loop None bindings
  in
  match only_set_of_closures with
  | None -> Let_symbol { bindings; value_slots = None; body }
  | Some { bindings = _; elements = value_slots } ->
    let bindings =
      List.concat_map
        (fun (binding : Fexpr.symbol_binding) ->
          match binding with
          | Set_of_closures { bindings; elements = _ } ->
            List.map (fun closure -> Fexpr.Closure closure) bindings
          | Data _ | Code _ | Deleted_code _ | Closure _ -> [binding])
        bindings
    in
    Let_symbol { bindings; value_slots; body }

and let_cont_expr env (lc : Flambda.Let_cont_expr.t) =
  match lc with
  | Non_recursive { handler; _ } ->
    Flambda.Non_recursive_let_cont_handler.pattern_match handler
      ~f:(fun c ~body ->
        let sort = Continuation.sort c in
        let c, body_env = Env.bind_named_continuation env c in
        let binding =
          cont_handler env c sort
            (Flambda.Non_recursive_let_cont_handler.handler handler)
        in
        let body = expr body_env body in
        Fexpr.Let_cont { recursive = Nonrecursive; bindings = [binding]; body })
  | Recursive handlers ->
    Flambda.Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params:_ ~body handlers ->
        (* TODO support them *)
        let env =
          Continuation.Set.fold
            (fun c env ->
              let _, env = Env.bind_named_continuation env c in
              env)
            (Flambda.Continuation_handlers.domain handlers)
            env
        in
        let bindings =
          List.map
            (fun (c, handler) ->
              let sort = Continuation.sort c in
              let c =
                match Env.find_continuation_exn env c with
                | Named c -> c
                | Special _ -> assert false
              in
              cont_handler env c sort handler)
            (handlers |> Flambda.Continuation_handlers.to_map
           |> Continuation.Map.bindings)
        in
        let body = expr env body in
        Fexpr.Let_cont { recursive = Recursive; bindings; body })

and cont_handler env cont_id (sort : Continuation.Sort.t) h =
  let is_exn_handler = Flambda.Continuation_handler.is_exn_handler h in
  let sort : Fexpr.continuation_sort option =
    match sort with
    | Normal_or_exn -> if is_exn_handler then Some Exn else None
    | Define_root_symbol ->
      assert (not is_exn_handler);
      Some Define_root_symbol
    | Return | Toplevel_return -> assert false
  in
  Flambda.Continuation_handler.pattern_match h
    ~f:(fun params ~handler : Fexpr.continuation_binding ->
      let params, env =
        map_accum_left kinded_parameter env (Bound_parameters.to_list params)
      in
      let handler = expr env handler in
      { name = cont_id; params; sort; handler })

and apply_expr env (app : Apply_expr.t) : Fexpr.expr =
  (* CR mshinwell: support optional callee *)
  let callee =
    match Apply_expr.callee app with
    | None ->
      Misc.fatal_errorf "Missing callees are not yet supported:@ %a"
        Apply_expr.print app
    | Some callee -> callee
  in
  let func = simple env callee in
  let continuation : Fexpr.result_continuation =
    match Apply_expr.continuation app with
    | Return c -> Return (Env.find_continuation_exn env c)
    | Never_returns -> Never_returns
  in
  let exn_continuation =
    let ec = Apply_expr.exn_continuation app in
    let c =
      match Exn_continuation.extra_args ec with
      | [] -> Exn_continuation.exn_handler ec
      | _ -> Misc.fatal_error "TODO: extra args for exn continuation"
    in
    Env.find_continuation_exn env c
  in
  let args = List.map (simple env) (Apply_expr.args app) in
  let call_kind : Fexpr.call_kind =
    match Apply_expr.call_kind app with
    | Function { function_call = Direct code_id; alloc_mode } ->
      let code_id = Env.find_code_id_exn env code_id in
      let function_slot = None in
      (* CR mshinwell: remove [function_slot] *)
      let alloc = alloc_mode_for_allocations env alloc_mode in
      Function (Direct { code_id; function_slot; alloc })
    | Function
        { function_call = Indirect_unknown_arity | Indirect_known_arity;
          alloc_mode
        } ->
      let alloc = alloc_mode_for_allocations env alloc_mode in
      Function (Indirect alloc)
    | C_call { needs_caml_c_call; _ } -> C_call { alloc = needs_caml_c_call }
    | Method _ -> Misc.fatal_error "TODO: Method call kind"
  in
  let param_arity = Apply_expr.args_arity app in
  let return_arity = Apply_expr.return_arity app in
  let arities : Fexpr.function_arities option =
    match Apply_expr.call_kind app with
    | Function { function_call = Indirect_known_arity; alloc_mode = _ } ->
      let params_arity = Some (complex_arity param_arity) in
      let ret_arity = arity return_arity in
      Some { params_arity; ret_arity }
    | Function { function_call = Direct _; alloc_mode = _ } ->
      if is_default_arity return_arity
      then None
      else
        let params_arity =
          (* Parameter arity is never specified for a direct call *)
          None
        in
        let ret_arity = arity return_arity in
        Some { params_arity; ret_arity }
    | C_call _ ->
      let params_arity = Some (complex_arity param_arity) in
      let ret_arity = arity return_arity in
      Some { params_arity; ret_arity }
    | Function { function_call = Indirect_unknown_arity; alloc_mode = _ }
    | Method _ ->
      None
  in
  let inlined : Fexpr.inlined_attribute option =
    if Flambda2_terms.Inlined_attribute.is_default (Apply_expr.inlined app)
    then None
    else
      match Apply_expr.inlined app with
      | Default_inlined -> Some Default_inlined
      | Hint_inlined -> Some Hint_inlined
      | Always_inlined _ -> Some Always_inlined
      | Unroll (n, _) -> Some (Unroll n)
      | Never_inlined -> Some Never_inlined
  in
  let inlining_state = inlining_state (Apply_expr.inlining_state app) in
  Apply
    { func;
      continuation;
      exn_continuation;
      args;
      call_kind;
      inlined;
      inlining_state;
      arities
    }

and apply_cont_expr env app_cont : Fexpr.expr =
  Apply_cont (apply_cont env app_cont)

and apply_cont env app_cont : Fexpr.apply_cont =
  let cont =
    Env.find_continuation_exn env (Apply_cont_expr.continuation app_cont)
  in
  let trap_action =
    Apply_cont_expr.trap_action app_cont
    |> Option.map (fun (action : Trap_action.t) : Fexpr.trap_action ->
           match action with
           | Push { exn_handler } ->
             let exn_handler = Env.find_continuation_exn env exn_handler in
             Push { exn_handler }
           | Pop { exn_handler; raise_kind } ->
             let exn_handler = Env.find_continuation_exn env exn_handler in
             Pop { exn_handler; raise_kind })
  in
  let args = List.map (simple env) (Apply_cont_expr.args app_cont) in
  { cont; trap_action; args }

and switch_expr env switch : Fexpr.expr =
  let scrutinee = simple env (Switch_expr.scrutinee switch) in
  let cases =
    List.map
      (fun (imm, app_cont) ->
        let tag =
          imm |> Targetint_31_63.to_targetint |> Targetint_32_64.to_int
        in
        let app_cont = apply_cont env app_cont in
        tag, app_cont)
      (Switch_expr.arms switch |> Targetint_31_63.Map.bindings)
  in
  Switch { scrutinee; cases }

and invalid_expr _env ~message : Fexpr.expr = Invalid { message }

(* Iter on all sets of closures of a given program. *)
module Iter = struct
  let rec expr f_c f_s e =
    match (Expr.descr e : Expr.descr) with
    | Let e' -> let_expr f_c f_s e'
    | Let_cont e' -> let_cont f_c f_s e'
    | Apply e' -> apply_expr f_c f_s e'
    | Apply_cont e' -> apply_cont f_c f_s e'
    | Switch e' -> switch f_c f_s e'
    | Invalid { message = _ } -> ()

  and named let_expr (bound_pattern : Bound_pattern.t) f_c f_s n =
    match (n : Named.t) with
    | Simple _ | Prim _ | Rec_info _ -> ()
    | Set_of_closures s ->
      let is_phantom =
        Name_mode.is_phantom (Bound_pattern.name_mode bound_pattern)
      in
      f_s ~closure_symbols:None ~is_phantom s
    | Static_consts consts -> (
      match bound_pattern with
      | Static bound_static -> static_consts f_c f_s bound_static consts
      | Singleton _ | Set_of_closures _ ->
        Misc.fatal_errorf
          "[Static_const] can only be bound to a [Static] pattern:@ %a"
          Let.print let_expr)

  and let_expr f_c f_s t =
    Let.pattern_match t ~f:(fun bound_pattern ~body ->
        let e = Let.defining_expr t in
        named t bound_pattern f_c f_s e;
        expr f_c f_s body)

  and let_cont f_c f_s (let_cont : Flambda.Let_cont.t) =
    match let_cont with
    | Non_recursive { handler; _ } ->
      Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
          let h = Non_recursive_let_cont_handler.handler handler in
          let_cont_aux f_c f_s k h body)
    | Recursive handlers ->
      Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~invariant_params:_ ~body conts ->
          assert (not (Continuation_handlers.contains_exn_handler conts));
          let_cont_rec f_c f_s conts body)

  and let_cont_aux f_c f_s k h body =
    continuation_handler f_c f_s k h;
    expr f_c f_s body

  and let_cont_rec f_c f_s conts body =
    let map = Continuation_handlers.to_map conts in
    Continuation.Map.iter (continuation_handler f_c f_s) map;
    expr f_c f_s body

  and continuation_handler f_c f_s _ h =
    Continuation_handler.pattern_match h ~f:(fun _ ~handler ->
        expr f_c f_s handler)

  (* Expression application, continuation application and Switches only use
     single expressions and continuations, so no sets_of_closures can
     syntatically appear inside. *)
  and apply_expr _ _ _ = ()

  and apply_cont _ _ _ = ()

  and switch _ _ _ = ()

  and static_consts f_c f_s bound_static static_consts =
    Static_const_group.match_against_bound_static static_consts bound_static
      ~init:()
      ~code:(fun () code_id (code : Code.t) ->
        f_c ~id:code_id (Some code);
        let params_and_body = Code.params_and_body code in
        Function_params_and_body.pattern_match params_and_body
          ~f:(fun
               ~return_continuation:_
               ~exn_continuation:_
               _
               ~body
               ~my_closure:_
               ~is_my_closure_used:_
               ~my_region:_
               ~my_depth:_
               ~free_names_of_body:_
             -> expr f_c f_s body))
      ~deleted_code:(fun () code_id -> f_c ~id:code_id None)
      ~set_of_closures:(fun () ~closure_symbols set_of_closures ->
        f_s ~closure_symbols:(Some closure_symbols) ~is_phantom:false
          set_of_closures)
      ~block_like:(fun () _ _ -> ())
end

let ignore_code ~id:_ _ = ()

let ignore_set_of_closures ~closure_symbols:_ ~is_phantom:_ _ = ()

let iter ?(code = ignore_code) ?(set_of_closures = ignore_set_of_closures) unit
    =
  Iter.expr code set_of_closures (Flambda_unit.body unit)

let bind_all_code_ids env unit =
  let env = ref env in
  iter unit ~code:(fun ~id _code ->
      let _id, new_env = Env.bind_code_id !env id in
      env := new_env);
  !env

let conv flambda_unit =
  let done_ = Flambda_unit.return_continuation flambda_unit in
  let error = Flambda_unit.exn_continuation flambda_unit in
  let toplevel = Flambda_unit.toplevel_my_region flambda_unit in
  let env = Env.create () in
  let env = Env.bind_special_continuation env done_ ~to_:Done in
  let env = Env.bind_special_continuation env error ~to_:Error in
  let env = Env.bind_toplevel_region env toplevel in
  (* Bind all code ids in toplevel let bindings at the start, since they don't
     necessarily occur in dependency order *)
  let env = bind_all_code_ids env flambda_unit in
  let body = expr env (Flambda_unit.body flambda_unit) in
  { Fexpr.body }
