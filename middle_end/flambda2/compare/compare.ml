open! Int_replace_polymorphic_compare
open Flambda

(* General notes on comparison
 *
 * The particular purpose of this comparison code is comparing two Flambda
 * units, where one was written by a human as the expected result of a test case and the other was
 * written by the simplifier.  Since the human can't anticipate the names of
 * things that get generated inside the simplifier, our notion of equivalence
 * has to account not only for bound names of variables and continuations but
 * also for symbols, code ids, closure ids, and closure variables.  Of these,
 * closure ids and closure variables are particularly tricky since they have no
 * binding occurrences, so we need unification to figure out which ones
 * correspond.
 *
 * Also, in the case that two terms are different, we want to produce a useful
 * diff that isn't cluttered by differences that are ignored by the comparison
 * process.  Therefore the main comparison functions, in the case that the
 * inputs are different, return an *approximant*: a term that is
 * alpha-equivalent (in our expanded sense) to the first input but as close as
 * possible to the second.
 *
 * Suppose we have two terms, F(a1, b1, ...) and G(a2, b2, ...) where F and G
 * are constructors (possibly the same constructor, of course).  Things
 * proceed like so (subst is a function described below):
 *
 *   1. If F and G are the same, assume for illustration that F has two fields,
 *      so our LHS term is F(a1, b1) and our RHS term is F(a2, b2). Then:
 *      (a) If a1 and b2 are equivalent, now compare b1 and b2.
 *          (i) If they are equivalent, the terms are equivalent.
 *          (ii) Otherwise, we have an approximant b1', and the whole
 *               approximant is F(a2, b1'). (* see note below *)
 *      (b) Otherwise, we have an approximant a1', and ...
 *          (i) If b1 and b2 are of types that may contain names that require
 *              unification, compare b1 and b2.
 *              (I) If they are equivalent, the approximant is F(a1', b2).
 *              (II) Otherwise, we have an approximant b1', and the whole
 *                   approximant is F(a1', b1').
 *          (ii) Otherwise, take the approximant to be F(a1', subst b1).
 *   2. If F and G are different, the approximant is subst F(a1, b1, ...).
 *
 * Threaded throughout this pseudocode is an environment that gets updated
 * either by binding constructs (for symbols and code ids) or, for identifier
 * classes with no binding constructs (closure ids and closure vars), by
 * unification as a side effect.  This is why step 1(b)(i) compares terms even
 * though we know we're returning [Different]---we need to do unification on
 * those sub-terms.
 *
 * Note in step 1(a)(ii) that we use a2, not a1.  Remember, the approximant is
 * something alpha-equivalent to the LHS and as close as possible to the RHS.
 * Since we know a1 and a2 are alpha-equivalent, we therefore want to use a2.
 * Step 1(b)(i)(I) is similar.
 *
 * The subst function performs all substitutions in the environment on the given
 * term.  It's used when we have no common structure in the two terms, so
 * comparison cannot continue, but we still want the approximant to be
 * consistent with outer bindings and unifications. *)

module Comparison = struct
  type 'a t =
    | Equivalent
    | Different of { approximant : 'a }

  let map ~f = function
    | Equivalent -> Equivalent
    | Different { approximant } -> Different { approximant = f approximant }

  (* let map2 ~f c1 c2 = match c1, c2 with | Equivalent, Equivalent ->
     Equivalent | different, Equivalent | Equivalent, different -> different |
     Different { approximant = a1 }, Different { approximant = a2 } -> *)

  let is_equivalent = function Equivalent -> true | Different _ -> false

  let approximant t ~default =
    match t with
    | Equivalent -> default
    | Different { approximant } -> approximant

  (* If equivalent, return [if_equivalent]; otherwise, return the approximant
   * and set the given flag to false. Intended for chaining several comparisons
   * together without needing to do a case analysis on each one. *)
  let chain t ~if_equivalent ~ok =
    match t with
    | Equivalent -> if_equivalent
    | Different { approximant } ->
      ok := false;
      approximant

  (* Check that a Boolean condition holds in addition to the usual comparison.
   * Uses the supplied approximant (thunk) if the comparison is [Equivalent] and
   * the condition fails. *)
  let add_condition t ~cond ~approximant =
    if cond then t else Different { approximant = approximant () }

  let [@ocamlformat "disable"] print f ppf t =
    match t with
    | Equivalent -> Format.fprintf ppf "Equivalent"
    | Different { approximant } ->
      Format.fprintf ppf "@[<hov>Different {@;<1 2>approximant = %a@ }@]"
        f approximant
end

let debugging = false

let debugging_verbose = false

let log f e1 e2 thunk =
  if debugging
  then begin
    if debugging_verbose
    then begin
      Format.eprintf
        "@[<v>@[<hv>COMPARING@;<1 2>%a@;<1 0>TO@;<1 2>%a@]@,---@;<0 2>" f e1 f
        e2;
      let ans = thunk () in
      Format.eprintf "%a@]@," (Comparison.print f) ans;
      ans
    end
    else
      let ans : _ Comparison.t = thunk () in
      begin
        match ans with
        | Equivalent -> ()
        | Different { approximant } ->
          Format.eprintf
            "@[<hv>FOUND DIFFERENCE:@;\
             <1 2>%a@;\
             <1 0>!=@;\
             <1 2>%a@;\
             <1 0>approx@;\
             <1 2>%a@]\n\
             %!"
            f e1 f e2 f approximant
      end;
      ans
  end
  else thunk ()

let log_rel f e1 rel e2 =
  if debugging && debugging_verbose
  then Format.eprintf "@[<hv>%a@;<1 2>%s@;<1 0>%a@]@," f e1 rel f e2

let log_eq p f e1 e2 =
  if debugging && debugging_verbose
  then
    let rel = if p e1 e2 then "=" else "/=" in
    log_rel f e1 rel e2

let log_comp c f e1 e2 =
  if debugging && debugging_verbose
  then
    let rel = match c e1 e2 with n when n < 0 -> "<" | 0 -> "=" | _ -> ">" in
    log_rel f e1 rel e2

module Env = struct
  (* We rely on unification for closure ids and closure vars, so for those,
   * we need to keep a map both ways so that we know whether a given (say)
   * closure id in the right-hand term has already been mapped to something
   * different.  In contrast, we expect to have already seen a symbol or code
   * id at its binding site, so if it's not in the map we know straight away
   * something's wrong. *)
  type t =
    { symbols : Symbol.t Symbol.Tbl.t;
      code_ids : Code_id.t Code_id.Tbl.t;
      closure_ids : Closure_id.t Closure_id.Tbl.t;
      closure_ids_rev : Closure_id.t Closure_id.Tbl.t;
      closure_vars : Var_within_closure.t Var_within_closure.Tbl.t;
      closure_vars_rev : Var_within_closure.t Var_within_closure.Tbl.t
    }

  let create () =
    { symbols = Symbol.Tbl.create 10;
      code_ids = Code_id.Tbl.create 10;
      closure_ids = Closure_id.Tbl.create 10;
      closure_ids_rev = Closure_id.Tbl.create 10;
      closure_vars = Var_within_closure.Tbl.create 10;
      closure_vars_rev = Var_within_closure.Tbl.create 10
    }

  let add_symbol t symbol1 symbol2 = Symbol.Tbl.add t.symbols symbol1 symbol2

  let add_code_id t code_id1 code_id2 =
    Code_id.Tbl.add t.code_ids code_id1 code_id2

  let add_closure_id t closure_id1 closure_id2 =
    Closure_id.Tbl.add t.closure_ids closure_id1 closure_id2;
    Closure_id.Tbl.add t.closure_ids_rev closure_id2 closure_id1

  let add_closure_var t closure_var1 closure_var2 =
    Var_within_closure.Tbl.add t.closure_vars closure_var1 closure_var2;
    Var_within_closure.Tbl.add t.closure_vars_rev closure_var2 closure_var1

  let find_symbol t sym = Symbol.Tbl.find_opt t.symbols sym

  let find_code_id t code_id = Code_id.Tbl.find_opt t.code_ids code_id

  let find_closure_id t closure_id =
    Closure_id.Tbl.find_opt t.closure_ids closure_id

  let find_closure_id_rev t closure_id =
    Closure_id.Tbl.find_opt t.closure_ids_rev closure_id

  let find_closure_var t closure_var =
    Var_within_closure.Tbl.find_opt t.closure_vars closure_var

  let find_closure_var_rev t closure_var =
    Var_within_closure.Tbl.find_opt t.closure_vars_rev closure_var
end

let subst_closure_id (env : Env.t) closure_id =
  Env.find_closure_id env closure_id |> Option.value ~default:closure_id

let subst_code_id (env : Env.t) code_id =
  Env.find_code_id env code_id |> Option.value ~default:code_id

let subst_symbol (env : Env.t) symbol =
  Env.find_symbol env symbol |> Option.value ~default:symbol

let subst_closure_var (env : Env.t) var =
  Env.find_closure_var env var |> Option.value ~default:var

let subst_name env n =
  Name.pattern_match n
    ~var:(fun _ -> n)
    ~symbol:(fun s -> Name.symbol (subst_symbol env s))

let subst_simple env s =
  Simple.pattern_match s
    ~const:(fun _ -> s)
    ~name:(fun n ~coercion:_ ->
      (* CR lmaurer: Coercion dropped! *)
      Simple.name (subst_name env n))

let subst_unary_primitive env (p : Flambda_primitive.unary_primitive) :
    Flambda_primitive.unary_primitive =
  match p with
  | Select_closure { move_from; move_to } ->
    let move_from = subst_closure_id env move_from in
    let move_to = subst_closure_id env move_to in
    Select_closure { move_from; move_to }
  | Project_var { project_from; var } ->
    let project_from = subst_closure_id env project_from in
    let var = subst_closure_var env var in
    Project_var { project_from; var }
  | _ -> p

let subst_primitive env (p : Flambda_primitive.t) : Flambda_primitive.t =
  match p with
  | Unary (unary_primitive, arg) ->
    Unary (subst_unary_primitive env unary_primitive, subst_simple env arg)
  | _ -> p

let subst_func_decl env code_id = subst_code_id env code_id

let subst_func_decls env decls =
  Function_declarations.funs_in_order decls
  |> Closure_id.Lmap.bindings
  |> List.map (fun (closure_id, func_decl) ->
         let closure_id = subst_closure_id env closure_id in
         let func_decl = subst_func_decl env func_decl in
         closure_id, func_decl)
  |> Closure_id.Lmap.of_list |> Function_declarations.create

let subst_set_of_closures env set =
  let decls = subst_func_decls env (Set_of_closures.function_decls set) in
  let closure_elements =
    Set_of_closures.closure_elements set
    |> Var_within_closure.Map.bindings
    |> List.map (fun (var, simple) ->
           subst_closure_var env var, subst_simple env simple)
    |> Var_within_closure.Map.of_list
  in
  Set_of_closures.create decls ~closure_elements

let subst_rec_info_expr _env ri =
  (* Only depth variables can occur in [Rec_info_expr], and we only mess with
     symbols and other global names *)
  ri

let subst_field env (field : Field_of_static_block.t) =
  match field with
  | Symbol symbol -> Field_of_static_block.Symbol (subst_symbol env symbol)
  | Tagged_immediate _ | Dynamically_computed _ -> field

let subst_call_kind env (call_kind : Call_kind.t) : Call_kind.t =
  match call_kind with
  | Function (Direct { code_id; closure_id; return_arity }) ->
    let code_id = subst_code_id env code_id in
    let closure_id = subst_closure_id env closure_id in
    Call_kind.direct_function_call code_id closure_id ~return_arity
  | _ -> call_kind

let rec subst_expr env e =
  match Expr.descr e with
  | Let let_expr -> subst_let_expr env let_expr
  | Let_cont let_cont -> subst_let_cont env let_cont
  | Apply apply -> subst_apply env apply
  | Apply_cont apply_cont ->
    subst_apply_cont env apply_cont |> Expr.create_apply_cont
  | Switch switch -> subst_switch env switch
  | Invalid _ -> e

and subst_let_expr env let_expr =
  Let_expr.pattern_match let_expr ~f:(fun bound_pattern ~body ->
      let bound_pattern = subst_bound_pattern env bound_pattern in
      let defining_expr = subst_named env (Let_expr.defining_expr let_expr) in
      let body = subst_expr env body in
      Let.create bound_pattern defining_expr ~body ~free_names_of_body:Unknown
      |> Expr.create_let)

and subst_named env (n : Named.t) =
  match n with
  | Simple s -> Named.create_simple (subst_simple env s)
  | Prim (p, dbg) -> Named.create_prim (subst_primitive env p) dbg
  | Set_of_closures set ->
    Named.create_set_of_closures (subst_set_of_closures env set)
  | Static_consts sc -> Named.create_static_consts (subst_static_consts env sc)
  | Rec_info ri -> Named.create_rec_info (subst_rec_info_expr env ri)

and subst_static_consts env (g : Static_const_group.t) =
  Static_const_group.map g ~f:(subst_static_const env)

and subst_bound_pattern env (blb : Bound_pattern.t) =
  match blb with
  | Symbols { bound_symbols } ->
    let bound_symbols = subst_bound_symbols env bound_symbols in
    Bound_pattern.symbols bound_symbols
  | _ -> blb

and subst_bound_symbols env bound_symbols =
  List.map (subst_pattern env) (bound_symbols |> Bound_symbols.to_list)
  |> Bound_symbols.create

and subst_pattern env (pattern : Bound_symbols.Pattern.t) :
    Bound_symbols.Pattern.t =
  match pattern with
  | Set_of_closures closure_symbols ->
    (* The symbols are in binding position, so we don't need to substitute, but
       we still need to substitute the closure ids *)
    let closure_symbols =
      Closure_id.Lmap.bindings closure_symbols
      |> List.map (fun (closure_id, symbol) ->
             let closure_id = subst_closure_id env closure_id in
             closure_id, symbol)
      |> Closure_id.Lmap.of_list
    in
    Bound_symbols.Pattern.set_of_closures closure_symbols
  | Block_like symbol -> Bound_symbols.Pattern.block_like symbol
  | Code code_id -> Bound_symbols.Pattern.code code_id

and subst_static_const env (static_const : Static_const_or_code.t) :
    Static_const_or_code.t =
  match static_const with
  | Code code -> Static_const_or_code.create_code (subst_code env code)
  | Static_const (Block (tag, mut, fields)) ->
    let fields = List.map (subst_field env) fields in
    Static_const_or_code.create_static_const (Block (tag, mut, fields))
  | Static_const (Set_of_closures set_of_closures) ->
    Static_const_or_code.create_static_const
      (Set_of_closures (subst_set_of_closures env set_of_closures))
  | _ -> static_const

and subst_code env (code : Code.t) : Code.t =
  let params_and_body =
    match Code.params_and_body code with
    | Or_deleted.Present params_and_body ->
      let params_and_body = subst_params_and_body env params_and_body in
      let _names_and_closure_vars names =
        Name_occurrences.(
          union
            (restrict_to_closure_vars names)
            (with_only_names_and_code_ids names |> without_code_ids))
      in
      let free_names =
        (* CR mshinwell: This needs fixing XXX *)
        Name_occurrences.empty
        (* Flambda.Function_params_and_body.free_names params_and_body |>
           names_and_closure_vars *)
      in
      Or_deleted.Present (params_and_body, free_names)
    | Or_deleted.Deleted -> Or_deleted.Deleted
  in
  let newer_version_of =
    Option.map (subst_code_id env) (Code.newer_version_of code)
  in
  code
  |> Code.with_params_and_body ~cost_metrics:(Code.cost_metrics code)
       params_and_body
  |> Code.with_newer_version_of newer_version_of

and subst_params_and_body env params_and_body =
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_depth
         ~free_names_of_body
       ->
      let body = subst_expr env body in
      let dbg = Function_params_and_body.debuginfo params_and_body in
      Function_params_and_body.create ~return_continuation ~exn_continuation
        params ~dbg ~body ~my_closure ~free_names_of_body ~my_depth)

and subst_let_cont env (let_cont_expr : Let_cont_expr.t) =
  match let_cont_expr with
  | Non_recursive { handler; num_free_occurrences = _ } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
        let body = subst_expr env body in
        let handler =
          subst_cont_handler env
            (Non_recursive_let_cont_handler.handler handler)
        in
        Let_cont_expr.create_non_recursive cont handler ~body
          ~free_names_of_body:Unknown)
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body handlers ->
        let body = subst_expr env body in
        let handlers =
          Continuation.Map.map (subst_cont_handler env)
            (handlers |> Continuation_handlers.to_map)
        in
        Let_cont_expr.create_recursive handlers ~body)

and subst_cont_handler env cont_handler =
  Continuation_handler.pattern_match cont_handler ~f:(fun params ~handler ->
      let handler = subst_expr env handler in
      Continuation_handler.create params ~handler ~free_names_of_handler:Unknown
        ~is_exn_handler:(Continuation_handler.is_exn_handler cont_handler))

and subst_apply env apply =
  let callee = subst_simple env (Apply_expr.callee apply) in
  let continuation = Apply_expr.continuation apply in
  let exn_continuation = Apply_expr.exn_continuation apply in
  let args = List.map (subst_simple env) (Apply_expr.args apply) in
  let call_kind = subst_call_kind env (Apply_expr.call_kind apply) in
  let dbg = Apply_expr.dbg apply in
  let inline = Apply_expr.inline apply in
  let inlining_state = Apply_expr.inlining_state apply in
  Apply_expr.create ~callee ~continuation exn_continuation ~args ~call_kind dbg
    ~inline ~inlining_state ~probe_name:None
  |> Expr.create_apply

and subst_apply_cont env apply_cont =
  let trap_action = Apply_cont_expr.trap_action apply_cont in
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = List.map (subst_simple env) (Apply_cont_expr.args apply_cont) in
  let dbg = Apply_cont_expr.debuginfo apply_cont in
  Apply_cont_expr.create ?trap_action cont ~args ~dbg

and subst_switch env switch =
  let scrutinee = subst_simple env (Switch_expr.scrutinee switch) in
  let arms =
    Targetint_31_63.Map.map (subst_apply_cont env) (Switch_expr.arms switch)
  in
  Expr.create_switch (Switch_expr.create ~scrutinee ~arms)

module Comparator = struct
  type 'a t = Env.t -> 'a -> 'a -> 'a Comparison.t

  let of_predicate ~(f : 'a -> 'a -> bool) ?(subst : (Env.t -> 'a -> 'a) option)
      : 'a t =
   fun env a1 a2 ->
    if f a1 a2
    then Equivalent
    else
      let approximant =
        match subst with Some subst -> subst env a1 | None -> a1
      in
      Different { approximant }

  let of_ordering ~(f : 'a -> 'a -> int) ?(subst : (Env.t -> 'a -> 'a) option) :
      'a t =
    of_predicate ~f:(fun a1 a2 -> f a1 a2 = 0) ?subst
end

(* If subst2 is given and the first components of the pairs are unequal, then
 * rather than call f2 on the second components to produce the approximants (see
 * step 1(b)(ii) in the note at the top of the file), it will use the
 * (presumably faster) subst2 function instead. This is *only* safe if
 * unification is unnecessary because the second components cannot contain any
 * closure ids or closure variables. *)
let pairs ~(f1 : 'a Comparator.t) ~(f2 : 'b Comparator.t)
    ?(subst2 : (Env.t -> 'b -> 'b) option) : ('a * 'b) Comparator.t =
 fun env (a1, b1) (a2, b2) ->
  match f1 env a1 a2 with
  | Equivalent -> f2 env b1 b2 |> Comparison.map ~f:(fun b1' -> a2, b1')
  | Different { approximant = a1' } -> begin
    match subst2 with
    | Some subst2 -> Different { approximant = a1', subst2 env b1 }
    | None -> begin
      match f2 env b1 b2 with
      | Equivalent -> Different { approximant = a1', b2 }
      | Different { approximant = b1' } -> Different { approximant = a1', b1' }
    end
  end

let triples ~(f1 : 'a Comparator.t) ~(f2 : 'b Comparator.t)
    ~(f3 : 'c Comparator.t) ?(subst2 : (Env.t -> 'b -> 'b) option)
    ?(subst3 : (Env.t -> 'c -> 'c) option) : ('a * 'b * 'c) Comparator.t =
 fun env (a1, b1, c1) (a2, b2, c2) ->
  let subst23 =
    match subst2, subst3 with
    | Some subst2, Some subst3 ->
      Some (fun env (a, b) -> subst2 env a, subst3 env b)
    | _, _ -> None
  in
  let f2 env bc1 bc2 = pairs ~f1:f2 ~f2:f3 ?subst2:subst3 env bc1 bc2 in
  pairs ~f1 ~f2 ?subst2:subst23 env (a1, (b1, c1)) (a2, (b2, c2))
  |> Comparison.map ~f:(fun (a1', (b1', c1')) -> a1', b1', c1')

let rec lists ~(f : 'a Comparator.t) ~(subst : Env.t -> 'a -> 'a) ~subst_snd :
    'a list Comparator.t =
 fun env list1 list2 ->
  match list1, list2 with
  | a1 :: list1, a2 :: list2 ->
    let subst2 =
      if subst_snd then Some (fun env -> List.map (subst env)) else None
    in
    pairs ~f1:f
      ~f2:(lists ~f ~subst ~subst_snd)
      ?subst2 env (a1, list1) (a2, list2)
    |> Comparison.map ~f:(fun (a1', list1') -> a1' :: list1')
  | [], [] -> Equivalent
  | _, _ ->
    let approximant = List.map (subst env) list1 in
    Different { approximant }

let options ~f ~subst env o1 o2 =
  match o1, o2 with
  | Some a1, Some a2 -> f env a1 a2 |> Comparison.map ~f:(fun a1' -> Some a1')
  | None, None -> Comparison.Equivalent
  | Some a1, None -> Comparison.Different { approximant = Some (subst env a1) }
  | None, Some _ -> Comparison.Different { approximant = None }

let symbols env symbol1 symbol2 : Symbol.t Comparison.t =
  log Symbol.print symbol1 symbol2 (fun () ->
      let symbol1 = subst_symbol env symbol1 in
      if Symbol.equal symbol1 symbol2
      then Equivalent
      else Different { approximant = symbol1 })

let code_ids env code_id1 code_id2 : Code_id.t Comparison.t =
  let code_id1 = subst_code_id env code_id1 in
  if Code_id.equal code_id1 code_id2
  then Equivalent
  else Different { approximant = code_id1 }

let closure_ids env closure_id1 closure_id2 : Closure_id.t Comparison.t =
  match Env.find_closure_id env closure_id1 with
  | Some closure_id ->
    if Closure_id.equal closure_id closure_id2
    then Equivalent
    else Different { approximant = closure_id }
  | None -> (
    match Env.find_closure_id_rev env closure_id2 with
    | Some _ -> Different { approximant = closure_id1 }
    | None ->
      Env.add_closure_id env closure_id1 closure_id2;
      Equivalent)

let closure_vars env closure_var1 closure_var2 :
    Var_within_closure.t Comparison.t =
  match Env.find_closure_var env closure_var1 with
  | Some closure_var ->
    if Var_within_closure.equal closure_var closure_var2
    then Equivalent
    else Different { approximant = closure_var }
  | None -> (
    match Env.find_closure_var_rev env closure_var2 with
    | Some _ -> Different { approximant = closure_var1 }
    | None ->
      Env.add_closure_var env closure_var1 closure_var2;
      Equivalent)

let names env name1 name2 : Name.t Comparison.t =
  log Name.print name1 name2 (fun () ->
      Name.pattern_match name1
        ~var:(fun var1 ->
          Name.pattern_match name2
            ~var:(fun var2 : Name.t Comparison.t ->
              if Variable.equal var1 var2
              then Equivalent
              else
                Different { approximant = name1 }
                (* don't bother with subst when name1 is var *))
            ~symbol:(fun _ -> Comparison.Different { approximant = name1 }))
        ~symbol:(fun symbol1 ->
          Name.pattern_match name2
            ~var:(fun _ ->
              Comparison.Different { approximant = subst_name env name1 })
            ~symbol:(fun symbol2 ->
              symbols env symbol1 symbol2 |> Comparison.map ~f:Name.symbol)))

let simple_exprs env simple1 simple2 : Simple.t Comparison.t =
  Simple.pattern_match simple1
    ~name:(fun name1 ~coercion:_ ->
      (* CR lmaurer: Coercion dropped! *)
      Simple.pattern_match simple2
        ~name:(fun name2 ~coercion:_ ->
          (* CR lmaurer: Coercion dropped! *)
          names env name1 name2 |> Comparison.map ~f:Simple.name)
        ~const:(fun _ ->
          Comparison.Different { approximant = subst_simple env simple1 }))
    ~const:(fun const1 ->
      Simple.pattern_match simple2
        ~name:(fun _ ~coercion:_ ->
          Comparison.Different { approximant = simple1 })
        ~const:(fun const2 : Simple.t Comparison.t ->
          if Reg_width_const.equal const1 const2
          then Equivalent
          else Different { approximant = simple1 }))

let print_list f ppf l =
  let pp_sep ppf () = Format.fprintf ppf ";@;<1 2>" in
  Format.fprintf ppf "@[<hv>[@ %a@ ]@]" (Format.pp_print_list ~pp_sep f) l

let simple_lists env list1 list2 : Simple.t list Comparison.t =
  log (print_list Simple.print) list1 list2 (fun () ->
      lists ~f:simple_exprs ~subst:subst_simple ~subst_snd:true env list1 list2)

let unary_prim_ops env (prim_op1 : Flambda_primitive.unary_primitive)
    (prim_op2 : Flambda_primitive.unary_primitive) :
    Flambda_primitive.unary_primitive Comparison.t =
  match prim_op1, prim_op2 with
  | ( Select_closure { move_from = move_from1; move_to = move_to1 },
      Select_closure { move_from = move_from2; move_to = move_to2 } ) ->
    pairs ~f1:closure_ids ~f2:closure_ids env (move_from1, move_to1)
      (move_from2, move_to2)
    |> Comparison.map ~f:(fun (move_from1', move_to1') ->
           Flambda_primitive.Select_closure
             { move_from = move_from1'; move_to = move_to1' })
  | ( Project_var { project_from = closure_id1; var = var1 },
      Project_var { project_from = closure_id2; var = var2 } ) ->
    pairs ~f1:closure_ids ~f2:closure_vars env (closure_id1, var1)
      (closure_id2, var2)
    |> Comparison.map ~f:(fun (closure_id1', var1') ->
           Flambda_primitive.Project_var
             { project_from = closure_id1'; var = var1' })
  | _, _ ->
    if Flambda_primitive.equal_unary_primitive prim_op1 prim_op2
    then Equivalent
    else Different { approximant = subst_unary_primitive env prim_op1 }

let primitives env prim1 prim2 : Flambda_primitive.t Comparison.t =
  match (prim1 : Flambda_primitive.t), (prim2 : Flambda_primitive.t) with
  | Unary (prim_op1, arg1), Unary (prim_op2, arg2) ->
    pairs ~f1:unary_prim_ops ~f2:simple_exprs ~subst2:subst_simple env
      (prim_op1, arg1) (prim_op2, arg2)
    |> Comparison.map ~f:(fun (prim_op1', arg1') ->
           Flambda_primitive.Unary (prim_op1', arg1'))
  | Binary (prim_op1, arg1_1, arg2_1), Binary (prim_op2, arg1_2, arg2_2) ->
    if Flambda_primitive.equal_binary_primitive prim_op1 prim_op2
    then
      simple_lists env [arg1_1; arg2_1] [arg1_2; arg2_2]
      |> Comparison.map ~f:(function
           | [arg1; arg2] -> Flambda_primitive.Binary (prim_op1, arg1, arg2)
           | _ -> assert false)
    else
      let approximant =
        Flambda_primitive.Binary
          (prim_op1, subst_simple env arg1_1, subst_simple env arg2_1)
      in
      Different { approximant }
  | ( Ternary (prim_op1, arg1_1, arg2_1, arg3_1),
      Ternary (prim_op2, arg1_2, arg2_2, arg3_2) ) ->
    if Flambda_primitive.equal_ternary_primitive prim_op1 prim_op2
    then
      simple_lists env [arg1_1; arg2_1; arg3_1] [arg1_2; arg2_2; arg3_2]
      |> Comparison.map ~f:(function
           | [arg1; arg2; arg3] ->
             Flambda_primitive.Ternary (prim_op1, arg1, arg2, arg3)
           | _ -> assert false)
    else
      let approximant =
        Flambda_primitive.Ternary
          ( prim_op1,
            subst_simple env arg1_1,
            subst_simple env arg2_1,
            subst_simple env arg3_1 )
      in
      Different { approximant }
  | Variadic (prim_op1, args1), Variadic (prim_op2, args2) ->
    if Flambda_primitive.equal_variadic_primitive prim_op1 prim_op2
    then
      simple_lists env args1 args2
      |> Comparison.map ~f:(fun args : Flambda_primitive.t ->
             Variadic (prim_op2, args))
    else
      let approximant : Flambda_primitive.t =
        Variadic (prim_op1, List.map (subst_simple env) args1)
      in
      Different { approximant }
  | _, _ -> Different { approximant = subst_primitive env prim1 }

(* Returns unit because the approximant isn't used by sets_of_closures *)
let function_decls env code_id1 code_id2 : unit Comparison.t =
  if code_ids env code_id1 code_id2 |> Comparison.is_equivalent
  then Equivalent
  else Different { approximant = () }

(** Match up equal elements in two lists and iterate through both of them, using
    [f] analogously to [Map.S.merge] *)
let iter2_merged l1 l2 ~compare ~f =
  let l1 = List.sort compare l1 in
  let l2 = List.sort compare l2 in
  let rec go l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | a1 :: l1, [] ->
      f (Some a1) None;
      go l1 []
    | [], a2 :: l2 ->
      f None (Some a2);
      go [] l2
    | a1 :: l1, a2 :: l2 -> begin
      match compare a1 a2 with
      | 0 ->
        f (Some a1) (Some a2);
        go l1 l2
      | c when c < 0 ->
        f (Some a1) None;
        go l1 (a2 :: l2)
      | _ ->
        f None (Some a2);
        go (a1 :: l1) l2
    end
  in
  go l1 l2

let sets_of_closures env set1 set2 : Set_of_closures.t Comparison.t =
  (* Need to do unification on closure vars and closure ids, we we're going to
   * invert both maps, figuring the closure vars with the same value should be
   * the same.  There is a risk that two closure vars will be mapped to the
   * same value, but that should be rare.  Later, we'll do something
   * similar (and less worrisome) with closure ids. *)
  let closure_vars_by_value set =
    Var_within_closure.Map.bindings (Set_of_closures.closure_elements set)
    |> List.map (fun (var, value) -> subst_simple env value, var)
  in
  (* We want to process the whole map to find new correspondences between
   * closure vars, so we need to remember whether we've found any mismatches *)
  let ok = ref true in
  let () =
    let compare (value1, _var1) (value2, _var2) =
      Simple.compare value1 value2
    in
    iter2_merged (closure_vars_by_value set1) (closure_vars_by_value set2)
      ~compare ~f:(fun elt1 elt2 ->
        match elt1, elt2 with
        | None, None -> ()
        | Some _, None | None, Some _ -> ok := false
        | Some (_value1, var1), Some (_value2, var2) -> begin
          match closure_vars env var1 var2 with
          | Equivalent -> ()
          | Different { approximant = _ } -> ok := false
        end)
  in
  let closure_ids_and_fun_decls_by_code_id set =
    let map = Function_declarations.funs (Set_of_closures.function_decls set) in
    Closure_id.Map.bindings map
    |> List.map (fun (closure_id, code_id) ->
           subst_code_id env code_id, (closure_id, code_id))
    |> Code_id.Map.of_list
  in
  (* Using merge here as a map version of [List.iter2]; always returning None
   * means the returned map is always empty, so this shouldn't waste much *)
  let (_ : unit Code_id.Map.t) =
    Code_id.Map.merge
      (fun _code_id value1 value2 ->
        begin
          match value1, value2 with
          | None, None -> ()
          | Some _, None | None, Some _ -> ok := false
          | Some (closure_id1, fun_decl1), Some (closure_id2, fun_decl2) ->
            begin
            begin
              match closure_ids env closure_id1 closure_id2 with
              | Equivalent -> ()
              | Different _ -> ok := false
            end;
            match function_decls env fun_decl1 fun_decl2 with
            | Equivalent -> ()
            | Different _ -> ok := false
          end
        end;
        None)
      (closure_ids_and_fun_decls_by_code_id set1)
      (closure_ids_and_fun_decls_by_code_id set2)
  in
  if !ok
  then Equivalent
  else Different { approximant = subst_set_of_closures env set1 }

let rec_info_exprs _env rec_info_expr1 rec_info_expr2 :
    Rec_info_expr.t Comparison.t =
  (* Rec_info expressions only ever have occurrences of local variables, so we
     don't need to do any of this alpha-equivalence stuff *)
  if Rec_info_expr.equal rec_info_expr1 rec_info_expr2
  then Equivalent
  else Different { approximant = rec_info_expr1 }

let named_exprs env named1 named2 : Named.t Comparison.t =
  match (named1 : Named.t), (named2 : Named.t) with
  | Simple simple1, Simple simple2 ->
    simple_exprs env simple1 simple2 |> Comparison.map ~f:Named.create_simple
  | Prim (prim1, dbg1), Prim (prim2, _) ->
    primitives env prim1 prim2
    |> Comparison.map ~f:(fun prim -> Named.create_prim prim dbg1)
  | Set_of_closures set1, Set_of_closures set2 ->
    sets_of_closures env set1 set2
    |> Comparison.map ~f:Named.create_set_of_closures
  | Rec_info rec_info_expr1, Rec_info rec_info_expr2 ->
    rec_info_exprs env rec_info_expr1 rec_info_expr2
    |> Comparison.map ~f:Named.create_rec_info
  | Static_consts _static_consts1, Static_consts _static_consts2 ->
    (* CR lmaurer: Oops. Use of a wildcard pattern left this case unimplemented
       back when [Static_consts] was added. Remember, kids, don't use catch-all
       cases. *)
    assert false
  | (Simple _ | Prim _ | Set_of_closures _ | Static_consts _ | Rec_info _), _ ->
    Different { approximant = subst_named env named1 }

(* Compares the two patterns for compatibility *and* adds the
 * correspondences to the environment. *)
let patterns env (pattern1 : Bound_symbols.Pattern.t)
    (pattern2 : Bound_symbols.Pattern.t) : Bound_symbols.Pattern.t Comparison.t
    =
  match pattern1, pattern2 with
  | Code code_id1, Code code_id2 ->
    Env.add_code_id env code_id1 code_id2;
    Equivalent
  | Block_like symbol1, Block_like symbol2 ->
    Env.add_symbol env symbol1 symbol2;
    Equivalent
  | Set_of_closures closure_symbols1, Set_of_closures closure_symbols2 ->
    (* The symbol in a closure binding is in binding position, so we don't
     * need them to match but we do need to record them *)
    let closure_bindings env (closure_id1, symbol1) (closure_id2, symbol2) :
        (Closure_id.t * Symbol.t) Comparison.t =
      Env.add_symbol env symbol1 symbol2;
      closure_ids env closure_id1 closure_id2
      |> Comparison.map ~f:(fun closure_id1' -> closure_id1', symbol2)
    in
    let subst_closure_binding env (closure_id, symbol) =
      subst_closure_id env closure_id, symbol
    in
    let closure_binding_lists =
      lists ~f:closure_bindings ~subst:subst_closure_binding ~subst_snd:false
    in
    closure_binding_lists env
      (closure_symbols1 |> Closure_id.Lmap.bindings)
      (closure_symbols2 |> Closure_id.Lmap.bindings)
    |> Comparison.map ~f:(fun bindings ->
           Bound_symbols.Pattern.set_of_closures
             (bindings |> Closure_id.Lmap.of_list))
  | _, _ -> Different { approximant = subst_pattern env pattern1 }

(* Compares the two sets of bound symbols for compatibility *and* adds the
 * correspondences to the environment. *)
let bound_symbols env bound_symbols1 bound_symbols2 :
    Bound_symbols.t Comparison.t =
  lists ~f:patterns ~subst:subst_pattern ~subst_snd:false env
    (bound_symbols1 |> Bound_symbols.to_list)
    (bound_symbols2 |> Bound_symbols.to_list)
  |> Comparison.map ~f:Bound_symbols.create

let fields env (field1 : Field_of_static_block.t)
    (field2 : Field_of_static_block.t) : Field_of_static_block.t Comparison.t =
  match field1, field2 with
  | Symbol symbol1, Symbol symbol2 ->
    symbols env symbol1 symbol2
    |> Comparison.map ~f:(fun symbol1' -> Field_of_static_block.Symbol symbol1')
  | _, _ ->
    Comparator.of_predicate ~f:Field_of_static_block.equal env field1 field2

let blocks env block1 block2 =
  triples
    ~f1:(Comparator.of_predicate ~f:Tag.Scannable.equal)
    ~f2:(Comparator.of_ordering ~f:Mutability.compare)
    ~f3:(lists ~f:fields ~subst:subst_field ~subst_snd:true)
    ~subst2:(fun _ mut -> mut)
    ~subst3:(fun env -> List.map (subst_field env))
    env block1 block2

let method_kinds _env (method_kind1 : Call_kind.method_kind)
    (method_kind2 : Call_kind.method_kind) : Call_kind.method_kind Comparison.t
    =
  match method_kind1, method_kind2 with
  | Self, Self | Public, Public | Cached, Cached -> Equivalent
  | _, _ -> Different { approximant = method_kind1 }

let call_kinds env (call_kind1 : Call_kind.t) (call_kind2 : Call_kind.t) :
    Call_kind.t Comparison.t =
  match call_kind1, call_kind2 with
  | ( Function
        (Direct
          { code_id = code_id1;
            closure_id = closure_id1;
            return_arity = return_arity1
          }),
      Function
        (Direct
          { code_id = code_id2;
            closure_id = closure_id2;
            return_arity = return_arity2
          }) ) ->
    triples ~f1:code_ids ~f2:closure_ids
      ~f3:(Comparator.of_predicate ~f:Flambda_arity.With_subkinds.equal)
      ~subst3:(fun _ arity -> arity)
      env
      (code_id1, closure_id1, return_arity1)
      (code_id2, closure_id2, return_arity2)
    |> Comparison.map ~f:(fun (code_id, closure_id, return_arity) ->
           Call_kind.direct_function_call code_id closure_id ~return_arity)
  | ( Function
        (Indirect_known_arity
          { param_arity = param_arity1; return_arity = return_arity1 }),
      Function
        (Indirect_known_arity
          { param_arity = param_arity2; return_arity = return_arity2 }) ) ->
    if Flambda_arity.With_subkinds.equal param_arity1 param_arity2
       && Flambda_arity.With_subkinds.equal return_arity1 return_arity2
    then Equivalent
    else Different { approximant = call_kind1 }
  | Function Indirect_unknown_arity, Function Indirect_unknown_arity ->
    Equivalent
  | Method { kind = kind1; obj = obj1 }, Method { kind = kind2; obj = obj2 } ->
    pairs ~f1:method_kinds ~f2:simple_exprs ~subst2:subst_simple env
      (kind1, obj1) (kind2, obj2)
    |> Comparison.map ~f:(fun (kind, obj) -> Call_kind.method_call kind ~obj)
  | ( C_call
        { alloc = alloc1;
          param_arity = param_arity1;
          return_arity = return_arity1
        },
      C_call
        { alloc = alloc2;
          param_arity = param_arity2;
          return_arity = return_arity2
        } ) ->
    if Bool.equal alloc1 alloc2
       && Flambda_arity.equal param_arity1 param_arity2
       && Flambda_arity.equal return_arity1 return_arity2
    then Equivalent
    else Different { approximant = call_kind1 }
  | _, _ -> Different { approximant = call_kind1 }

let inlining_states_equal is1 is2 : bool =
  (* CR lmaurer: Compare inlining arguments once we've added them to the Flambda
     syntax *)
  Inlining_state.depth is1 = Inlining_state.depth is2

let apply_exprs env apply1 apply2 : Expr.t Comparison.t =
  let atomic_things_equal =
    Apply.Result_continuation.equal
      (Apply.continuation apply1)
      (Apply.continuation apply2)
    && Exn_continuation.equal
         (Apply.exn_continuation apply1)
         (Apply.exn_continuation apply2)
    && Inline_attribute.equal (Apply.inline apply1) (Apply.inline apply2)
    && inlining_states_equal
         (Apply.inlining_state apply1)
         (Apply.inlining_state apply2)
  in
  let ok = ref atomic_things_equal in
  let callee1' =
    simple_exprs env (Apply.callee apply1) (Apply.callee apply2)
    |> Comparison.chain ~if_equivalent:(Apply.callee apply2) ~ok
  in
  let args1' =
    simple_lists env (Apply.args apply1) (Apply.args apply2)
    |> Comparison.chain ~if_equivalent:(Apply.args apply2) ~ok
  in
  let call_kind1' =
    call_kinds env (Apply.call_kind apply1) (Apply.call_kind apply2)
    |> Comparison.chain ~if_equivalent:(Apply.call_kind apply2) ~ok
  in
  if !ok
  then Equivalent
  else
    Different
      { approximant =
          Apply.create ~callee:callee1'
            ~continuation:(Apply.continuation apply1)
            (Apply.exn_continuation apply1)
            ~args:args1' ~call_kind:call_kind1' (Apply.dbg apply1)
            ~inline:(Apply.inline apply1)
            ~inlining_state:(Apply.inlining_state apply1)
            ~probe_name:None
          |> Expr.create_apply
      }

let apply_cont_exprs env apply_cont1 apply_cont2 : Apply_cont.t Comparison.t =
  let cont1 = Apply_cont.continuation apply_cont1 in
  let cont2 = Apply_cont.continuation apply_cont2 in
  log_eq Continuation.equal Continuation.print cont1 cont2;
  log_comp
    (Option.compare Trap_action.compare)
    (Format.pp_print_option Trap_action.print)
    (Apply_cont.trap_action apply_cont1)
    (Apply_cont.trap_action apply_cont2);
  if Option.compare Trap_action.compare
       (Apply_cont.trap_action apply_cont1)
       (Apply_cont.trap_action apply_cont2)
     = 0
     && Continuation.equal
          (Apply_cont.continuation apply_cont1)
          (Apply_cont.continuation apply_cont2)
  then
    simple_lists env (Apply_cont.args apply_cont1) (Apply_cont.args apply_cont2)
    |> Comparison.map ~f:(fun args1' ->
           Apply_cont.create
             ?trap_action:(Apply_cont.trap_action apply_cont1)
             (Apply_cont.continuation apply_cont1)
             ~args:args1'
             ~dbg:(Apply_cont.debuginfo apply_cont1))
  else Different { approximant = subst_apply_cont env apply_cont1 }

let switch_exprs env switch1 switch2 : Expr.t Comparison.t =
  let compare_arms env arms1 arms2 =
    lists
      ~f:
        (pairs
           ~f1:(Comparator.of_predicate ~f:Targetint_31_63.equal)
           ~f2:apply_cont_exprs ~subst2:subst_apply_cont)
      ~subst:(fun env (target_imm, apply_cont) ->
        target_imm, subst_apply_cont env apply_cont)
      ~subst_snd:true env
      (Targetint_31_63.Map.bindings arms1)
      (Targetint_31_63.Map.bindings arms2)
    |> Comparison.map ~f:Targetint_31_63.Map.of_list
  in
  pairs ~f1:compare_arms ~f2:simple_exprs ~subst2:subst_simple env
    (Switch.arms switch1, Switch.scrutinee switch1)
    (Switch.arms switch2, Switch.scrutinee switch2)
  |> Comparison.map ~f:(fun (arms, scrutinee) ->
         Expr.create_switch (Switch.create ~scrutinee ~arms))

let rec exprs env e1 e2 : Expr.t Comparison.t =
  log Expr.print e1 e2 (fun () ->
      match Expr.descr e1, Expr.descr e2 with
      | Let let_expr1, Let let_expr2 -> let_exprs env let_expr1 let_expr2
      | Let_cont let_cont1, Let_cont let_cont2 ->
        let_cont_exprs env let_cont1 let_cont2
      | Apply apply1, Apply apply2 -> apply_exprs env apply1 apply2
      | Apply_cont apply_cont1, Apply_cont apply_cont2 ->
        apply_cont_exprs env apply_cont1 apply_cont2
        |> Comparison.map ~f:Expr.create_apply_cont
      | Switch switch1, Switch switch2 -> switch_exprs env switch1 switch2
      | Invalid invalid1, Invalid invalid2 ->
        if Invalid_term_semantics.compare invalid1 invalid2 = 0
        then Equivalent
        else Different { approximant = e1 }
      | _, _ -> Different { approximant = subst_expr env e1 })

and let_exprs env let_expr1 let_expr2 : Expr.t Comparison.t =
  let named1 = Let_expr.defining_expr let_expr1 in
  let named2 = Let_expr.defining_expr let_expr2 in
  Let_expr.pattern_match_pair let_expr1 let_expr2
    ~dynamic:(fun bound_pattern ~body1 ~body2 : Expr.t Comparison.t ->
      let named_comp = named_exprs env named1 named2 in
      let body_comp = exprs env body1 body2 in
      match named_comp, body_comp with
      | Equivalent, Equivalent -> Equivalent
      | _, _ ->
        let defining_expr = Comparison.approximant named_comp ~default:named2 in
        let body = Comparison.approximant body_comp ~default:body2 in
        let approximant =
          Let_expr.create bound_pattern defining_expr ~body
            ~free_names_of_body:Unknown
          |> Expr.create_let
        in
        Different { approximant })
    ~static:(fun ~bound_symbols1 ~bound_symbols2 ~body1 ~body2 ->
      match named1, named2 with
      | Static_consts static_consts1, Static_consts static_consts2 ->
        let_symbol_exprs env
          (bound_symbols1, static_consts1, body1)
          (bound_symbols2, static_consts2, body2)
      | _, _ -> Misc.fatal_error "Static LHS has dynamic RHS")
  |> function
  | Ok comp -> comp
  | Error _ ->
    Comparison.Different { approximant = subst_let_expr env let_expr1 }

and let_symbol_exprs env
    ((bound_symbols1 : Bound_pattern.symbols), static_consts1, body1)
    ((bound_symbols2 : Bound_pattern.symbols), static_consts2, body2) :
    Expr.t Comparison.t =
  let ok = ref true in
  let bound_symbols1 = bound_symbols1.bound_symbols in
  let bound_symbols2 = bound_symbols2.bound_symbols in
  let bound_symbols1' : Bound_symbols.t =
    bound_symbols env bound_symbols1 bound_symbols2
    |> Comparison.chain ~ok ~if_equivalent:bound_symbols2
  in
  let static_consts_comp : Static_const_group.t Comparison.t =
    lists ~f:static_consts ~subst:subst_static_const ~subst_snd:false env
      (static_consts1 |> Static_const_group.to_list)
      (static_consts2 |> Static_const_group.to_list)
    |> Comparison.map ~f:Static_const_group.create
  in
  let static_consts1' =
    Comparison.chain static_consts_comp ~ok ~if_equivalent:static_consts2
  in
  let body1' =
    exprs env body1 body2 |> Comparison.chain ~ok ~if_equivalent:body2
  in
  if !ok
  then Equivalent
  else
    let approximant =
      Let.create
        (Bound_pattern.symbols bound_symbols1')
        (Named.create_static_consts static_consts1')
        ~body:body1' ~free_names_of_body:Unknown
      |> Expr.create_let
    in
    Different { approximant }

and static_consts env (const1 : Static_const_or_code.t)
    (const2 : Static_const_or_code.t) : Static_const_or_code.t Comparison.t =
  match const1, const2 with
  | Code code1, Code code2 ->
    codes env code1 code2 |> Comparison.map ~f:Static_const_or_code.create_code
  | ( Static_const (Block (tag1, mut1, fields1)),
      Static_const (Block (tag2, mut2, fields2)) ) ->
    blocks env (tag1, mut1, fields1) (tag2, mut2, fields2)
    |> Comparison.map
         ~f:(fun (tag1', mut1', fields1') : Static_const_or_code.t ->
           Static_const_or_code.create_static_const
             (Block (tag1', mut1', fields1')))
  | Static_const (Set_of_closures set1), Static_const (Set_of_closures set2) ->
    sets_of_closures env set1 set2
    |> Comparison.map ~f:(fun set1' : Static_const_or_code.t ->
           Static_const_or_code.create_static_const (Set_of_closures set1'))
  | _, _ ->
    if Static_const_or_code.equal const1 const2
    then Equivalent
    else Different { approximant = subst_static_const env const1 }

and codes env (code1 : Code.t) (code2 : Code.t) =
  let bodies env params_and_body1 params_and_body2 =
    Function_params_and_body.pattern_match_pair params_and_body1
      params_and_body2
      ~f:(fun
           ~return_continuation
           ~exn_continuation
           params
           ~body1
           ~body2
           ~my_closure
           ~my_depth
         ->
        exprs env body1 body2
        |> Comparison.map ~f:(fun body1' ->
               let dbg = Function_params_and_body.debuginfo params_and_body1 in
               Function_params_and_body.create ~return_continuation
                 ~exn_continuation params ~dbg ~body:body1' ~my_closure
                 ~my_depth ~free_names_of_body:Unknown))
  in

  let bodies_or_deleted env body1 body2 : _ Or_deleted.t Comparison.t =
    match (body1 : _ Or_deleted.t), (body2 : _ Or_deleted.t) with
    | Present body1, Present body2 ->
      bodies env body1 body2
      |> Comparison.map ~f:(fun body1' -> Or_deleted.Present body1')
    | Deleted, Deleted -> Equivalent
    | Present body1, Deleted ->
      Different { approximant = Present (subst_params_and_body env body1) }
    | Deleted, Present _ -> Different { approximant = Deleted }
  in
  pairs ~f1:bodies_or_deleted
    ~f2:(options ~f:code_ids ~subst:subst_code_id)
    env
    (Code.params_and_body code1, Code.newer_version_of code1)
    (Code.params_and_body code2, Code.newer_version_of code2)
  |> Comparison.map ~f:(fun (params_and_body, newer_version_of) ->
         let params_and_body : _ Or_deleted.t =
           match (params_and_body : _ Or_deleted.t) with
           | Deleted -> Deleted
           | Present params_and_body ->
             Present
               ( params_and_body,
                 (* CR mshinwell: This needs fixing XXX *)
                 Name_occurrences.empty
                 (* Function_params_and_body.free_names params_and_body *) )
         in
         code1
         |> Code.with_code_id (Code.code_id code2)
         |> Code.with_params_and_body ~cost_metrics:(Code.cost_metrics code2)
              params_and_body
         |> Code.with_newer_version_of newer_version_of)
  |> Comparison.add_condition
       ~approximant:(fun () -> subst_code env code1)
       ~cond:
         (Flambda_arity.With_subkinds.equal (Code.params_arity code1)
            (Code.params_arity code2)
         && Flambda_arity.With_subkinds.equal (Code.result_arity code1)
              (Code.result_arity code2)
         && Bool.equal (Code.stub code1) (Code.stub code2)
         && Inline_attribute.equal (Code.inline code1) (Code.inline code2)
         && Bool.equal (Code.is_a_functor code1) (Code.is_a_functor code2)
         && Recursive.equal (Code.recursive code1) (Code.recursive code2))

and let_cont_exprs env (let_cont1 : Let_cont.t) (let_cont2 : Let_cont.t) :
    Expr.t Comparison.t =
  match let_cont1, let_cont2 with
  | ( Non_recursive { handler = handler1; num_free_occurrences = _ },
      Non_recursive { handler = handler2; num_free_occurrences = _ } ) ->
    let module Non_rec = Non_recursive_let_cont_handler in
    let sorts_match =
      let sort handler =
        Non_rec.pattern_match handler ~f:(fun cont ~body:_ ->
            Continuation.sort cont)
      in
      Continuation.Sort.equal (sort handler1) (sort handler2)
    in
    Non_rec.pattern_match_pair handler1 handler2 ~f:(fun cont ~body1 ~body2 ->
        pairs ~f1:cont_handlers ~f2:exprs env
          (Non_rec.handler handler1, body1)
          (Non_rec.handler handler2, body2)
        |> Comparison.add_condition
             ~approximant:(fun () ->
               ( subst_cont_handler env (Non_rec.handler handler1),
                 subst_expr env body1 ))
             ~cond:sorts_match
        |> Comparison.map ~f:(fun (handler, body) ->
               Let_cont.create_non_recursive cont handler ~body
                 ~free_names_of_body:Unknown))
  | Recursive handlers1, Recursive handlers2 ->
    let compare_handler_maps env map1 map2 :
        Continuation_handler.t Continuation.Map.t Comparison.t =
      lists
        ~f:(fun env (cont, handler1) (_cont, handler2) ->
          cont_handlers env handler1 handler2
          (* Note that cont and _cont should be equal thanks to
           * [pattern_match_pair] *)
          |> Comparison.map ~f:(fun handler1' -> cont, handler1'))
        ~subst:(fun env (cont, handler) -> cont, subst_cont_handler env handler)
        ~subst_snd:false env
        (map1 |> Continuation.Map.bindings)
        (map2 |> Continuation.Map.bindings)
      |> Comparison.map ~f:Continuation.Map.of_list
    in
    Recursive_let_cont_handlers.pattern_match_pair handlers1 handlers2
      ~f:(fun ~body1 ~body2 cont_handlers1 cont_handlers2 ->
        pairs ~f1:exprs ~f2:compare_handler_maps
          ~subst2:(fun env map ->
            Continuation.Map.map (subst_cont_handler env) map)
          env
          (body1, cont_handlers1 |> Continuation_handlers.to_map)
          (body2, cont_handlers2 |> Continuation_handlers.to_map)
        |> Comparison.map ~f:(fun (body, handlers) ->
               Let_cont_expr.create_recursive handlers ~body))
  | _, _ -> Different { approximant = subst_let_cont env let_cont1 }

and cont_handlers env handler1 handler2 =
  Flambda.Continuation_handler.pattern_match_pair handler1 handler2
    ~f:(fun params ~handler1:expr1 ~handler2:expr2 ->
      exprs env expr1 expr2
      |> Comparison.map ~f:(fun handler ->
             Continuation_handler.create params ~handler
               ~free_names_of_handler:Unknown
               ~is_exn_handler:(Continuation_handler.is_exn_handler handler2))
      |> Comparison.add_condition
           ~cond:
             (Bool.equal
                (Continuation_handler.is_exn_handler handler1)
                (Continuation_handler.is_exn_handler handler2))
           ~approximant:(fun () -> subst_cont_handler env handler1))
  |> function
  | Ok comp -> comp
  | Error _ ->
    Comparison.Different { approximant = subst_cont_handler env handler1 }

let flambda_units u1 u2 =
  let ret_cont = Continuation.create ~sort:Toplevel_return () in
  let exn_cont = Continuation.create () in
  let mk_perm u =
    let perm = Renaming.empty in
    let perm =
      Renaming.add_fresh_continuation perm
        (Flambda_unit.return_continuation u)
        ~guaranteed_fresh:ret_cont
    in
    let perm =
      Renaming.add_fresh_continuation perm
        (Flambda_unit.exn_continuation u)
        ~guaranteed_fresh:exn_cont
    in
    perm
  in
  let env = Env.create () in
  let body1 = Expr.apply_renaming (Flambda_unit.body u1) (mk_perm u1) in
  let body2 = Expr.apply_renaming (Flambda_unit.body u2) (mk_perm u2) in
  exprs env body1 body2
  |> Comparison.map ~f:(fun body ->
         let module_symbol = Flambda_unit.module_symbol u1 in
         Flambda_unit.create ~return_continuation:ret_cont
           ~exn_continuation:exn_cont ~body ~module_symbol
           ~used_closure_vars:Unknown)
