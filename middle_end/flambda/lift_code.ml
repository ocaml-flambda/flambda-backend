(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

let check_invariants () = !Clflags.flambda_invariant_checks

type lifter = Flambda.program -> Flambda.program

(* A fragment of syntax that we build up as we traverse the term. These compose
   together to form a context, which we then pass to [rebuild_let] to form the
   final term. *)
type def =
  | Immutable of Variable.t * Flambda.named Flambda.With_free_variables.t
    (* [let x = M in ...] *)
  | Mutable of Mutable_variable.t * Variable.t * Lambda.layout
    (* [let mutable x = y in ...] *)
  | Region
    (* [region (...)] *)
  | Exclave
    (* [exclave (...)] *)

(* A context composed from some number of [def]s. These are inside-first, so for
   example (with abbreviated syntax),
     [[Exclave; Immutable("x", 42); Mutable("y", "z"); Region]]
   represents the fragment
     [region (let mutable y = z in let x = 42 in exclave (...))]. *)
type defs = def list

let rebuild_let (defs : defs) (body : Flambda.t) =
  let module W = Flambda.With_free_variables in
  List.fold_left (fun body def ->
    match def with
    | Immutable(var, def) ->
        W.create_let_reusing_defining_expr var def body
    | Mutable(var, initial_value, contents_kind) ->
        Flambda.Let_mutable {var; initial_value; contents_kind; body}
    | Region ->
        Flambda.Region body
    | Exclave ->
        Flambda.Exclave body)
    body defs

(* Whether [let x = region (... M) in N] can be rewritten as
   [region (... let x = M in exclave N)]. Only true if [M] has no exclaves
   (besides those in lambdas and nested regions) and no tail calls. (In the case
   of tail calls, for [M] to have a tail call, the [N] in the above must be
   simply [x] - in fact, it will be the special [lifted_let] variable created in
   [lift_lets_expr] and we're relying on [split_defs] to substitute out the [x]
   later.) *)
let rec liftable_region_body0 (expr : Flambda.t) ~depth =
  match expr with
  | Exclave expr ->
      depth > 0 (* Nested exclaves are fine *)
      && liftable_region_body0 expr ~depth:(depth - 1)
  | Apply { reg_close = Rc_close_at_apply; _ }
  | Send { reg_close = Rc_close_at_apply; _ } ->
      (* Tail calls must be kept tail calls *)
      false
  | Region expr ->
      liftable_region_body0 expr ~depth:(depth + 1)
  | Let { body; _ }
  | Let_mutable { body; _ }
  | Let_rec (_, body) ->
      liftable_region_body0 body ~depth
  | If_then_else (_, ifso, ifnot, _) ->
      liftable_region_body0 ifso ~depth && liftable_region_body0 ifnot ~depth
  | Switch (_, { consts; blocks; failaction; _ }) ->
      liftable_cases0 consts ~depth
      && liftable_cases0 blocks ~depth
      &&
      begin match failaction with
      | None -> true
      | Some expr -> liftable_region_body0 expr ~depth
      end
  | String_switch (_, cases, failaction, _) ->
      liftable_cases0 cases ~depth
      &&
      begin match failaction with
      | None -> true
      | Some expr -> liftable_region_body0 expr ~depth
      end
  | Static_catch (_, _, body, handler, _)
  | Try_with (body, _, handler, _) ->
      liftable_region_body0 body ~depth && liftable_region_body0 handler ~depth
  | Var _
  | Apply _
  | Send _
  | Assign _
  | Static_raise _
  | While _
  | For _
  | Proved_unreachable ->
    true

and liftable_cases0 : 'a. ('a * Flambda.t) list -> depth:int -> bool =
 fun cases ~depth ->
  List.for_all (fun (_, expr) -> liftable_region_body0 expr ~depth) cases

let liftable_region_body expr = liftable_region_body0 expr ~depth:0

(* Attempt to rewrite something of the form [... in let x = M in x] as
   [... in M]. This is important because [M] might be a tail call or exclave, so
   it must end up back in tail position.

   Note that it's tempting to play games like turning [let x = M in exclave x]
   into simply [M]. This is valid in that particular case, but it doesn't
   actually win anything and it produces defs that are dangerous to use for
   anything but wrapping exactly the returned expression. In particular, the
   defs may be unbalanced, leaving a region open. *)
let rec split_defs defs var : def list * Flambda.expr =
  let module W = Flambda.With_free_variables in
  match defs with
  | Immutable (var', named) :: defs' when Variable.equal var var' -> begin
      match W.contents named with
      | Expr expr -> defs', expr
      | _ -> defs, Var var
    end
  | Immutable (var', _) :: _ ->
    Misc.fatal_errorf "Expected binding for %a@ but found %a"
      Variable.print var Variable.print var'
  | Exclave :: more_defs ->
    (* We're ending up let-binding [var] after all, so make sure it's safe to
       do so *)
    if check_invariants () then begin
      let _, inner_expr = split_defs more_defs var in
      if not (liftable_region_body inner_expr) then
        Misc.fatal_errorf "@[<hv>Attempting to let-bind@ %a@]"
          Flambda.print inner_expr;
    end;
    defs, Var var
  | (Mutable _ | Region) :: _ | [] ->
    Misc.fatal_errorf "Expected binding for %a"
      Variable.print var

let rebuild_expr defs var =
  let defs, body = split_defs defs var in
  rebuild_let defs body

let region_delta defs =
  List.fold_left
    (fun delta def ->
       match def with
       | Exclave -> delta - 1
       | Region -> delta + 1
       | Immutable _ | Mutable _ -> delta)
    0 defs

let defs_open_region defs =
  region_delta defs > 0

let defs_close_region defs =
  region_delta defs < 0

let check_defs defs =
  if check_invariants () then
    assert (not (defs_open_region defs))

type yes_no_or_maybe = Yes | No | Maybe

let should_lift_regions ~in_closure =
  match in_closure with
  | No | Maybe ->
      (* Don't lift regions if we may be outside of any closure. Doing so would
         create top-level exclaves, which interact badly with
         [Lift_let_to_initialize_symbol]. *)
      false
  | Yes -> true

let rec extract_let_expr (acc:def list) dest let_expr ~in_closure =
  let module W = Flambda.With_free_variables in
  let acc =
    match (let_expr : Flambda.let_expr) with
    | { var = v1; defining_expr = Expr (Let let2); _ } ->
        extract_let_expr acc v1 let2 ~in_closure
    | { var = v1; defining_expr = Expr (Let_mutable let_mut); _ } ->
        extract_let_mutable acc v1 let_mut ~in_closure
    | { var = v1; defining_expr = Expr (Region expr); _ }
          when should_lift_regions ~in_closure ->
        extract_region acc v1 expr ~in_closure
    | { var = v1;
        defining_expr = Expr (Apply ({ reg_close = Rc_close_at_apply; _ }
                                     as apply)) }
          when should_lift_regions ~in_closure ->
        extract_tail_call acc v1 apply
    | { var = v1;
        defining_expr = Expr (Send ({ reg_close = Rc_close_at_apply; _ }
                                    as send)) }
          when should_lift_regions ~in_closure ->
        extract_tail_send acc v1 send
    | { var = v; _ } ->
        Immutable(v, W.of_defining_expr_of_let let_expr) :: acc
  in
  let body = W.of_body_of_let let_expr in
  extract acc dest body ~in_closure

and extract_let_mutable acc dest let_mut ~in_closure =
  let module W = Flambda.With_free_variables in
  let { Flambda.var; initial_value; contents_kind; body } = let_mut in
  let acc = Mutable(var, initial_value, contents_kind) :: acc in
  extract acc dest (W.of_expr body) ~in_closure

(* C[let x = region M in x] *)
and extract_region acc dest body ~in_closure =
  let module W = Flambda.With_free_variables in
  (* Ideally, we would recurse with the same [dest] since we're ultimately going
     to store there anyway. Unfortunately, if [body] has an unliftable tail,
     we're going to need [inner_dest] as an intermediary. *)
  let inner_dest = Variable.rename dest in
  (* C[let x = region (let y = M in y) in x] =>
     C[let x = region (D[y]) in x]  *)
  let acc_expr = extract [] inner_dest (W.of_expr body) ~in_closure in
  (* If possible, recover the expression that gets assigned to [inner_dest] so
     we can directly assign [dest] to it instead *)
  match split_defs acc_expr inner_dest with
  | acc_expr, body when liftable_region_body body ->
    (* The accumulator must remain balanced between [Region] and [Exclave] (see
       [check_defs]), since it defines a scope into which [extract_let_expr]
       will move arbitrary computations - if there is a [Region] but no
       [Exclave], this means we're moving those computations into a different
       region. It may be that [acc_expr] already has an [Exclave] (because we
       lifted it out of [body]), but otherwise we need to add it. *)
    let need_tail = not (defs_close_region acc_expr) in
    List.concat
      [ if need_tail then [ Exclave ] else [];
        [ Immutable (dest, W.expr (W.of_expr body)) ];
        acc_expr;
        [ Region ];
        acc ]
  | _ ->
    (* We can't lift it, so we just have to bundle everything back up in a
       region. *)
    let expr = Flambda.Region (rebuild_expr acc_expr inner_dest) in
    Immutable(dest, W.expr (W.of_expr expr)) :: acc

and extract_tail_call acc dest (apply : Flambda.apply) =
  let module W = Flambda.With_free_variables in
  (* Rewrite a close-at-apply call as a normal call in an [Exclave] so that we
     can float the [Exclave] *)
  let apply = { apply with reg_close = Rc_normal } in
  Immutable (dest, W.expr (W.of_expr (Apply apply))) :: Exclave :: acc

and extract_tail_send acc dest (send : Flambda.send) =
  let module W = Flambda.With_free_variables in
  (* Same as [extract_tail_call] but with sends *)
  let send = { send with reg_close = Rc_normal } in
  Immutable (dest, W.expr (W.of_expr (Send send))) :: Exclave :: acc

(* [C[let x = M in x]] *)
and extract acc dest expr ~in_closure =
  let module W = Flambda.With_free_variables in
  check_defs acc;
  match (W.contents expr : Flambda.t) with
  | Let let_expr ->
    extract_let_expr acc dest let_expr ~in_closure
  | Let_mutable let_mutable ->
    extract_let_mutable acc dest let_mutable ~in_closure
  | Region expr when should_lift_regions ~in_closure ->
    extract_region acc dest expr ~in_closure
  | Exclave expr when should_lift_regions ~in_closure ->
    (* One might worry about just adding [Exclave] to the accumulator, since in
       general the accumulator defines a scope into which we're moving arbitrary
       expressions. In [extract_region], we're careful to make sure the
       accumulator remains "balanced" between [Region] and [Exclave] for this
       reason. Here we can get away with unconditionally tossing [Exclave] onto
       the accumulator because one of the following must be true:

       1. We are in the tail of a [Region] being handled by [extract_region],
          which will see that we've added this [Exclave] and not add one of
          its own.

       2. We are in the tail of the entire expression (that is, the argument to
          [lift_lets_expr]), and thus [expr] is the very last expression we're
          processing, so nothing else will be moved into this [Exclave]. *)
    extract (Exclave :: acc) dest (W.of_expr expr) ~in_closure
  | Apply ({ reg_close = Rc_close_at_apply; _ } as apply)
      when should_lift_regions ~in_closure ->
    extract_tail_call acc dest apply
  | Send ({ reg_close = Rc_close_at_apply; _ } as send)
      when should_lift_regions ~in_closure ->
    extract_tail_send acc dest send
  | _ ->
    Immutable (dest, W.expr expr) :: acc

let rec lift_lets_expr (expr:Flambda.t) ~toplevel ~in_closure
  : Flambda.t =
  match expr with
  | Let let_expr ->
    (* For uniformity, wrap everything in another [let] binding, which
       [rebuild_expr] will try to eliminate. Sometimes we can't eliminate it
       easily (see comments on [split_defs]), but it's harmless and not worth
       the complexity to avoid it. *)
    let dest = Variable.create Internal_variable_names.lifted_let in
    let defs = extract_let_expr [] dest let_expr ~in_closure in
    let rev_defs = List.rev_map (lift_lets_def ~toplevel ~in_closure) defs in
    rebuild_expr (List.rev rev_defs) dest
  | Let_mutable let_mut ->
    let dest = Variable.create Internal_variable_names.lifted_let in
    let defs = extract_let_mutable [] dest let_mut ~in_closure in
    let rev_defs = List.rev_map (lift_lets_def ~toplevel ~in_closure) defs in
    rebuild_expr (List.rev rev_defs) dest
  | e ->
    Flambda_iterators.map_subexpressions
      (lift_lets_expr ~toplevel ~in_closure)
      (lift_lets_named ~toplevel ~in_closure)
      e

and lift_lets_def def ~toplevel ~in_closure =
  let module W = Flambda.With_free_variables in
  match def with
  | Mutable _ -> def
  | Immutable(var, named) ->
    let named =
      match W.contents named with
      | Expr e -> W.expr (W.of_expr (lift_lets_expr e ~toplevel ~in_closure))
      | Set_of_closures set when not toplevel ->
        W.of_named
          (Set_of_closures
             (Flambda_iterators.map_function_bodies
                ~f:(lift_lets_expr ~toplevel ~in_closure:Yes) set))
      | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field (_, _) | Project_closure _
      | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Set_of_closures _ ->
        named
    in
    Immutable(var, named)
  | Region | Exclave -> def

and lift_lets_named _var (named:Flambda.named) ~toplevel ~in_closure
  : Flambda.named =
  match named with
  | Expr e ->
    Expr (lift_lets_expr e ~toplevel ~in_closure)
  | Set_of_closures set when not toplevel ->
    Set_of_closures
      (Flambda_iterators.map_function_bodies
        ~f:(lift_lets_expr ~toplevel ~in_closure:Yes) set)
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field (_, _) | Project_closure _ | Move_within_set_of_closures _
  | Project_var _ | Prim _ | Set_of_closures _ ->
    named

module Sort_lets = Strongly_connected_components.Make (Variable)

let rebuild_let_rec (defs:(Variable.t * Flambda.named) list) body =
  let map = Variable.Map.of_list defs in
  let graph =
    Variable.Map.map
      (fun named ->
         Variable.Set.filter (fun v -> Variable.Map.mem v map)
           (Flambda.free_variables_named named))
      map
  in
  let components =
    Sort_lets.connected_components_sorted_from_roots_to_leaf graph
  in
  Array.fold_left (fun body (component:Sort_lets.component) ->
      match component with
      | No_loop v ->
          let def = Variable.Map.find v map in
          Flambda.create_let v def body
      | Has_loop l ->
          Flambda.Let_rec
            (List.map (fun v -> v, Variable.Map.find v map) l,
             body))
    body components

let lift_let_rec program =
  Flambda_iterators.map_exprs_at_toplevel_of_program program
    ~f:(Flambda_iterators.map_expr
          (fun expr -> match expr with
             | Let_rec (defs, body) ->
                 rebuild_let_rec defs body
             | expr -> expr))

let lift_lets program =
  let program = lift_let_rec program in
  Flambda_iterators.map_exprs_at_toplevel_of_program_with_under_lambda
    program
    ~f:(fun expr ~under_lambda ->
        let in_closure = if under_lambda then Yes else No in
        lift_lets_expr ~toplevel:true ~in_closure expr)

let lifting_helper exprs ~evaluation_order ~create_body ~name =
  let vars, lets =
    (* [vars] corresponds elementwise to [exprs]; the order is unchanged. *)
    List.fold_right (fun (flam : Flambda.t) (vars, lets) ->
        match flam with
        | Var v ->
          (* Note that [v] is (statically) always an immutable variable. *)
          v::vars, lets
        | expr ->
          let v =
            Variable.create name ~current_compilation_unit:
                (Compilation_unit.get_current_exn ())
          in
          v::vars, (v, expr)::lets)
      exprs ([], [])
  in
  let lets =
    match evaluation_order with
    | `Right_to_left -> lets
    | `Left_to_right -> List.rev lets
  in
  List.fold_left (fun body (v, expr) ->
      Flambda.create_let v (Expr expr) body)
    (create_body vars) lets
