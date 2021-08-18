(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module VB = Var_in_binding_pos

module T0 = struct
  type t = {
    num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t;
    body : Expr.t;
  }

  let print_with_cache ~cache ppf
        { body; num_normal_occurrences_of_bound_vars = _; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(body@ %a)@]\
        )@]"
      (Expr.print_with_cache ~cache) body

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { body; num_normal_occurrences_of_bound_vars = _; } =
    Expr.free_names body

  let apply_renaming
        ({ body; num_normal_occurrences_of_bound_vars; } as t) perm =
    let body' = Expr.apply_renaming body perm in
    let changed = ref (body != body') in
    let num_normal_occurrences_of_bound_vars =
      Variable.Map.fold (fun var num result ->
          let var' = Renaming.apply_variable perm var in
          changed := !changed || (var != var');
          Variable.Map.add var' num result)
        num_normal_occurrences_of_bound_vars
        Variable.Map.empty
    in
    if not !changed then t
    else { body = body'; num_normal_occurrences_of_bound_vars; }

  let all_ids_for_export { body; num_normal_occurrences_of_bound_vars = _; } =
    Expr.all_ids_for_export body
end

module A = Name_abstraction.Make (Bindable_let_bound) (T0)

type t = {
  name_abstraction : A.t;
  defining_expr : Named.t;
}

let pattern_match t ~f =
  A.pattern_match t.name_abstraction
    ~f:(fun bindable_let_bound t0 -> f bindable_let_bound ~body:t0.body)

let pattern_match' t ~f =
  A.pattern_match t.name_abstraction
    ~f:(fun bindable_let_bound t0 ->
      let num_normal_occurrences_of_bound_vars =
        t0.num_normal_occurrences_of_bound_vars
      in
      f bindable_let_bound ~num_normal_occurrences_of_bound_vars ~body:t0.body)

module Pattern_match_pair_error = struct
  type t = Mismatched_let_bindings

  let to_string = function
    | Mismatched_let_bindings -> "Mismatched let bindings"
end

let pattern_match_pair t1 t2 ~dynamic ~static =
  A.pattern_match t1.name_abstraction
    ~f:(fun bindable_let_bound1 t0_1 ->
      let body1 = t0_1.body in
      A.pattern_match t2.name_abstraction
        ~f:(fun bindable_let_bound2 t0_2 ->
          let body2 = t0_2.body in
          let dynamic_case () =
            let ans =
              A.pattern_match_pair
                t1.name_abstraction
                t2.name_abstraction
                ~f:(fun bindable_let_bound t0_1 t0_2 ->
                  dynamic bindable_let_bound ~body1:t0_1.body ~body2:t0_2.body)
            in
            Ok ans
          in
          match bindable_let_bound1, bindable_let_bound2 with
          | Bindable_let_bound.Singleton _,
            Bindable_let_bound.Singleton _ ->
            dynamic_case ()
          | Set_of_closures { closure_vars = vars1; _ },
            Set_of_closures { closure_vars = vars2; _ } ->
            if List.compare_lengths vars1 vars2 = 0
            then dynamic_case ()
            else Error Pattern_match_pair_error.Mismatched_let_bindings
          | Symbols bound_symbols1,
            Symbols bound_symbols2 ->
            let patterns1 =
              bound_symbols1.bound_symbols |> Bound_symbols.to_list
            in
            let patterns2 =
              bound_symbols2.bound_symbols |> Bound_symbols.to_list
            in
            if List.compare_lengths patterns1 patterns2 = 0
            then
              let ans = static ~bound_symbols1 ~bound_symbols2 ~body1 ~body2 in
              Ok ans
            else Error Pattern_match_pair_error.Mismatched_let_bindings
          | _, _ ->
            Error Pattern_match_pair_error.Mismatched_let_bindings))

(* For printing "let symbol": *)

type flattened_for_printing_descr =
  | Code of Code_id.t * Code.t
  | Set_of_closures of Symbol.t Closure_id.Lmap.t * Set_of_closures.t
  | Block_like of Symbol.t * Static_const.t

type flattened_for_printing = {
  second_or_later_binding_within_one_set : bool;
  second_or_later_rec_binding : bool;
  descr : flattened_for_printing_descr;
}

let shape_colour descr =
  match descr with
  | Code _ -> Flambda_colours.code_id ()
  | Set_of_closures _ | Block_like _ -> Flambda_colours.symbol ()

(* CR mshinwell: Remove [second_or_later_binding_within_one_set] if it
   doesn't become used soon. *)

let flatten_for_printing0 bound_symbols defining_exprs =
  Static_const.Group.match_against_bound_symbols defining_exprs bound_symbols
    ~init:([], false)
    ~code:(fun (flattened_acc, second_or_later_rec_binding)
          code_id code ->
      let flattened =
        { second_or_later_binding_within_one_set = false;
          second_or_later_rec_binding;
          descr = Code (code_id, code);
        }
      in
      flattened_acc @ [flattened], true)
    ~set_of_closures:(fun (flattened_acc, second_or_later_rec_binding)
          ~closure_symbols set_of_closures ->
      let flattened =
        if Set_of_closures.is_empty set_of_closures then []
        else
          let second_or_later_binding_within_one_set = false in
          [{ second_or_later_binding_within_one_set;
             second_or_later_rec_binding;
             descr = Set_of_closures (closure_symbols, set_of_closures);
          }]
      in
      flattened_acc @ flattened, true)
    ~block_like:(fun (flattened_acc, second_or_later_rec_binding)
          symbol defining_expr ->
      let flattened =
        { second_or_later_binding_within_one_set = false;
          second_or_later_rec_binding;
          descr = Block_like (symbol, defining_expr);
        }
      in
      flattened_acc @ [flattened], true)

let flatten_for_printing t =
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    match bindable_let_bound with
    | Symbols { bound_symbols; } ->
      let flattened, _ =
        flatten_for_printing0 bound_symbols
          (Named.must_be_static_consts t.defining_expr)
      in
      Some (flattened, body)
    | Singleton _ | Set_of_closures _ -> None)

let print_closure_binding ppf (closure_id, sym) =
  Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
    Symbol.print sym
    (Flambda_colours.elide ())
    (Flambda_colours.elide ())
    Closure_id.print closure_id

let print_flattened_descr_lhs ppf descr =
  match descr with
  | Code (code_id, _) -> Code_id.print ppf code_id
  | Set_of_closures (closure_symbols, _) ->
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () ->
          Format.fprintf ppf "@<0>%s,@ @<0>%s"
            (Flambda_colours.elide ())
            (Flambda_colours.normal ()))
        print_closure_binding)
      (Closure_id.Lmap.bindings closure_symbols)
  | Block_like (symbol, _) -> Symbol.print ppf symbol

(* CR mshinwell: Use [print_with_cache]? *)
let print_flattened_descr_rhs ppf descr =
  match descr with
  | Code (_, code) -> Code.print ppf code
  | Set_of_closures (_, set) -> Set_of_closures.print ppf set
  | Block_like (_, static_const) -> Static_const.print ppf static_const

let print_flattened ppf
      { second_or_later_binding_within_one_set = _;
        second_or_later_rec_binding;
        descr;
      } =
  fprintf ppf "@[<hov 0>";
  (*
  if second_or_later_rec_binding
    && not second_or_later_binding_within_one_set
  then begin
    fprintf ppf "@<0>%sand_set @<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  end else *) if second_or_later_rec_binding then begin
    fprintf ppf "@<0>%sand @<0>%s"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
  end else begin
    let shape = "\u{25b7}" (* unfilled triangle *) in
    fprintf ppf "@<0>%s@<1>%s @<0>%s"
      (shape_colour descr)
      shape
      (Flambda_colours.normal ())
  end;
  fprintf ppf
    "%a@<0>%s =@<0>%s@ %a@]"
    print_flattened_descr_lhs descr
    (Flambda_colours.elide ())
    (Flambda_colours.normal ())
    print_flattened_descr_rhs descr

let flatten_let_symbol t : _ * Expr.t =
  let rec flatten (expr : Expr.t) : _ * Expr.t =
    match Expr.descr expr with
    | Let t ->
      begin match flatten_for_printing t with
      | Some (flattened, body) ->
        let flattened', body = flatten body in
        flattened @ flattened', body
      | None -> [], expr
      end
    | _ -> [], expr
  in
  match flatten_for_printing t with
  | Some (flattened, body) ->
    let flattened', body = flatten body in
    flattened @ flattened', body
  | None -> assert false  (* see below *)

(* CR mshinwell: Merge the "let symbol" and "normal let" cases to use the
   same flattened type? *)
let print_let_symbol_with_cache ~cache ppf t =
  let rec print_more flattened =
    match flattened with
    | [] -> ()
    | flat::flattened ->
      fprintf ppf "@ ";
      print_flattened ppf flat;
      print_more flattened
  in
  let flattened, body = flatten_let_symbol t in
  match flattened with
  | [] -> assert false
  | flat::flattened ->
    fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ @[<v 0>%a"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      print_flattened flat;
    print_more flattened;
    fprintf ppf "@]@ %a)@]" (Expr.print_with_cache ~cache) body

(* For printing all kinds of let-expressions: *)

let print_with_cache ~cache ppf
      ({ name_abstraction = _; defining_expr; } as t) =
  let let_bound_var_colour bindable_let_bound defining_expr =
    let name_mode = Bindable_let_bound.name_mode bindable_let_bound in
    if Name_mode.is_phantom name_mode then Flambda_colours.elide ()
    else match (defining_expr : Named.t) with
      | Rec_info _ ->
        Flambda_colours.depth_variable ()
      | Simple _ | Prim _ | Set_of_closures _ | Static_consts _ ->
        Flambda_colours.variable ()
  in
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ name_abstraction = _; defining_expr; } as t) ->
      pattern_match t
        ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
          match bindable_let_bound with
          | Singleton _ | Set_of_closures _ ->
            fprintf ppf
              "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
              (let_bound_var_colour bindable_let_bound defining_expr)
              Bindable_let_bound.print bindable_let_bound
              (Flambda_colours.elide ())
              (Flambda_colours.normal ())
              (Named.print_with_cache ~cache) defining_expr;
            let_body body
          | Symbols _ -> expr)
    | _ -> expr
  in
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    match bindable_let_bound with
    | Symbols _ -> print_let_symbol_with_cache ~cache ppf t
    | Singleton _ | Set_of_closures _ ->
      fprintf ppf "@[<v 1>(@<0>%slet@<0>%s@ (@[<v 0>\
          @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
        (let_bound_var_colour bindable_let_bound defining_expr)
        Bindable_let_bound.print bindable_let_bound
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Named.print_with_cache ~cache) defining_expr;
      let expr = let_body body in
      fprintf ppf "@])@ %a)@]"
        (Expr.print_with_cache ~cache) expr)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create bindable_let_bound defining_expr ~body
      ~(free_names_of_body : _ Or_unknown.t) =
  let num_normal_occurrences_of_bound_vars =
    match free_names_of_body with
    | Unknown -> Variable.Map.empty
    | Known free_names_of_body ->
      let free_names_of_bindable =
        Bindable_let_bound.free_names bindable_let_bound
      in
      Name_occurrences.fold_variables free_names_of_bindable
        ~init:Variable.Map.empty
        ~f:(fun num_occurrences var ->
          let num =
            Name_occurrences.count_variable_normal_mode
              free_names_of_body var
          in
          Variable.Map.add var num num_occurrences)
  in
  let t0 : T0.t =
    { num_normal_occurrences_of_bound_vars;
      body;
    }
  in
  { name_abstraction = A.create bindable_let_bound t0;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  Named.invariant env t.defining_expr;
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    let env =
      match t.defining_expr, bindable_let_bound with
      | Set_of_closures _, Set_of_closures { closure_vars; _ } ->
        List.fold_left (fun env closure_var ->
            let closure_var = VB.var closure_var in
            E.add_variable env closure_var K.value)
          env
          closure_vars
      | Set_of_closures _, Singleton _ ->
        Misc.fatal_errorf "Cannot bind a [Set_of_closures] to a \
            [Singleton]:@ %a"
          print t
      | _, Set_of_closures _ ->
        Misc.fatal_errorf "Cannot bind a non-[Set_of_closures] to a \
            [Set_of_closures]:@ %a"
          print t
      | Prim (prim, _dbg), Singleton var ->
        let var = VB.var var in
        E.add_variable env var (Flambda_primitive.result_kind' prim)
      | Simple _simple, Singleton _var -> Misc.fatal_error "To be deleted"
      | Static_consts _, Symbols _ -> env
      | Static_consts _, Singleton _ ->
        Misc.fatal_errorf "Cannot bind a [Static_const] to a [Singleton]:@ %a"
          print t
      | (Simple _ | Prim _ | Set_of_closures _ | Rec_info _), Symbols _ ->
        Misc.fatal_errorf "Cannot bind a non-[Static_const] to [Symbols]:@ %a"
          print t
      | Rec_info _, Singleton _ -> env
    in
    Expr.invariant env body)

let defining_expr t = t.defining_expr

let free_names
      ({ name_abstraction = _; defining_expr; } as t) =
  pattern_match t ~f:(fun bindable_let_bound ~body ->
    let from_bindable = Bindable_let_bound.free_names bindable_let_bound in
    let from_defining_expr =
      let name_mode = Bindable_let_bound.name_mode bindable_let_bound in
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Named.free_names defining_expr)
        name_mode
    in
    let from_body = Expr.free_names body in
    (* CR mshinwell: See comment in expr.rec.ml *)
    (* Care: there can be recursive bindings. *)
    Name_occurrences.diff
      (Name_occurrences.union from_defining_expr from_body)
      from_bindable)

let apply_renaming ({ name_abstraction; defining_expr; } as t) perm =
  let name_abstraction' = A.apply_renaming name_abstraction perm in
  let defining_expr' = Named.apply_renaming defining_expr perm in
  if name_abstraction == name_abstraction' && defining_expr == defining_expr'
  then t
  else
    { name_abstraction = name_abstraction';
      defining_expr = defining_expr';
    }

let all_ids_for_export { name_abstraction; defining_expr; } =
  let defining_expr_ids = Named.all_ids_for_export defining_expr in
  let name_abstraction_ids = A.all_ids_for_export name_abstraction in
  Ids_for_export.union defining_expr_ids name_abstraction_ids
