(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module T = Flow_types
module G = Strongly_connected_components.Make (Variable)

type t =
  { required_names : Name.Set.t;
    params_kind : Flambda_kind.With_subkind.t Variable.Map.t;
    graph : G.directed_graph;
    dominator_roots : Variable.Set.t
    (* variables that are dominated only by themselves, usually because a
       constant or a symbol can flow to that variable, and thus that
       variable cannot be dominated by another variable. *)
  }

type alias_map = Variable.t Variable.Map.t

let empty ~required_names =
  let graph = Variable.Map.empty in
  let dominator_roots = Variable.Set.empty in
  let params_kind = Variable.Map.empty in
  { required_names; params_kind; graph; dominator_roots }

let add_node t var =
  if not (Name.Set.mem (Name.var var) t.required_names)
  then t
  else
    let graph =
      Variable.Map.update var
        (function None -> Some Variable.Set.empty | Some _ as res -> res)
        t.graph
    in
    { t with graph }

let add_root var t =
  if not (Name.Set.mem (Name.var var) t.required_names)
  then t
  else { t with dominator_roots = Variable.Set.add var t.dominator_roots }

let add_edge ~src ~dst t =
  if not (Name.Set.mem (Name.var src) t.required_names)
  then t
  else
    Simple.pattern_match' dst
      ~const:(fun _ -> add_root src t)
      ~symbol:(fun _ ~coercion:_ -> add_root src t)
      ~var:(fun dst ~coercion:_ ->
          let graph =
            Variable.Map.update src
              (function
                | None -> Some (Variable.Set.singleton dst)
                | Some s -> Some (Variable.Set.add dst s))
              t.graph
          in
          { t with graph })

let add_continuation_info map _k (elt : T.Continuation_info.t) t ~return_continuation ~exn_continuation
  =
  let t =
    List.fold_left
      (fun t bp ->
         let var = Bound_parameter.var bp in
         let t = add_node t var in
         let params_kind =
           Variable.Map.add var (Bound_parameter.kind bp) t.params_kind
         in
         { t with params_kind })
      t
      (Bound_parameters.to_list elt.params)
  in
  let t =
    Variable.Map.fold
      (fun src dst t -> add_edge ~src ~dst t)
      elt.direct_aliases t
  in
  Continuation.Map.fold
    (fun k rewrite_ids t ->
       if Continuation.equal return_continuation k
       || Continuation.equal exn_continuation k
       then t
       else
         let params =
           match Continuation.Map.find k map with
           | elt -> Array.of_list (Bound_parameters.vars elt.T.Continuation_info.params)
           | exception Not_found ->
             Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
               Continuation.print k
         in
         Apply_cont_rewrite_id.Map.fold
           (fun _rewrite_id args t ->
              Numeric_types.Int.Map.fold
                (fun i (dst : T.Cont_arg.t) t ->
                   (* Note on the direction of the edge:

                      We later do a dominator analysis on this graph. To do so,
                      we consider that an edge from ~src to ~dst means: ~dst is
                      used as argument (of an apply_cont), that maps to ~src (as
                      param of a continuation). *)
                   let src = params.(i) in
                   match dst with
                   | Simple dst -> add_edge ~src ~dst t
                   | Function_result -> add_root src t
                   | New_let_binding (var, _) ->
                     let t = add_root var t in
                     add_edge ~src ~dst:(Simple.var var) t)
                args t)
           rewrite_ids t)
    elt.apply_cont_args t

let create ~required_names ~return_continuation ~exn_continuation map =
  let t = empty ~required_names in
  let t =
    Continuation.Map.fold
      (add_continuation_info ~return_continuation ~exn_continuation map)
      map t
  in
  let all_variables =
    Variable.Map.fold
      (fun v dsts acc -> Variable.Set.add v (Variable.Set.union dsts acc))
      t.graph t.dominator_roots
  in
  (* ensure that all variable are mapped: this is a requirement for the SCC
     computation *)
  let t =
    Variable.Set.fold
      (fun var t ->
         let graph =
           Variable.Map.update var
             (function
               | Some _ as res -> res | None -> Some Variable.Set.empty)
             t.graph
         in
         { t with graph })
      all_variables t
  in
  (* Format.eprintf "GRAPH:@\n%a@." (Variable.Map.print Variable.Set.print)
     t.graph; *)
  t

let find_dom var doms =
  (* there are tow cases where the variable is not in the "doms" maps:

     - is not mapped in the graph, which means that it is a let-bound
       variable, in which case it can only be dominated by itself.

     - we are in th efirst iteration of a loop fixpoint, in which case we also
       want to initialize the dominator to the variable itself. *)
  try Variable.Map.find var doms with Not_found -> var

let update_doms_for_one_var { dominator_roots; graph; _ } doms var =
  let dom =
    if Variable.Set.mem var dominator_roots
    then var
    else
      match Variable.Map.find var graph with
      | exception Not_found -> var
      | predecessors ->
        let s =
          Variable.Set.map
            (fun predecessor -> find_dom predecessor doms)
            predecessors
        in
        if Variable.Set.cardinal s = 1 then Variable.Set.choose s else var
  in
  Variable.Map.add var dom doms

let initialize_doms_for_fixpoint { graph; _ } doms vars =
  (* Note: since all vars are in a cycle, all_predecessors will include all
     vars *)
  let all_predecessors =
    List.fold_left
      (fun acc var ->
         let predecessors =
           try Variable.Map.find var graph with Not_found -> assert false
         in
         Variable.Set.union predecessors acc)
      Variable.Set.empty vars
  in
  let init_doms =
    Variable.Set.map (fun var -> find_dom var doms) all_predecessors
  in
  let outside_cycle =
    Variable.Map.of_set
      (fun var -> Variable.Set.singleton (find_dom var doms))
      (Variable.Set.diff all_predecessors (Variable.Set.of_list vars))
  in
  List.fold_left
    (fun doms var -> Variable.Map.add var init_doms doms)
    outside_cycle vars

let rec dom_fixpoint ({ graph; dominator_roots; _ } as t) acc vars =
  let acc' =
    List.fold_left
      (fun acc var ->
         if Variable.Set.mem var dominator_roots
         then Variable.Map.add var (Variable.Set.singleton var) acc
         else
           let init_doms = Variable.Map.find var acc in
           let predecessors =
             try Variable.Map.find var graph with Not_found -> assert false
           in
           let new_doms =
             Variable.Set.fold
               (fun predecessor new_doms ->
                  Variable.Set.inter new_doms
                    (Variable.Map.find predecessor acc))
               predecessors init_doms
           in
           let new_doms = Variable.Set.add var new_doms in
           Variable.Map.add var new_doms acc)
      acc vars
  in
  if Variable.Map.equal Variable.Set.equal acc acc'
  then acc
  else dom_fixpoint t acc' vars

let extract_doms doms fixpoint_result vars =
  let var_set = Variable.Set.of_list vars in
  List.fold_left
    (fun doms var ->
       let fixpoint_doms = Variable.Map.find var fixpoint_result in
       let var_doms = Variable.Set.diff fixpoint_doms var_set in
       let cardinal = Variable.Set.cardinal var_doms in
       assert (cardinal <= 1);
       let dom = if cardinal = 1 then Variable.Set.choose var_doms else var in
       Variable.Map.add var dom doms)
    doms vars

let dominator_analysis ({ graph; _ } as t) : alias_map =
  let components = G.connected_components_sorted_from_roots_to_leaf graph in
  let dominators =
    Array.fold_right
      (fun component doms ->
         match component with
         | G.No_loop var -> update_doms_for_one_var t doms var
         | G.Has_loop vars ->
           let loop_doms = initialize_doms_for_fixpoint t doms vars in
           let loop_result = dom_fixpoint t loop_doms vars in
           let doms = extract_doms doms loop_result vars in
           doms)
      components Variable.Map.empty
  in
  dominators

let aliases_kind { params_kind; required_names; _ } aliases =
  Variable.Map.fold
    (fun param kind acc ->
       if not (Name.Set.mem (Name.var param) required_names)
       then acc
       else
         let alias = Variable.Map.find param aliases in
         (* CR: Not sure this is absolutely necessary, but it's simpler. The
            alternative would be to do a join of all kinds with subkinds for
            all the members of the alias class. *)
         let kind = Flambda_kind.With_subkind.kind kind in
         (match Variable.Map.find alias acc with
          | exception Not_found -> ()
          | alias_kind ->
            if not (Flambda_kind.equal kind alias_kind)
            then Misc.fatal_errorf "Incoherent kinds for aliases !");
         Variable.Map.add alias kind acc)
    params_kind Variable.Map.empty

module Dot = struct
  let node_id ~ctx ppf (variable : Variable.t) =
    Format.fprintf ppf "node_%d_%d" ctx (variable :> int)

  let node ~ctx ~root ppf var =
    if root
    then
      Format.fprintf ppf "%a [shape=record label=\"%a\"];@\n" (node_id ~ctx)
        var Variable.print var
    else
      Format.fprintf ppf "%a [label=\"%a\"];@\n" (node_id ~ctx) var
        Variable.print var

  let nodes ~ctx ~roots ppf var_map =
    Variable.Map.iter
      (fun var _ ->
         let root = Variable.Set.mem var roots in
         node ~ctx ~root ppf var)
      var_map

  let edge ~ctx ~color ppf src dst =
    Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
      (node_id ~ctx) dst color

  let edges ~ctx ~color ppf edge_map =
    Variable.Map.iter
      (fun src dst_set ->
         Variable.Set.iter (fun dst -> edge ~ctx ~color ppf src dst) dst_set)
      edge_map

  let edges' ~ctx ~color ppf edge_map =
    Variable.Map.iter (fun src dst -> edge ~ctx ~color ppf src dst) edge_map

  let print ~ctx ~print_name ~doms ppf t =
    Flambda_colours.without_colours ~f:(fun () ->
        Format.fprintf ppf
          "subgraph cluster_%d { label=\"%s\"@\n%a@\n%a@\n%a@\n}@." ctx
          print_name
          (nodes ~ctx ~roots:t.dominator_roots)
          t.graph
          (edges ~ctx ~color:"black")
          t.graph (edges' ~ctx ~color:"red") doms)
end
