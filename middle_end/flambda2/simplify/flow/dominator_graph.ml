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
module G = Strongly_connected_components.Make (Simple)

type t =
  { required_names : Name.Set.t;
    params_kind : Flambda_kind.With_subkind.t Variable.Map.t;
    graph : G.directed_graph;
    dominator_roots : Simple.Set.t
        (* variables that are dominated only by themselves, such as variables
           that are let-bound in a continuation (and that are not aliases). *)
  }

type alias_map = Simple.t Variable.Map.t

let empty ~required_names =
  let graph = Simple.Map.empty in
  let dominator_roots = Simple.Set.empty in
  let params_kind = Variable.Map.empty in
  { required_names; params_kind; graph; dominator_roots }

(* Adding new nodes *)

let add_node_aux t simple =
  let graph =
    Simple.Map.update simple
      (function None -> Some Simple.Set.empty | Some _ as res -> res)
      t.graph
  in
  { t with graph }

let add_node t simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
      if Name.Set.mem name t.required_names then add_node_aux t simple else t)
    ~const:(fun _ -> add_node_aux t simple)

(* Adding roots *)

let add_root var t =
  if not (Name.Set.mem (Name.var var) t.required_names)
  then t
  else
    { t with
      dominator_roots = Simple.Set.add (Simple.var var) t.dominator_roots
    }

(* Adding edges *)

let add_edge ~src ~dst t =
  if not (Name.Set.mem (Name.var src) t.required_names)
  then t
  else
    let graph =
      Simple.Map.update (Simple.var src)
        (function
          | None -> Some (Simple.Set.singleton dst)
          | Some s -> Some (Simple.Set.add dst s))
        t.graph
    in
    { t with graph }

(* Import continuation info into the graph *)

let add_continuation_info map _k (elt : T.Continuation_info.t) t
    ~return_continuation ~exn_continuation =
  let t =
    List.fold_left
      (fun t bp ->
        let var = Bound_parameter.var bp in
        let t = add_node t (Simple.var var) in
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
          | elt ->
            Array.of_list (Bound_parameters.vars elt.T.Continuation_info.params)
          | exception Not_found ->
            Misc.fatal_errorf "Continuation not found during Data_flow: %a@."
              Continuation.print k
        in
        Apply_cont_rewrite_id.Map.fold
          (fun _rewrite_id args t ->
            Numeric_types.Int.Map.fold
              (fun i (dst : T.Cont_arg.t) t ->
                (* Note on the direction of the edge:

                   We later do a dominator analysis on this graph. To do so, we
                   consider that an edge from ~src to ~dst means: ~dst is used
                   as argument (of an apply_cont), that maps to ~src (as param
                   of a continuation). *)
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
  let all_simples =
    Simple.Map.fold
      (fun s dsts acc -> Simple.Set.add s (Simple.Set.union dsts acc))
      t.graph t.dominator_roots
  in
  (* ensure that all simples are mapped: this is a requirement for the SCC
     computation *)
  let t =
    Simple.Set.fold
      (fun var t ->
        let graph =
          Simple.Map.update var
            (function Some _ as res -> res | None -> Some Simple.Set.empty)
            t.graph
        in
        { t with graph })
      all_simples t
  in
  t

let find_dom simple doms =
  Simple.pattern_match'
    simple (* Symbols and constants are always their own dominators *)
    ~symbol:(fun _ ~coercion:_ -> simple)
    ~const:(fun _ -> simple)
    ~var:(fun var ~coercion:_ ->
      (* there are two cases where the variable is not in the "doms" maps:

         - is not mapped in the graph, which means that it is a let-bound
         variable, in which case it can only be dominated by itself.

         - we are in the first iteration of a loop fixpoint, in which case we
         also want to initialize the dominator to the variable itself. *)
      try Variable.Map.find var doms with Not_found -> simple)

let update_doms_for_one_var { dominator_roots; graph; _ } doms var =
  let simple = Simple.var var in
  let dom =
    if Simple.Set.mem simple dominator_roots
    then simple
    else
      match Simple.Map.find simple graph with
      | exception Not_found -> simple
      | predecessors -> (
        let s =
          Simple.Set.map
            (fun predecessor -> find_dom predecessor doms)
            predecessors
        in
        match Simple.Set.get_singleton s with Some ret -> ret | None -> simple)
  in
  Variable.Map.add var dom doms

let initialize_doms_for_fixpoint { graph; _ } doms simples =
  (* Note: since all simples are in a cycle, all_predecessors will include all
     of the simples from the cycle. Also, since we're a in a cycle, all simples
     should actually be variables. *)
  let all_predecessors =
    List.fold_left
      (fun acc var ->
        let predecessors =
          try Simple.Map.find var graph with Not_found -> assert false
        in
        Simple.Set.union predecessors acc)
      Simple.Set.empty simples
  in
  let init_doms =
    Simple.Set.map (fun simple -> find_dom simple doms) all_predecessors
  in
  let outside_cycle =
    Simple.Map.of_set
      (fun simple -> Simple.Set.singleton (find_dom simple doms))
      (Simple.Set.diff all_predecessors (Simple.Set.of_list simples))
  in
  List.fold_left
    (fun doms simple -> Simple.Map.add simple init_doms doms)
    outside_cycle simples

let rec dom_fixpoint ({ graph; dominator_roots; _ } as t) acc simples =
  let acc' =
    List.fold_left
      (fun acc simple ->
        if Simple.Set.mem simple dominator_roots
        then Simple.Map.add simple (Simple.Set.singleton simple) acc
        else
          let init_doms = Simple.Map.find simple acc in
          let predecessors =
            try Simple.Map.find simple graph with Not_found -> assert false
          in
          let new_doms =
            Simple.Set.fold
              (fun predecessor new_doms ->
                Simple.Set.inter new_doms (Simple.Map.find predecessor acc))
              predecessors init_doms
          in
          let new_doms = Simple.Set.add simple new_doms in
          Simple.Map.add simple new_doms acc)
      acc simples
  in
  if Simple.Map.equal Simple.Set.equal acc acc'
  then acc
  else dom_fixpoint t acc' simples

let extract_doms doms fixpoint_result simples =
  let simple_set = Simple.Set.of_list simples in
  List.fold_left
    (fun doms simple ->
      let var =
        Simple.pattern_match' simple
          ~var:(fun v ~coercion:_ -> v)
          ~symbol:(fun _ ~coercion:_ ->
            Misc.fatal_errorf
              "Cycles in the dominator analysis should never contain symbols")
          ~const:(fun _ ->
            Misc.fatal_errorf
              "Cycles in the dominator analysis should never contain constants")
      in
      let fixpoint_doms = Simple.Map.find simple fixpoint_result in
      let var_doms = Simple.Set.diff fixpoint_doms simple_set in
      let cardinal = Simple.Set.cardinal var_doms in
      assert (cardinal <= 1);
      let dom = if cardinal = 1 then Simple.Set.choose var_doms else simple in
      Variable.Map.add var dom doms)
    doms simples

let dominator_analysis ({ graph; _ } as t) : alias_map =
  let components = G.connected_components_sorted_from_roots_to_leaf graph in
  let dominators =
    Array.fold_right
      (fun component doms ->
        match component with
        | G.No_loop simple ->
          Simple.pattern_match' simple
            ~var:(fun var ~coercion:_ -> update_doms_for_one_var t doms var)
            ~symbol:(fun _ ~coercion:_ -> doms)
            ~const:(fun _ -> doms)
        | G.Has_loop simples ->
          let loop_doms = initialize_doms_for_fixpoint t doms simples in
          let loop_result = dom_fixpoint t loop_doms simples in
          let doms = extract_doms doms loop_result simples in
          doms)
      components Variable.Map.empty
  in
  dominators

let aliases_kind { params_kind; required_names; _ } (aliases : alias_map) =
  Variable.Map.fold
    (fun param kind acc ->
      if not (Name.Set.mem (Name.var param) required_names)
      then acc
      else
        let alias = Variable.Map.find param aliases in
        (* CR: Not sure this is absolutely necessary, but it's simpler. The
           alternative would be to do a join of all kinds with subkinds for all
           the members of the alias class. *)
        let kind = Flambda_kind.With_subkind.kind kind in
        Simple.pattern_match' alias
          ~symbol:(fun _ ~coercion:_ -> acc)
          ~const:(fun _ -> acc)
          ~var:(fun v ~coercion:_ ->
            (match Variable.Map.find v acc with
            | exception Not_found -> ()
            | alias_kind ->
              if not (Flambda_kind.equal kind alias_kind)
              then Misc.fatal_errorf "Incoherent kinds for aliases !");
            Variable.Map.add v kind acc))
    params_kind Variable.Map.empty

module Dot = struct
  let node_id ~ctx ppf (simple : Simple.t) =
    Format.fprintf ppf "node_%d_%d" ctx (simple :> int)

  let node ~ctx ~root ppf simple =
    if root
    then
      Format.fprintf ppf "%a [shape=record label=\"%a\"];@\n" (node_id ~ctx)
        simple Simple.print simple
    else
      Format.fprintf ppf "%a [label=\"%a\"];@\n" (node_id ~ctx) simple
        Simple.print simple

  let nodes ~ctx ~roots ppf map =
    Simple.Map.iter
      (fun simple _ ->
        let root =
          Simple.pattern_match' simple
            ~symbol:(fun _ ~coercion:_ -> true)
            ~const:(fun _ -> true)
            ~var:(fun _ ~coercion:_ -> Simple.Set.mem simple roots)
        in
        node ~ctx ~root ppf simple)
      map

  let edge ~ctx ~color ppf src dst =
    Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
      (node_id ~ctx) dst color

  let edges ~ctx ~color ppf edge_map =
    Simple.Map.iter
      (fun src dst_set ->
        Simple.Set.iter (fun dst -> edge ~ctx ~color ppf src dst) dst_set)
      edge_map

  let edges' ~ctx ~color ppf edge_map =
    Variable.Map.iter
      (fun src dst -> edge ~ctx ~color ppf (Simple.var src) dst)
      edge_map

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
