type graph = Cleanup_deps.graph

type dep = Cleanup_deps.Dep.t

type field = Cleanup_deps.field

module DepSet = Cleanup_deps.DepSet

(* module SCC_S : Strongly_connected_components.Id = struct *)

(*   type t = Code_id_or_name.t *)

(*   module Set = Code_id_or_name.Set *)

(*   module Map = Code_id_or_name.Map *)

(*   let print = Code_id_or_name.print *)

(* end *)

module SCC = Strongly_connected_components.Make(Code_id_or_name)

module Make_SCC = struct
  let dep (d : Cleanup_deps.Dep.t) =
    match d with
    | Alias n
    | Use n
    | Field (_, n)
    | Return_of_that_function n ->
      Code_id_or_name.Set.singleton (Code_id_or_name.name n)
    | Contains cn
    | Block (_, cn) ->
      Code_id_or_name.Set.singleton cn
    | Apply (n, c) ->
      Code_id_or_name.Set.add
        (Code_id_or_name.code_id c)
        (Code_id_or_name.Set.singleton (Code_id_or_name.name n))

  let depset (d : DepSet.t) : Code_id_or_name.Set.t =
    DepSet.fold (fun d acc ->
      Code_id_or_name.Set.union acc (dep d))
      d Code_id_or_name.Set.empty

  let add_fungraph (acc : SCC.directed_graph) (fun_graph : Cleanup_deps.fun_graph) =
    Hashtbl.fold (fun cn d acc ->
      Code_id_or_name.Map.add cn (depset d) acc)
      fun_graph.name_to_dep acc

  let from_graph (graph : graph) : SCC.directed_graph =
    Hashtbl.fold (fun _fun fun_graph acc ->
      add_fungraph acc fun_graph)
      graph.function_graphs
      (add_fungraph Code_id_or_name.Map.empty graph.toplevel_graph )

  (* let id_depth graph = *)
  (*   let components = *)
  (*     Scc.connected_components_sorted_from_roots_to_leaf *)
  (*       (from_graph graph) *)
  (*   in *)
  (*   let acc = Hashtbl.create 10 in *)
  (*   Array.iteri (fun i component -> *)
  (*       match component with *)
  (*       | No_loop id -> Hashtbl. *)
  (*     ) components *)
end

module Field = struct
  module M = struct
    type t = field

    let compare : t -> t -> int = compare

    let equal a b = compare a b = 0

    let hash = Hashtbl.hash

    let print = Cleanup_deps.pp_field
  end

  module Container = Container_types.Make (M)
  (* module Set = Container.Set *)
  module Map = Container.Map
end

let max_depth = 2

type elt =
  | Top
  | Fields of int * elt Field.Map.t
  | Bottom

(* To avoid cut_at, elt could be int*elt and everything bellow the depth = 0 is
   Top *)

let rec pp_elt ppf elt =
  match elt with
  | Top -> Format.pp_print_string ppf "⊤"
  | Bottom -> Format.pp_print_string ppf "⊥"
  | Fields (_, f) -> Format.fprintf ppf "{ %a }" (Field.Map.print pp_elt) f

let rec equal_elt e1 e2 =
  match e1, e2 with
  | Bottom, Bottom -> true
  | Bottom, (Top | Fields _) | (Top | Fields _), Bottom -> false
  | Top, Top -> true
  | Top, Fields _ | Fields _, Top -> false
  | Fields (_, f1), Fields (_, f2) ->
    if f1 == f2 then true else Field.Map.equal equal_elt f1 f2

let depth_of e = match e with Bottom | Top -> 0 | Fields (d, _) -> d

let rec join_elt e1 e2 =
  if e1 == e2
  then e1, depth_of e1
  else
    match e1, e2 with
    | Bottom, e | e, Bottom -> e, depth_of e
    | Top, _ | _, Top -> Top, 0
    | Fields (_, f1), Fields (_, f2) ->
      let max_depth = ref 0 in
      let unioner _k e1 e2 =
        let e, depth = join_elt e1 e2 in
        max_depth := max depth !max_depth;
        Some e
      in
      let fields = Field.Map.union unioner f1 f2 in
      Fields (1 + !max_depth, fields), 1 + !max_depth

let join_elt e1 e2 = fst @@ join_elt e1 e2

let rec cut_at depth elt =
  match elt with
  | Top | Bottom -> elt
  | Fields (d, fields) ->
    if d <= depth
    then elt
    else if depth = 0
    then Top
    else
      let fields = Field.Map.map (cut_at (depth - 1)) fields in
      Fields (depth, fields)

let propagate (elt : elt) (dep : dep) : (Code_id_or_name.t * elt) option =
  match elt with
  | Bottom -> None
  | Top | Fields _ -> begin
    match dep with
    | Return_of_that_function n -> begin
      match elt with
      | Bottom -> assert false
      | Fields _ -> None
      | Top -> Some (Code_id_or_name.name n, Top)
    end
    | Alias n -> Some (Code_id_or_name.name n, elt)
    | Apply (n, _) -> Some (Code_id_or_name.name n, Top)
    | Contains n -> Some (n, Top)
    | Use n -> Some (Code_id_or_name.name n, Top)
    | Field (f, n) ->
      let elt = cut_at (max_depth - 1) elt in
      let depth = depth_of elt + 1 in
      Some (Code_id_or_name.name n, Fields (depth, Field.Map.singleton f elt))
    | Block (f, n) -> begin
      match elt with
      | Bottom -> assert false
      | Top -> Some (n, Top)
      | Fields (_, path) -> (
        match Field.Map.find_opt f path with
        | None -> None
        | Some elt -> Some (n, elt))
    end
  end

type result = (Code_id_or_name.t, elt) Hashtbl.t

let pp_result ppf (res : result) =
  let elts = List.of_seq @@ Hashtbl.to_seq res in
  let pp ppf l =
    let pp_sep ppf () = Format.fprintf ppf ",@ " in
    let pp ppf (name, elt) =
      Format.fprintf ppf "%a: %a" Code_id_or_name.print name pp_elt elt
    in
    Format.pp_print_list ~pp_sep pp ppf l
  in
  Format.fprintf ppf "@[<hov 2>{@ %a@ }@]" pp elts

type fixpoint_state = {
  result : result;
  all_deps : (Code_id_or_name.t, DepSet.t) Hashtbl.t;
  added_fungraph : (Code_id.t, unit) Hashtbl.t;
}

let add_subgraph push state (fun_graph : Cleanup_deps.fun_graph) =
  let result = state.result in
  let add_used used =
    Hashtbl.iter
      (fun n () ->
        Hashtbl.replace result n Top;
        push n)
      used
  in
  let add_called called =
    Hashtbl.iter (fun code_id () ->
        Hashtbl.replace result (Code_id_or_name.code_id code_id) (Fields (0, Field.Map.singleton Apply Top));
        (* check_and_add_fungraph (Code_id_or_name.code_id code_id) (Fields (0, Field.Map.singleton Apply Top)) *)
        push (Code_id_or_name.code_id code_id)
      ) called
  in
  add_used fun_graph.used;
  add_called fun_graph.called

let create_state (graph : graph) =
  let state =
    { result = Hashtbl.create 100;
      all_deps = Hashtbl.copy graph.toplevel_graph.name_to_dep;
      added_fungraph = Hashtbl.create 100 }
  in
  add_subgraph (fun _ -> ()) state graph.toplevel_graph;
  state

let fixpoint_component
    (state : fixpoint_state)
    (component : SCC.component)
    (graph : graph) =
  let result : result = state.result in
  let all_deps = state.all_deps in
  let q = Queue.create () in
  let push =
    match component with
    | No_loop id ->
      Queue.push id q;
      (fun _ -> ())
    | Has_loop ids ->
      List.iter (fun id -> Queue.push id q) ids;
      let elements = Code_id_or_name.Set.of_list ids in
      fun n ->
        if Code_id_or_name.Set.mem n elements then
          Queue.push n q
  in
  let rec add_fungraph (fungraph : Cleanup_deps.fun_graph) =
    Hashtbl.iter (fun n deps ->
        (match Hashtbl.find_opt all_deps n with
         | None -> Hashtbl.add all_deps n deps
         | Some deps2 -> Hashtbl.replace all_deps n (DepSet.union deps deps2));
        if Hashtbl.mem result n then push n
      ) fungraph.name_to_dep;
    add_subgraph push state fungraph;
  and check_and_add_fungraph n elt =
    let added_fungraphs = state.added_fungraph in
    Code_id_or_name.pattern_match'
      ~name:(fun _ -> ())
      ~code_id:(fun code_id ->
          if (match elt with Bottom -> false | Fields _ | Top -> true) && not (Hashtbl.mem added_fungraphs code_id) && Hashtbl.mem graph.function_graphs code_id then begin
            add_fungraph (Hashtbl.find graph.function_graphs code_id);
            Hashtbl.add added_fungraphs code_id ()
          end
        ) n
  in
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    let deps =
      match Hashtbl.find_opt all_deps n with
      | None -> DepSet.empty
      | Some s -> s
    in
    match Hashtbl.find_opt result n with
    | None -> (* Unreachable *) ()
    | Some elt ->
    check_and_add_fungraph n elt;
    DepSet.iter
      (fun dep ->
        match propagate elt dep with
        | None -> ()
        | Some (dep_upon, dep_elt) -> (
          assert (dep_elt <> Bottom);
          (* if Code_id_or_name.equal dep_upon n then begin *)
          (*   Misc.fatal_errorf "ICI %a" Code_id_or_name.print n *)
          (* end; *)
          match Hashtbl.find_opt result dep_upon with
          | None ->
            Hashtbl.replace result dep_upon dep_elt;
            push dep_upon
          | Some prev_dep ->
            (* Format.printf "new case@."; *)
            let u = join_elt dep_elt prev_dep in
            (* if !count > 2000000 then begin *)

            (*  Format.printf "dep_elt  %a@." pp_elt dep_elt;  *)
            (*  Format.printf "prev_dep %a@." pp_elt prev_dep; *)
            (*  Format.printf "union    %a@." pp_elt u *)
            (*   end; *)
            if not (equal_elt u prev_dep)
            then begin
              Hashtbl.replace result dep_upon u;
              push dep_upon
            end))
      deps
  done

let fixpoint_topo (graph : graph) : result =
  let state = create_state graph in
  let components =
    SCC.connected_components_sorted_from_roots_to_leaf
      (Make_SCC.from_graph graph)
  in
  Array.iter (fun component ->
      fixpoint_component state component graph) components;
  state.result

let fixpoint (graph : graph) : result =
  (* TODO topological sort *)
  let result : result = Hashtbl.create 100 in
  let q = Queue.create () in
  let all_deps = Hashtbl.copy graph.toplevel_graph.name_to_dep in
  let add_used used =
    Hashtbl.iter
      (fun n () ->
         Hashtbl.replace result n Top;
         Queue.push n q)
      used
  in
  let added_fungraphs = Hashtbl.create 100 in
  let rec add_called called =
    Hashtbl.iter (fun code_id () ->
        Hashtbl.replace result (Code_id_or_name.code_id code_id) (Fields (0, Field.Map.singleton Apply Top));
        (* check_and_add_fungraph (Code_id_or_name.code_id code_id) (Fields (0, Field.Map.singleton Apply Top)) *)
        Queue.push (Code_id_or_name.code_id code_id) q (* CR do better, push_front? *)
      ) called
  and add_fungraph (fungraph : Cleanup_deps.fun_graph) =
    Hashtbl.iter (fun n deps ->
        (match Hashtbl.find_opt all_deps n with
         | None -> Hashtbl.add all_deps n deps
         | Some deps2 -> Hashtbl.replace all_deps n (DepSet.union deps deps2));
        if Hashtbl.mem result n then Queue.push n q
      ) fungraph.name_to_dep;
    add_used fungraph.used;
    add_called fungraph.called;
  and check_and_add_fungraph n elt =
    Code_id_or_name.pattern_match'
      ~name:(fun _ -> ())
      ~code_id:(fun code_id ->
          if (match elt with Bottom -> false | Fields _ | Top -> true) && not (Hashtbl.mem added_fungraphs code_id) && Hashtbl.mem graph.function_graphs code_id then begin
            add_fungraph (Hashtbl.find graph.function_graphs code_id);
            Hashtbl.add added_fungraphs code_id ()
          end
        ) n
  in
  add_used graph.toplevel_graph.used;
  add_called graph.toplevel_graph.called;
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    let deps =
      match Hashtbl.find_opt all_deps n with
      | None -> DepSet.empty
      | Some s -> s
    in
    let elt = Hashtbl.find result n in
    check_and_add_fungraph n elt;
    DepSet.iter
      (fun dep ->
        match propagate elt dep with
        | None -> ()
        | Some (dep_upon, dep_elt) -> (
          assert (dep_elt <> Bottom);
          (* if Code_id_or_name.equal dep_upon n then begin *)
          (*   Misc.fatal_errorf "ICI %a" Code_id_or_name.print n *)
          (* end; *)
          match Hashtbl.find_opt result dep_upon with
          | None ->
            Hashtbl.replace result dep_upon dep_elt;
            Queue.push dep_upon q
          | Some prev_dep ->
            (* Format.printf "new case@."; *)
            let u = join_elt dep_elt prev_dep in
            (* if !count > 2000000 then begin *)

            (*  Format.printf "dep_elt  %a@." pp_elt dep_elt;  *)
            (*  Format.printf "prev_dep %a@." pp_elt prev_dep; *)
            (*  Format.printf "union    %a@." pp_elt u *)
            (*   end; *)
            if not (equal_elt u prev_dep)
            then begin
              Hashtbl.replace result dep_upon u;
              Queue.push dep_upon q
            end))
      deps
  done;
  result

let equal_result r1 r2 =
  let s1 = List.of_seq @@ Hashtbl.to_seq r1 in
  let s2 = List.of_seq @@ Hashtbl.to_seq r2 in
  let l1 = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) s1 in
  let l2 = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) s2 in
  List.equal (fun (k1, e1) (k2, e2) ->
      Code_id_or_name.equal k1 k2 &&
      equal_elt e1 e2) l1 l2

let fixpoint graph =
  let result_topo = fixpoint_topo graph in
  let result = fixpoint graph in
  assert(equal_result result_topo result);
  result
