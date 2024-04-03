type graph = Cleanup_deps.graph

type dep = Cleanup_deps.Dep.t

type field = Cleanup_deps.field

module DepSet = Cleanup_deps.DepSet
module SCC = Strongly_connected_components.Make (Code_id_or_name)


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

(** Represents the part of a value that can be accessed *)
type elt =
  | Top  (** Value completely accessed *)
  | Fields of { depth : int; fields : elt Field.Map.t }
    (** Those fields of the value are accessed;
        invariants:
        * depth is the maximum of fields depth + 1
        * no element of fields is Bottom *)
  | Bottom  (** Value not accessed *)

module Make_SCC = struct
  let dep (d : Cleanup_deps.Dep.t) =
    match d with
    | Alias n | Use n | Field (_, n) | Return_of_that_function n ->
      Code_id_or_name.Set.singleton (Code_id_or_name.name n)
    | Contains cn | Block (_, cn) -> Code_id_or_name.Set.singleton cn
    | Apply (n, c) ->
      Code_id_or_name.Set.add
        (Code_id_or_name.code_id c)
        (Code_id_or_name.Set.singleton (Code_id_or_name.name n))

  let depset (d : DepSet.t) : Code_id_or_name.Set.t =
    DepSet.fold
      (fun d acc -> Code_id_or_name.Set.union acc (dep d))
      d Code_id_or_name.Set.empty

  let ensure_domain acc s =
    Code_id_or_name.Set.fold
      (fun x acc ->
        if Code_id_or_name.Map.mem x acc
        then acc
        else Code_id_or_name.Map.add x Code_id_or_name.Set.empty acc)
      s acc

  let add_fungraph code_id (acc : SCC.directed_graph)
      (fun_graph : Cleanup_deps.fun_graph) =
    let acc =
      Hashtbl.fold
        (fun cn d acc ->
          let deps = depset d in
          let ndeps =
            match Code_id_or_name.Map.find_opt cn acc with
            | None -> deps
            | Some deps2 -> Code_id_or_name.Set.union deps deps2
          in
          ensure_domain (Code_id_or_name.Map.add cn ndeps acc) deps)
        fun_graph.name_to_dep acc
    in
    match code_id with
    | None -> acc
    | Some code_id ->
      let code_id = Code_id_or_name.code_id code_id in
      let acc = ensure_domain acc (Code_id_or_name.Set.singleton code_id) in
      let invert = false in
      let acc =
        Hashtbl.fold
          (fun cn () acc ->
            let cn, code_id = if invert then code_id, cn else cn, code_id in
            let d =
              match Code_id_or_name.Map.find_opt code_id acc with
              | None -> Code_id_or_name.Set.empty
              | Some d -> d
            in
            let acc = ensure_domain acc (Code_id_or_name.Set.singleton cn) in
            Code_id_or_name.Map.add code_id (Code_id_or_name.Set.add cn d) acc)
          fun_graph.used acc
      in
      let acc =
        Hashtbl.fold
          (fun cid2 () acc ->
            let cn = Code_id_or_name.code_id cid2 in
            let cn, code_id = if invert then code_id, cn else cn, code_id in
            let d =
              match Code_id_or_name.Map.find_opt code_id acc with
              | None -> Code_id_or_name.Set.empty
              | Some d -> d
            in
            let acc = ensure_domain acc (Code_id_or_name.Set.singleton cn) in
            Code_id_or_name.Map.add code_id (Code_id_or_name.Set.add cn d) acc)
          fun_graph.called acc
      in
      Hashtbl.fold
        (fun _ ds acc ->
           DepSet.fold (fun dp acc -> Code_id_or_name.Set.fold (fun cn acc ->
          let cn, code_id = if invert then code_id, cn else cn, code_id in
          let d =
            match Code_id_or_name.Map.find_opt code_id acc with
            | None -> Code_id_or_name.Set.empty
            | Some d -> d
          in
          let acc = ensure_domain acc (Code_id_or_name.Set.singleton cn) in
          Code_id_or_name.Map.add code_id (Code_id_or_name.Set.add cn d) acc) (dep dp) acc) ds acc)
        fun_graph.name_to_dep acc

  let from_graph (graph : graph) result : SCC.directed_graph =
    let acc = Hashtbl.fold
      (fun code_id fun_graph acc -> add_fungraph (Some code_id) acc fun_graph)
      graph.function_graphs
      (add_fungraph None Code_id_or_name.Map.empty graph.toplevel_graph)
    in
    ignore result;
    if true then
    Code_id_or_name.Map.map (Code_id_or_name.Set.filter (fun n -> Hashtbl.find_opt result n <> Some Top)) acc
else Code_id_or_name.Map.mapi (fun n d -> if Hashtbl.find_opt result n = Some Top then Code_id_or_name.Set.empty else d) acc

end

(* To avoid cut_at, elt could be int*elt and everything bellow the depth = 0 is
   Top *)

let rec pp_elt ppf elt =
  match elt with
  | Top -> Format.pp_print_string ppf "⊤"
  | Bottom -> Format.pp_print_string ppf "⊥"
  | Fields { depth; fields } ->
    Format.fprintf ppf "%d{ %a }" depth (Field.Map.print pp_elt) fields

let rec equal_elt e1 e2 =
  match e1, e2 with
  | Bottom, Bottom -> true
  | Bottom, (Top | Fields _) | (Top | Fields _), Bottom -> false
  | Top, Top -> true
  | Top, Fields _ | Fields _, Top -> false
  | Fields { fields = f1; _ }, Fields { fields = f2; _ } ->
    if f1 == f2 then true else Field.Map.equal equal_elt f1 f2

let depth_of e = match e with Bottom | Top -> 0 | Fields f -> f.depth

let rec join_elt e1 e2 =
  if e1 == e2
  then e1
  else
    match e1, e2 with
    | Bottom, e | e, Bottom -> e
    | Top, _ | _, Top -> Top
    | Fields f1, Fields f2 ->
      let unioner _k e1 e2 =
        let e = join_elt e1 e2 in
        Some e
      in
      let fields = Field.Map.union unioner f1.fields f2.fields in
      let max_depth =
        Field.Map.fold (fun _k e depth -> max depth (depth_of e)) fields 0
      in
      Fields { depth = 1 + max_depth; fields }

let rec cut_at depth elt =
  match elt with
  | Top | Bottom -> elt
  | Fields { depth = d; fields } ->
    if d <= depth
    then elt
    else if depth = 0
    then Top
    else
      let fields = Field.Map.map (cut_at (depth - 1)) fields in
      Fields {depth; fields}

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
      Some (Code_id_or_name.name n, Fields {depth; fields = Field.Map.singleton f elt})
    | Block (f, n) -> begin
      match elt with
      | Bottom -> assert false
      | Top -> Some (n, Top)
      | Fields { fields = path; _ } -> (
        match Field.Map.find_opt f path with
        | None -> None
        | Some elt -> Some (n, elt))
    end
  end

let propagate_top (dep : dep) : Code_id_or_name.t option =
    match dep with
    | Return_of_that_function n ->
      Some (Code_id_or_name.name n)
    | Alias n -> Some (Code_id_or_name.name n)
    | Apply (n, _) -> Some (Code_id_or_name.name n)
    | Contains n -> Some (n)
    | Use n -> Some (Code_id_or_name.name n)
    | Field _ -> None
    | Block (_, n) -> 
      Some (n)

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

type fixpoint_state =
  { result : result;
    all_added : (Code_id_or_name.t, unit) Hashtbl.t;
    all_deps : (Code_id_or_name.t, DepSet.t) Hashtbl.t;
    added_fungraph : (Code_id.t, unit) Hashtbl.t
  }

let add_subgraph push state (fun_graph : Cleanup_deps.fun_graph) =
  let result = state.result in
  let add_used used =
    Hashtbl.iter
      (fun n () ->
        if Hashtbl.find_opt result n <> Some Top then begin
           Hashtbl.replace result n Top;
           push n
        end)
      used
  in
  let add_called called =
    Hashtbl.iter
      (fun code_id () ->
        let code_id = Code_id_or_name.code_id code_id in
        if not (Hashtbl.mem result code_id)
        then begin
          Hashtbl.replace result code_id
            (Fields { depth = 1; fields = Field.Map.singleton Apply Top });
            push code_id
        end
        (* check_and_add_fungraph (Code_id_or_name.code_id code_id) (Fields (1,
           Field.Map.singleton Apply Top)) *))
      called
  in
  add_used fun_graph.used;
  add_called fun_graph.called

let create_state (graph : graph) =
  let state =
    { result = Hashtbl.create 100;
      all_added = Hashtbl.create 100;
      all_deps = graph.toplevel_graph.name_to_dep;
      added_fungraph = Hashtbl.create 100
    }
  in
  let stack = ref [] in
  let called_stack = ref [] in
  let add_used used =
    if not (Hashtbl.mem state.result used) then begin
      Hashtbl.replace state.result used Top;
      stack := used :: !stack
    end
  in
  let add_called called =
    if Hashtbl.mem graph.function_graphs called then begin
      Hashtbl.replace state.added_fungraph called ();
      let subgraph = Hashtbl.find graph.function_graphs called in
      Hashtbl.remove graph.function_graphs called;
      called_stack := subgraph :: !called_stack
    end
  in
  let add_subgraph (subgraph : Cleanup_deps.fun_graph) =
    Hashtbl.iter (fun n () -> add_used n) subgraph.used;
    Hashtbl.iter (fun code_id () -> add_called code_id) subgraph.called;
    Hashtbl.iter
      (fun n deps ->
         if Hashtbl.mem state.result n then
           DepSet.iter (fun dep -> match propagate_top dep with None -> () | Some n2 -> add_used n2) deps;
         (match Hashtbl.find_opt state.all_deps n with
          | None -> Hashtbl.add state.all_deps n deps
          | Some deps2 -> Hashtbl.replace state.all_deps n (DepSet.union deps deps2)))
      subgraph.name_to_dep
  in
  let rec loop () =
    match !stack with
    | n :: rest ->
      stack := rest;
      let cur_deps = try Hashtbl.find state.all_deps n with Not_found -> DepSet.empty in
      DepSet.iter (fun dep -> match propagate_top dep with None -> () | Some n2 -> add_used n2) cur_deps;
      Code_id_or_name.pattern_match' n ~name:(fun _ -> ()) ~code_id:(fun code_id ->
        add_called code_id
        );
      loop ()
    | [] ->
      match !called_stack with
      | (subgraph : Cleanup_deps.fun_graph) :: rest ->
        called_stack := rest;
        add_subgraph subgraph;
        loop ()
      | [] -> ()
  in
  Hashtbl.iter (fun n () -> add_used n) graph.toplevel_graph.used;
  Hashtbl.iter (fun code_id () -> add_called code_id) graph.toplevel_graph.called;
  loop ();
  Hashtbl.iter (fun n _ -> Hashtbl.replace graph.toplevel_graph.used n ()) state.result;
  Hashtbl.iter (fun code_id0 () -> 
      let code_id = Code_id_or_name.code_id code_id0 in
      if not (Hashtbl.mem state.result code_id)
      then begin
        Hashtbl.replace state.result code_id
          (Fields { depth = 1; fields = Field.Map.singleton Apply Top });
        Hashtbl.replace graph.toplevel_graph.called code_id0 ()
      end
    ) state.added_fungraph;
  state

let dbg = Sys.getenv_opt "ABCD" <> None

let fixpoint_component (state : fixpoint_state) (component : SCC.component)
    (graph : graph) =
  let result : result = state.result in
  let all_deps = state.all_deps in
  let q = Queue.create () in
  (* Invariants: [!q_s] contails the elements that may be pushed in [q], that
     is, the elements of [ids] that are not already in [q]. *)
  let q_s = ref Code_id_or_name.Set.empty in
  let push =
    match component with
    | No_loop id ->
      Queue.push id q;
      fun n -> if dbg then Format.eprintf "PUSH %a@." Code_id_or_name.print n; assert (not (Hashtbl.mem state.all_added n))
    | Has_loop ids ->
      List.iter (fun id -> Queue.push id q) ids;
      fun n ->
        if dbg then Format.eprintf "PUSH %a@." Code_id_or_name.print n;
        assert (not (Hashtbl.mem state.all_added n));
        if Code_id_or_name.Set.mem n !q_s
        then begin
          Queue.push n q;
          q_s := Code_id_or_name.Set.remove n !q_s
        end
  in
  if dbg then Format.eprintf "=====@.";
  let rec add_fungraph (fungraph : Cleanup_deps.fun_graph) =
    Hashtbl.iter
      (fun n deps ->
        (match Hashtbl.find_opt all_deps n with
        | None -> Hashtbl.add all_deps n deps
        | Some deps2 -> Hashtbl.replace all_deps n (DepSet.union deps deps2));
        if Hashtbl.mem result n then propagate_elt n (Hashtbl.find result n) deps)
      fungraph.name_to_dep;
    add_subgraph push state fungraph
  and check_and_add_fungraph n elt =
    let added_fungraphs = state.added_fungraph in
    Code_id_or_name.pattern_match'
      ~name:(fun _ -> ())
      ~code_id:(fun code_id ->
        if (match elt with Bottom -> false | Fields _ | Top -> true)
           && (not (Hashtbl.mem added_fungraphs code_id))
           && Hashtbl.mem graph.function_graphs code_id
        then begin
          if dbg then Format.eprintf "ADD_FUNGRAPH %a@." Code_id.print code_id;
          add_fungraph (Hashtbl.find graph.function_graphs code_id);
          Hashtbl.add added_fungraphs code_id ()
        end)
      n
  and propagate_elt n elt deps =
    if dbg then Format.eprintf "PROPAGATE %a %a@." Code_id_or_name.print n pp_elt elt;
    check_and_add_fungraph n elt;
    DepSet.iter
      (fun dep ->
         match propagate elt dep with
         | None -> ()
         | Some (dep_upon, dep_elt) -> (
             assert (dep_elt <> Bottom);
             match Hashtbl.find_opt result dep_upon with
             | None ->
               Hashtbl.replace result dep_upon dep_elt;
               push dep_upon
             | Some prev_dep ->
               let u = join_elt dep_elt prev_dep in
               if not (equal_elt u prev_dep)
               then begin
                 Hashtbl.replace result dep_upon u;
                 push dep_upon
               end))
      deps
  in
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    q_s := Code_id_or_name.Set.add n !q_s;
    let deps =
      match Hashtbl.find_opt all_deps n with
      | None -> DepSet.empty
      | Some s -> s
    in
    match Hashtbl.find_opt result n with
    | None -> if dbg then Format.eprintf "POP %a %a@." Code_id_or_name.print n pp_elt Bottom; ()
    | Some elt ->
      if dbg then Format.eprintf "POP %a %a@." Code_id_or_name.print n pp_elt elt;
      propagate_elt n elt deps
  done;
  match component with
  | No_loop id -> Hashtbl.add state.all_added id ()
  | Has_loop ids -> List.iter (fun n -> Hashtbl.add state.all_added n ()) ids

let fixpoint_topo (graph : graph) : result =
  let state = create_state graph in
  let components =
    SCC.connected_components_sorted_from_roots_to_leaf
      (Make_SCC.from_graph graph state.result)
  in
  if Sys.getenv_opt "SHOWCOMP" <> None
  then begin
    Format.eprintf "ncomps: %d, max size: %d@." (Array.length components)
      (Array.fold_left
         (fun m -> function
           | SCC.No_loop _ -> m
           | SCC.Has_loop l -> max m (List.length l))
         0 components);
    Array.iter (function
      | SCC.No_loop id -> Format.eprintf "%a@ " Code_id_or_name.print id
      | SCC.Has_loop l -> Format.eprintf "[%a]@ " (Format.pp_print_list Code_id_or_name.print) l
      ) components;
    Format.eprintf "@."
  end;
  Array.iter
    (fun component -> fixpoint_component state component graph)
    components;
  state.result

let fixpoint (graph : graph) : result =
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
    Hashtbl.iter
      (fun code_id () ->
        let code_id = Code_id_or_name.code_id code_id in
        if not (Hashtbl.mem result code_id)
        then
          Hashtbl.replace result code_id
            (Fields { depth = 1; fields = Field.Map.singleton Apply Top });
        (* check_and_add_fungraph (Code_id_or_name.code_id code_id) (Fields (1,
           Field.Map.singleton Apply Top)) *)
        Queue.push code_id q (* CR do better, push_front? *))
      called
  and add_fungraph (fungraph : Cleanup_deps.fun_graph) =
    Hashtbl.iter
      (fun n deps ->
        (match Hashtbl.find_opt all_deps n with
        | None -> Hashtbl.add all_deps n deps
        | Some deps2 -> Hashtbl.replace all_deps n (DepSet.union deps deps2));
        if Hashtbl.mem result n then Queue.push n q)
      fungraph.name_to_dep;
    add_used fungraph.used;
    add_called fungraph.called
  and check_and_add_fungraph n elt =
    Code_id_or_name.pattern_match'
      ~name:(fun _ -> ())
      ~code_id:(fun code_id ->
        if (match elt with Bottom -> false | Fields _ | Top -> true)
           && (not (Hashtbl.mem added_fungraphs code_id))
           && Hashtbl.mem graph.function_graphs code_id
        then begin
          add_fungraph (Hashtbl.find graph.function_graphs code_id);
          Hashtbl.add added_fungraphs code_id ()
        end)
      n
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
          match Hashtbl.find_opt result dep_upon with
          | None ->
            Hashtbl.replace result dep_upon dep_elt;
            Queue.push dep_upon q
          | Some prev_dep ->
            let u = join_elt dep_elt prev_dep in
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
  List.equal
    (fun (k1, e1) (k2, e2) -> Code_id_or_name.equal k1 k2 && equal_elt e1 e2)
    l1 l2

let print_diff r1 r2 =
  let s1 = List.of_seq @@ Hashtbl.to_seq r1 in
  let s2 = List.of_seq @@ Hashtbl.to_seq r2 in
  let l1 = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) s1 in
  let l2 = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) s2 in
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | (k1, _) :: t1, [] ->
      Format.printf "--- %a@." Code_id_or_name.print k1;
      loop t1 []
    | [], (k2, _) :: t2 ->
      Format.printf "+++ %a@." Code_id_or_name.print k2;
      loop [] t2
    | (k1, e1) :: t1, (k2, e2) :: t2 ->
      let c = Code_id_or_name.compare k1 k2 in
      if c < 0
      then (Format.printf "--- %a@." Code_id_or_name.print k1; loop t1 ((k2, e2) :: t2))
      else if c > 0
      then (Format.printf "+++ %a@." Code_id_or_name.print k2; loop ((k1, e1) :: t1) t2)
      else (if not (equal_elt e1 e2)
      then
        Format.printf "DIFF %a: %a %a@." Code_id_or_name.print k1 pp_elt e1
          pp_elt e2;
      loop t1 t2)
  in
  loop l1 l2

let fixpoint graph =
  let result = fixpoint graph in
  let result_topo = fixpoint_topo graph in
  if Sys.getenv_opt "TOTO" <> None
  then (
    Format.eprintf "TOPO:@.%a@.@." pp_result result_topo;
    Format.eprintf "NORMAL:@.%a@.@." pp_result result);
  if not (equal_result result_topo result)
  then begin
    Format.printf "NOT EQUAL:@.%a@.XXX@.%a@.END@.XXXXXXX@.DIFF:@." pp_result
      result_topo pp_result result;
    print_diff result result_topo;
    assert false
  end;
  assert (equal_result result_topo result);
  result
