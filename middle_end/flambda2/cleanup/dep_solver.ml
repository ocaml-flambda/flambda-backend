type dep = Global_flow_graph.Dep.t

module Field = Global_flow_graph.Field

module type Graph = sig
  type graph

  module Node : Container_types.S

  type edge

  val fold_nodes : graph -> (Node.t -> 'a -> 'a) -> 'a -> 'a

  val fold_edges : graph -> Node.t -> (edge -> 'a -> 'a) -> 'a -> 'a

  val target : edge -> Node.t

  type state

  type elt

  val top : elt

  val is_top : elt -> bool

  val is_bottom : elt -> bool

  val join : state -> elt -> elt -> elt

  val widen : state -> old:elt -> elt -> elt

  val less_equal : state -> elt -> elt -> bool

  val propagate : state -> elt -> edge -> elt

  val propagate_top : state -> edge -> bool

  val get : state -> Node.t -> elt

  val set : state -> Node.t -> elt -> unit
end

module Make_Fixpoint (G : Graph) = struct
  module Node = G.Node
  module SCC = Strongly_connected_components.Make (Node)

  module Make_SCC = struct
    let depset (graph : G.graph) (n : Node.t) : Node.Set.t =
      G.fold_edges graph n
        (fun edge acc -> Node.Set.add (G.target edge) acc)
        Node.Set.empty

    let ensure_domain acc s =
      Node.Set.fold
        (fun x acc ->
          if Node.Map.mem x acc then acc else Node.Map.add x Node.Set.empty acc)
        s acc

    let from_graph (graph : G.graph) (state : G.state) : SCC.directed_graph =
      G.fold_nodes graph
        (fun n acc ->
          let deps = depset graph n in
          let acc = ensure_domain acc deps in
          (* For nodes which are already as [top], the fixpoint is already
             reached. We can safely ignore the dependency and process these
             nodes at the beginning, cutting some cycles. *)
          let deps =
            Node.Set.filter
              (fun other -> not (G.is_top (G.get state other)))
              deps
          in
          Node.Map.add n deps acc)
        Node.Map.empty
  end

  let propagate_tops (graph : G.graph) (roots : Node.Set.t) (state : G.state) =
    let rec loop stack =
      match stack with
      | [] -> ()
      | n :: stack ->
        let stack =
          G.fold_edges graph n
            (fun dep stack ->
              if G.propagate_top state dep
              then
                let target = G.target dep in
                if G.is_top (G.get state target)
                then stack
                else (
                  G.set state n G.top;
                  target :: stack)
              else stack)
            stack
        in
        loop stack
    in
    let stack = Node.Set.elements roots in
    List.iter (fun n -> G.set state n G.top) stack;
    loop stack

  let fixpoint_component (graph : G.graph) (state : G.state)
      (component : SCC.component) =
    match component with
    | No_loop id ->
      let current_elt = G.get state id in
      if not (G.is_bottom current_elt)
      then
        G.fold_edges graph id
          (fun dep () ->
            let propagated = G.propagate state current_elt dep in
            if not (G.is_bottom propagated)
            then
              let target = G.target dep in
              let old = G.get state target in
              G.set state target (G.join state old propagated))
          ()
    | Has_loop ids ->
      let q = Queue.create () in
      (* Invariants: [!q_s] contails the elements that may be pushed in [q],
         that is, the elements of [ids] that are not already in [q]. *)
      let in_loop = Node.Set.of_list ids in
      let q_s = ref Node.Set.empty in
      List.iter (fun id -> Queue.push id q) ids;
      let push n =
        if Node.Set.mem n !q_s
        then begin
          Queue.push n q;
          q_s := Node.Set.remove n !q_s
        end
      in
      let propagate id =
        let current_elt = G.get state id in
        if not (G.is_bottom current_elt)
        then
          G.fold_edges graph id
            (fun dep () ->
              let propagated = G.propagate state current_elt dep in
              if not (G.is_bottom propagated)
              then
                let target = G.target dep in
                let old = G.get state target in
                if Node.Set.mem target in_loop
                then begin
                  let widened = G.widen state ~old propagated in
                  if not (G.less_equal state widened old)
                  then (
                    G.set state target widened;
                    push target)
                end
                else G.set state target (G.join state old propagated))
            ()
      in
      while not (Queue.is_empty q) do
        let n = Queue.pop q in
        q_s := Node.Set.add n !q_s;
        propagate n
      done

  let fixpoint_topo (graph : G.graph) (roots : Node.Set.t) (state : G.state) =
    propagate_tops graph roots state;
    let components =
      SCC.connected_components_sorted_from_roots_to_leaf
        (Make_SCC.from_graph graph state)
    in
    if Sys.getenv_opt "SHOWCOMP" <> None
    then begin
      Format.eprintf "ncomps: %d, max size: %d@." (Array.length components)
        (Array.fold_left
           (fun m -> function
             | SCC.No_loop _ -> m
             | SCC.Has_loop l -> max m (List.length l))
           0 components);
      Array.iter
        (function
          | SCC.No_loop id -> Format.eprintf "%a@ " Node.print id
          | SCC.Has_loop l ->
            Format.eprintf "[%a]@ " (Format.pp_print_list Node.print) l)
        components;
      Format.eprintf "@."
    end;
    Array.iter
      (fun component -> fixpoint_component graph state component)
      components
end

let max_depth = 2

(** Represents the part of a value that can be accessed *)
type elt = Dep_solver_safe.elt =
  | Top  (** Value completely accessed *)
  | Fields of
      { depth : int;
        fields : elt Field.Map.t
      }
      (** Those fields of the value are accessed;
      invariants:
    * depth is the maximum of fields depth + 1
    * no element of fields is Bottom *)
  | Bottom  (** Value not accessed *)

(* To avoid cut_at, elt could be int*elt and everything bellow the depth = 0 is
   Top *)

let rec pp_elt ppf elt =
  match elt with
  | Top -> Format.pp_print_string ppf "⊤"
  | Bottom -> Format.pp_print_string ppf "⊥"
  | Fields { depth; fields } ->
    Format.fprintf ppf "%d{ %a }" depth (Field.Map.print pp_elt) fields

let rec less_equal_elt e1 e2 =
  match e1, e2 with
  | Bottom, _ | _, Top -> true
  | (Top | Fields _), Bottom | Top, Fields _ -> false
  | Fields { fields = f1; _ }, Fields { fields = f2; _ } ->
    if f1 == f2
    then true
    else
      let ok = ref true in
      ignore
        (Field.Map.merge
           (fun _ e1 e2 ->
             let e1 = Option.value e1 ~default:Bottom in
             let e2 = Option.value e2 ~default:Bottom in
             if not (less_equal_elt e1 e2) then ok := false;
             None)
           f1 f2);
      !ok

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
      Fields { depth; fields }

let target (dep : dep) : Code_id_or_name.t =
  match dep with
  | Alias n -> Code_id_or_name.name n
  | Contains n -> n
  | Use n -> n
  | Field (_, n) -> Code_id_or_name.name n
  | Block (_, n) -> n
  | Alias_if_def (n, _) -> Code_id_or_name.name n
  | Propagate (n, _) -> Code_id_or_name.name n

let propagate uses (elt : elt) (dep : dep) : elt =
  match elt with
  | Bottom -> Bottom
  | Top | Fields _ -> begin
    match dep with
    | Alias _ -> elt
    | Contains _ -> (
        assert false
      )
    | Use _ -> Top
    | Field (f, _) ->
      let depth = depth_of elt + 1 in
      Fields { depth; fields = Field.Map.singleton f elt }
    | Block (f, _) -> begin
      match elt with
      | Bottom -> assert false
      | Top -> Top
      | Fields { fields = path; _ } -> (
        match Field.Map.find_opt f path with None -> Bottom | Some elt -> elt)
    end
    | Alias_if_def (_, c) -> begin
      match Hashtbl.find_opt uses (Code_id_or_name.code_id c) with
      | None | Some Bottom -> Bottom
      | Some (Fields _ | Top) -> elt
    end
    | Propagate (_, n) -> begin
      match Hashtbl.find_opt uses (Code_id_or_name.name n) with
      | None -> Bottom
      | Some elt -> elt
    end
  end

let propagate_top uses (dep : dep) : bool =
  match dep with
  | Alias _ -> true
  | Contains _ -> true
  | Use _ -> true
  | Field _ -> false
  | Block (_, _) -> true
  | Alias_if_def (_, c) -> begin
    match Hashtbl.find_opt uses (Code_id_or_name.code_id c) with
    | None | Some Bottom -> false
    | Some (Fields _ | Top) -> true
  end
  | Propagate (_, n2) -> begin
    match Hashtbl.find_opt uses (Code_id_or_name.name n2) with
    | None | Some (Bottom | Fields _) -> false
    | Some Top -> true
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

module Graph = struct
  type graph = Global_flow_graph.fun_graph

  module Node = Code_id_or_name

  type edge = Global_flow_graph.Dep.t

  let fold_nodes graph f init =
    Hashtbl.fold
      (fun n _ acc -> f n acc)
      graph.Global_flow_graph.name_to_dep init

  let fold_edges graph n f init =
    match Hashtbl.find_opt graph.Global_flow_graph.name_to_dep n with
    | None -> init
    | Some deps -> Global_flow_graph.Dep.Set.fold f deps init

  let target = target

  type nonrec elt = elt

  type state = result

  let top = Top

  let is_top = function Top -> true | Bottom | Fields _ -> false

  let is_bottom = function Bottom -> true | Top | Fields _ -> false

  let widen _ ~old:elt1 elt2 = cut_at max_depth (join_elt elt1 elt2)

  let join _ elt1 elt2 = cut_at max_depth (join_elt elt1 elt2)
  (* XXX no need to cut *)

  let less_equal _ elt1 elt2 = less_equal_elt elt1 elt2

  let propagate = propagate

  let propagate_top = propagate_top

  let get state n =
    match Hashtbl.find_opt state n with None -> Bottom | Some elt -> elt

  let set state n elt = Hashtbl.replace state n elt
end

module Solver = Make_Fixpoint (Graph)

let fixpoint graph_old (graph_new : Global_flow_graph.fun_graph) =
  let result = Dep_solver_safe.fixpoint graph_old in
  let result_topo = Hashtbl.create 17 in
  Solver.fixpoint_topo graph_new
    (graph_new.Global_flow_graph.used |> Hashtbl.to_seq_keys |> List.of_seq
   |> Code_id_or_name.Set.of_list)
    result_topo;
  if Sys.getenv_opt "TOTO" <> None
  then (
    Format.eprintf "TOPO:@.%a@.@." pp_result result_topo;
    Format.eprintf "NORMAL:@.%a@.@." pp_result result);
  if not (Dep_solver_safe.equal_result result_topo result)
  then begin
    Format.printf "NOT EQUAL:@.%a@.XXX@.%a@.END@.XXXXXXX@.DIFF:@." pp_result
      result_topo pp_result result;
    Dep_solver_safe.print_diff result result_topo;
    assert false
  end;
  assert (Dep_solver_safe.equal_result result_topo result);
  result
