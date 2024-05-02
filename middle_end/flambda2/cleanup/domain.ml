
type field = Global_flow_graph.Field.t

type edge =
  | Alias
  | Field of field

type node = {
  id : Code_id_or_name.t;
  mutable is_top : bool; (* Start false *)
  mutable is_bot : bool; (* Start true *)
  edges : (edge * Code_id_or_name.t, node) Hashtbl.t;
}

type graph = (Code_id_or_name.t, node) Hashtbl.t

type elt = Code_id_or_name.t
type dep = Global_flow_graph.Dep.t

let get_or_create_node (g : graph) (id : Code_id_or_name.t) : node =
  match Hashtbl.find_opt g id with
  | Some node -> node
  | None ->
    let node =
      { id;
        is_top = false;
        is_bot = true;
        edges = Hashtbl.create 0 }
    in
    Hashtbl.add g id node;
    node

type updated = Updated | Not_updated

let propagate (uses : graph) (elt : elt) (dep : dep) : updated =
  let node = get_or_create_node uses elt in
  let set_top node updated =
    if node.is_top then updated
    else (node.is_top <- true; Updated)
  in
  let set_not_bottom node updated =
    if not node.is_bot then updated
    else (node.is_bot <- false; Updated)
  in
  let add_alias src src_node target acc =
    if src_node.is_bot then acc
    else
      let acc =
        if Hashtbl.mem target.edges (Alias, src) then
          acc
        else begin
          Hashtbl.replace target.edges (Alias, src) src_node;
          Updated
        end
      in
      let acc = if src_node.is_top then set_top target acc else acc in
      set_not_bottom target acc
  in
  let find_fields_from_aliases field node =
    let aliases = ref (Code_id_or_name.Set.singleton node.id) in
    let targets = ref [node] in
    let work_stack = ref [node] in
    while !work_stack <> [] do
      let node =
        match !work_stack with
        | [] -> assert false
        | hd :: tl ->
          work_stack := tl;
          hd
      in
      Hashtbl.iter (fun (edge, id) target_node ->
        match edge with
          | Field f ->
            if Global_flow_graph.Field.equal f field then
              targets := target_node :: !targets
          | Alias ->
            if not (Code_id_or_name.Set.mem id !aliases) then begin
              aliases := Code_id_or_name.Set.add id !aliases;
              work_stack := target_node :: !work_stack
            end)
        node.edges
    done;
    !targets
  in
  if node.is_bot then Not_updated
  else begin
    match dep with
    | Alias n ->
      let target = get_or_create_node uses (Code_id_or_name.name n) in
      add_alias elt node target Not_updated
    | Contains _ -> (
        assert false
      )
    | Use n ->
      let target = get_or_create_node uses n in
      let acc = Not_updated in
      let acc = set_top target acc in
      set_not_bottom target acc
    | Field (f, n) ->
      let target = get_or_create_node uses (Code_id_or_name.name n) in
      let acc =
        if Hashtbl.mem target.edges (Field f, elt) then
          Not_updated
        else begin
          Hashtbl.replace target.edges (Field f, elt) node;
          Updated
        end
      in
      set_not_bottom target acc
    | Block (f, n) -> begin
        let target = get_or_create_node uses n in
        let targets = find_fields_from_aliases f target in
        let acc = ref Not_updated in
        List.iter (fun target -> acc := add_alias elt node target !acc) targets;
        !acc
    end
    | Alias_if_def (n, c) ->
      let control = get_or_create_node uses (Code_id_or_name.code_id c) in
      let target = get_or_create_node uses (Code_id_or_name.name n) in
      if not (control.is_bot) then
        add_alias elt node target Not_updated
      else
        Not_updated
    | Propagate (n1, n2) ->
      let t1 = get_or_create_node uses (Code_id_or_name.name n1) in
      let t2 = get_or_create_node uses (Code_id_or_name.name n2) in
      if not (t2.is_bot) then
        add_alias (Code_id_or_name.name n2) t2 t1 Not_updated
      else
        Not_updated
  end
