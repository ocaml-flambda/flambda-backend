type graph = Global_flow_graph.graph

type dep = Global_flow_graph.Dep.t

module Field = Global_flow_graph.Field
module DepSet = Global_flow_graph.Dep.Set

let max_depth = 2

(** Represents the part of a value that can be accessed *)
type elt =
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
      Fields { depth; fields }

let propagate (elt : elt) (dep : dep) : (Code_id_or_name.t * elt) option =
  match elt with
  | Bottom -> None
  | Top | Fields _ -> begin
    match dep with
    | Alias n -> Some (Code_id_or_name.name n, elt)
    | Contains n -> (
      match elt with
      | Bottom -> assert false
      | Fields _ -> None
      | Top -> Some (n, Top))
    | Use n -> Some (n, Top)
    | Field (f, n) ->
      let elt = cut_at (max_depth - 1) elt in
      let depth = depth_of elt + 1 in
      Some
        ( Code_id_or_name.name n,
          Fields { depth; fields = Field.Map.singleton f elt } )
    | Block (f, n) -> begin
      match elt with
      | Bottom -> assert false
      | Top -> Some (n, Top)
      | Fields { fields = path; _ } -> (
        match Field.Map.find_opt f path with
        | None -> None
        | Some elt -> Some (n, elt))
    end
    | Alias_if_def _ | Propagate _ -> assert false
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
  let rec add_fungraph (fungraph : Global_flow_graph.fun_graph) =
    Hashtbl.iter
      (fun n deps ->
        (match Hashtbl.find_opt all_deps n with
        | None -> Hashtbl.add all_deps n deps
        | Some deps2 -> Hashtbl.replace all_deps n (DepSet.union deps deps2));
        if Hashtbl.mem result n then Queue.push n q)
      fungraph.name_to_dep;
    add_used fungraph.used
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
      then (
        Format.printf "--- %a@." Code_id_or_name.print k1;
        loop t1 ((k2, e2) :: t2))
      else if c > 0
      then (
        Format.printf "+++ %a@." Code_id_or_name.print k2;
        loop ((k1, e1) :: t1) t2)
      else (
        if not (equal_elt e1 e2)
        then
          Format.printf "DIFF %a: %a %a@." Code_id_or_name.print k1 pp_elt e1
            pp_elt e2;
        loop t1 t2)
  in
  loop l1 l2
