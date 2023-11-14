type graph = Cleanup_deps.graph

type dep = Cleanup_deps.Dep.t

type field = Cleanup_deps.field

module DepSet = Cleanup_deps.DepSet

module Field = struct
  module M = struct
    type t = field

    let compare : t -> t -> int = compare
  end

  module Set = Set.Make (M)
  module Map = Map.Make (M)
end

type elt =
  | Top
  | Fields of elt Field.Map.t
  | Bottom

let pp_elt ppf elt =
  match elt with
  | Top -> Format.pp_print_string ppf "⊤"
  | Bottom -> Format.pp_print_string ppf "⊥"
  | Fields _f -> Format.pp_print_string ppf "S"

let rec equal_elt e1 e2 =
  match e1, e2 with
  | Bottom, Bottom -> true
  | Bottom, (Top | Fields _) | (Top | Fields _), Bottom -> false
  | Top, Top -> true
  | Top, Fields _ | Fields _, Top -> false
  | Fields f1, Fields f2 ->
    if f1 == f2 then true else Field.Map.equal equal_elt f1 f2

let rec join_elt e1 e2 =
  if e1 == e2
  then e1
  else
    match e1, e2 with
    | Bottom, e | e, Bottom -> e
    | Top, _ | _, Top -> Top
    | Fields f1, Fields f2 -> Fields (Field.Map.union unioner f1 f2)

and unioner _k e1 e2 = Some (join_elt e1 e2)

let propagate (elt : elt) (dep : dep) : (Code_id_or_name.t * elt) option =
  match elt with
  | Bottom -> None
  | Top | Fields _ -> begin
    match dep with
    | Return_of_that_function n ->
      Format.eprintf "TODO@.";
      Some (Code_id_or_name.name n, elt)
    | Alias n -> Some (Code_id_or_name.name n, elt)
    | Apply (n, _) -> Some (Code_id_or_name.name n, Top)
    | Contains n -> Some (n, Top)
    | Use n -> Some (Code_id_or_name.name n, Top)
    | Field (f, n) ->
      Some (Code_id_or_name.name n, Fields (Field.Map.singleton f elt))
    | Block (f, n) -> begin
      match elt with
      | Bottom -> assert false
      | Top -> Some (n, Top)
      | Fields path -> (
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

let fixpoint (graph : graph) : result =
  (* TODO topological sort *)
  let result : result = Hashtbl.create 100 in
  let q = Queue.create () in
  Hashtbl.iter
    (fun n () ->
      let n = Code_id_or_name.name n in
      Hashtbl.replace result n Top;
      Queue.push n q)
    graph.used;
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    let deps =
      match Hashtbl.find_opt graph.name_to_dep n with
      | None -> DepSet.empty
      | Some s -> s
    in
    let elt = Hashtbl.find result n in
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
              Hashtbl.replace result dep_upon dep_elt;
              Queue.push dep_upon q
            end))
      deps
  done;
  result
