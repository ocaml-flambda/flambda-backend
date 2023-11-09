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
  match e1, e2 with
  | Bottom, e | e, Bottom -> e
  | Top, _ | _, Top -> Top
  | Fields f1, Fields f2 -> Fields (Field.Map.union unioner f1 f2)

and unioner _k e1 e2 = Some (join_elt e1 e2)

let propagate (elt : elt) (dep : dep) : (Name.t * elt) option =
  match elt with
  | Bottom -> None
  | Top | Fields _ -> begin
    match dep with
    | Alias n -> Some (n, elt)
    | Apply (n, _) | Contains n | Use n -> Some (n, Top)
    | Field (f, n) -> Some (n, Fields (Field.Map.singleton f elt))
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

type result = (Name.t, elt) Hashtbl.t

let pp_result ppf (res : result) =
  let elts = List.of_seq @@ Hashtbl.to_seq res in
  let pp ppf l =
    let pp_sep ppf () = Format.pp_print_string ppf ",@ " in
    let pp ppf (name, elt) =
      Format.fprintf ppf "%a: %a" Name.print name pp_elt elt
    in
    Format.pp_print_list ~pp_sep pp ppf l
  in
  Format.fprintf ppf "{ %a }" pp elts

let fixpoint (graph : graph) : result =
  (* TODO topological sort *)
  let result : result = Hashtbl.create 100 in
  let q = Queue.create () in
  Hashtbl.iter
    (fun n () ->
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
