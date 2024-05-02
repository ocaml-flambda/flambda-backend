let debug_print = false

module Field = struct
  module M = struct
    type t =
      | Block of int
      | Value_slot of Value_slot.t
      | Function_slot of Function_slot.t
      | Code_of_closure

    let compare t1 t2 =
      match t1, t2 with
      | Block n1, Block n2 -> Int.compare n1 n2
      | Value_slot v1, Value_slot v2 -> Value_slot.compare v1 v2
      | Function_slot f1, Function_slot f2 -> Function_slot.compare f1 f2
      | Code_of_closure, Code_of_closure -> 0
      | Block _, (Value_slot _ | Function_slot _ | Code_of_closure) -> -1
      | (Value_slot _ | Function_slot _ | Code_of_closure), Block _ -> 1
      | Value_slot _, (Function_slot _ | Code_of_closure) -> -1
      | (Function_slot _ | Code_of_closure), Value_slot _ -> 1
      | Function_slot _, Code_of_closure -> -1
      | Code_of_closure, Function_slot _ -> 1

    let equal a b = compare a b = 0

    let hash = Hashtbl.hash

    let print ppf = function
      | Block i -> Format.fprintf ppf "%i" i
      | Value_slot s -> Format.fprintf ppf "%a" Value_slot.print s
      | Function_slot f -> Format.fprintf ppf "%a" Function_slot.print f
      | Code_of_closure -> Format.fprintf ppf "Code"
  end

  include M
  module Container = Container_types.Make (M)

  (* module Set = Container.Set *)
  module Map = Container.Map
end

module Dep = struct
  module M = struct
    type t =
      | Alias of Name.t
      | Use of Code_id_or_name.t
      | Contains of Code_id_or_name.t
      | Field of Field.t * Name.t
      | Block of Field.t * Code_id_or_name.t
      | Alias_if_def of Name.t * Code_id.t
      | Propagate of Name.t * Name.t

    let compare t1 t2 =
      let numbering = function
        | Alias _ -> 0
        | Use _ -> 1
        | Contains _ -> 2
        | Field _ -> 3
        | Block _ -> 4
        | Alias_if_def _ -> 5
        | Propagate _ -> 6
      in
      match t1, t2 with
      | Alias v1, Alias v2 -> Name.compare v1 v2
      | Use v1, Use v2 -> Code_id_or_name.compare v1 v2
      | Contains v1, Contains v2 -> Code_id_or_name.compare v1 v2
      | Field (f1, v1), Field (f2, v2) ->
        let c = Field.compare f1 f2 in
        if c <> 0 then c else Name.compare v1 v2
      | Block (f1, v1), Block (f2, v2) ->
        let c = Field.compare f1 f2 in
        if c <> 0 then c else Code_id_or_name.compare v1 v2
      | Alias_if_def (n1, c1), Alias_if_def (n2, c2) ->
        let c = Name.compare n1 n2 in
        if c <> 0 then c else Code_id.compare c1 c2
      | Propagate (n1, m1), Propagate (n2, m2) ->
        let c = Name.compare n1 n2 in
        if c <> 0 then c else Name.compare m1 m2
      | ( ( Alias _ | Use _ | Contains _ | Field _ | Block _
          | Alias_if_def _ | Propagate _ ),
          _ ) ->
        Int.compare (numbering t1) (numbering t2)

    let equal x y = compare x y = 0

    let hash = Hashtbl.hash

    let print ppf = function
      | Alias n -> Format.fprintf ppf "Alias %a" Name.print n
      | Use n -> Format.fprintf ppf "Use %a" Code_id_or_name.print n
      | Contains n -> Format.fprintf ppf "Contains %a" Code_id_or_name.print n
      | Field (f, n) ->
        Format.fprintf ppf "Field %a %a" Field.print f Name.print n
      | Block (f, n) ->
        Format.fprintf ppf "Block %a %a" Field.print f Code_id_or_name.print n
      | Alias_if_def (n, c) ->
        Format.fprintf ppf "Alias_if_def %a %a" Name.print n Code_id.print c
      | Propagate (n1, n2) ->
        Format.fprintf ppf "Propagate %a %a" Name.print n1 Name.print n2
  end

  include M
  module Container = Container_types.Make (M)
  module Set = Container.Set
end

type fun_graph =
  { name_to_dep : (Code_id_or_name.t, Dep.Set.t) Hashtbl.t;
    used : (Code_id_or_name.t, unit) Hashtbl.t
  }

type graph =
  { toplevel_graph : fun_graph;
    function_graphs : (Code_id.t, fun_graph) Hashtbl.t
  }

let pp_used_fun_graph ppf (graph : fun_graph) =
  let elts = List.of_seq @@ Hashtbl.to_seq graph.used in
  let pp ppf l =
    let pp_sep ppf () = Format.pp_print_string ppf "@, " in
    let pp ppf (name, ()) =
      Format.fprintf ppf "%a" Code_id_or_name.print name
    in
    Format.pp_print_list ~pp_sep pp ppf l
  in
  Format.fprintf ppf "{ %a }" pp elts

let pp_used ppf (graph : graph) =
  Format.fprintf ppf "toplevel: %a@ " pp_used_fun_graph graph.toplevel_graph;
  Hashtbl.iter
    (fun code_id graph ->
      Format.fprintf ppf "%a: %a@ " Code_id.print code_id pp_used_fun_graph
        graph)
    graph.function_graphs

let create () = { name_to_dep = Hashtbl.create 100; used = Hashtbl.create 100 }

let insert t k v =
  match Hashtbl.find_opt t k with
  | None -> Hashtbl.add t k (Dep.Set.singleton v)
  | Some s -> Hashtbl.replace t k (Dep.Set.add v s)

let inserts t k v =
  match Hashtbl.find_opt t k with
  | None -> Hashtbl.add t k v
  | Some s -> Hashtbl.replace t k (Dep.Set.union v s)

let add_opaque_let_dependency t bp fv =
  let tbl = t.name_to_dep in
  let bound_to = Bound_pattern.free_names bp in
  let f () bound_to =
    Name_occurrences.fold_names fv
      ~f:(fun () dep ->
        insert tbl (Code_id_or_name.name bound_to) (Dep.Use (Code_id_or_name.name dep)))
      ~init:()
  in
  Name_occurrences.fold_names bound_to ~f ~init:()

let add_let_field t bp field name =
  let tbl = t.name_to_dep in
  let bound_to = Bound_pattern.free_names bp in
  let f () bound_to =
    insert tbl (Code_id_or_name.name bound_to) (Dep.Field (field, name))
  in
  Name_occurrences.fold_names bound_to ~f ~init:()

let add_dep t bound_to dep =
  let tbl = t.name_to_dep in
  insert tbl bound_to dep

let add_deps t bound_to deps =
  let tbl = t.name_to_dep in
  inserts tbl bound_to deps

let add_let_dep t bp dep =
  let tbl = t.name_to_dep in
  let bound_to = Bound_pattern.free_names bp in
  let f () bound_to = insert tbl (Code_id_or_name.name bound_to) dep in
  Name_occurrences.fold_names bound_to ~f ~init:()

let add_cont_dep t bp dep =
  let tbl = t.name_to_dep in
  insert tbl (Code_id_or_name.var bp) (Alias dep)

let add_func_param t ~param ~arg =
  let tbl = t.name_to_dep in
  insert tbl (Code_id_or_name.var param) (Alias arg)

let add_use t dep = Hashtbl.replace t.used dep ()

let add_called t code_id =
  Hashtbl.replace t.used (Code_id_or_name.code_id code_id) ()
