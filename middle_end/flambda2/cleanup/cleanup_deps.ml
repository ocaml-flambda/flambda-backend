type field =
  | Block of int
  | Value_slot of Value_slot.t
  | Function_slot of Function_slot.t

let pp_field ppf = function
  | Block i -> Format.fprintf ppf "%i" i
  | Value_slot s -> Format.fprintf ppf "%a" Value_slot.print s
  | Function_slot f -> Format.fprintf ppf "%a" Function_slot.print f

module Dep = struct
  type t =
    | Alias of Name.t
    | Use of Name.t
    | Contains of Code_id_or_name.t
    | Field of field * Name.t
    | Block of field * Code_id_or_name.t
    | Apply of Name.t * Code_id.t
    | Return_of_that_function of Name.t

  let compare = compare
end

module DepSet = Set.Make (Dep)

type fun_graph =
  { name_to_dep : (Code_id_or_name.t, DepSet.t) Hashtbl.t;
    used : (Code_id_or_name.t, unit) Hashtbl.t
  }

type graph =
  { toplevel_graph : fun_graph;
    function_graphs : (Code_id.t, fun_graph) Hashtbl.t
  }

let pp_used_fun_graph ppf (graph : fun_graph) =
  let elts = List.of_seq @@ Hashtbl.to_seq graph.used in
  let pp ppf l =
    let pp_sep ppf () = Format.pp_print_string ppf ",@ " in
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
  | None -> Hashtbl.add t k (DepSet.singleton v)
  | Some s -> Hashtbl.replace t k (DepSet.add v s)

let inserts t k v =
  match Hashtbl.find_opt t k with
  | None -> Hashtbl.add t k v
  | Some s -> Hashtbl.replace t k (DepSet.union v s)

let add_opaque_let_dependency t bp fv =
  let tbl = t.name_to_dep in
  let bound_to = Bound_pattern.free_names bp in
  let f () bound_to =
    Name_occurrences.fold_names fv
      ~f:(fun () dep ->
        insert tbl (Code_id_or_name.name bound_to) (Dep.Use dep))
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
