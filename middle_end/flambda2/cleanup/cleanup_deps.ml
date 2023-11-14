type field =
  | Block of int
  | Value_slot of Value_slot.t
  | Function_slot of Function_slot.t

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

type graph =
  { name_to_dep : (Code_id_or_name.t, DepSet.t) Hashtbl.t;
    used : (Name.t, unit) Hashtbl.t (* TODO: Conditionnal on a Code_id *)
  }

let pp_used ppf (graph : graph) =
  let elts = List.of_seq @@ Hashtbl.to_seq graph.used in
  let pp ppf l =
    let pp_sep ppf () = Format.pp_print_string ppf ",@ " in
    let pp ppf (name, ()) = Format.fprintf ppf "%a" Name.print name in
    Format.pp_print_list ~pp_sep pp ppf l
  in
  Format.fprintf ppf "{ %a }" pp elts

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
