let do_print = Sys.getenv_opt "PRT" <> None

module Field = struct
  module M = struct
    type t = 
      | Block of int
      | Value_slot of Value_slot.t
      | Function_slot of Function_slot.t
      | Apply

    let compare t1 t2 =
      match t1, t2 with
      | Block n1, Block n2 -> Int.compare n1 n2
      | Value_slot v1, Value_slot v2 -> Value_slot.compare v1 v2
      | Function_slot f1, Function_slot f2 -> Function_slot.compare f1 f2
      | Apply, Apply -> 0
      | Block _, (Value_slot _ | Function_slot _ | Apply) -> -1
      | (Value_slot _ | Function_slot _ | Apply), Block _ -> 1
      | Value_slot _, (Function_slot _ | Apply) -> -1
      | (Function_slot _ | Apply), Value_slot _ -> 1
      | Function_slot _, Apply -> -1
      | Apply, Function_slot _ -> 1

    let equal a b = compare a b = 0

    let hash = Hashtbl.hash

    let print ppf = function
      | Block i -> Format.fprintf ppf "%i" i
      | Value_slot s -> Format.fprintf ppf "%a" Value_slot.print s
      | Function_slot f -> Format.fprintf ppf "%a" Function_slot.print f
      | Apply -> Format.fprintf ppf "apply"
  end

  include M

  module Container = Container_types.Make (M)

  (* module Set = Container.Set *)
  module Map = Container.Map
end


module Dep = struct
  type t =
    | Alias of Name.t
    | Use of Name.t
    | Contains of Code_id_or_name.t
    | Field of Field.t * Name.t
    | Block of Field.t * Code_id_or_name.t
    | Apply of Name.t * Code_id.t
    | Return_of_that_function of Name.t

  let compare = compare

  let equal x y = compare x y = 0

  let hash = Hashtbl.hash

  let print ppf = function
    | Alias n -> Format.fprintf ppf "Alias %a" Name.print n
    | Use n -> Format.fprintf ppf "Use %a" Name.print n
    | Contains n -> Format.fprintf ppf "Contains %a" Code_id_or_name.print n
    | Field (f, n) -> Format.fprintf ppf "Field %a %a" Field.print f Name.print n
    | Block (f, n) ->
      Format.fprintf ppf "Block %a %a" Field.print f Code_id_or_name.print n
    | Apply (n, c) ->
      Format.fprintf ppf "Apply %a %a" Name.print n Code_id.print c
    | Return_of_that_function n ->
      Format.fprintf ppf "Return_of_that_function %a" Name.print n
end

module C = Container_types.Make (Dep)
module DepSet = C.Set

type fun_graph =
  { name_to_dep : (Code_id_or_name.t, DepSet.t) Hashtbl.t;
    used : (Code_id_or_name.t, unit) Hashtbl.t;
    called : (Code_id.t, unit) Hashtbl.t
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

let create () =
  { name_to_dep = Hashtbl.create 100;
    used = Hashtbl.create 100;
    called = Hashtbl.create 100
  }

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

let add_called t code_id = Hashtbl.replace t.called code_id ()
