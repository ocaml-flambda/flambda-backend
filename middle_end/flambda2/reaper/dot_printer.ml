(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Graph = Global_flow_graph

type code_dep = Traverse_acc.code_dep

let dep_graph_ppf =
  lazy
    (let filename = "dep.dot" in
     let ch = open_out filename in
     let ppf = Format.formatter_of_out_channel ch in
     (* Format.fprintf ppf "digraph g {@\n"; *)
     (* at_exit (fun () -> *)
     (*     Format.fprintf ppf "@\n}@."; *)
     (*     close_out ch); *)
     ppf)

let dot_count = ref ~-1

let print_graph ~print ~print_name ~lazy_ppf ~graph =
  incr dot_count;
  let ppf = Lazy.force lazy_ppf in
  Format.fprintf ppf "digraph g {@\n";
  print ~ctx:!dot_count ~print_name ppf graph;
  Format.fprintf ppf "@\n}@."

module P = struct
  let node_id ~ctx ppf (variable : Code_id_or_name.t) =
    Format.fprintf ppf "node_%d_%d" ctx (variable :> int)

  let node ~ctx ~root ~print_color ppf name =
    if root
    then
      Format.fprintf ppf
        "%a [shape=record label=\"%a\" style=\"filled\" fillcolor=\"%s\"];@\n"
        (node_id ~ctx) name Code_id_or_name.print name (print_color name)
    else
      Format.fprintf ppf
        "%a [label=\"%a\" style=\"filled\" fillcolor=\"%s\"];@\n" (node_id ~ctx)
        name Code_id_or_name.print name (print_color name)

  let dep_names (dep : Graph.Dep.t) =
    match dep with
    | Graph.Dep.Alias { target } | Graph.Dep.Accessor { target; _ } ->
      [Code_id_or_name.name target]
    | Graph.Dep.Use { target } | Graph.Dep.Constructor { target; _ } -> [target]
    | Graph.Dep.Alias_if_def { target; if_defined } ->
      [Code_id_or_name.name target; Code_id_or_name.code_id if_defined]
    | Graph.Dep.Propagate { target; source } ->
      [Code_id_or_name.name target; Code_id_or_name.name source]

  let all_names t =
    let names = Hashtbl.create 100 in
    Hashtbl.iter
      (fun name dep ->
        let dep_names =
          Graph.Dep.Set.fold (fun dep acc -> dep_names dep @ acc) dep []
        in
        List.iter (fun name -> Hashtbl.replace names name ()) (name :: dep_names))
      (Graph.name_to_dep t);
    names

  let nodes ~ctx ~print_color ppf t =
    Hashtbl.iter
      (fun name _ ->
        let root =
          Code_id_or_name.pattern_match'
            ~code_id:(fun _ -> false)
            ~name:(fun name ->
              Hashtbl.mem (Graph.used t) (Code_id_or_name.name name))
            name
        in
        node ~ctx ~root ~print_color ppf name)
      (all_names t)

  let edge ~ctx ppf src (dst : Graph.Dep.t) =
    let color, deps =
      match dst with
      | Alias { target } -> "black", [Code_id_or_name.name target]
      | Use { target } -> "red", [target]
      | Accessor { target; _ } -> "green", [Code_id_or_name.name target]
      | Constructor { target; _ } -> "blue", [target]
      | Alias_if_def { target; _ } -> "pink", [Code_id_or_name.name target]
      | Propagate { target; _ } -> "brown", [Code_id_or_name.name target]
    in
    List.iter
      (fun dst ->
        Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
          (node_id ~ctx) dst color)
      deps

  let edges ~ctx ppf t =
    Hashtbl.iter
      (fun src dst_set ->
        Graph.Dep.Set.iter (fun dst -> edge ~ctx ppf src dst) dst_set)
      (Graph.name_to_dep t)

  let code_deps ~ctx ~code_id ~print_color ppf
      (code_dep : Traverse_acc.code_dep) =
    node ~ctx ~root:false ~print_color ppf (Code_id_or_name.code_id code_id);
    node ~ctx ~root:false ~print_color ppf
      (Code_id_or_name.var code_dep.my_closure);
    List.iter
      (fun v -> node ~ctx ~root:false ~print_color ppf (Code_id_or_name.var v))
      ((code_dep.exn :: code_dep.return) @ code_dep.params)

  let print ~ctx ~print_color ~print_name ppf
      ((_code_dep, t) : code_dep Code_id.Map.t * Graph.graph) =
    Flambda_colours.without_colours ~f:(fun () ->
        Format.fprintf ppf "subgraph cluster_%d { label=\"%s\"@\n%a@\n%a}@." ctx
          print_name (nodes ~ctx ~print_color) t (edges ~ctx) t)
end

let white_color _id = "white"

let print_dep dep =
  print_graph ~print_name:"dep" ~lazy_ppf:dep_graph_ppf ~graph:dep
    ~print:(P.print ~print_color:white_color)

let print_solved_dep (result : Dep_solver.result) dep =
  print_graph ~print_name:"solve_dep" ~lazy_ppf:dep_graph_ppf ~graph:dep
    ~print:(P.print ~print_color:(Dep_solver.print_color result))
