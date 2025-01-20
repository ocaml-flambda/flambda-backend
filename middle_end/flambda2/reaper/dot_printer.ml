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
     Format.formatter_of_out_channel ch)

let dot_count = ref ~-1

let print_graph ~print ~lazy_ppf ~graph =
  incr dot_count;
  let ppf = Lazy.force lazy_ppf in
  Format.fprintf ppf "digraph g {@\n";
  print ~ctx:!dot_count ppf graph;
  Format.fprintf ppf "@\n}@."

let print ~ctx ~iter_edges ~print_color ppf graph =
  let all_nodes = Hashtbl.create 17 in
  let node_id ppf (variable : Code_id_or_name.t) =
    Format.fprintf ppf "node_%d_%d" ctx (variable :> int)
  in
  let node ppf name =
    if not (Hashtbl.mem all_nodes name)
    then (
      Hashtbl.add all_nodes name ();
      Format.fprintf ppf
        "%a [label=\"%a\" style=\"filled\" fillcolor=\"%s\"];@\n" node_id name
        Code_id_or_name.print name (print_color name))
  in
  let print_edge (n1, n2, color) =
    node ppf n1;
    node ppf n2;
    Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" node_id n1 node_id n2 color
  in
  iter_edges ~print_edge graph

let white_color _id = "white"

let print_dep dep =
  print_graph ~lazy_ppf:dep_graph_ppf ~graph:dep
    ~print:(print ~iter_edges:Graph.print_iter_edges ~print_color:white_color)

let print_solved_dep (result : Dep_solver.result) dep =
  print_graph ~lazy_ppf:dep_graph_ppf ~graph:dep
    ~print:
      (print ~iter_edges:Graph.print_iter_edges
         ~print_color:(Dep_solver.print_color result))
