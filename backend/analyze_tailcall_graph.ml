(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2024 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

module String = Misc.Stdlib.String

module Graph = struct
  module Vertex = struct
    type t =
      (* The Unknown_fn vertex an implicit tail edge to every other vertex. *)
      | Unknown_fn
      | Known_fn of string

    let equal v1 v2 =
      match v1, v2 with
      | Unknown_fn, Unknown_fn -> true
      | Known_fn fn1, Known_fn fn2 -> String.equal fn1 fn2
      | Unknown_fn, Known_fn _ | Known_fn _, Unknown_fn -> false
    ;;
  end

  module Edge_head = struct
    type label =
      | Unknown_edge
      (* [@tail] *)
      | Explicit_tail
      (* No annotation. Was in tail position and was tail call optimized *)
      | Implicit_tail
      (* No annotation. Was in tail position and was NOT tail cail optimized *)
      | Implicit_nontail

    type t =
      { to_ : Vertex.t
      ; label : label
      }
  end

  type t =
    { (* Only need to store the edges / vertices for the Known_fn case. *)
      adjacencies : Edge_head.t list String.Tbl.t
    }

  let create () = { adjacencies = String.Tbl.create 100 }
  let reset t = String.Tbl.reset t.adjacencies

  let successors t (v : Vertex.t) : Edge_head.t Seq.t =
    match v with
    | Unknown_fn ->
      String.Tbl.to_seq_keys t.adjacencies
      |> Seq.map (fun fun_name : Edge_head.t ->
        { to_ = Known_fn fun_name; label = Unknown_edge })
      |> Seq.cons ({ to_ = Unknown_fn; label = Unknown_edge } : Edge_head.t)
    | Known_fn fun_name -> String.Tbl.find t.adjacencies fun_name |> List.to_seq
  ;;

  let add_edge
    t
    ~(from : string)
    ~(to_ : Vertex.t)
    ~(apos : [ `Tail | `Nontail ])
    ~(opos : Typedtree.position_and_tail_attribute)
    =
    let label : Edge_head.label option =
      let fail () = failwith "case should be impossible" in
      match apos, opos with
      | _, Unknown_position -> Some Unknown_edge
      | `Tail, Tail_position attr ->
        (match attr with
         | Explicit_tail -> Some Explicit_tail
         | Hint_tail -> Some Explicit_tail
         | Explicit_non_tail -> fail ()
         | Default_tail -> Some Implicit_tail)
      | `Tail, Not_tail_position attr ->
        (match attr with
         | Explicit_tail -> fail ()
         | Hint_tail -> Some Explicit_tail
         | Explicit_non_tail -> fail ()
         | Default_tail -> Some Implicit_tail)
      | `Nontail, Tail_position attr ->
        (match attr with
         | Explicit_tail -> fail ()
         | Hint_tail -> fail ()
         (* This case might be interesting for further analysis *)
         | Explicit_non_tail -> None
         | Default_tail -> Some Implicit_nontail)
      | `Nontail, Not_tail_position attr ->
        (match attr with
         | Explicit_tail -> None
         | Hint_tail -> None
         | Explicit_non_tail -> None
         | Default_tail -> None)
    in
    match label with
    | None -> ()
    | Some label ->
      let edge : Edge_head.t = { to_; label } in
      (match String.Tbl.find_opt t.adjacencies from with
       | None -> String.Tbl.add t.adjacencies from [ edge ]
       | Some lst -> String.Tbl.replace t.adjacencies from (edge :: lst))
  ;;
end

module Global = struct
  let graph : Graph.t option ref = ref None
  let reset_unit_info () = graph := None

  let cfg fmt ~future_funcnames cfg_with_layout =
    let graph : Graph.t =
      match !graph with
      | None ->
        let g = Graph.create () in
        graph := Some g;
        g
      | Some g -> g
    in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let from = Cfg.fun_name cfg in
    let to_ (op : Cfg.func_call_operation) : Graph.Vertex.t =
      match op with
      | Indirect -> Unknown_fn
      | Direct { sym_name; _ } -> Known_fn sym_name
    in
    Cfg.iter_blocks cfg ~f:(fun _ block ->
      match block.terminator.desc with
      | Tailcall_self { original_position = opos; _ } ->
        Graph.add_edge graph ~from ~to_:(Known_fn from) ~apos:`Tail ~opos
      | Tailcall_func { original_position = opos; op } ->
        Graph.add_edge graph ~from ~to_:(to_ op) ~apos:`Tail ~opos
      | Call { original_position = opos; op; _ } ->
        Graph.add_edge graph ~from ~to_:(to_ op) ~apos:`Nontail ~opos
      (* (less-tco) Handle Call_no_return and Prim? *)
      | Call_no_return _
      | Prim _
      | Never
      | Always _
      | Parity_test _
      | Truth_test _
      | Float_test _
      | Int_test _
      | Switch _
      | Return
      | Raise _
      | Specific_can_raise _ -> ());
    cfg_with_layout
  ;;

  let replace ~c ~with_ str = String.split_on_char c str |> String.concat with_

  let mangle_vertex (v : Graph.Vertex.t) =
    match v with
    | Unknown_fn -> "Unknown_fn"
    | Known_fn fn -> "K_" ^ (fn |> replace ~c:'.' ~with_:"_dot_")
  ;;

  let label_of_vertex (v : Graph.Vertex.t) =
    match v with
    | Unknown_fn -> "<unknown>"
    | Known_fn fn -> fn
  ;;

  let print_vertex ppf kv =
    Format.fprintf ppf "%s [label=\"%s\"]" (mangle_vertex kv) (label_of_vertex kv)
  ;;

  let print_edge ppf ((kv, { to_; label }) : Graph.Vertex.t * Graph.Edge_head.t) =
    let color =
      match label with
      | Unknown_edge -> "black"
      | Explicit_tail -> "blue"
      | Implicit_tail -> "black"
      | Implicit_nontail -> "red"
    in
    Format.fprintf
      ppf
      "%s -> %s [color=\"%s\"]"
      (mangle_vertex kv)
      (mangle_vertex to_)
      color
  ;;

  let print_dot ppf =
    match !graph with
    | None -> ()
    | Some g ->
      Format.fprintf ppf "digraph {\n";
      String.Tbl.iter
        (fun kv edges ->
          let kv : Graph.Vertex.t = Known_fn kv in
          Format.fprintf ppf "  %a\n" print_vertex kv;
          List.iter (fun e -> Format.fprintf ppf "  %a\n" print_edge (kv, e)) edges;
          ())
        g.adjacencies;
      Format.fprintf ppf "}\n\n"
  ;;
end
