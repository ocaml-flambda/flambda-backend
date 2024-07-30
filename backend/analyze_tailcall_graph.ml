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
module Int = Numbers.Int

module Hashset = struct
  module type S = sig
    type elt
    type t

    val create : int -> t
    val add : t -> elt -> unit
    val remove : t -> elt -> unit
    val mem : t -> elt -> bool
    val iter : t -> f:(elt -> unit) -> unit
  end

  module Make (T : Hashtbl.S) : S with type elt = T.key = struct
    type elt = T.key
    type t = unit T.t

    let create n = T.create n
    let add t k = T.replace t k ()
    let remove t k = T.remove t k
    let mem t k = T.mem t k
    let iter t ~f = T.iter (fun e () -> f e) t
  end
end

module Graph : sig
  module Vertex : sig
    type t

    val id : t -> int
    val unknown : t
    val is_unknown : t -> bool
    val to_dot_id : t -> string
    val to_dot_label : t -> string

    module Tbl : Hashtbl.S with type key = t
    module Hashset : Hashset.S with type elt = t
  end

  module Edge : sig
    (* For the purpose of analyzing whether TCO inference might break existing
       code (by causing a stack overflow) we are interested in whether there
       are any cycles with Old_tail_edges and Inferred_nontail_edges.
       To find these cycles, we plan on finding SCC's for the subgraph
       consisting of just edges with those labels. *)
    type label =
      (* "Old" edges are edges that are (very likely) not changing their
         TCO behavior as a result of adding TCO inference. I.e., before TCO
         inference, they were tail (nontail) if and only if after TCO
         inference they are tail (nontail). *)
      | Old_tail_edge
      | Old_nontail_edge
      (* "Inferred" edges are edges that might possibly change their TCO
         behavior as a result of adding TCO inference. *)
      | Inferred_tail_edge
      | Inferred_nontail_edge

    type t =
      { from : Vertex.t
      ; to_ : Vertex.t
      ; label : label
      }

    module Tbl : Hashtbl.S with type key = t
    module Hashset : Hashset.S with type elt = t
  end

  type t

  val create : unit -> t
  val reset : t -> unit
  val successors : t -> Vertex.t -> Edge.Hashset.t
  val find_or_add_vertex : t -> fn_name:string -> Vertex.t

  val add_edge
    :  t
    -> from:Vertex.t
    -> to_:Vertex.t
    -> actual_position:[ `Tail | `Nontail ]
    -> original_position:Typedtree.position_and_tail_attribute
    -> unit

  val print_dot : t -> Format.formatter -> unit
end = struct
  module Vertex = struct
    module T = struct
      type t =
        | Unknown_fn
        | Known_fn of
            { id : int
            ; name : string
            }

      let unknown = Unknown_fn

      let is_unknown t =
        match t with
        | Unknown_fn -> true
        | Known_fn _ -> false
      ;;

      let id t =
        match t with
        | Unknown_fn -> -1
        | Known_fn { id; _ } -> id
      ;;

      let equal t1 t2 = id t1 = id t2
      let hash t = Int.hash (id t)
    end

    include T

    let to_dot_id t =
      match t with
      | Unknown_fn -> "unknown"
      | Known_fn { id; _ } -> Int.to_string id
    ;;

    let to_dot_label t =
      match t with
      | Unknown_fn -> "<unknown>"
      | Known_fn { name; _ } -> name
    ;;

    module Tbl = Hashtbl.Make (struct
        include T
      end)

    module Hashset = Hashset.Make (Tbl)
  end

  module Edge = struct
    module T = struct
      type label =
        | Old_tail_edge
        | Old_nontail_edge
        | Inferred_tail_edge
        | Inferred_nontail_edge

      type t =
        { from : Vertex.t
        ; to_ : Vertex.t
        ; label : label
        }

      let label_equal l1 l2 =
        match l1, l2 with
        | Old_tail_edge, Old_tail_edge -> true
        | Old_nontail_edge, Old_nontail_edge -> true
        | Inferred_tail_edge, Inferred_tail_edge -> true
        | Inferred_nontail_edge, Inferred_nontail_edge -> true
        | _ -> false
      ;;

      let equal e1 e2 =
        label_equal e1.label e2.label
        && Vertex.equal e1.from e2.from
        && Vertex.equal e1.to_ e2.to_
      ;;

      let hash e1 = Vertex.hash e1.from + Vertex.hash e1.to_
    end

    include T

    module Tbl = Hashtbl.Make (struct
        include T
      end)

    module Hashset = Hashset.Make (Tbl)
  end

  type t =
    { vertex_by_name : Vertex.t String.Tbl.t
    ; adjacencies : Edge.Hashset.t Vertex.Tbl.t
    }

  let init_unknown_edges t =
    Vertex.Tbl.replace t.adjacencies Vertex.unknown (Edge.Hashset.create 100)
  ;;

  let create () =
    let t =
      { vertex_by_name = String.Tbl.create 100; adjacencies = Vertex.Tbl.create 100 }
    in
    init_unknown_edges t;
    t
  ;;

  let reset t =
    String.Tbl.reset t.vertex_by_name;
    Vertex.Tbl.reset t.adjacencies;
    init_unknown_edges t
  ;;

  let successors t (v : Vertex.t) : Edge.Hashset.t = Vertex.Tbl.find t.adjacencies v

  let find_or_add_vertex t ~(fn_name : string) : Vertex.t =
    let vertex =
      match String.Tbl.find_opt t.vertex_by_name fn_name with
      | None ->
        let id = String.Tbl.length t.vertex_by_name in
        let vertex : Vertex.t = Known_fn { id; name = fn_name } in
        String.Tbl.replace t.vertex_by_name fn_name vertex;
        (* Add edge from unknown *)
        let unknown = Vertex.unknown in
        let edge_from_unknown : Edge.t =
          { from = unknown; to_ = vertex; label = Old_tail_edge }
        in
        Edge.Hashset.add (successors t unknown) edge_from_unknown;
        (* Initialize vertex's adjacency set *)
        let edges_from_vertex = Edge.Hashset.create 10 in
        Vertex.Tbl.replace t.adjacencies vertex edges_from_vertex;
        vertex
      | Some vertex -> vertex
    in
    vertex
  ;;

  let add_edge
    t
    ~(from : Vertex.t)
    ~(to_ : Vertex.t)
    ~(actual_position : [ `Tail | `Nontail ])
    ~(original_position : Typedtree.position_and_tail_attribute)
    =
    let label =
      let impossible_because ~case fmt =
        Misc.fatal_errorf ("case " ^^ case ^^ " impossible because " ^^ fmt)
      in
      match original_position, actual_position with
      (* CR less-tco: Clarify Unknown_position.

         Right now we use it for "fake" applications (like tuplify) which might makes our
         analysis less precise -- e.g. tuplify really is a leaf function, so it cannot form
         a cycle in the call graph.
      *)
      | Unknown_position, `Tail -> `Unknown_tail
      | Unknown_position, `Nontail -> `Unknown_nontail
      | Not_tail_position Explicit_tail, _ ->
        impossible_because
          "[@tail] not allowed on applications not in tail position"
          ~case:"Not_tail_position Explicit_tail, _"
      | Tail_position Explicit_tail, `Nontail ->
        impossible_because
          "[@tail] was not optimized to a tailcall"
          ~case:"Tail_position Explicit_tail, `Nontail"
      | Tail_position Hint_tail, `Nontail ->
        impossible_because
          "[@tail hint] on application in tail position was not optimized to a tailcall"
          ~case:"Tail_position Hint_tail, `Nontail"
      | Tail_position Explicit_non_tail, `Tail
      | Not_tail_position Explicit_non_tail, `Tail ->
        impossible_because
          "[@nontail] was optimized to a tailcall"
          ~case:"_ Explicit_non_tail, `Tail"
      | Tail_position (Explicit_tail | Hint_tail), `Tail -> `Requested_tail
      | Tail_position Default_tail, `Tail -> `Inferred_tail
      | Tail_position Explicit_non_tail, `Nontail -> `Requested_nontail
      | Tail_position Default_tail, `Nontail -> `Inferred_nontail
      | Not_tail_position Hint_tail, `Tail -> `Requested_tail
      | Not_tail_position Default_tail, `Tail -> `Became_tail_after_optimizations
      | Not_tail_position _, `Nontail -> `Not_in_tail_position
    in
    let label : Edge.label =
      match label with
      | `Unknown_tail -> Old_tail_edge
      | `Unknown_nontail -> Old_nontail_edge
      | `Not_in_tail_position -> Old_nontail_edge
      | `Requested_nontail -> Old_nontail_edge
      | `Requested_tail -> Old_tail_edge
      | `Became_tail_after_optimizations ->
        (* This is a conservative approximation *)
        Old_tail_edge
      | `Inferred_tail -> Inferred_tail_edge
      | `Inferred_nontail -> Inferred_nontail_edge
    in
    let edge : Edge.t = { from; to_; label } in
    match from with
    | Unknown_fn ->
      (* An edge already exists from Unknown_fn (added when the vertex was created) *)
      ()
    | Known_fn _ ->
      let edges = Vertex.Tbl.find t.adjacencies from in
      Edge.Hashset.add edges edge
  ;;

  let replace ~c ~with_ str = String.split_on_char c str |> String.concat with_

  let print_vertex_line ppf kv =
    let color = if Vertex.is_unknown kv then "red" else "black" in
    Format.fprintf
      ppf
      "  %s [label=\"%s\" shape=box color=\"%s\" fontcolor=\"%s\"]\n"
      (Vertex.to_dot_id kv)
      (Vertex.to_dot_label kv)
      color
      color
  ;;

  let hide_unknown_edges = true

  let print_edge_line ppf ({ from; to_; label } : Edge.t) =
    if Vertex.is_unknown from && hide_unknown_edges
    then ()
    else (
      let color =
        match label with
        | Old_tail_edge -> "black"
        | Old_nontail_edge -> "lightgrey"
        | Inferred_tail_edge -> "blue"
        | Inferred_nontail_edge -> "red"
      in
      let style =
        let maybe_unknown_style () =
          if Vertex.is_unknown from then "dashed" else "solid"
        in
        match label with
        | Old_tail_edge -> maybe_unknown_style ()
        | Old_nontail_edge -> maybe_unknown_style ()
        | Inferred_tail_edge -> "solid"
        | Inferred_nontail_edge -> "solid"
      in
      Format.fprintf
        ppf
        "  %s -> %s [color=\"%s\" style=\"%s\"]\n"
        (Vertex.to_dot_id from)
        (Vertex.to_dot_id to_)
        color
        style)
  ;;

  let print_dot t ppf =
    Format.fprintf ppf "strict digraph {\n";
    Format.fprintf ppf "  rankdir=LR\n";
    Vertex.Tbl.iter
      (fun vtx edges ->
        print_vertex_line ppf vtx;
        Edge.Hashset.iter edges ~f:(fun e -> print_edge_line ppf e);
        ())
      t.adjacencies;
    Format.fprintf ppf "}\n\n"
  ;;
end

module Global_state = struct
  module Vertex = Graph.Vertex
  module Edge = Graph.Edge

  let graph : Graph.t = Graph.create ()
  let reset_unit_info () = Graph.reset graph

  let cfg fmt ~future_funcnames cfg_with_layout =
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let from = Graph.find_or_add_vertex graph ~fn_name:(Cfg.fun_name cfg) in
    let to_ (op : Cfg.func_call_operation) : Vertex.t =
      match op with
      | Indirect -> Vertex.unknown
      | Direct { sym_name; _ } -> Graph.find_or_add_vertex graph ~fn_name:sym_name
    in
    Cfg.iter_blocks cfg ~f:(fun _ block ->
      let add_edge = Graph.add_edge graph ~from in
      match block.terminator.desc with
      | Tailcall_self { original_position; _ } ->
        add_edge ~to_:from ~actual_position:`Tail ~original_position
      | Tailcall_func { original_position; op } ->
        add_edge ~to_:(to_ op) ~actual_position:`Tail ~original_position
      | Call { original_position; op; _ } ->
        add_edge ~to_:(to_ op) ~actual_position:`Nontail ~original_position
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

  let print_dot ppf = Graph.print_dot graph ppf
end
