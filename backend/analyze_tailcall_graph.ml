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
[@@@ocaml.warning "+a-30-40-41-42"]

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

    val length : t -> int

    val to_seq : t -> elt Seq.t
  end

  module Make (T : Hashtbl.S) : S with type elt = T.key = struct
    type elt = T.key

    type t = unit T.t

    let create n = T.create n

    let add t k = T.replace t k ()

    let remove t k = T.remove t k

    let mem t k = T.mem t k

    let iter t ~f = T.iter (fun e () -> f e) t

    let length t = T.length t

    let to_seq t = T.to_seq_keys t
  end
end

module Graph : sig
  module Vertex : sig
    type t

    val unknown : t

    module Hashset : Hashset.S with type elt = t
  end

  type t

  val create : unit -> t

  val reset : t -> unit

  val find_or_add_vertex : t -> fn_name:string -> Vertex.t

  type actual_position =
    | Tail
    | Nontail

  val add_edge :
    t ->
    from:Vertex.t ->
    to_:Vertex.t ->
    actual_position:actual_position ->
    original_position:Typedtree.position_and_tail_attribute ->
    unit

  (* Partitions vertices into hashsets where two vertices are in the same
     hashset iff they are in the same strongly-connected component.

     Explicit_nontail_edges are ignored in this SCC decomposition. *)
  val decompose_tailcall_sccs : t -> Vertex.Hashset.t list

  val print_dot : Format.formatter -> t -> sccs:Vertex.Hashset.t list -> unit
end = struct
  module Vertex = struct
    module T = struct
      type t =
        | Unknown_fn
        | Known_fn of
            { id : int;
              name : string
            }

      let unknown = Unknown_fn

      let is_unknown t = match t with Unknown_fn -> true | Known_fn _ -> false

      let id t = match t with Unknown_fn -> -1 | Known_fn { id; _ } -> id

      let equal t1 t2 = id t1 = id t2

      let hash t = Int.hash (id t)
    end

    include T

    let to_dot_id t =
      match t with
      | Unknown_fn -> "unknown"
      | Known_fn { id; _ } -> Int.to_string id

    let to_dot_label t =
      match t with Unknown_fn -> "<unknown>" | Known_fn { name; _ } -> name

    module Tbl = Hashtbl.Make (T)
    module Hashset = Hashset.Make (Tbl)
  end

  module Edge = struct
    module T = struct
      module Label = struct
        (* For the purpose of analyzing whether TCO inference might break
           existing code (by causing a stack overflow) we are interested in
           whether there are any cycles with Explicit_tail_edges and
           Inferred_nontail_edges. To find these cycles, we plan on finding
           SCC's for the subgraph consisting of just edges with those labels. *)
        type t =
          (* "Explicit" here really means "not inferred." "Explicit" edges are
             edges that are (very likely) not changing their TCO behavior as a
             result of adding TCO inference. I.e., before TCO inference, they
             were tail (nontail) if and only if after TCO inference they are
             tail (nontail). *)
          | Explicit_tail_edge
          | Explicit_nontail_edge
          (* "Inferred" edges are edges that might possibly change their TCO
             behavior as a result of adding TCO inference. *)
          | Inferred_tail_edge
          | Inferred_nontail_edge

        let equal t1 t2 =
          match t1, t2 with
          | Explicit_tail_edge, Explicit_tail_edge -> true
          | Explicit_nontail_edge, Explicit_nontail_edge -> true
          | Inferred_tail_edge, Inferred_tail_edge -> true
          | Inferred_nontail_edge, Inferred_nontail_edge -> true
          | ( ( Explicit_tail_edge | Explicit_nontail_edge | Inferred_tail_edge
              | Inferred_nontail_edge ),
              _ ) ->
            false

        let hash t =
          match t with
          | Explicit_tail_edge -> 0
          | Explicit_nontail_edge -> 1
          | Inferred_tail_edge -> 2
          | Inferred_nontail_edge -> 3
      end

      type t =
        { from : Vertex.t;
          label : Label.t;
          to_ : Vertex.t
        }

      let equal e1 e2 =
        Vertex.equal e1.from e2.from
        && Label.equal e1.label e2.label
        && Vertex.equal e1.to_ e2.to_

      let hash e1 =
        Vertex.hash e1.from + Label.hash e1.label + Vertex.hash e1.to_
    end

    include T
    module Tbl = Hashtbl.Make (T)
    module Hashset = Hashset.Make (Tbl)
  end

  type t =
    { vertex_by_name : Vertex.t String.Tbl.t;
      adjacencies : Edge.Hashset.t Vertex.Tbl.t
    }

  let init_unknown_edges t =
    Vertex.Tbl.replace t.adjacencies Vertex.unknown (Edge.Hashset.create 100)

  let create () =
    let t =
      { vertex_by_name = String.Tbl.create 100;
        adjacencies = Vertex.Tbl.create 100
      }
    in
    init_unknown_edges t;
    t

  let reset t =
    String.Tbl.reset t.vertex_by_name;
    Vertex.Tbl.reset t.adjacencies;
    init_unknown_edges t

  let successors t (v : Vertex.t) : Edge.Hashset.t =
    Vertex.Tbl.find t.adjacencies v

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
          { from = unknown; to_ = vertex; label = Explicit_tail_edge }
        in
        Edge.Hashset.add (successors t unknown) edge_from_unknown;
        (* Initialize vertex's adjacency set *)
        let edges_from_vertex = Edge.Hashset.create 10 in
        Vertex.Tbl.replace t.adjacencies vertex edges_from_vertex;
        vertex
      | Some vertex -> vertex
    in
    vertex

  type actual_position =
    | Tail
    | Nontail

  let add_edge t ~(from : Vertex.t) ~(to_ : Vertex.t)
      ~(actual_position : actual_position)
      ~(original_position : Typedtree.position_and_tail_attribute) =
    let label : Edge.Label.t =
      let impossible_because ~case fmt =
        Misc.fatal_errorf ("case " ^^ case ^^ " impossible because " ^^ fmt)
      in
      match original_position, actual_position with
      (* CR less-tco: Clarify Unknown_position.

         Right now we use it for "fake" applications (like tuplify) which might
         makes our analysis less precise -- e.g. tuplify really is a leaf
         function, so it cannot form a cycle in the call graph. *)
      | Unknown_position, Tail -> Explicit_tail_edge
      | Unknown_position, Nontail -> Explicit_nontail_edge
      | Not_tail_position Explicit_tail, _ ->
        impossible_because
          "[@tail] not allowed on applications not in tail position"
          ~case:"Not_tail_position Explicit_tail, _"
      | Tail_position Explicit_tail, Nontail ->
        impossible_because "[@tail] was not optimized to a tailcall"
          ~case:"Tail_position Explicit_tail, Nontail"
      | Tail_position Hint_tail, Nontail ->
        impossible_because
          "[@tail hint] on application in tail position was not optimized to a \
           tailcall"
          ~case:"Tail_position Hint_tail, Nontail"
      | Tail_position Explicit_non_tail, Tail
      | Not_tail_position Explicit_non_tail, Tail ->
        impossible_because "[@nontail] was optimized to a tailcall"
          ~case:"_ Explicit_non_tail, Tail"
      | Tail_position (Explicit_tail | Hint_tail), Tail ->
        (* Requested tail *) Explicit_tail_edge
      | Tail_position Default_tail, Tail -> Inferred_tail_edge
      | Tail_position Explicit_non_tail, Nontail ->
        (* Requested nontail *) Explicit_nontail_edge
      | Tail_position Default_tail, Nontail -> Inferred_nontail_edge
      | Not_tail_position Hint_tail, Tail ->
        (* Requested tail *) Explicit_tail_edge
      | Not_tail_position Default_tail, Tail ->
        (* Became tail after optimizations. This is a conservative
           approximation. *)
        Explicit_tail_edge
      | Not_tail_position _, Nontail ->
        (* Not in tail position *) Explicit_nontail_edge
    in
    let edge : Edge.t = { from; to_; label } in
    match from with
    | Unknown_fn ->
      (* An edge already exists from Unknown_fn (added when the vertex was
         created) *)
      ()
    | Known_fn _ ->
      let edges = Vertex.Tbl.find t.adjacencies from in
      Edge.Hashset.add edges edge

  type vertex_state =
    { preorder : int;
      mutable lowlink : int
    }

  type vertex_visited =
    | Not_visited
    | Visited of vertex_state

  let decompose_tailcall_sccs t =
    (* CR less-tco: Refactor this to be more functional. *)
    let states =
      t.adjacencies |> Vertex.Tbl.to_seq_keys
      |> Seq.map (fun v -> v, Not_visited)
      |> Vertex.Tbl.of_seq
    in
    (* Invariant: A vertex is in stack iff it is in stack_set. *)
    let stack = Stack.create () and stack_set = Vertex.Hashset.create 10 in
    let push_vertex v =
      Vertex.Hashset.add stack_set v;
      Stack.push v stack
    and pop_vertex () =
      let popped = Stack.pop stack in
      Vertex.Hashset.remove stack_set popped;
      popped
    in
    let next_num =
      let num_visited = ref 0 in
      fun () ->
        let num = !num_visited in
        incr num_visited;
        num
    in
    let sccs = Stack.create () in
    (* Invariant: v must not be visited. *)
    let rec visit (vertex : Vertex.t) : vertex_state =
      let num = next_num () in
      let state = { preorder = num; lowlink = num } in
      Vertex.Tbl.replace states vertex (Visited state);
      push_vertex vertex;
      (* Recursively traverse successors and update this vertex's state
         accordingly. *)
      Edge.Hashset.iter (successors t vertex) ~f:(fun { label; to_; _ } ->
          match label with
          | Explicit_nontail_edge ->
            (* Ignore these; before less-tco they alreeady allocate stack space,
               so are unlikely to be part of some cycle that blows the stack. *)
            ()
          | Explicit_tail_edge | Inferred_tail_edge | Inferred_nontail_edge -> (
            let to_state = Vertex.Tbl.find states to_ in
            match to_state with
            | Not_visited ->
              let to_state = visit to_ in
              state.lowlink <- min state.lowlink to_state.lowlink
            | Visited to_state ->
              if Vertex.Hashset.mem stack_set to_
              then
                (* Back-edge *)
                state.lowlink <- min state.lowlink to_state.preorder));
      (* After recursively visiting all successors, if the original vertex is
         the root of the SCC in the DFS tree, pop from the stack to get all the
         vertices in the SCC. *)
      if state.preorder = state.lowlink
      then (
        let scc = Vertex.Hashset.create 10 in
        let cur = ref (pop_vertex ()) in
        while not (Vertex.equal !cur vertex) do
          Vertex.Hashset.add scc !cur;
          cur := pop_vertex ()
        done;
        (* Stopping condition: !cur = vertex *)
        Vertex.Hashset.add scc vertex;
        Stack.push scc sccs);
      state
    in
    Vertex.Tbl.iter
      (fun v _edges ->
        match Vertex.Tbl.find states v with
        | Not_visited -> ignore (visit v)
        | Visited _ -> ())
      t.adjacencies;
    sccs |> Stack.to_seq |> List.of_seq

  let possibly_overflowing_edges t ~(scc : Vertex.Hashset.t) =
    Vertex.Hashset.to_seq scc
    |> Seq.concat_map (fun v ->
           successors t v |> Edge.Hashset.to_seq
           |> Seq.filter (fun (e : Edge.t) ->
                  match e.label with
                  | Explicit_tail_edge | Explicit_nontail_edge
                  | Inferred_tail_edge ->
                    false
                  | Inferred_nontail_edge -> Vertex.Hashset.mem scc e.to_))
    |> List.of_seq

  let print_vertex_line ~indent ppf kv =
    let color = if Vertex.is_unknown kv then "red" else "black" in
    Format.fprintf ppf "%s%s [label=\"%s\" color=\"%s\" fontcolor=\"%s\"]\n"
      indent (Vertex.to_dot_id kv) (Vertex.to_dot_label kv) color color

  let hide_unknown_edges = true

  let print_edge_line ~indent ppf ({ from; to_; label } : Edge.t) =
    if Vertex.is_unknown from && hide_unknown_edges
    then ()
    else
      let color =
        match label with
        | Explicit_tail_edge -> "black"
        | Explicit_nontail_edge -> "lightgrey"
        | Inferred_tail_edge -> "blue"
        | Inferred_nontail_edge -> "red"
      in
      let style =
        let maybe_unknown_style () =
          if Vertex.is_unknown from then "dashed" else "solid"
        in
        match label with
        | Explicit_tail_edge -> maybe_unknown_style ()
        | Explicit_nontail_edge -> maybe_unknown_style ()
        | Inferred_tail_edge -> "solid"
        | Inferred_nontail_edge -> "solid"
      in
      Format.fprintf ppf "%s%s -> %s [color=\"%s\" style=\"%s\"]\n" indent
        (Vertex.to_dot_id from) (Vertex.to_dot_id to_) color style

  let print_dot ppf t ~sccs =
    Format.fprintf ppf "digraph {\n";
    Format.fprintf ppf "  rankdir=LR\n\n";
    List.iteri
      (fun idx scc ->
        let indent = "    " in
        let has_possibly_overflowing_edges =
          List.length (possibly_overflowing_edges t ~scc) > 0
        in
        Format.fprintf ppf "  subgraph cluster_%d {\n" idx;
        Format.fprintf ppf "    label=\"%d\"\n" idx;
        if has_possibly_overflowing_edges
        then (
          Format.fprintf ppf "    color=mistyrose\n";
          Format.fprintf ppf "    style=filled\n")
        else if Vertex.Hashset.length scc = 1
        then Format.fprintf ppf "    style=invis\n";
        Vertex.Hashset.iter scc ~f:(fun vtx ->
            let edges = successors t vtx in
            print_vertex_line ~indent ppf vtx;
            Edge.Hashset.iter edges ~f:(fun e ->
                (* If an edge is in a cluster, dot seems to layout the to_ node
                   within the same cluster. So only print interior SCC edges
                   here. *)
                if Vertex.Hashset.mem scc e.to_
                then print_edge_line ~indent ppf e);
            ());
        Format.fprintf ppf "  }\n";
        (* Print cross-SCC edges here. *)
        Vertex.Hashset.iter scc ~f:(fun vtx ->
            let edges = successors t vtx in
            Edge.Hashset.iter edges ~f:(fun e ->
                if not (Vertex.Hashset.mem scc e.to_)
                then print_edge_line ~indent:"  " ppf e));
        Format.fprintf ppf "\n")
      sccs;
    Format.fprintf ppf "}\n\n"
end

module Global_state = struct
  module Vertex = Graph.Vertex

  let graph : Graph.t = Graph.create ()

  let reset_unit_info () = Graph.reset graph

  let cfg cfg_with_layout =
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let from = Graph.find_or_add_vertex graph ~fn_name:(Cfg.fun_name cfg) in
    let to_ (op : Cfg.func_call_operation) : Vertex.t =
      match op with
      | Indirect -> Vertex.unknown
      | Direct { sym_name; _ } ->
        Graph.find_or_add_vertex graph ~fn_name:sym_name
    in
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        let add_edge = Graph.add_edge graph ~from in
        match block.terminator.desc with
        | Tailcall_self { original_position; _ } ->
          add_edge ~to_:from ~actual_position:Tail ~original_position
        | Tailcall_func { original_position; op } ->
          add_edge ~to_:(to_ op) ~actual_position:Tail ~original_position
        | Call { original_position; op; _ } ->
          add_edge ~to_:(to_ op) ~actual_position:Nontail ~original_position
        (* (less-tco) Handle Call_no_return and Prim? *)
        | Call_no_return _ | Prim _ | Never | Always _ | Parity_test _
        | Truth_test _ | Float_test _ | Int_test _ | Switch _ | Return | Raise _
        | Specific_can_raise _ ->
          ());
    cfg_with_layout

  let print_dot ppf =
    let sccs = Graph.decompose_tailcall_sccs graph in
    Graph.print_dot ppf graph ~sccs
end
