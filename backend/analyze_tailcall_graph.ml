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
    loc:Location.t ->
    unit

  (* Partitions vertices into hashsets where two vertices are in the same
     hashset iff they are in the same strongly-connected component.

     Explicit_nontail_edges are ignored in this SCC decomposition. *)
  val decompose_tailcall_sccs : t -> Vertex.Hashset.t list

  (* Emits warnings for vertices that have an Inferred_nontail_edge to another
     vertex in the same SCC. Taking a trip across such an edge may increase
     stack consumption, which may lead to stack overflow when traversing the
     cycle repeatedly. *)
  val warn_inferred_nontail_in_tco'd_cycle :
    t -> sccs:Vertex.Hashset.t list -> unit

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

      let compare t1 t2 = Int.compare (id t1) (id t2)

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
           whether there are any cycles with *_tail_edges and
           Inferred_nontail_edges. To find these cycles, we decompose the call
           graph into SCCs. *)
        type t =
          (* "Explicit" here really means "not inferred." "Explicit" edges are
             edges that are (very likely) not changing their TCO behavior as a
             result of adding TCO inference. I.e., before TCO inference, they
             were tail (nontail) if and only if after TCO inference they are
             tail (nontail). *)
          | Explicit_tail_edge
          | Explicit_nontail_edge
          (* Unknown_position edges are from Unknown_position applications,
             which are generated when we synthesize function applications
             internally in the compiler. These are treated the same as Explicit
             edges in the SCC decomposition and for warnings; for debugging
             purposes we display them differently in the dot output.*)
          | Unknown_position_tail_edge
          | Unknown_position_nontail_edge
          (* "Inferred" edges are edges that might possibly change their TCO
             behavior as a result of adding TCO inference. *)
          | Inferred_tail_edge
          | Inferred_nontail_edge

        let equal t1 t2 =
          match t1, t2 with
          | Explicit_tail_edge, Explicit_tail_edge -> true
          | Explicit_nontail_edge, Explicit_nontail_edge -> true
          | Unknown_position_tail_edge, Unknown_position_tail_edge -> true
          | Unknown_position_nontail_edge, Unknown_position_nontail_edge -> true
          | Inferred_tail_edge, Inferred_tail_edge -> true
          | Inferred_nontail_edge, Inferred_nontail_edge -> true
          | ( ( Explicit_tail_edge | Explicit_nontail_edge
              | Unknown_position_tail_edge | Unknown_position_nontail_edge
              | Inferred_tail_edge | Inferred_nontail_edge ),
              _ ) ->
            false

        let hash t =
          match t with
          | Explicit_tail_edge -> 0
          | Explicit_nontail_edge -> 1
          | Unknown_position_tail_edge -> 2
          | Unknown_position_nontail_edge -> 3
          | Inferred_tail_edge -> 4
          | Inferred_nontail_edge -> 5

        let compare t1 t2 = hash t1 - hash t2
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

      let compare e1 e2 =
        let c = Vertex.compare e1.from e2.from in
        if c <> 0
        then c
        else
          let c = Label.compare e1.label e2.label in
          if c <> 0 then c else Vertex.compare e1.to_ e2.to_

      let hash e1 =
        Vertex.hash e1.from + Label.hash e1.label + Vertex.hash e1.to_
    end

    include T

    module With_loc = struct
      type nonrec t =
        { edge : t;
          loc : Location.t
        }

      let compare (e1 : t) (e2 : t) =
        let c = Location.compare e1.loc e2.loc in
        if c <> 0 then c else compare e1.edge e2.edge

      let to_error_string t =
        let call_type =
          match t.edge.label with
          | Explicit_tail_edge -> "explicit tail"
          | Explicit_nontail_edge -> "explicit nontail"
          | Unknown_position_tail_edge -> "synthesized tail"
          | Unknown_position_nontail_edge -> "synthesized nontail"
          | Inferred_tail_edge -> "inferred tail"
          | Inferred_nontail_edge -> "inferred nontail"
        in
        if Vertex.is_unknown t.edge.from
        then Format.asprintf "<unknown target>"
        else
          Format.asprintf "%a: calls (in %s position)"
            Location.print_loc_in_lowercase t.loc call_type
    end

    let to_dot_tooltip (with_loc : With_loc.t) =
      let label =
        match with_loc.edge.label with
        | Explicit_tail_edge -> "explicit tail"
        | Explicit_nontail_edge -> "explicit nontail"
        | Unknown_position_tail_edge -> "unknown_pos tail"
        | Unknown_position_nontail_edge -> "unknown_pos nontail"
        | Inferred_tail_edge -> "inferred tail"
        | Inferred_nontail_edge -> "inferred nontail"
      in
      let escaped_loc : string =
        Format.asprintf "%a" Location.print_loc with_loc.loc
        |> String.split_on_char '"' |> String.concat "\\\""
      in
      Format.sprintf "%s (%s)" escaped_loc label

    module Tbl = Hashtbl.Make (T)
  end

  type t =
    { vertex_by_name : Vertex.t String.Tbl.t;
      (* This is a table from Vertex.t (a function definition) to a table of
         edges (keyed by target function and call type) to a list of locations
         (using Edge.Tbl.t as a multimap).

         Each location for an edge corresponds to a distinct callsite inside the
         function. E.g. if A calls B in tail position multiple times in
         different locations, we will record one location for each call. We need
         to treat each call as distinct when emitting warnings. *)
      adjacencies : Location.t Edge.Tbl.t Vertex.Tbl.t
    }

  let init_unknown_edges t =
    Vertex.Tbl.replace t.adjacencies Vertex.unknown (Edge.Tbl.create 100)

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

  let successors_mod_loc t (v : Vertex.t) : Edge.t Seq.t =
    Vertex.Tbl.find t.adjacencies v |> Edge.Tbl.to_seq_keys

  (* Prefer sorting lexiographically by location so that warnings are emitted in
     source order, and also so that the dot order is deterministic. *)
  let successors t (v : Vertex.t) : Edge.With_loc.t list =
    Vertex.Tbl.find t.adjacencies v
    |> Edge.Tbl.to_seq
    |> Seq.map (fun (e, loc) : Edge.With_loc.t -> { edge = e; loc })
    |> List.of_seq
    |> List.sort Edge.With_loc.compare

  let find_or_add_vertex t ~(fn_name : string) : Vertex.t =
    let vertex =
      match String.Tbl.find_opt t.vertex_by_name fn_name with
      | None ->
        let id = String.Tbl.length t.vertex_by_name in
        let vertex : Vertex.t = Known_fn { id; name = fn_name } in
        String.Tbl.add t.vertex_by_name fn_name vertex;
        (* Add edge from unknown *)
        let unknown = Vertex.unknown in
        let edge_from_unknown : Edge.t =
          { from = unknown; to_ = vertex; label = Explicit_tail_edge }
        in
        let edges_from_unknown = Vertex.Tbl.find t.adjacencies unknown in
        Edge.Tbl.add edges_from_unknown edge_from_unknown Location.none;
        (* Initialize vertex's adjacency set. The adjacency list is already
           initialized because we use Vertex.Tbl as a multimap.*)
        let edges_from_vertex = Edge.Tbl.create 10 in
        Vertex.Tbl.add t.adjacencies vertex edges_from_vertex;
        vertex
      | Some vertex -> vertex
    in
    vertex

  type actual_position =
    | Tail
    | Nontail

  let add_edge t ~(from : Vertex.t) ~(to_ : Vertex.t)
      ~(actual_position : actual_position)
      ~(original_position : Typedtree.position_and_tail_attribute)
      ~(loc : Location.t) =
    let label : Edge.Label.t =
      let impossible_because ~case fmt =
        Misc.fatal_errorf
          ("case " ^^ case ^^ " impossible because " ^^ fmt ^^ "; %a")
          Location.print_loc loc
      in
      match original_position, actual_position with
      (* CR less-tco: Clarify Unknown_position.

         Right now we use it for "fake" applications (like tuplify) which might
         makes our analysis less precise -- e.g. tuplify really is a leaf
         function, so it cannot form a cycle in the call graph. *)
      | Unknown_position, Tail -> Unknown_position_tail_edge
      | Unknown_position, Nontail -> Unknown_position_nontail_edge
      | Inlined_into_not_tail_position _, Tail ->
        impossible_because "if this happens we should know about it"
          ~case:"Inlined_into_not_tail_position _, Tail"
      | Inlined_into_not_tail_position _, Nontail -> Explicit_nontail_edge
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
      (* `from`'s adjacency list was initialized in `find_or_add_vertex` *)
      let edges = Vertex.Tbl.find t.adjacencies from in
      Edge.Tbl.add edges edge loc

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
      (* Recursively traverse successors_mod_loc and update this vertex's state
         accordingly. *)
      Seq.iter
        (fun ({ label; to_; _ } : Edge.t) ->
          match label with
          | Explicit_nontail_edge | Unknown_position_nontail_edge ->
            (* Ignore these; before less-tco they already allocate stack space,
               so are unlikely to be part of some cycle that blows the stack. *)
            ()
          | Explicit_tail_edge | Unknown_position_tail_edge | Inferred_tail_edge
          | Inferred_nontail_edge -> (
            let to_state = Vertex.Tbl.find states to_ in
            match to_state with
            | Not_visited ->
              let to_state = visit to_ in
              state.lowlink <- min state.lowlink to_state.lowlink
            | Visited to_state ->
              if Vertex.Hashset.mem stack_set to_
              then
                (* Back-edge *)
                state.lowlink <- min state.lowlink to_state.preorder))
        (successors_mod_loc t vertex);
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
      (fun v (_ : Location.t Edge.Tbl.t) ->
        match Vertex.Tbl.find states v with
        | Not_visited -> ignore (visit v)
        | Visited _ -> ())
      t.adjacencies;
    sccs |> Stack.to_seq |> List.of_seq

  let possibly_newly_overflowing_edges t ~(scc : Vertex.Hashset.t) =
    Vertex.Hashset.to_seq scc |> List.of_seq
    |> List.concat_map (fun v ->
           successors t v
           |> List.filter (fun (e : Edge.With_loc.t) ->
                  match e.edge.label with
                  | Explicit_tail_edge | Explicit_nontail_edge
                  | Unknown_position_tail_edge | Unknown_position_nontail_edge
                  | Inferred_tail_edge ->
                    false
                  | Inferred_nontail_edge -> Vertex.Hashset.mem scc e.edge.to_))
    |> List.sort Edge.With_loc.compare

  module Cycle_witness = struct
    (* Finds a witness/proof of a tco'd cycle containing a specific inferred
       non-tail edge. The proof prioritizes definite cycles (i.e. cycles that do
       not involve Unknown_fn), then cycles in increasing order of their length.
       We find a proof using Dijkstra's and an appropriate priority key. *)

    module Tendril : sig
      module Priority : sig
        (* Priority is a sort key for Dijkstra's algorithm. Because we order
           Doesn't_use_unknown_edge's before Uses_unknown_edge's, the algorithm
           is essentially BFS in two phases: the first phase searches through
           all paths that do not traverse through the unknown vertex in length
           order, saving those paths for the second phase.

           We could use a normal queue for the first phase, but we do need to
           use a priority queue for the second phase. For example, suppose at
           the end of the first phase, the priority queue looks like [(A, 1),
           (B, 4)] where all paths go through unknown. We pop A, and find one
           successor (C, 2). But now pushing (C, 2) at the end of the normal
           queue would break the sorted queue invariant. *)
        type phase =
          | Doesn't_use_unknown_edge
          | Uses_unknown_edge

        type t

        val phase : t -> phase

        module Pqueue : Regalloc_gi_utils.Priority_queue with type priority = t
      end

      type t

      val priority : t -> Priority.t

      val edges : t -> Edge.With_loc.t list

      val tip : t -> Vertex.t

      val singleton : Edge.With_loc.t -> t

      val cons : Edge.With_loc.t -> t -> t
    end = struct
      module Priority = struct
        module T = struct
          type phase =
            | Doesn't_use_unknown_edge
            | Uses_unknown_edge

          (* The invariants in this type are maintained by Tendril.singleton and
             Tendril.cons. *)
          type t =
            { (* Invariant: `t.phase = Uses_unknown_edge` iff for some `e` in
                 `edges` this priority is associated with, `e.from =
                 Unknown_fn`. *)
              phase : phase;
              (* Invariant: `t.length = List.length edges` *)
              length : int
            }

          let phase t = t.phase

          (* Compare candidate paths lexiographically by (phase, length). Order
             paths that don't use an unknown edge earlier. *)
          let compare fst snd =
            let phase_to_ord = function
              | Doesn't_use_unknown_edge -> 0
              | Uses_unknown_edge -> 1
            in
            let c =
              Int.compare (phase_to_ord fst.phase) (phase_to_ord snd.phase)
            in
            if c <> 0 then c else Int.compare fst.length snd.length
        end

        include T

        module Pqueue = Regalloc_gi_utils.Make_min_priority_queue (struct
          include T

          let to_string _ = "dummy unused"
        end)
      end

      type t =
        { priority : Priority.t;
          edges : Edge.With_loc.t list (* Nonempty list *)
        }

      let priority (t : t) = t.priority

      let edges (t : t) = t.edges

      let tip (t : t) = (List.hd t.edges).edge.to_

      let singleton (with_loc : Edge.With_loc.t) : t =
        let phase : Priority.phase =
          match with_loc.edge.from with
          | Unknown_fn -> Uses_unknown_edge
          | Known_fn _ -> Doesn't_use_unknown_edge
        in
        { priority = { phase; length = 1 }; edges = [with_loc] }

      let cons (with_loc : Edge.With_loc.t) ({ priority; edges } : t) : t =
        let phase : Priority.phase =
          match with_loc.edge.from with
          | Unknown_fn -> Uses_unknown_edge
          | Known_fn _ -> priority.phase
        in
        { priority = { phase; length = priority.length + 1 };
          edges = with_loc :: edges
        }
    end

    (* Gas is decremented once per edge examined. *)
    let find_cycle_with_edge graph ~(with_loc : Edge.With_loc.t) ~gas :
        (Edge.With_loc.t list * bool) option * int =
      let module Priority = Tendril.Priority in
      let visited = Vertex.Hashset.create 100 in
      let enqueue, dequeue =
        let q : Tendril.t Priority.Pqueue.t =
          Priority.Pqueue.make ~initial_capacity:100
        in
        let enqueue tendril =
          Priority.Pqueue.add q ~priority:(Tendril.priority tendril)
            ~data:tendril
        in
        let dequeue () =
          if Priority.Pqueue.is_empty q
          then None
          else
            let elt = Priority.Pqueue.get_and_remove q in
            Some (elt.priority, elt.data)
        in
        enqueue, dequeue
      in
      let source = with_loc.edge.from in
      Vertex.Hashset.add visited source;
      enqueue (Tendril.singleton with_loc);
      let rec loop gas =
        if gas = 0
        then None, gas
        else
          match dequeue () with
          | None -> None, gas
          | Some (priority, cur) ->
            let tip = Tendril.tip cur in
            if Vertex.equal tip source
            then
              let uses_unknown =
                match Priority.phase priority with
                | Doesn't_use_unknown_edge -> false
                | Uses_unknown_edge -> true
              in
              Some (List.rev (Tendril.edges cur), uses_unknown), gas
            else if Vertex.Hashset.mem visited tip
            then loop gas
            else (
              Vertex.Hashset.add visited tip;
              let gas =
                successors graph tip
                |> List.fold_left
                     (fun gas succ ->
                       enqueue (Tendril.cons succ cur);
                       gas - 1)
                     gas
              in
              loop gas)
      in
      loop gas
  end

  let warn_inferred_nontail_in_tco'd_cycle t ~sccs =
    let gas = 100_000 in
    let gas =
      sccs
      |> List.concat_map (fun scc -> possibly_newly_overflowing_edges t ~scc)
      |> List.sort Edge.With_loc.compare
      |> List.fold_left
           (fun gas (e : Edge.With_loc.t) ->
             let cycle, gas =
               if gas = 0
               then None, 0
               else Cycle_witness.find_cycle_with_edge t ~with_loc:e ~gas
             in
             let to_warning_info (cycle, uses_unknown) =
               let cycle = cycle |> List.map Edge.With_loc.to_error_string in
               cycle, uses_unknown
             in
             let warning_info = cycle |> Option.map to_warning_info in
             Location.prerr_warning e.loc
               (Warnings.Inferred_nontail_in_tcod_cycle warning_info);
             gas)
           gas
    in
    ignore (gas : int)

  let print_vertex_line ~indent ppf kv =
    let color = if Vertex.is_unknown kv then "red" else "black" in
    Format.fprintf ppf "%s%s [label=\"%s\" color=\"%s\" fontcolor=\"%s\"]\n"
      indent (Vertex.to_dot_id kv) (Vertex.to_dot_label kv) color color

  let hide_edges_from_unknown = true

  let hide_ignored_nontail_edges = true

  let print_edge_line ~indent ppf (with_loc : Edge.With_loc.t) =
    let Edge.{ from; to_; label } = with_loc.edge in
    let is_ignored_nontail_edge =
      match label with
      | Explicit_nontail_edge | Unknown_position_nontail_edge -> true
      | Explicit_tail_edge | Unknown_position_tail_edge | Inferred_tail_edge
      | Inferred_nontail_edge ->
        false
    in
    if (hide_edges_from_unknown && Vertex.is_unknown from)
       || (hide_ignored_nontail_edges && is_ignored_nontail_edge)
    then ()
    else
      let color =
        match label with
        | Explicit_tail_edge | Unknown_position_tail_edge -> "black"
        | Explicit_nontail_edge | Unknown_position_nontail_edge -> "lightgrey"
        | Inferred_tail_edge -> "blue"
        | Inferred_nontail_edge -> "red"
      in
      let style =
        let maybe_unknown_style () =
          if Vertex.is_unknown from then "dotted" else "solid"
        in
        match label with
        | Explicit_tail_edge -> maybe_unknown_style ()
        | Explicit_nontail_edge -> maybe_unknown_style ()
        | Unknown_position_tail_edge -> "dashed"
        | Unknown_position_nontail_edge -> "dashed"
        | Inferred_tail_edge -> "solid"
        | Inferred_nontail_edge -> "solid"
      in
      Format.fprintf ppf "%s%s -> %s [color=\"%s\" style=\"%s\" label=\"%s\"]\n"
        indent (Vertex.to_dot_id from) (Vertex.to_dot_id to_) color style
        (Edge.to_dot_tooltip with_loc)

  let print_dot ppf t ~sccs =
    Format.fprintf ppf "digraph {\n";
    Format.fprintf ppf "  rankdir=LR\n\n";
    List.iteri
      (fun idx scc ->
        let indent = "    " in
        let has_possibly_overflowing_edges =
          List.length (possibly_newly_overflowing_edges t ~scc) > 0
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
            List.iter
              (fun (with_loc : Edge.With_loc.t) ->
                (* If an edge is in a cluster, dot seems to layout the to_ node
                   within the same cluster. So only print interior SCC edges
                   here. *)
                if Vertex.Hashset.mem scc with_loc.edge.to_
                then print_edge_line ~indent ppf with_loc)
              edges;
            ());
        Format.fprintf ppf "  }\n";
        (* Print cross-SCC edges here. *)
        Vertex.Hashset.iter scc ~f:(fun vtx ->
            let edges = successors t vtx in
            List.iter
              (fun (with_loc : Edge.With_loc.t) ->
                if not (Vertex.Hashset.mem scc with_loc.edge.to_)
                then print_edge_line ~indent:"  " ppf with_loc)
              edges);
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
        let term = block.terminator in
        let loc = Debuginfo.to_location term.dbg in
        let add_edge = Graph.add_edge graph ~from in
        match term.desc with
        | Tailcall_self { original_position; _ } ->
          let to_ = from in
          add_edge ~to_ ~actual_position:Tail ~original_position ~loc
        | Tailcall_func { original_position; op } ->
          let to_ = to_ op in
          add_edge ~to_ ~actual_position:Tail ~original_position ~loc
        | Call { original_position; op; _ } ->
          let to_ = to_ op in
          add_edge ~to_ ~actual_position:Nontail ~original_position ~loc
        (* (less-tco) Handle Call_no_return and Prim? *)
        | Call_no_return _ | Prim _ | Never | Always _ | Parity_test _
        | Truth_test _ | Float_test _ | Int_test _ | Switch _ | Return | Raise _
        | Specific_can_raise _ ->
          ());
    cfg_with_layout

  let print_dot ppf =
    let sccs = Graph.decompose_tailcall_sccs graph in
    Graph.print_dot ppf graph ~sccs

  let emit_warnings () =
    let sccs = Graph.decompose_tailcall_sccs graph in
    Graph.warn_inferred_nontail_in_tco'd_cycle graph ~sccs
end

let fixup_inlined_tailcalls (fundecl : Cmm.fundecl) =
  let rec fix (expr : Cmm.expression) ~(tail_pos_ctx : bool) : Cmm.expression =
    let possibly_tail = fix ~tail_pos_ctx in
    let nontail = fix ~tail_pos_ctx:false in
    match expr with
    | Cop (op, exprs, dbg) ->
      let op : Cmm.operation =
        (* Warning 4 is [fragile-match] *)
        match[@ocaml.warning "-4"] op with
        | Capply (machtype, region_close, original_position) ->
          let inlined_position : Lambda.position_and_tail_attribute =
            match original_position with
            | Tail_position _ when not tail_pos_ctx ->
              Inlined_into_not_tail_position { original_position }
            | _ -> original_position
          in
          Capply (machtype, region_close, inlined_position)
        | _ -> op
      in
      let exprs = List.map nontail exprs in
      Cop (op, exprs, dbg)
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_vec128 _ | Cconst_symbol _ | Cvar _ ->
      expr
    | Clet (var, defn, body) -> Clet (var, nontail defn, possibly_tail body)
    | Clet_mut (var, machtype, defn, body) ->
      Clet_mut (var, machtype, nontail defn, possibly_tail body)
    | Cphantom_let (var, phantom_defn, body) ->
      Cphantom_let (var, phantom_defn, possibly_tail body)
    | Cassign (var, defn) -> Cassign (var, nontail defn)
    | Ctuple exprs -> Ctuple (List.map nontail exprs)
    | Csequence (first, second) ->
      Csequence (nontail first, possibly_tail second)
    | Cifthenelse (cond, cond_dbg, ifso, ifso_dbg, ifnot, ifnot_dbg, kind) ->
      Cifthenelse
        ( nontail cond,
          cond_dbg,
          possibly_tail ifso,
          ifso_dbg,
          possibly_tail ifnot,
          ifnot_dbg,
          kind )
    | Cswitch (expr, table, cases, dbg', kind) ->
      Cswitch
        ( expr,
          table,
          Array.map (fun (e, dbg) -> possibly_tail e, dbg) cases,
          dbg',
          kind )
    | Ccatch (rec_flag, handlers, body, kind) ->
      let handlers =
        List.map
          (fun (n, ids, handler, dbg, is_cold) ->
            n, ids, possibly_tail handler, dbg, is_cold)
          handlers
      in
      Ccatch (rec_flag, handlers, possibly_tail body, kind)
    | Ctrywith (e1, label, id, e2, dbg, kind) ->
      Ctrywith (possibly_tail e1, label, id, possibly_tail e2, dbg, kind)
    | Cexit (label, args, traps) -> Cexit (label, List.map nontail args, traps)
  in
  { fundecl with fun_body = fix fundecl.fun_body ~tail_pos_ctx:true }
