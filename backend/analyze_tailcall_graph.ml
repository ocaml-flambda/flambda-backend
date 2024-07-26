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

module Graph : sig
  module Vertex : sig
    type t

    val id : t -> int
    val unknown : t
    val is_unknown : t -> bool
    val to_dot_id : t -> string
    val to_dot_label : t -> string

    module Tbl : Hashtbl.S with type key = t
  end

  module Edge : sig
    (* For the purpose of analyzing whether TCO inference might break existing
       code (by causing a stack overflow) we are interested in whether there
       are any cycles with (Firm_tail_edges _) and Inferred_nontail_edges.
       To find these cycles, we plan on finding SCC's for the subgraph
       consistening of just edges with those labels. *)
    type label =
      (* "Firm" edges are edges that are (very likely) not changing their
         TCO behavior as a result of adding TCO inference. I.e., before TCO
         inference, they were tail (nontail) if and only if after TCO
         inference they are tail (nontail). *)
      | Firm_tail_edge of { unknown_caller : bool }
      | Firm_nontail_edge of { unknown_caller : bool }
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
  end

  type t

  val create : unit -> t
  val reset : t -> unit
  val successors : t -> Vertex.t -> unit Edge.Tbl.t
  val find_or_add_vertex : t -> [ `Known_fn of string ] -> Vertex.t

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
        (* The Unknown_fn vertex an implicit tail edge to every other vertex. *)
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
  end

  module Edge = struct
    module T = struct
      type label =
        | Firm_tail_edge of { unknown_caller : bool }
        | Firm_nontail_edge of { unknown_caller : bool }
        | Inferred_tail_edge
        | Inferred_nontail_edge

      type t =
        { from : Vertex.t
        ; to_ : Vertex.t
        ; label : label
        }

      let label_equal l1 l2 =
        match l1, l2 with
        | Firm_tail_edge { unknown_caller = u1 }, Firm_tail_edge { unknown_caller = u2 }
          -> u1 = u2
        | ( Firm_nontail_edge { unknown_caller = u1 }
          , Firm_nontail_edge { unknown_caller = u2 } ) -> u1 = u2
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
  end

  type t =
    { vertex_by_name : Vertex.t String.Tbl.t
    ; (* Only need to store the edges / vertices for the Known_fn case. *)
      adjacencies : unit Edge.Tbl.t Vertex.Tbl.t
    }

  (* This is nice so we don't have to check for the empty case when adding an edge from
     unknown in find_or_add_vertex. *)
  let add_unknown_self_loop t =
    let edge : Edge.t =
      { from = Vertex.unknown
      ; to_ = Vertex.unknown
      ; label = Firm_tail_edge { unknown_caller = true }
      }
    in
    Vertex.Tbl.replace
      t.adjacencies
      Vertex.unknown
      ([ edge, () ] |> List.to_seq |> Edge.Tbl.of_seq)
  ;;

  let create () =
    let t =
      { vertex_by_name = String.Tbl.create 100; adjacencies = Vertex.Tbl.create 100 }
    in
    add_unknown_self_loop t;
    t
  ;;

  let reset t =
    String.Tbl.reset t.vertex_by_name;
    Vertex.Tbl.reset t.adjacencies;
    add_unknown_self_loop t
  ;;

  let successors t : Vertex.t -> unit Edge.Tbl.t = Vertex.Tbl.find t.adjacencies

  let find_or_add_vertex t (fn : [ `Known_fn of string ]) : Vertex.t =
    let (`Known_fn name) = fn in
    let vertex =
      match String.Tbl.find_opt t.vertex_by_name name with
      | None ->
        let id = String.Tbl.length t.vertex_by_name in
        let v : Vertex.t = Known_fn { id; name } in
        String.Tbl.replace t.vertex_by_name name v;
        v
      | Some v -> v
    in
    let unknown = Vertex.unknown in
    let edge : Edge.t =
      { from = unknown; to_ = vertex; label = Firm_tail_edge { unknown_caller = true } }
    in
    let edges = Vertex.Tbl.find t.adjacencies unknown in
    Edge.Tbl.replace edges edge ();
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
        Misc.fatal_errorf ("case " ^^ " impossible because " ^^ fmt)
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
      | `Unknown_tail -> Firm_tail_edge { unknown_caller = true }
      | `Unknown_nontail -> Firm_nontail_edge { unknown_caller = true }
      | `Not_in_tail_position -> Firm_nontail_edge { unknown_caller = false }
      | `Requested_nontail -> Firm_nontail_edge { unknown_caller = false }
      | `Requested_tail -> Firm_tail_edge { unknown_caller = false }
      | `Became_tail_after_optimizations ->
        (* This is a conservative approximation *)
        Firm_tail_edge { unknown_caller = false }
      | `Inferred_tail -> Inferred_tail_edge
      | `Inferred_nontail -> Inferred_nontail_edge
    in
    let edge : Edge.t = { from; to_; label } in
    match from with
    | Unknown_fn -> (* An edge already exists from Unknown_fn to every other vertex *) ()
    | Known_fn _ ->
      let edges =
        match Vertex.Tbl.find_opt t.adjacencies from with
        | None ->
          let edges = Edge.Tbl.create 10 in
          Vertex.Tbl.replace t.adjacencies from edges;
          edges
        | Some edges -> edges
      in
      Edge.Tbl.replace edges edge ()
  ;;

  let replace ~c ~with_ str = String.split_on_char c str |> String.concat with_

  let print_vertex ppf kv =
    Format.fprintf ppf "%s [label=\"%s\"]" (Vertex.to_dot_id kv) (Vertex.to_dot_label kv)
  ;;

  let print_edge ppf ({ from; to_; label } : Edge.t) =
    let color =
      match label with
      | Firm_tail_edge _ -> "black"
      | Firm_nontail_edge _ -> "lightgrey"
      | Inferred_tail_edge -> "blue"
      | Inferred_nontail_edge -> "red"
    in
    let style =
      let unknown_style unknown_caller = if unknown_caller then "dashed" else "solid" in
      match label with
      | Firm_tail_edge { unknown_caller } -> unknown_style unknown_caller
      | Firm_nontail_edge { unknown_caller } -> unknown_style unknown_caller
      | Inferred_tail_edge -> "solid"
      | Inferred_nontail_edge -> "solid"
    in
    Format.fprintf
      ppf
      "%s -> %s [color=\"%s\" style=\"%s\"]"
      (Vertex.to_dot_id from)
      (Vertex.to_dot_id to_)
      color
      style
  ;;

  let print_dot t ppf =
    Format.fprintf ppf "digraph {\n";
    Vertex.Tbl.iter
      (fun vtx edges ->
        if not Vertex.(vtx = unknown) then Format.fprintf ppf "  %a\n" print_vertex vtx;
        Edge.Tbl.iter (fun e () -> Format.fprintf ppf "  %a\n" print_edge e) edges;
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
    let from = Graph.find_or_add_vertex graph (`Known_fn (Cfg.fun_name cfg)) in
    let to_ (op : Cfg.func_call_operation) : Vertex.t =
      match op with
      | Indirect -> Vertex.unknown
      | Direct { sym_name; _ } -> Graph.find_or_add_vertex graph (`Known_fn sym_name)
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
