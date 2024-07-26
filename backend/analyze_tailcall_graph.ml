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

    val unknown : t
    val to_dot_id : t -> string
    val to_dot_label : t -> string
  end

  module Edge_head : sig
    type label =
      | Unknown_edge
      (* [@tail] *)
      | Always_tail_edge
      | Inferred_tail_edge
      | Inferred_nontail_edge

    type t =
      { to_ : Vertex.t
      ; label : label
      }
  end

  type t

  val create : unit -> t
  val reset : t -> unit
  val successors : t -> Vertex.t -> Edge_head.t Seq.t
  val find_vertex : t -> [ `Known_fn of string ] -> Vertex.t

  val add_edge
    :  t
    -> from:Vertex.t
    -> to_:Vertex.t
    -> actual_position:[ `Tail | `Nontail ]
    -> original_position:Typedtree.position_and_tail_attribute
    -> unit
end = struct
  module Vertex = struct
    type t =
      (* The Unknown_fn vertex an implicit tail edge to every other vertex. *)
      | Unknown_fn
      | Known_fn of
          { id : int
          ; name : string
          }

    let unknown = Unknown_fn

    let to_dot_id t =
      match t with
      | Unknown_fn -> "0"
      | Known_fn { id; _ } -> Int.to_string id
    ;;

    let to_dot_label t =
      match t with
      | Unknown_fn -> "<unknown>"
      | Known_fn { name; _ } -> name
    ;;
  end

  module Edge_head = struct
    type label =
      | Unknown_edge
      | Always_tail_edge
      | Inferred_tail_edge
      | Inferred_nontail_edge

    type t =
      { to_ : Vertex.t
      ; label : label
      }
  end

  type t =
    { vertex_by_name : Vertex.t String.Tbl.t
    ; (* Only need to store the edges / vertices for the Known_fn case. *)
      adjacencies : Edge_head.t list Int.Tbl.t
    }

  let create () =
    { vertex_by_name = String.Tbl.create 100; adjacencies = Int.Tbl.create 100 }
  ;;

  let reset t =
    String.Tbl.reset t.vertex_by_name;
    Int.Tbl.reset t.adjacencies
  ;;

  let successors t (v : Vertex.t) : Edge_head.t Seq.t =
    match v with
    | Unknown_fn ->
      String.Tbl.to_seq_values t.vertex_by_name
      |> Seq.map (fun vtx : Edge_head.t -> { to_ = vtx; label = Unknown_edge })
      |> Seq.cons ({ to_ = Unknown_fn; label = Unknown_edge } : Edge_head.t)
    | Known_fn { id; _ } -> id |> Int.Tbl.find t.adjacencies |> List.to_seq
  ;;

  let find_vertex t (fn : [ `Known_fn of string ]) : Vertex.t =
    let (`Known_fn name) = fn in
    match String.Tbl.find_opt t.vertex_by_name name with
    | None ->
      let id = String.Tbl.length t.vertex_by_name in
      let v : Vertex.t = Known_fn { id; name } in
      String.Tbl.add t.vertex_by_name name v;
      v
    | Some v -> v
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
      | Unknown_position, _ -> `Unknown_edge
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
      | Not_tail_position attr, `Nontail -> `Not_in_tail_position
      | Tail_position (Explicit_tail | Hint_tail), `Tail
      | Not_tail_position Hint_tail, `Tail -> `Explicit_tail
      | Tail_position Default_tail, `Tail -> `Inferred_tail
      | Not_tail_position Default_tail, `Tail -> `Became_tail_after_optimizations
      | Tail_position Explicit_non_tail, `Nontail -> `Requested_nontail
      | Tail_position Default_tail, `Nontail -> `Inferred_nontail
    in
    let label : Edge_head.label option =
      match label with
      | `Not_in_tail_position | `Requested_nontail -> None
      | `Unknown_edge -> Some Unknown_edge
      | `Explicit_tail -> Some Always_tail_edge
      | `Became_tail_after_optimizations ->
        (* This is a conservative approximation *) Some Always_tail_edge
      | `Inferred_tail -> Some Inferred_tail_edge
      | `Inferred_nontail -> Some Inferred_nontail_edge
    in
    match label with
    | None -> ()
    | Some label ->
      let edge : Edge_head.t = { to_; label } in
      (match from with
       | Unknown_fn -> ()
       | Known_fn { id = from; _ } ->
         (match Int.Tbl.find_opt t.adjacencies from with
          | None -> Int.Tbl.add t.adjacencies from [ edge ]
          | Some lst -> Int.Tbl.replace t.adjacencies from (edge :: lst)))
  ;;
end

module Global = struct
  module Vertex = Graph.Vertex
  module Edge_head = Graph.Edge_head

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
    let from = Graph.find_vertex graph (`Known_fn (Cfg.fun_name cfg)) in
    let to_ (op : Cfg.func_call_operation) : Vertex.t =
      match op with
      | Indirect -> Vertex.unknown
      | Direct { sym_name; _ } -> Graph.find_vertex graph (`Known_fn sym_name)
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

  let replace ~c ~with_ str = String.split_on_char c str |> String.concat with_

  let print_vertex ppf kv =
    Format.fprintf ppf "%s [label=\"%s\"]" (Vertex.to_dot_id kv) (Vertex.to_dot_label kv)
  ;;

  let print_edge ppf ((kv, { to_; label }) : Vertex.t * Edge_head.t) =
    let color =
      match label with
      | Unknown_edge -> "black"
      | Always_tail_edge -> "black"
      | Inferred_tail_edge -> "blue"
      | Inferred_nontail_edge -> "red"
    in
    let style =
      match label with
      | Unknown_edge -> "dotted"
      | Always_tail_edge -> "solid"
      | Inferred_tail_edge -> "solid"
      | Inferred_nontail_edge -> "solid"
    in
    Format.fprintf
      ppf
      "%s -> %s [color=\"%s\" style=\"%s\"]"
      (Vertex.to_dot_id kv)
      (Vertex.to_dot_id to_)
      color
      style
  ;;

  let print_dot ppf =
    match !graph with
    | None -> ()
    | Some g ->
      Format.fprintf ppf "digraph {\n";
      String.Tbl.iter
        (fun kv edges ->
          let kv : Vertex.t = Known_fn kv in
          Format.fprintf ppf "  %a\n" print_vertex kv;
          List.iter (fun e -> Format.fprintf ppf "  %a\n" print_edge (kv, e)) edges;
          ())
        g.adjacencies;
      Format.fprintf ppf "}\n\n"
  ;;
end
