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

  module Edge = struct
    type label =
      | Unknown_fn
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
      adjacencies : Edge.t list String.Tbl.t
    }

  let create () = { adjacencies = String.Tbl.create 100 }
  let reset t = String.Tbl.reset t.adjacencies

  let successors t (v : Vertex.t) : Edge.t Seq.t =
    match v with
    | Unknown_fn ->
      String.Tbl.to_seq_keys t.adjacencies
      |> Seq.map (fun fun_name : Edge.t ->
        { to_ = Known_fn fun_name; label = Unknown_fn })
      |> Seq.cons ({ to_ = Unknown_fn; label = Unknown_fn } : Edge.t)
    | Known_fn fun_name -> String.Tbl.find t.adjacencies fun_name |> List.to_seq
  ;;

  let add_edge
    t
    ~(from : string)
    ~(to_ : Vertex.t)
    ~(apos : [ `Tail | `Nontail ])
    ~(opos : Typedtree.position_and_tail_attribute)
    =
    let label : Edge.label option =
      let fail () = failwith "case should be impossible" in
      match apos, opos with
      | _, Unknown_position -> Some Unknown_fn
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
      let edge : Edge.t = { to_; label } in
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
      | Direct { sym_name; _ } -> Known_fn from
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

  let print_dot ppf =
    match !graph with
    | None -> ()
    | Some g -> Format.pp_print_string ppf "Graph = Some"
  ;;
end
