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

module Unit_info = struct
  module Vertex = struct
    type t =
      (* The Unknown vertex an implicit tail edge to every other vertex. *)
      | Unknown
      | Known of Label.t

    let equal v1 v2 =
      match v1, v2 with
      | Unknown, Unknown -> true
      | Known k1, Known k2 -> Label.equal k1 k2
      | Unknown, Known _ | Known _, Unknown -> false
    ;;
  end

  module Edge = struct
    type label =
      (* [@tail] *)
      | Explicit_tail
      (* No annotation. Was in tail position and was tail call optimized *)
      | Implicit_tail
      (* No annotation. Was in tail position and was NOT tail cail optimized *)
      | Implicit_nontail

    type t = Vertex.t * label
  end

  type t =
    { adj_list :
        Edge.t list Label.Tbl.t (* Only need to store the edges for the Known case. *)
    ; vertices : Label.Tbl.t
    }

  let edges (v : Vertex.t) : Edge.t list



  let create () = { callgraph = Label.Tbl.create 100 }
  let reset t = Label.Tbl.reset t.callgraph
end

let unit_info = Unit_info.create ()
let reset_unit_info () = Unit_info.reset unit_info

(* let cfg fmt ~future_funcnames = *)
