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

(* Isomorphic to bool *)
type position =
  | Doesn't_contain_indirect_call_in_tail_position (* false *)
  | Contains_indirect_call_in_tail_position (* true *)

module String = Misc.Stdlib.String

type t = position String.Map.t

let empty = String.Map.empty

let add_exn t ~fn ~pos =
  String.Map.update fn
    (fun old ->
      match old with
      | None -> Some pos
      | Some _ -> Misc.fatal_errorf "tried to add fn %s again" fn)
    t

let find t ~fn = String.Map.find_opt fn t

let print ppf t =
  t
  |> String.Map.iter (fun fn pos ->
         Format.fprintf ppf "%s: %s\n" fn
           (match pos with
           | Doesn't_contain_indirect_call_in_tail_position -> "safe"
           | Contains_indirect_call_in_tail_position -> "unsafe"))

let merge_exn t1 t2 =
  let f key _ _ = Misc.fatal_errorf "key %s exists in both maps" key in
  String.Map.union f t1 t2

module Global_state = struct
  let cached = ref empty

  let add_to_cache t = cached := merge_exn !cached t
end

module Raw = struct
  type t = (string * position) list
end

let to_raw (t : t) : Raw.t = String.Map.bindings t

let of_raw (t : Raw.t) : t = t |> List.to_seq |> String.Map.of_seq
