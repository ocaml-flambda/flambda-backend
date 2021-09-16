(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
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

type t

val create :
  Cfg.t ->
  layout:Label.t list ->
  preserve_orig_labels:bool ->
  new_labels:Label.Set.t ->
  t

val cfg : t -> Cfg.t

val layout : t -> Label.t list

val preserve_orig_labels : t -> bool

val new_labels : t -> Label.Set.t

val set_layout : t -> Label.t list -> unit

(** Remove from cfg, layout, and other data-structures that track labels. *)
val remove_block : t -> Label.t -> unit

val is_trap_handler : t -> Label.t -> bool

val save_as_dot :
  t ->
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_block:(Label.t -> string) ->
  ?annotate_succ:(Label.t -> Label.t -> string) ->
  string ->
  unit

val print : t -> out_channel -> string -> unit

val dump : Format.formatter -> t -> msg:string -> unit
