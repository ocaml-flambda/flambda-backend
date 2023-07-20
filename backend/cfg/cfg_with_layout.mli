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

type layout = Label.t Flambda_backend_utils.Doubly_linked_list.t

val create :
  Cfg.t ->
  layout:layout ->
  preserve_orig_labels:bool ->
  new_labels:Label.Set.t ->
  t

val cfg : t -> Cfg.t

val layout : t -> layout

val preserve_orig_labels : t -> bool

val new_labels : t -> Label.Set.t

val set_layout : t -> layout -> unit

(** Add to cfg, layout, and other data-structures that track labels. *)
val add_block : t -> Cfg.basic_block -> after:Label.t -> unit

val assign_blocks_to_section : t -> Label.t list -> string -> unit

val get_section : t -> Label.t -> string option

(** Remove from cfg, layout, and other data-structures that track labels. *)
val remove_block : t -> Label.t -> unit

val remove_blocks : t -> Label.Set.t -> unit

val is_trap_handler : t -> Label.t -> bool

val save_as_dot :
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_instr:
    (Format.formatter ->
    [ `Basic of Cfg.basic Cfg.instruction
    | `Terminator of Cfg.terminator Cfg.instruction ] ->
    unit)
    list ->
  ?annotate_block:(int -> string) ->
  ?annotate_block_end:(Format.formatter -> Cfg.basic_block -> unit) ->
  ?annotate_succ:(int -> int -> string) ->
  ?filename:string ->
  t ->
  string ->
  unit

val print_dot :
  ?show_instr:bool ->
  ?show_exn:bool ->
  ?annotate_instr:
    (Format.formatter ->
    [ `Basic of Cfg.basic Cfg.instruction
    | `Terminator of Cfg.terminator Cfg.instruction ] ->
    unit)
    list ->
  ?annotate_block:(Label.t -> string) ->
  ?annotate_block_end:(Format.formatter -> Cfg.basic_block -> unit) ->
  ?annotate_succ:(Label.t -> Label.t -> string) ->
  Format.formatter ->
  t ->
  unit

val dump : Format.formatter -> t -> msg:string -> unit

(** Change layout: randomly reorder the blocks, keeping the entry block first.
    This function is intended for testing and enabled by compiler flag
    "-reorder-blocks-random".

    Side-effects [random_state] by repeated calls to [Random.State.int] and
    [Random.State.bool]. *)
val reorder_blocks_random : ?random_state:Random.State.t -> t -> unit

val reorder_blocks : comparator:(Label.t -> Label.t -> int) -> t -> unit

val iter_instructions :
  t ->
  instruction:(Cfg.basic Cfg.instruction -> unit) ->
  terminator:(Cfg.terminator Cfg.instruction -> unit) ->
  unit

val fold_instructions :
  t ->
  instruction:('a -> Cfg.basic Cfg.instruction -> 'a) ->
  terminator:('a -> Cfg.terminator Cfg.instruction -> 'a) ->
  init:'a ->
  'a
