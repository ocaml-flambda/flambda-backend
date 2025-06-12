(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

(** Outcome for user-defined actions. *)
type outcome =
  | Accept  (** Accept the currently selected binding. *)
  | Skip
      (** Skip the currently selected binding, advancing to the next binding at
          the same level. *)

module Make (Iterator : sig
  include Leapfrog.Iterator

  include Heterogenous_list.S with type 'a t := 'a t
end) : sig
  (** Implementation of a virtual machine for iterating over a nested sequence
      of iterators.

      Given a list [I1, I2, ..., In] of column iterators (i.e. iterators that
      produce a single value at a time), produce tuples [v1, v2, ..., vn] where
      each [vi] is produced by [Ii] in lexicographic order.

      The virtual machine maintains a stack of currently active iterators. The
      stack initially starts empty, and iterators are then pushed incrementally
      to the stack using the [open_] instruction.

      Each iterator in the stack is associated with:

        - A reference, where the current value of the iterator is stored (this
          reference is the only way for users to access the current value of the
          iterator)
        - An instruction, to be executed for each value of the iterator

      The [iterator] function is provided as a convenience when a list of
      column iterators needs to be iterated at once; for more advanced usage
      (e.g. filtering the values dynamically), an [instruction] must be
      constructed manually. *)

  (** The type [('a, 'y, 's) instruction] represents instructions of the virtual
      machine. The type parameters are interpreted as follows:

      - The ['a] type parameter represents the type of user-defined actions
        (introduced with [action])

      - The ['y] type parameter represents the type of values yielded by the
      virtual machine (in [fold] or [iter]).

      - The ['s] type parameter represents the stack of the virtual machine. *)
  type ('a, 's) instruction

  val pp_instruction :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    ('a, 's) instruction ->
    unit

  (** [advance] is a terminating instruction.

      If the stack is empty, the iteration is finished. Otherwise, advance the
      iterator at the current level, then call [dispatch]. *)
  val advance : ('a, 's) instruction

  (** [dispatch] is a terminating instruction on non-empty stacks.

      If the iterator at the current level is exhausted, go back [up] one level
      and call [advance].

      Otherwise, store the value of the current level in the corresponding
      reference and execute the instruction associated with the current level.
  *)
  val dispatch : ('a, _ -> 's) instruction

  val seek :
    'a option ref with_name ->
    'a Iterator.t with_name ->
    ('b, 's) instruction ->
    ('b, 's) instruction

  (** [up k] moves back to the previous level in the stack, then executes
      [k] at that level. *)
  val up : ('a, 's) instruction -> ('a, _ -> 's) instruction

  (** [open iterator cell each_value k] initializes a new level.

      The [iterator] is [init]ialized, and pushed onto the stack with associated
      reference [cell] and instruction [each_value]. Then, execute instruction
      [k].
  *)
  val open_ :
    'i Iterator.t with_name ->
    'i option ref with_name ->
    ('a, 'i -> 's) instruction ->
    ('a, 'i -> 's) instruction ->
    ('a, 's) instruction

  (** [action action k] executes the action [action] then the instruction [k].

      Actions are user-defined and evaluated with the [evaluate] argument to
      [create]. *)
  val action : 'a -> ('a, 's) instruction -> ('a, 's) instruction

  (** [call f ~name rs k] calls [f] with the values of the references in [rs] as a
      heterogeneous list of values, then executes the instruction [k]. [name] is used when printing.

      {b Note}: The references in [rs] are intended to be the references
      associated with levels in the stack at the point the [call] instruction
      is executed, and {b must not} be [None] at that point. *)
  val call :
    ('a Constant.hlist -> unit) ->
    name:string ->
    'a Option_ref.hlist with_names ->
    ('x, 's) instruction ->
    ('x, 's) instruction

  type t

  val create : evaluate:('a -> outcome) -> ('a, nil) instruction -> t

  val run : t -> unit

  type 'a iterator

  (** [iterator] is a convenience function for creating a virtual machine that
      iterates over all the values of an iterator heterogenous list. *)
  val iterator : 's Iterator.hlist with_names -> 's iterator

  val iter : ('y Constant.hlist -> unit) -> 'y iterator -> unit
end
