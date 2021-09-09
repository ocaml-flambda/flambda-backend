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

(* Mutable representation of a stack of handlers.

   Intended for computing a stack of trap handlers reachable at each program
   location in a function. The top of the stack represents the current trap
   handler, which will be called if the current instruction raises an
   exception. *)

module type D = sig
  type t

  (** for unification *)
  val equal : t -> t -> bool

  (** for debug printing *)
  val to_string : t -> string
end

module type S = sig
  (** The type of elements pushed on the stack *)
  type d

  (** The stack of elements *)
  type t

  exception Unresolved

  (** Returns the representation of an empty stack. *)
  val empty : unit -> t

  (** Returns the representation of an unknown stack. *)
  val unknown : unit -> t

  (** [pop t] removes the top trap handler from the stack [t] and returns the
      new stack. *)
  val pop : t -> t

  (** [push t d] adds trap handler [d] on top of the stack [t] and returns the
      resulting stack. *)
  val push : t -> d -> t

  (** Returns a list representation of the stack [t], with the head of the list
      representing the top of the stack. Raises [Unresolved] if any part of [t]
      is [unknown]. *)
  val to_list_exn : t -> d list

  (** Returns the top of the stack [t], or [None] if [t] is empty. Raises
      [Unresolved] if the top of [t] is [unknown]. *)
  val top_exn : t -> d option

  (** [unify s1 s2] fails if s1 and s2 do not agree and resolves [unknown] parts
      of s1 and s2 whenever possible, destructively modifying [s1] and [s2].
      Fails if the destructive update would create a cycle in the data
      structure. *)
  val unify : t -> t -> unit

  (* Debug printing *)

  val print : t -> unit

  val print_pair : string -> t -> t -> unit
end

(** Functor building stacks of handlers *)
module Make (D : D) : S with type d = D.t
