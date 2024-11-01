(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {Continuation Lifting Status}

    Continuation lifting is done in simplify on the way down. Considering
    a term of the form: *)

(** * let_cont k x =
    *   let_cont k' y =
    *     ...
    *   in
    *   ..
    * in
    * ..
    *)

(** The decision to lift continuations (e.g. k') out of another (e.g. k),
    is made once Simplify has reached the bottom of the handler of k **but**
    has not yet explored the handler of k'. The [t] type represente the current
    decision about lifting of continuations:

    - it is set to [No_lifting] at top-level (because continuations cannot be
      lifted above the top-level)
    - it is set to [Analyzing] when going downwards through the handler of a
      continuation.
    - when we make the choice of whether to lift or not[1], if we do decide
      to lift, we set it to [Lifting_out_of].

    See occurrences of the constructors in [simplify_let_cont_expr] and the
    associated comments for more details.

    [1]: typically when we simplify leafs of expressions, so mostly in
    simplify_switch because that will be the interesting case for the
    match-in-match optimisation. *)

(** The current continuation lifting status, stored in the dacc. *)
type t = private
  | Not_lifting
  | Analyzing of
      { continuation : Continuation.t;
        uses : Continuation_uses.t
      }
  | Lifting_out_of of { continuation : Continuation.t }
(**)

(** Printing function. *)
val print : Format.formatter -> t -> unit

(** Prevent any lifting of continuation. *)
val no_lifting : t

(** Delay the choice of lifting until the leaf of a continuation's handler. *)
val think_about_lifting_out_of : Continuation.t -> Continuation_uses.t -> t

(** Instruct [simplify_let_cont] to lift continuations. *)
val lift_continuations_out_of : Continuation.t -> t
