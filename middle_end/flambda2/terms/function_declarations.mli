(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The representation of a set of function declarations (possibly mutually
    recursive). Such a set encapsulates the declarations themselves, information
    about their defining environment, and information used specifically for
    optimization.

    Before a function can be applied it must be "projected" from a set of
    closures to yield a "closure". This is done using [Project_closure] (see
    above). Given a closure, not only can it be applied, but information about
    its defining environment can be retrieved (using [Project_var], see above).

    At runtime, a [set_of_closures] corresponds to an OCaml value with tag
    [Closure_tag] (possibly with inline [Infix_tag](s)). As an optimization, an
    operation ([Select_closure]) is provided (see above) which enables one
    closure within a set to be located given another closure in the same set.
    This avoids keeping a pointer to the whole set of closures alive when
    compiling, for example, mutually-recursive functions. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val empty : t

val is_empty : t -> bool

(** Create a set of function declarations in the given order. *)
val create : Code_id.t Closure_id.Lmap.t -> t

(** The function(s) defined by the set of function declarations, indexed by
    closure ID. *)
val funs : t -> Code_id.t Closure_id.Map.t

(** The function(s) defined by the set of function declarations, in the order
    originally given. *)
val funs_in_order : t -> Code_id.t Closure_id.Lmap.t

(** [find f t] raises [Not_found] if [f] is not in [t]. *)
val find : t -> Closure_id.t -> Code_id.t

val binds_closure_id : t -> Closure_id.t -> bool

val compare : t -> t -> int

val filter : t -> f:(Closure_id.t -> Code_id.t -> bool) -> t
