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

open Heterogenous_list

(** {2 Facts database} *)

(** {2 Inference rules} *)

(** [compile_rule vars f]

    As for [Cursor.create], the order of the variables in [vars] determines the
    iteration order, and variables must appear in the same order in rules (see
    the documentation of the {!Cursor} module for more details).

    {b Example}

    The following creates a rule stating that the successor of marked nodes
    should themselves be marked.

    {[
    let mark_successors_rule =
      compile_rule ["X"; "Y"] (fun [x; y] ->
          rule [edge_rel [x; y]; marked_pred [x]] (marked_pred [y]))
    ]}
*)

(** {2 Query language} *)

(** Fact retrieval is supported through a query expressed using (typed) Datalog
    queries.
*)

module Term : sig
  include Heterogenous_list.S

  val constant : 'a -> 'a t
end

module String : sig
  include Heterogenous_list.S with type 'a t := string
end

(** The type [('free, 'p, 'v) program] is the type of programs returning
      values of type ['v] with free parameters ['free] and used parameters
      ['p].

      Only programs with no free parameters (i.e. ['free] is
      [Heterogenous_list.nil]) can be compiled, see [compile].

      The output of programs is either queries or rules; the use of a shared
      types allows writing combinators that work in both cases.
  *)
type ('p, 'a) program

val map_program : ('p, 'a) program -> ('a -> 'b) -> ('p, 'b) program

val compile : 'v String.hlist -> ('v Term.hlist -> (nil, 'a) program) -> 'a

val with_parameters :
  'p String.hlist -> ('p Term.hlist -> ('p, 'a) program) -> (nil, 'a) program

val foreach :
  'a String.hlist -> ('a Term.hlist -> ('p, 'b) program) -> ('p, 'b) program

val where_atom :
  ('t, 'k, 'v) Table.Id.t ->
  'k Term.hlist ->
  ('p, 'a) program ->
  ('p, 'a) program

val unless_atom :
  ('t, 'k, 'v) Table.Id.t ->
  'k Term.hlist ->
  ('p, 'a) program ->
  ('p, 'a) program

val yield : 'v Term.hlist -> ('p, ('p, 'v) Cursor.With_parameters.t) program
