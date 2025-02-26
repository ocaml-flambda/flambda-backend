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

module Datalog = struct
  type nonrec nil = nil = Nil

  module Constant = Constant

  module Column = struct
    type (_, _, _) repr =
      | Patricia_tree_repr : ('a Patricia_tree.map, int, 'a) repr

    type ('t, 'k, 'v) id =
      { name : string;
        print : Format.formatter -> 'k -> unit;
        repr : ('t, 'k, 'v) repr
      }

    let print_key { print; _ } = print

    type (_, _, _) hlist =
      | [] : ('v, nil, 'v) hlist
      | ( :: ) :
          ('t, 'k, 's) id * ('s, 'ks, 'v) hlist
          -> ('t, 'k -> 'ks, 'v) hlist

    let rec print_keys :
        type t k v.
        (t, k, v) hlist -> Format.formatter -> k Constant.hlist -> unit =
     fun columns ppf keys ->
      match columns, keys with
      | [], [] -> ()
      | [column], [key] -> print_key column ppf key
      | column :: (_ :: _ as columns), key :: keys ->
        Format.fprintf ppf "%a,@ %a" (print_key column) key (print_keys columns)
          keys

    (* We could expose the fact that we do not support relations without
       arguments in the types, but a runtime error here allows us to give a
       better error message. Plus, we might support constant relations
       (represented as an option) in the future. *)
    let rec is_trie : type t k v. (t, k, v) hlist -> (t, k, v) Trie.is_trie =
      function
      | [] -> Misc.fatal_error "Cannot create relation with no arguments"
      | [{ repr = Patricia_tree_repr; _ }] -> Trie.patricia_tree_is_trie
      | { repr = Patricia_tree_repr; _ } :: (_ :: _ as columns) ->
        Trie.patricia_tree_of_trie (is_trie columns)

    module type S = sig
      type t

      val print : Format.formatter -> t -> unit

      module Set : Container_types.Set with type elt = t

      module Map :
        Container_types.Map_plus_iterator
          with type key = t
          with module Set = Set

      val datalog_column_id : ('a Map.t, t, 'a) id
    end

    module Make (X : sig
      val name : string

      val print : Format.formatter -> int -> unit
    end) =
    struct
      type t = int

      let print = X.print

      module Tree = Patricia_tree.Make (X)
      module Set = Tree.Set
      module Map = Tree.Map

      let datalog_column_id =
        { name = X.name; print; repr = Patricia_tree_repr }
    end

    module type Columns = sig
      type keys

      type value

      type t

      val empty : t

      val is_trie : (t, keys, value) Trie.is_trie
    end

    module Make_operations (C : Columns) = struct
      let empty = C.empty

      let is_empty trie = Trie.is_empty C.is_trie trie

      let singleton keys value = Trie.singleton C.is_trie keys value

      let add_or_replace keys value trie =
        Trie.add_or_replace C.is_trie keys value trie

      let remove keys trie = Trie.remove C.is_trie keys trie

      let union f trie1 trie2 = Trie.union C.is_trie f trie1 trie2

      let find_opt keys trie = Trie.find_opt C.is_trie keys trie
    end
  end

  include Datalog

  type ('t, 'k, 'v) table = ('t, 'k, 'v) Table.Id.t

  let create_table ~name ~default_value columns =
    Table.Id.create ~name ~is_trie:(Column.is_trie columns)
      ~print_keys:(Column.print_keys columns)
      ~default_value

  type ('t, 'k) relation = ('t, 'k, unit) table

  let create_relation ~name columns =
    create_table ~name ~default_value:() columns

  module Schema = struct
    module type S0 = sig
      type keys

      type value

      type t

      val columns : (t, keys, value) Column.hlist

      val default_value : value
    end

    module type S = sig
      include S0

      val create : name:string -> (t, keys, value) table

      val empty : t

      val is_empty : t -> bool

      val singleton : keys Constant.hlist -> value -> t

      val add_or_replace : keys Constant.hlist -> value -> t -> t

      val remove : keys Constant.hlist -> t -> t

      val union : (value -> value -> value option) -> t -> t -> t

      val find_opt : keys Constant.hlist -> t -> value option
    end

    module type Relation = S with type value = unit

    module type C = Column.S

    module Nil = struct
      type keys = nil

      type value = unit

      type t = value

      let columns : (t, keys, value) Column.hlist = []

      let default_value = ()
    end

    module Cons (C : C) (S : S0) = struct
      module T = struct
        type keys = C.t -> S.keys

        type t = S.t C.Map.t

        type value = S.value

        let columns : (t, keys, value) Column.hlist =
          C.datalog_column_id :: S.columns

        let default_value = S.default_value

        let create ~name = create_table ~name columns ~default_value

        let is_trie = Column.is_trie columns

        let empty = C.Map.empty
      end

      include T
      include Column.Make_operations (T)
    end

    module Relation1 (C1 : C) = Cons (C1) (Nil)
    module Relation2 (C1 : C) (C2 : C) = Cons (C1) (Relation1 (C2))
    module Relation3 (C1 : C) (C2 : C) (C3 : C) =
      Cons (C1) (Relation2 (C2) (C3))
    module Relation4 (C1 : C) (C2 : C) (C3 : C) (C4 : C) =
      Cons (C1) (Relation3 (C2) (C3) (C4))
  end

  let add_fact id args db =
    Table.Map.set id
      (Trie.add_or_replace (Table.Id.is_trie id) args () (Table.Map.get id db))
      db

  type database = Table.Map.t

  let empty = Table.Map.empty

  let get_table = Table.Map.get

  let set_table = Table.Map.set

  let print = Table.Map.print

  module Schedule = Schedule

  type rule = Schedule.rule

  type deduction =
    [ `Atom of atom
    | `And of deduction list ]

  let and_ atoms = `And atoms

  let deduce = Schedule.deduce

  type hypothesis =
    [ `Atom of atom
    | `Not_atom of atom ]

  let atom id args = `Atom (Atom (id, args))

  let not (`Atom atom) = `Not_atom atom

  let where predicates f =
    List.fold_left
      (fun f predicate ->
        match predicate with
        | `Atom (Atom (id, args)) -> where_atom id args f
        | `Not_atom (Atom (id, args)) -> unless_atom id args f)
      f predicates

  module Cursor = struct
    type ('p, 'v) with_parameters = ('p, 'v) Cursor.With_parameters.t
    (* ('p, (action, 'v Constant.hlist, nil) Cursor0.instruction) cursor *)

    type 'v t = (nil, 'v) with_parameters

    let print = Cursor.With_parameters.print

    let create variables f =
      compile variables @@ fun variables ->
      where (f variables) @@ yield variables

    let create_with_parameters ~parameters variables f =
      compile variables (fun variables ->
          with_parameters parameters (fun parameters ->
              where (f parameters variables) @@ yield variables))

    let fold_with_parameters cursor parameters database ~init ~f =
      Cursor.With_parameters.naive_fold cursor parameters database f init

    let fold cursor database ~init ~f =
      Cursor.With_parameters.naive_fold cursor [] database f init

    let iter_with_parameters cursor parameters database ~f =
      Cursor.With_parameters.naive_iter cursor parameters database f

    let iter cursor database ~f =
      Cursor.With_parameters.naive_iter cursor [] database f
  end
end
