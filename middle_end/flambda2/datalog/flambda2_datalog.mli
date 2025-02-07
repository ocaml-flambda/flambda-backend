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

module Datalog : sig
  (** Well-typed Datalog implementation. *)

  (** Marker type for the end of heterogenous lists. *)
  type nil = private Nil

  module Column : sig
    (** Datalog tables are represented as a nesting of multiple maps; for
        instance, a binary relation on integers is represented as a
        [unit Int.Map.t Int.Map.t].

        A [Column] represents a single argument of a relation, and contains
        enough information to be combined with other columns into a n-ary
        relation.

        {b Note}: The [Column.id] and [Column.hlist] types encode some precise
        type-level information that enables an easy-to-use syntax for the
        [create_relation] function below. Understanding the type parameters for
        [Column.id] and [Column.hlist] is {b not} necessary to use this module.
    *)

    (** A value of type [('t, 'k, 'v) id] represents a column with keys of type
        ['k].

        The ['t] and ['v] parameters are only relevant when applied to a column
        that is part of a schema.
    *)
    type ('t, 'k, 'v) id

    type (_, _, _) hlist =
      | [] : ('v, nil, 'v) hlist
      | ( :: ) :
          ('t, 'k, 's) id * ('s, 'ks, 'v) hlist
          -> ('t, 'k -> 'ks, 'v) hlist

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
    end) : S with type t = int
  end

  type ('t, 'k, 'v) table

  val create_table :
    name:string ->
    default_value:'v ->
    ('t, 'k, 'v) Column.hlist ->
    ('t, 'k, 'v) table

  type ('t, 'k) relation = ('t, 'k, unit) table

  (** [create_relation ~name schema] creates a new relation with name [name] and
        schema [schema].

        The schema is given as a heterogenous list of column types, and the relation
        is represented in memory as a series of nested maps following this list. If
        the schema [ty1; ty2; ty3] is provided, the relation will be represented as
        a map from [ty1] whose values are maps from [ty2] to [ty2]. The order of
        arguments provided to a relation thus have profound implication for the
        performance of iterations on the relation, and needs to be chosen carefully.

        @raise Misc.Fatal_error if [schema] is empty.

        {b Example}

        The following code defines a binary edge relationship between nodes,
        represented as a map from a node to its successors, and an unary predicate
        to distinguish some sort of {e marked} nodes.

        {[
        let marked_pred : node rel1 =
          create_relation ~name:"marked" [node]

        let edge_rel : (node, node) rel2 =
          create_relation ~name:"edge" [node; node]
        ]}
          *)
  val create_relation :
    name:string -> ('t, 'k, unit) Column.hlist -> ('t, 'k) relation

  module Constant : sig
    (** The [Constant] module only provides a heterogenous list to represent
        lists of values.

        It is intended to be used with type-directed disambiguation. *)

    type _ hlist =
      | [] : nil hlist
      | ( :: ) : 'a * 'b hlist -> ('a -> 'b) hlist
  end

  module Term : sig
    type 'a t

    type _ hlist =
      | [] : nil hlist
      | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist

    val constant : 'a -> 'a t
  end

  type atom

  type hypothesis =
    [ `Atom of atom
    | `Not_atom of atom ]

  (** [atom rel args] represents the application of relation [rel] to the
      arguments [args].

      When writing queries or rules, it can be helpful to partially apply [atom]
      to relations in order to improve readability.

      {b Example}

      {[
      let marked = atom marked_pred
      let edge = atom edge_rel
      ]}
  *)
  val atom : ('t, 'k) relation -> 'k Term.hlist -> [> `Atom of atom]

  val not : [< `Atom of atom] -> [> `Not_atom of atom]

  type database

  val print : Format.formatter -> database -> unit

  val empty : database

  val get_table : ('t, 'k, 'v) table -> database -> 't

  val set_table : ('t, 'k, 'v) table -> 't -> database -> database

  (** [add_fact rel args db] records a fact into the database [db].

      {b Example}

      The following code populates a database with a small graph and a single
      marked node [n1].

      {[
      let n1 = Node.make ()
      let n2 = Node.make ()
      let n3 = Node.make ()
      let n4 = Node.make ()
      let n5 = Node.make ()

      let db =
        add_fact marked_pred [n1]
        @@ add_fact edge_rel [n1; n2]
        @@ add_fact edge_rel [n3; n2]
        @@ add_fact edge_rel [n2; n5]
        @@ add_fact edge_rel [n5; n4]
        @@ add_fact edge_rel [n4; n2]
        @@ empty
      ]}
  *)
  val add_fact : ('t, 'k) relation -> 'k Constant.hlist -> database -> database

  module String : sig
    (** Pseudo-heterogenous lists of strings.

        The type ['a String.hlist] is equivalent to the type [string list], but
        with extra type information, which we leverage to provide the [program]
        API.
    *)

    type _ hlist =
      | [] : nil hlist
      | ( :: ) : string * 'b hlist -> ('a -> 'b) hlist
  end

  module Cursor : sig
    (** A cursor represents a query on the database. Cursors provide [iter] and
        [fold] functions to iterate over the matching facts.

        {b Warning}: Cursors are {b mutable} data structures that are temporarily
        bound to a database and modified internally by iteration functions such as
        [iter] or [fold]. Reusing a cursor while it is being iterated over is
        unspecified behavior.

        {b Binding order}

        The order in which variables are provided to [Cursor.create] and
        [Cursor.create_with_parameters] is called the {b binding order}. For
        parameterized queries, the parameters appear before the variables in the
        binding order.

        This order corresponds to the nesting order of the loop nest that will be
        used to evaluate the query, so it can have dramatic performance impact and
        need to be chosen carefully.

        In order for the engine to be able to evaluate the query, variables must
        appear in the same order in at least one of the atoms constituting the
        query; for instance, it is not possible to iterate over
        [[edge [x; y]]] in the [[y; x]] binding order: we are requesting that [y]
        be bound before [x], but in order to find the set of possible values for
        [y] from [edge] we need to first know the possible values for [x]. These
        cases will raise an error.

        It is, however, possible to iterate on the query
        [[edge [x; y]; edge [y; x]] using the [[y; x]] binding order: we
        can use the [edge [y; x]] instance to bind the variables for [y] and [x]
        in this order, then check if [(x, y)] is in the [edge] relation for
        each [(y, x)] pair that we find. In this case, the occurrences of [y] and
        [x] in [edge [y; x]] are said to be {b binding}, while their occurrences
        in [edge [x; y]] are {b non-binding}.

        More precisely, an occurrence of a variable [x] in a positive atom is
        binding if all the previous arguments of the atom appear before [x] in the
        binding order; all other occurrences are non-binding. In order to
        evaluate a query, we require that all variables have at least one binding
        occurrence in the query.

        Occurrences from negated atoms (from [not]) are never binding.
      *)

    (** Parameterized cursors take an additional argument of type
            ['p Constant.hlist] that is provided when evaluating the cursor. *)
    type ('p, 'v) with_parameters

    (** Cursors yielding values of type ['v Constant.hlist]. *)
    type 'v t = (nil, 'v) with_parameters

    (** [Cursor.create vars f] creates a low-level [Cursor.t] from a high-level
        query, expressed as a conjunction of atoms.

        {b Warning}: The order of the variables in [vars] is crucial as it
        dictates the iteration order of the loop nest that will be used to
        evaluate the query. See the documentation of the {!Cursor} module.

        {b Note}: If you need to perform queries that depend on the value of a
        variable outside the database, consider using parameterized cursors (see
        {!Cursor.create_with_parameters}). Reusing a parameterized cursor is more
        efficient than creating new cursors for each value of the parameters.

        {b Example}

        The following code creates cursors to iterate on the marked nodes
        ([marked_cursor]), and on all the edge pairs in the graphs
        ([edge_cursor]), respectively.

        {[
        let marked_cursor =
          Cursor.create ["X"] (fun [x] -> [marked [x]])

        let edge_cursor =
          Cursor.create ["src"; "dst"] (fun [src; dst] -> [edge [src; dst]])
        ]}
    *)
    val create : 'v String.hlist -> ('v Term.hlist -> hypothesis list) -> 'v t

    (** Create a parameterized cursor.

        This is a more general version of [Cursor.create] that also takes an
        additional list of parameters. The

        {b Example}

        The following code creates two parameterized cursors for iterating over
        the successors or predecessors of a parametric node, which will be
        provided when evaluating the query.

        Notice that in the [successor_cursor], the parameters appears {e before}
        the variable in the [edge] relation, while in the [predecessor_cursor], it
        appears {e after} the variable.

        This means that the [successor_cursor] can be iterated efficiently,
        because it follows the structure of the relation: internally, the [edge]
        relation is represented as a map from nodes to their successors, and so
        evaluating the [successor_cursor] will result in a simple map lookup.

        On the other hand, evaluating [predecessor_cursor] requires iterating over
        all the (non-terminal) nodes to check whether it contains [p] in its
        successor map.

        {[
        let successor_cursor =
          Cursor.create_with_parameters ~parameters:["P"] ["X"] (fun [p] [x] ->
              [edge [p; x]])

        let predecessor_cursor =
          Cursor.create_with_parameters ~parameters:["P"] ["X"] (fun [p] [x] ->
              [edge [x; p]])
        ]}
    *)
    val create_with_parameters :
      parameters:'p String.hlist ->
      'v String.hlist ->
      ('p Term.hlist -> 'v Term.hlist -> hypothesis list) ->
      ('p, 'v) with_parameters

    (** [fold cursor db ~init ~f] accumulates [f] over all the variable bindings
        that match the query associated with [cursor] in [db].

        {b Warning}: [cursor] must not be used from inside [f].

        {b Example}

        The following code computes the list of reversed edges (edges from target
        to source).

        {[
        let reverse_edges =
          Cursor.fold edge_cursor db ~init:[] ~f:(fun [src; dst] acc ->
              (dst, src) :: acc)
        ]}
    *)
    val fold :
      'v t -> database -> init:'a -> f:('v Constant.hlist -> 'a -> 'a) -> 'a

    (** [iter cursor db ~f] applies [f] to all the variable bindings that match
        the query associated with [cursor] in [db].

        {b Warning}: [cursor] must not be used from inside [f].

        {b Example}

        The following code prints all the marked nodes.

        {[
        let () =
          Format.eprintf "@[<v 2>Marked nodes:@ ";
          Cursor.iter marked_cursor db ~f:(fun [n] ->
              Format.eprintf "- %a@ " Node.print n);
          Format.eprintf "@]@."
        ]}
    *)
    val iter : 'v t -> database -> f:('v Constant.hlist -> unit) -> unit

    (** [fold_with_parameters cursor params db ~init ~f] accumulates the function
        [f] over all the variable bindings that match the query in [db]. The
        values of the parameters are taken from the [params] list.

        {b Warning}: [cursor] must not be used from inside [f].

        {b Example}

        The following code accumulates the successors of node [n2] in a list.

        {[
        let successors =
          Cursor.fold_with_parameters successor_cursor [n2] db ~init:[]
            ~f:(fun [n] acc -> n :: acc)
        ]}
    *)
    val fold_with_parameters :
      ('p, 'v) with_parameters ->
      'p Constant.hlist ->
      database ->
      init:'a ->
      f:('v Constant.hlist -> 'a -> 'a) ->
      'a

    (** [iter_with_parameters cursor params db ~f] applies [f] to all the variable
        bindings that match the query in [db], where the parameter values are
        taken from [params].

        {b Warning}: [cursor] must not be used from inside [f].

        {b Example}

        The following code prints the predecessors of node [n2].

        {[
        let () =
          Format.eprintf "@[<v 2>Predecessors of %a:@ " Node.print n2;
          Cursor.iter_with_parameters predecessor_cursor [n2] db ~f:(fun [n] ->
              Format.eprintf "- %a@ " Node.print n);
          Format.eprintf "@]@."
        ]}
    *)
    val iter_with_parameters :
      ('p, 'v) with_parameters ->
      'p Constant.hlist ->
      database ->
      f:('v Constant.hlist -> unit) ->
      unit
  end

  (** The type of compiled rules.

      Rule specifications must be compiled to low-level rules using
      {!compile_rule} before being applied to a database using {!Schedule.rules}.

      {b Note}: Although compiled rules are mutable data structures, this
      mutability is only exploited while the compiled rule is executing (e.g.
      during {!Schedule.run}). It is thus safe to reuse a [rule] across
      multiple schedules or within the same schedule.
  *)
  type rule

  module Schedule : sig
    type t

    (** [saturate rules] is a schedule that repeatedly applies the rules in
          [rules] until reaching a fixpoint.

          {b Note}: [saturate rules] is equivalent to [fixpoint (rules rules)], but
          is (slightly) more efficient. It is not necessary to wrap a [saturate]
          schedule in a [fixpoint].
      *)
    val saturate : rule list -> t

    (** [fixpoint schedules] repeatedly runs the schedules in [schedules] until
          reaching a fixpoint.

          Facts added by previous schedules in the list are visible.
      *)
    val fixpoint : t list -> t

    type stats

    val create_stats : unit -> stats

    val print_stats : Format.formatter -> stats -> unit

    (** [run schedule db] runs the schedule [schedule] on the database [db].

          It returns a new database that contains all the facts in [db], plus all
          the facts that were inferred by [schedule].
      *)
    val run : ?stats:stats -> t -> database -> database
  end

  (** The type [('p, 'v) program] is the type of programs returning values
      of type ['v] with parameters ['p].

      The output of programs is either queries or rules; the use of a shared
      types allows writing combinators that work in both cases.
    *)
  type ('p, 'a) program

  (** Compile a program and returns the resulting value.

      {b Note}: As a convenience, [compile] takes a list of variables and binds
      them immediately with [foreach]. To compile an existing program with no
      free variables, use [compile [] (fun [] -> program)].

      Repeated compilation of a program building mutable values (such as
      cursors) create new values each time they are compiled.
    *)
  val compile : 'v String.hlist -> ('v Term.hlist -> (nil, 'a) program) -> 'a

  val with_parameters :
    'p String.hlist -> ('p Term.hlist -> ('p, 'a) program) -> (nil, 'a) program

  (** [foreach vars prog] binds the variables [vars] in [prog].

      The order variables are provided in [vars] is the iteration order during
      evaluation.
    *)
  val foreach :
    'a String.hlist -> ('a Term.hlist -> ('p, 'b) program) -> ('p, 'b) program

  val where : hypothesis list -> ('p, 'a) program -> ('p, 'a) program

  (** [yield args] is a query program that outputs the tuple [args]. *)
  val yield : 'v Term.hlist -> ('p, ('p, 'v) Cursor.with_parameters) program

  type deduction =
    [ `Atom of atom
    | `And of deduction list ]

  val and_ : 'a list -> [> `And of 'a list]

  (** [deduce rel args] adds the fact [rel args] to the database. *)
  val deduce : deduction -> (nil, rule) program

  module Schema : sig
    module type S = sig
      type keys

      type value

      type t

      val columns : (t, keys, value) Column.hlist

      val default_value : value

      val create : name:string -> (t, keys, value) table

      val empty : t

      val is_empty : t -> bool

      val singleton : keys Constant.hlist -> value -> t

      val add_or_replace : keys Constant.hlist -> value -> t -> t

      val remove : keys Constant.hlist -> t -> t

      val union : (value -> value -> value option) -> t -> t -> t

      val find_opt : keys Constant.hlist -> t -> value option
    end

    module Cons (C : Column.S) (S : S) :
      S
        with type keys = C.t -> S.keys
         and type t = S.t C.Map.t
         and type value = S.value

    module type Relation = S with type value = unit

    module Relation1 (C : Column.S) :
      Relation with type keys = C.t -> nil and type t = unit C.Map.t

    module Relation2 (C1 : Column.S) (C2 : Column.S) :
      Relation
        with type keys = C1.t -> Relation1(C2).keys
         and type t = Relation1(C2).t C1.Map.t

    module Relation3 (C1 : Column.S) (C2 : Column.S) (C3 : Column.S) :
      Relation
        with type keys = C1.t -> Relation2(C2)(C3).keys
         and type t = Relation2(C2)(C3).t C1.Map.t

    module Relation4
        (C1 : Column.S)
        (C2 : Column.S)
        (C3 : Column.S)
        (C4 : Column.S) :
      Relation
        with type keys = C1.t -> Relation3(C2)(C3)(C4).keys
         and type t = Relation3(C2)(C3)(C4).t C1.Map.t
  end
end
