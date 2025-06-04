(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type closure_entry_point =
  | Indirect_code_pointer
  | Direct_code_pointer

let closure_entry_point_to_int = function
  | Indirect_code_pointer -> 0
  | Direct_code_pointer -> 1

let closure_entry_point_to_string = function
  | Indirect_code_pointer -> "Indirect_code_pointer"
  | Direct_code_pointer -> "Direct_code_pointer"

module Field = struct
  module M = struct
    type return_kind =
      | Normal of int
      | Exn

    type t =
      | Block of int * Flambda_kind.t
      | Value_slot of Value_slot.t
      | Function_slot of Function_slot.t
      | Code_of_closure
      | Is_int
      | Get_tag
      | Apply of closure_entry_point * return_kind
      | Code_id_of_call_witness of int

    let compare_return_kind r1 r2 =
      match r1, r2 with
      | Normal i, Normal j -> compare i j
      | Exn, Exn -> 0
      | Normal _, Exn -> 1
      | Exn, Normal _ -> -1

    let compare t1 t2 =
      match t1, t2 with
      | Block (n1, k1), Block (n2, k2) ->
        let c = Int.compare n1 n2 in
        if c <> 0 then c else Flambda_kind.compare k1 k2
      | Value_slot v1, Value_slot v2 -> Value_slot.compare v1 v2
      | Function_slot f1, Function_slot f2 -> Function_slot.compare f1 f2
      | Code_of_closure, Code_of_closure -> 0
      | Is_int, Is_int -> 0
      | Get_tag, Get_tag -> 0
      | Apply (ep1, r1), Apply (ep2, r2) ->
        let c =
          Int.compare
            (closure_entry_point_to_int ep1)
            (closure_entry_point_to_int ep2)
        in
        if c <> 0 then c else compare_return_kind r1 r2
      | Code_id_of_call_witness c1, Code_id_of_call_witness c2 ->
        Int.compare c1 c2
      | ( Block _,
          ( Value_slot _ | Function_slot _ | Code_of_closure | Is_int | Get_tag
          | Apply _ | Code_id_of_call_witness _ ) ) ->
        -1
      | ( ( Value_slot _ | Function_slot _ | Code_of_closure | Is_int | Get_tag
          | Apply _ | Code_id_of_call_witness _ ),
          Block _ ) ->
        1
      | ( Value_slot _,
          ( Function_slot _ | Code_of_closure | Is_int | Get_tag | Apply _
          | Code_id_of_call_witness _ ) ) ->
        -1
      | ( ( Function_slot _ | Code_of_closure | Is_int | Get_tag | Apply _
          | Code_id_of_call_witness _ ),
          Value_slot _ ) ->
        1
      | ( Function_slot _,
          ( Code_of_closure | Is_int | Get_tag | Apply _
          | Code_id_of_call_witness _ ) ) ->
        -1
      | ( ( Code_of_closure | Is_int | Get_tag | Apply _
          | Code_id_of_call_witness _ ),
          Function_slot _ ) ->
        1
      | Code_of_closure, (Is_int | Get_tag | Apply _ | Code_id_of_call_witness _)
        ->
        -1
      | ( (Is_int | Get_tag | Apply _ | Code_id_of_call_witness _),
          Code_of_closure ) ->
        1
      | Is_int, (Get_tag | Apply _ | Code_id_of_call_witness _) -> -1
      | (Get_tag | Apply _ | Code_id_of_call_witness _), Is_int -> 1
      | Get_tag, (Apply _ | Code_id_of_call_witness _) -> -1
      | (Apply _ | Code_id_of_call_witness _), Get_tag -> 1
      | Apply _, Code_id_of_call_witness _ -> -1
      | Code_id_of_call_witness _, Apply _ -> 1

    let equal a b = compare a b = 0

    let hash = Hashtbl.hash

    let print ppf = function
      | Block (i, k) -> Format.fprintf ppf "%i_%a" i Flambda_kind.print k
      | Value_slot s -> Format.fprintf ppf "%a" Value_slot.print s
      | Function_slot f -> Format.fprintf ppf "%a" Function_slot.print f
      | Code_of_closure -> Format.fprintf ppf "Code"
      | Is_int -> Format.fprintf ppf "Is_int"
      | Get_tag -> Format.fprintf ppf "Get_tag"
      | Apply (ep, Normal i) ->
        Format.fprintf ppf "Apply (%s, Normal %i)"
          (closure_entry_point_to_string ep)
          i
      | Apply (ep, Exn) ->
        Format.fprintf ppf "Apply (%s, Exn)" (closure_entry_point_to_string ep)
      | Code_id_of_call_witness i ->
        Format.fprintf ppf "Code_id_of_call_witness %d" i
  end

  include M

  let kind : t -> _ = function
    | Block (_, kind) -> kind
    | Value_slot vs -> Value_slot.kind vs
    | Function_slot _ -> Flambda_kind.value
    | Is_int | Get_tag -> Flambda_kind.naked_immediate
    | (Code_of_closure | Apply _ | Code_id_of_call_witness _) as field ->
      Misc.fatal_errorf "[field_kind] for virtual field %a" print field

  module Container = Container_types.Make (M)
  module Map = Container.Map

  let encode, decode =
    let field_to_int = ref Map.empty in
    let int_to_field = ref Numeric_types.Int.Map.empty in
    let first_free = ref 0 in
    let encode field =
      match Map.find_opt field !field_to_int with
      | Some f -> f
      | None ->
        let r = !first_free in
        field_to_int := Map.add field r !field_to_int;
        int_to_field := Numeric_types.Int.Map.add r field !int_to_field;
        first_free := r + 1;
        r
    in
    let decode n = Numeric_types.Int.Map.find n !int_to_field in
    encode, decode
end

module FieldC = Datalog.Column.Make (struct
  let name = "field"

  let print ppf i = Field.print ppf (Field.decode i)
end)

module CoField = struct
  module M = struct
    type t = Param of closure_entry_point * int

    let compare t1 t2 =
      match t1, t2 with
      | Param (ep1, i1), Param (ep2, i2) ->
        let c =
          Int.compare
            (closure_entry_point_to_int ep1)
            (closure_entry_point_to_int ep2)
        in
        if c <> 0 then c else Int.compare i1 i2

    let equal a b = compare a b = 0

    let hash = Hashtbl.hash

    let print ppf = function
      | Param (ep, i) ->
        Format.fprintf ppf "Param (%s, %d)" (closure_entry_point_to_string ep) i
  end

  include M
  module Container = Container_types.Make (M)
  module Map = Container.Map

  let encode, decode =
    let field_to_int = ref Map.empty in
    let int_to_field = ref Numeric_types.Int.Map.empty in
    let first_free = ref 0 in
    let encode field =
      match Map.find_opt field !field_to_int with
      | Some f -> f
      | None ->
        let r = !first_free in
        field_to_int := Map.add field r !field_to_int;
        int_to_field := Numeric_types.Int.Map.add r field !int_to_field;
        first_free := r + 1;
        r
    in
    let decode n = Numeric_types.Int.Map.find n !int_to_field in
    encode, decode
end

module CoFieldC = Datalog.Column.Make (struct
  let name = "cofield"

  let print ppf i = CoField.print ppf (CoField.decode i)
end)

module Alias_rel = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module Use_rel = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module Accessor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (FieldC) (Code_id_or_name)
module Constructor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (FieldC) (Code_id_or_name)
module Propagate_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (Code_id_or_name) (Code_id_or_name)
module CoAccessor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (CoFieldC) (Code_id_or_name)
module CoConstructor_rel =
  Datalog.Schema.Relation3 (Code_id_or_name) (CoFieldC) (Code_id_or_name)
module Any_usage_pred = Datalog.Schema.Relation1 (Code_id_or_name)
module Any_source_pred = Datalog.Schema.Relation1 (Code_id_or_name)

type graph =
  { mutable alias_rel : Alias_rel.t;
    mutable use_rel : Use_rel.t;
    mutable accessor_rel : Accessor_rel.t;
    mutable constructor_rel : Constructor_rel.t;
    mutable coaccessor_rel : CoAccessor_rel.t;
    mutable coconstructor_rel : CoConstructor_rel.t;
    mutable propagate_rel : Propagate_rel.t;
    mutable any_usage_pred : Any_usage_pred.t;
    mutable any_source_pred : Any_source_pred.t
  }

let print_iter_edges ~print_edge graph =
  let iter_inner color target m =
    Code_id_or_name.Map.iter
      (fun source () -> print_edge (source, target, color))
      m
  in
  let iter_nn color m = Code_id_or_name.Map.iter (iter_inner color) m in
  let iter_nfn color m =
    Code_id_or_name.Map.iter
      (fun target m -> FieldC.Map.iter (fun _ m -> iter_inner color target m) m)
      m
  in
  let iter_ncn color m =
    Code_id_or_name.Map.iter
      (fun target m ->
        CoFieldC.Map.iter (fun _ m -> iter_inner color target m) m)
      m
  in
  iter_nn "black" graph.alias_rel;
  iter_nn "red" graph.use_rel;
  iter_nfn "green" graph.accessor_rel;
  iter_nfn "blue" graph.constructor_rel;
  iter_ncn "darkgreen" graph.coaccessor_rel;
  iter_ncn "darkblue" graph.coconstructor_rel;
  Code_id_or_name.Map.iter
    (fun _if_defined m -> iter_nn "purple" m)
    graph.propagate_rel

let alias_rel = Alias_rel.create ~name:"alias"

let use_rel = Use_rel.create ~name:"use"

let accessor_rel = Accessor_rel.create ~name:"accessor"

let constructor_rel = Constructor_rel.create ~name:"constructor"

let coaccessor_rel = CoAccessor_rel.create ~name:"coaccessor"

let coconstructor_rel = CoConstructor_rel.create ~name:"coconstructor"

let propagate_rel = Propagate_rel.create ~name:"propagate"

let any_usage_pred = Any_usage_pred.create ~name:"any_usage"

let any_source_pred = Any_source_pred.create ~name:"any_source"

let to_datalog graph =
  Datalog.set_table alias_rel graph.alias_rel
  @@ Datalog.set_table use_rel graph.use_rel
  @@ Datalog.set_table accessor_rel graph.accessor_rel
  @@ Datalog.set_table constructor_rel graph.constructor_rel
  @@ Datalog.set_table coaccessor_rel graph.coaccessor_rel
  @@ Datalog.set_table coconstructor_rel graph.coconstructor_rel
  @@ Datalog.set_table propagate_rel graph.propagate_rel
  @@ Datalog.set_table any_usage_pred graph.any_usage_pred
  @@ Datalog.set_table any_source_pred graph.any_source_pred
  @@ Datalog.empty

type 'a rel0 = [> `Atom of Datalog.atom] as 'a

type ('a, 'b) rel1 = 'a Datalog.Term.t -> 'b rel0

type ('a, 'b, 'c) rel2 = 'a Datalog.Term.t -> ('b, 'c) rel1

type ('a, 'b, 'c, 'd) rel3 = 'a Datalog.Term.t -> ('b, 'c, 'd) rel2

(* Naming:
 * to_ = from; (alias)
 * to_ = [...] from (use)
 * to_ = base.relation (accessor)
 * base = Make_block { from_ } (constructor)
 * *)

let alias_rel to_ from = Datalog.atom alias_rel [to_; from]

let use_rel to_ from = Datalog.atom use_rel [to_; from]

let accessor_rel to_ relation base =
  Datalog.atom accessor_rel [to_; relation; base]

let constructor_rel base relation from =
  Datalog.atom constructor_rel [base; relation; from]

let coaccessor_rel to_ relation base =
  Datalog.atom coaccessor_rel [to_; relation; base]

let coconstructor_rel base relation from =
  Datalog.atom coconstructor_rel [base; relation; from]

let propagate_rel if_used to_ from =
  Datalog.atom propagate_rel [if_used; to_; from]

let any_usage_pred var = Datalog.atom any_usage_pred [var]

let any_source_pred var = Datalog.atom any_source_pred [var]

let create () =
  { alias_rel = Alias_rel.empty;
    use_rel = Use_rel.empty;
    accessor_rel = Accessor_rel.empty;
    constructor_rel = Constructor_rel.empty;
    coaccessor_rel = CoAccessor_rel.empty;
    coconstructor_rel = CoConstructor_rel.empty;
    propagate_rel = Propagate_rel.empty;
    any_usage_pred = Any_usage_pred.empty;
    any_source_pred = Any_source_pred.empty
  }

let add_alias t ~to_ ~from =
  t.alias_rel <- Alias_rel.add_or_replace [to_; from] () t.alias_rel

let add_use_dep t ~to_ ~from =
  t.use_rel <- Use_rel.add_or_replace [to_; from] () t.use_rel

let encode_field _t field = Field.encode field

let add_constructor_dep t ~base relation ~from =
  t.constructor_rel
    <- Constructor_rel.add_or_replace
         [base; encode_field t relation; from]
         () t.constructor_rel

let add_accessor_dep t ~to_ relation ~base =
  t.accessor_rel
    <- Accessor_rel.add_or_replace
         [to_; encode_field t relation; base]
         () t.accessor_rel

let add_coaccessor_dep t ~to_ relation ~base =
  t.coaccessor_rel
    <- CoAccessor_rel.add_or_replace
         [to_; CoField.encode relation; base]
         () t.coaccessor_rel

let add_coconstructor_dep t ~base relation ~from =
  t.coconstructor_rel
    <- CoConstructor_rel.add_or_replace
         [base; CoField.encode relation; from]
         () t.coconstructor_rel

let add_propagate_dep t ~if_used ~to_ ~from =
  t.propagate_rel
    <- Propagate_rel.add_or_replace [if_used; to_; from] () t.propagate_rel

let add_opaque_let_dependency t ~to_ ~from =
  let bound_to = Bound_pattern.free_names to_ in
  let f () bound_to =
    Name_occurrences.fold_names from
      ~f:(fun () var ->
        add_use_dep t
          ~to_:(Code_id_or_name.name bound_to)
          ~from:(Code_id_or_name.name var))
      ~init:()
  in
  Name_occurrences.fold_names bound_to ~f ~init:()

let add_use t (var : Code_id_or_name.t) =
  t.any_usage_pred <- Any_usage_pred.add_or_replace [var] () t.any_usage_pred

let add_any_source t (var : Code_id_or_name.t) =
  t.any_source_pred <- Any_source_pred.add_or_replace [var] () t.any_source_pred
