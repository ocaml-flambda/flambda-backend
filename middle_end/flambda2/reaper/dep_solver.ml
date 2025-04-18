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

module Field = Global_flow_graph.Field

type 'a unboxed_fields =
  | Not_unboxed of 'a
  | Unboxed of 'a unboxed_fields Field.Map.t

let rec pp_unboxed_elt pp_unboxed ppf = function
  | Not_unboxed x -> pp_unboxed ppf x
  | Unboxed fields -> Field.Map.print (pp_unboxed_elt pp_unboxed) ppf fields

let print_unboxed_fields = pp_unboxed_elt

(* CR-someday ncourant: track fields that are known to be constant, here and in
   changed_representation, to avoid having them be represented. This is a bit
   complex for two main reasons:

   - At this point in the dependency solver, we do not know the specific value
   of the constant but only that it is one (an alias to all_constants)

   - For symbols, this could break dominator scoping. *)
type unboxed = Variable.t unboxed_fields Field.Map.t

type changed_representation =
  (* CR ncourant: this is currently never produced, because we need to rewrite
     the value_kinds to account for changed representations before enabling
     this *)
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) unboxed_fields Field.Map.t
      * int
  | Closure_representation of
      Value_slot.t unboxed_fields Field.Map.t
      * Function_slot.t Function_slot.Map.t (* old -> new *)
      * Function_slot.t (* OLD current function slot *)

let pp_changed_representation ff = function
  | Block_representation (fields, size) ->
    Format.fprintf ff "(fields %a) (size %d)"
      (Field.Map.print
         (pp_unboxed_elt (fun ff (field, _) -> Format.pp_print_int ff field)))
      fields size
  | Closure_representation (fields, function_slots, fs) ->
    Format.fprintf ff "(fields %a) (function_slots %a) (current %a)"
      (Field.Map.print (pp_unboxed_elt Value_slot.print))
      fields
      (Function_slot.Map.print Function_slot.print)
      function_slots Function_slot.print fs

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation : changed_representation Code_id_or_name.Map.t
  }

let pp_result ppf res = Format.fprintf ppf "%a@." Datalog.print res.db

module Syntax = struct
  include Datalog

  let ( let$ ) xs f = compile xs f

  let ( ==> ) h c = where h (deduce c)
end

module Cols = struct
  let n = Code_id_or_name.datalog_column_id

  let f = Global_flow_graph.FieldC.datalog_column_id
end

let rel1_r name schema =
  let r = Datalog.create_relation ~name schema in
  r, fun x -> Datalog.atom r [x]

let rel1 name schema = snd (rel1_r name schema)

let rel2_r name schema =
  let r = Datalog.create_relation ~name schema in
  r, fun x y -> Datalog.atom r [x; y]

let rel2 name schema = snd (rel2_r name schema)

let rel3 name schema =
  let r = Datalog.create_relation ~name schema in
  fun x y z -> Datalog.atom r [x; y; z]

let usages_rel = rel2 "usages" Cols.[n; n]

let sources_rel = rel2 "sources" Cols.[n; n]

let any_source_pred = rel1 "any_source" Cols.[n]

let field_sources_rel = rel3 "field_sources" Cols.[n; f; n]

let field_top_sources_rel = rel2 "field_top_sources" Cols.[n; f]

let rev_alias_rel = rel2 "rev_alias" Cols.[n; n]

let rev_constructor_rel = rel3 "rev_constructor" Cols.[n; f; n]

let rev_accessor_rel = rel3 "rev_accessor" Cols.[n; f; n]

(* The program is abstracted as a series of relations concerning the reading and
   writing of fields of values.

   There are 5 different relations:

   - [alias to_ from] corresponds to [let to_ = from]

   - [accessor to_ relation base] corresponds to [let to_ = base.relation]

   - [constructor base relation from] corresponds to constructing a block [let
   base = { relation = from }]

   - [propagate if_used to_ from] means [alias to_ from], but only if [is_used]
   is used

   - [use to_ from] corresponds to [let to_ = f(from)], creating an arbitrary
   result [to_] and consuming [from].

   We perform an analysis that computes the ways each value can be used: either
   entirely, not at all, or, for each of its fields, how that field might be
   used. We also perform a reverse analysis that computes where each value can
   come from: either an arbitrary source (for use and values coming from outside
   the compilation unit), or a given constructor. *)

let datalog_schedule =
  let open Global_flow_graph in
  let open! Syntax in
  (* Reverse relations, because datalog does not implement a more efficient
     representation yet. *)
  let rev_alias =
    let$ [to_; from] = ["to_"; "from"] in
    [alias_rel to_ from] ==> rev_alias_rel from to_
  in
  let rev_accessor =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [accessor_rel to_ relation base] ==> rev_accessor_rel base relation to_
  in
  let rev_constructor =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [constructor_rel base relation from]
    ==> rev_constructor_rel from relation base
  in
  (* usages *)
  let usages_accessor_1 =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [not (used_pred base); usages_rel to_ _var; accessor_rel to_ relation base]
    ==> usages_rel base base
  in
  let usages_accessor_2 =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [not (used_pred base); used_pred to_; accessor_rel to_ relation base]
    ==> usages_rel base base
  in
  let usages_alias =
    let$ [to_; from; usage] = ["to_"; "from"; "usage"] in
    [ not (used_pred from);
      not (used_pred to_);
      usages_rel to_ usage;
      alias_rel to_ from ]
    ==> usages_rel from usage
  in
  (* sources *)
  let sources_constructor_1 =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      sources_rel from _var;
      rev_constructor_rel from relation base ]
    ==> sources_rel base base
  in
  let sources_constructor_2 =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_source_pred from;
      rev_constructor_rel from relation base ]
    ==> sources_rel base base
  in
  (* let sources_constructor = let$ [from; relation; base] = ["from";
     "relation"; "base"] in [ not (any_source_pred base); rev_constructor_rel
     from relation base] ==> sources_rel base base in *)
  let sources_alias =
    let$ [from; to_; source] = ["from"; "to_"; "source"] in
    [ not (any_source_pred from);
      not (any_source_pred to_);
      sources_rel from source;
      rev_alias_rel from to_ ]
    ==> sources_rel to_ source
  in
  (* propagate *)
  let alias_from_used_propagate =
    let$ [if_used; to_; from] = ["if_used"; "to_"; "from"] in
    [used_pred if_used; propagate_rel if_used to_ from] ==> alias_rel to_ from
  in
  let used_from_alias_used =
    let$ [to_; from] = ["to_"; "from"] in
    [alias_rel to_ from; used_pred to_] ==> used_pred from
  in
  let any_source_from_alias_any_source =
    let$ [from; to_] = ["from"; "to_"] in
    [rev_alias_rel from to_; any_source_pred from] ==> any_source_pred to_
  in
  (* accessor-used *)
  let used_fields_from_accessor_used_fields =
    let$ [to_; relation; base; _var] = ["to_"; "relation"; "base"; "_var"] in
    [ not (used_pred base);
      not (used_pred to_);
      not (used_fields_top_rel base relation);
      accessor_rel to_ relation base;
      usages_rel to_ _var ]
    ==> used_fields_rel base relation to_
  in
  let used_fields_from_accessor_used_fields_top =
    let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
    [not (used_pred base); used_pred to_; accessor_rel to_ relation base]
    ==> used_fields_top_rel base relation
  in
  (* constructor-sources *)
  let field_sources_from_constructor_field_sources =
    let$ [from; relation; base; _var] = ["from"; "relation"; "base"; "_var"] in
    [ not (any_source_pred base);
      not (any_source_pred from);
      not (field_top_sources_rel base relation);
      rev_constructor_rel from relation base;
      sources_rel from _var ]
    ==> field_sources_rel base relation from
  in
  let field_sources_from_constructor_field_top_sources =
    let$ [from; relation; base] = ["from"; "relation"; "base"] in
    [ not (any_source_pred base);
      any_source_pred from;
      rev_constructor_rel from relation base ]
    ==> field_top_sources_rel base relation
  in
  (* constructor-used *)
  let alias_from_accessed_constructor =
    let$ [base; base_use; relation; from; to_] =
      ["base"; "base_use"; "relation"; "from"; "to_"]
    in
    [ not (used_pred from);
      not (used_fields_top_rel base_use relation);
      not (used_pred base);
      constructor_rel base relation from;
      usages_rel base base_use;
      used_fields_rel base_use relation to_ ]
    ==> alias_rel to_ from
  in
  let used_from_accessed_constructor =
    let$ [base; base_use; relation; from] =
      ["base"; "base_use"; "relation"; "from"]
    in
    [ constructor_rel base relation from;
      not (used_pred base);
      usages_rel base base_use;
      used_fields_top_rel base_use relation ]
    ==> used_pred from
  in
  let used_from_constructor_used =
    let$ [base; relation; from] = ["base"; "relation"; "from"] in
    [used_pred base; constructor_rel base relation from] ==> used_pred from
  in
  (* accessor-sources *)
  let alias_from_accessed_constructor_2 =
    let$ [base; base_source; relation; to_; from] =
      ["base"; "base_source"; "relation"; "to_"; "from"]
    in
    [ not (any_source_pred to_);
      not (field_top_sources_rel base_source relation);
      not (any_source_pred base);
      rev_accessor_rel base relation to_;
      sources_rel base base_source;
      field_sources_rel base_source relation from ]
    ==> alias_rel to_ from
  in
  let any_source_from_accessed_constructor =
    let$ [base; base_source; relation; to_] =
      ["base"; "base_source"; "relation"; "to_"]
    in
    [ rev_accessor_rel base relation to_;
      not (any_source_pred base);
      sources_rel base base_source;
      field_top_sources_rel base_source relation ]
    ==> any_source_pred to_
  in
  let any_source_from_accessor_any_source =
    let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
    [any_source_pred base; rev_accessor_rel base relation to_]
    ==> any_source_pred to_
  in
  (* use *)
  let used_from_use_1 =
    let$ [to_; from; _var] = ["to_"; "from"; "_var"] in
    [usages_rel to_ _var; use_rel to_ from] ==> used_pred from
  in
  let used_from_use_2 =
    let$ [to_; from] = ["to_"; "from"] in
    [used_pred to_; use_rel to_ from] ==> used_pred from
  in
  let any_source_use =
    let$ [to_; _from] = ["to_"; "_from"] in
    [use_rel to_ _from] ==> any_source_pred to_
  in
  Datalog.Schedule.(
    fixpoint
      [ saturate
          [ rev_accessor;
            rev_constructor;
            any_source_use;
            alias_from_used_propagate;
            used_from_alias_used;
            any_source_from_alias_any_source;
            used_from_constructor_used;
            used_from_use_1;
            used_from_use_2;
            used_from_accessed_constructor;
            any_source_from_accessed_constructor;
            any_source_from_accessor_any_source;
            rev_alias ];
        saturate
          [ alias_from_accessed_constructor;
            alias_from_accessed_constructor_2;
            used_fields_from_accessor_used_fields;
            used_fields_from_accessor_used_fields_top;
            field_sources_from_constructor_field_sources;
            field_sources_from_constructor_field_top_sources;
            usages_accessor_1;
            usages_accessor_2;
            usages_alias;
            sources_constructor_1;
            sources_constructor_2;
            sources_alias;
            rev_alias ] ])

let exists_with_parameters cursor params db =
  Datalog.Cursor.fold_with_parameters cursor params db ~init:false
    ~f:(fun [] _ -> true)

let mk_exists_query params existentials f =
  Datalog.(
    compile [] (fun [] ->
        with_parameters params (fun params ->
            foreach existentials (fun existentials ->
                where (f params existentials) (yield [])))))

let is_function_slot : Field.t -> _ = function[@ocaml.warning "-4"]
  | Function_slot _ -> true
  | _ -> false

let filter_field f x =
  let open! Syntax in
  filter (fun [x] -> f (Field.decode x)) [x]

let get_all_usages =
  (* CR-someday ncourant: once the datalog API supports something cleaner, use
     it. *)
  let out_tbl, out = rel1_r "out" Cols.[n] in
  let in_tbl, in_ = rel1_r "in_" Cols.[n] in
  let open! Syntax in
  let open! Global_flow_graph in
  let rs =
    [ (let$ [x; y] = ["x"; "y"] in
       [in_ x; usages_rel x y] ==> out y);
      (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
       [ out x;
         used_fields_rel x field y;
         filter_field is_function_slot field;
         usages_rel y z ]
       ==> out z) ]
  in
  fun db s ->
    let db = Datalog.set_table in_tbl s db in
    let db = Datalog.Schedule.run (Datalog.Schedule.saturate rs) db in
    Datalog.get_table out_tbl db

let fieldc_map_to_field_map m =
  Global_flow_graph.FieldC.Map.fold
    (fun k r acc -> Field.Map.add (Field.decode k) r acc)
    m Field.Map.empty

let get_fields =
  (* CR-someday ncourant: likewise here; I find this function particulartly
     ugly. *)
  let out_tbl1, out1 = rel1_r "out1" Cols.[f] in
  let out_tbl2, out2 = rel2_r "out2" Cols.[f; n] in
  let in_tbl, in_ = rel1_r "in_" Cols.[n] in
  let open! Syntax in
  let open! Global_flow_graph in
  let rs =
    [ (let$ [x; field] = ["x"; "field"] in
       [ in_ x;
         used_fields_top_rel x field;
         filter_field (fun x -> Stdlib.not (is_function_slot x)) field ]
       ==> out1 field);
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ in_ x;
         used_fields_rel x field y;
         not (out1 field);
         filter_field (fun x -> Stdlib.not (is_function_slot x)) field ]
       ==> out2 field y) ]
  in
  fun db s ->
    let db = Datalog.set_table in_tbl s db in
    let db =
      List.fold_left
        (fun db r -> Datalog.Schedule.(run (saturate [r])) db)
        db rs
    in
    fieldc_map_to_field_map
      (FieldC.Map.merge
         (fun k x y ->
           match x, y with
           | None, None -> assert false
           | Some _, Some _ ->
             Misc.fatal_errorf "Got two results for field %a" Field.print
               (Field.decode k)
           | Some (), None -> Some None
           | None, Some m -> Some (Some m))
         (Datalog.get_table out_tbl1 db)
         (Datalog.get_table out_tbl2 db))

type set_of_closures_def =
  | Not_a_set_of_closures
  | Set_of_closures of (Function_slot.t * Code_id_or_name.t) list

let get_set_of_closures_def =
  let q =
    Datalog.(
      compile [] (fun [] ->
          with_parameters ["x"] (fun [x] ->
              foreach ["field"; "y"] (fun [field; y] ->
                  where
                    [ Global_flow_graph.constructor_rel x field y;
                      filter_field is_function_slot field ]
                    (yield [field; y])))))
  in
  fun db v ->
    let l =
      Datalog.Cursor.fold_with_parameters q [v] db ~init:[] ~f:(fun [f; y] l ->
          ( (match[@ocaml.warning "-4"] Field.decode f with
            | Function_slot fs -> fs
            | _ -> assert false),
            y )
          :: l)
    in
    match l with [] -> Not_a_set_of_closures | _ :: _ -> Set_of_closures l

let used_pred_query =
  let open! Global_flow_graph in
  mk_exists_query ["X"] [] (fun [x] [] -> [used_pred x])

let is_top db x = exists_with_parameters used_pred_query [x] db

let has_use, field_used =
  let open! Global_flow_graph in
  let usages_query =
    mk_exists_query ["X"] ["Y"] (fun [x] [y] -> [usages_rel x y])
  in
  let used_field_top_query =
    mk_exists_query ["X"; "F"] ["U"] (fun [x; f] [u] ->
        [usages_rel x u; used_fields_top_rel u f])
  in
  let used_field_query =
    mk_exists_query ["X"; "F"] ["U"; "V"] (fun [x; f] [u; v] ->
        [usages_rel x u; used_fields_rel u f v])
  in
  ( (fun db x ->
      exists_with_parameters used_pred_query [x] db
      || exists_with_parameters usages_query [x] db),
    fun db x field ->
      let field = Field.encode field in
      exists_with_parameters used_pred_query [x] db
      || exists_with_parameters used_field_top_query [x; field] db
      || exists_with_parameters used_field_query [x; field] db )

let has_source =
  let open! Global_flow_graph in
  let any_source_query =
    mk_exists_query ["X"] [] (fun [x] [] -> [any_source_pred x])
  in
  let has_source_query =
    mk_exists_query ["X"] ["Y"] (fun [x] [y] -> [sources_rel x y])
  in
  fun db x ->
    exists_with_parameters any_source_query [x] db
    || exists_with_parameters has_source_query [x] db

let not_local_field_has_source =
  let open! Global_flow_graph in
  let any_source_query =
    mk_exists_query ["X"] [] (fun [x] [] -> [any_source_pred x])
  in
  let field_any_source_query =
    mk_exists_query ["X"; "F"] ["S"] (fun [x; f] [s] ->
        [sources_rel x s; field_top_sources_rel s f])
  in
  let field_source_query =
    mk_exists_query ["X"; "F"] ["S"; "V"] (fun [x; f] [s; v] ->
        [sources_rel x s; field_sources_rel s f v])
  in
  fun db x field ->
    let field = Field.encode field in
    exists_with_parameters any_source_query [x] db
    || exists_with_parameters field_any_source_query [x; field] db
    || exists_with_parameters field_source_query [x; field] db

let field_of_constructor_is_used =
  rel2 "field_of_constructor_is_used" Cols.[n; f]

let cannot_change_representation0 = rel1 "cannot_change_representation0" Cols.[n]

let cannot_change_representation1 = rel1 "cannot_change_representation1" Cols.[n]

let cannot_change_representation = rel1 "cannot_change_representation" Cols.[n]

let cannot_unbox0 = rel1 "cannot_unbox0" Cols.[n]

let cannot_unbox = rel1 "cannot_unbox" Cols.[n]

let to_unbox = rel1 "to_unbox" Cols.[n]

let to_change_representation = rel1 "to_change_representation" Cols.[n]

let datalog_rules =
  let open! Syntax in
  let open! Global_flow_graph in
  let field_cannot_be_destructured (i : Field.t) =
    match[@ocaml.warning "-4"] i with
    | Code_of_closure | Apply _ -> true
    | _ -> false
  in
  let real_field (i : Field.t) =
    match[@ocaml.warning "-4"] i with
    | Code_of_closure | Apply _ -> false
    | _ -> true
  in
  let relation_prevents_unboxing : Field.t -> _ = function
    | Block _ | Value_slot _ -> false
    | Function_slot _ -> false (* todo *)
    | Code_of_closure | Is_int | Get_tag -> true
    | Apply _ -> true (* todo? *)
  in
  let is_code_field : Field.t -> _ = function[@ocaml.warning "-4"]
    | Code_of_closure -> true
    | _ -> false
  in
  let is_apply_field : Field.t -> _ = function[@ocaml.warning "-4"]
    | Apply _ -> true
    | _ -> false
  in
  [ (let$ [base; relation; from] = ["base"; "relation"; "from"] in
     [constructor_rel base relation from; used_pred base]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; usage] =
       ["base"; "relation"; "from"; "usage"]
     in
     [ constructor_rel base relation from;
       usages_rel base usage;
       used_fields_top_rel usage relation ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; usage; _v] =
       ["base"; "relation"; "from"; "usage"; "_v"]
     in
     [ constructor_rel base relation from;
       usages_rel base usage;
       used_fields_rel usage relation _v ]
     ==> field_of_constructor_is_used base relation);
    (let$ [base; relation; from; coderel; indirect_call_witness] =
       ["base"; "relation"; "from"; "coderel"; "indirect_call_witness"]
     in
     [ constructor_rel base relation from;
       filter_field is_apply_field relation;
       constructor_rel base coderel indirect_call_witness;
       used_pred indirect_call_witness;
       filter_field is_code_field coderel ]
     ==> field_of_constructor_is_used base relation);
    (let$ [x] = ["x"] in
     [used_pred x] ==> cannot_change_representation0 x);
    (let$ [allocation_id; alias; alias_source; field; _v] =
       ["allocation_id"; "alias"; "alias_source"; "field"; "_v"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       not_equal alias_source allocation_id;
       filter_field real_field field;
       used_fields_rel alias field _v ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; alias_source; field] =
       ["allocation_id"; "alias"; "alias_source"; "field"]
     in
     [ usages_rel allocation_id alias;
       sources_rel alias alias_source;
       not_equal alias_source allocation_id;
       filter_field real_field field;
       used_fields_top_rel alias field ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; field; _v] =
       ["allocation_id"; "alias"; "field"; "_v"]
     in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       filter_field real_field field;
       used_fields_rel alias field _v ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; alias; field] = ["allocation_id"; "alias"; "field"] in
     [ usages_rel allocation_id alias;
       any_source_pred alias;
       filter_field real_field field;
       used_fields_top_rel alias field ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id; source] = ["allocation_id"; "source"] in
     [sources_rel allocation_id source; not_equal source allocation_id]
     ==> cannot_change_representation0 allocation_id);
    (* Used but not its own source: either from any source, or it has no source
       at all and it is dead code. In either case, do not unbox *)
    (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
     [ usages_rel allocation_id usage;
       not (sources_rel allocation_id allocation_id) ]
     ==> cannot_change_representation0 allocation_id);
    (let$ [allocation_id] = ["allocation_id"] in
     [any_source_pred allocation_id]
     ==> cannot_change_representation0 allocation_id);
    (* CR-someday ncourant: we completely prevent changing the representation of
       symbols. While allowing them to be unboxed is difficult, due to symbols
       being always values, we could at least change their representation. This
       would require rewriting in the types, which is not done yet. *)
    (let$ [x; _source] = ["x"; "_source"] in
     [ sources_rel x _source;
       filter
         (fun [x] ->
           Code_id_or_name.pattern_match x
             ~symbol:(fun _ -> true)
             ~var:(fun _ -> false)
             ~code_id:(fun _ -> false))
         [x] ]
     ==> cannot_change_representation0 x);
    (let$ [x] = ["x"] in
     [cannot_change_representation0 x] ==> cannot_change_representation1 x);
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ constructor_rel x field y;
       filter_field is_function_slot field;
       cannot_change_representation0 x ]
     ==> cannot_change_representation1 y);
    (let$ [x] = ["x"] in
     [cannot_change_representation1 x] ==> cannot_change_representation x);
    (* Due to value_kinds not taking representation changes into account for
       now, blocks cannot have their representation changed, so we prevent it
       here. *)
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ constructor_rel x field y;
       filter_field
         (fun (f : Field.t) ->
           match f with
           | Block _ | Is_int | Get_tag -> true
           | Value_slot _ | Function_slot _ | Code_of_closure | Apply _ -> false)
         field ]
     ==> cannot_change_representation x);
    (let$ [x] = ["x"] in
     [cannot_change_representation1 x] ==> cannot_unbox0 x);
    (let$ [x] = ["x"] in
     [used_pred x] ==> cannot_unbox0 x);
    (let$ [x; field] = ["x"; "field"] in
     [ field_of_constructor_is_used x field;
       filter_field field_cannot_be_destructured field ]
     ==> cannot_unbox0 x);
    (let$ [alias; allocation_id; relation; to_] =
       ["alias"; "allocation_id"; "relation"; "to_"]
     in
     [ sources_rel alias allocation_id;
       rev_constructor_rel alias relation to_;
       field_of_constructor_is_used to_ relation;
       filter_field relation_prevents_unboxing relation ]
     ==> cannot_unbox0 allocation_id);
    (let$ [alias; allocation_id; relation; to_] =
       ["alias"; "allocation_id"; "relation"; "to_"]
     in
     [ sources_rel alias allocation_id;
       rev_constructor_rel alias relation to_;
       field_of_constructor_is_used to_ relation;
       cannot_change_representation to_ ]
     ==> cannot_unbox0 allocation_id);
    (let$ [x] = ["x"] in
     [cannot_unbox0 x] ==> cannot_unbox x);
    (let$ [x; field; y] = ["x"; "field"; "y"] in
     [ cannot_unbox0 x;
       constructor_rel x field y;
       filter_field is_function_slot field ]
     ==> cannot_unbox y);
    (let$ [x] = ["x"] in
     [used_pred x; not (cannot_unbox x)] ==> to_unbox x);
    (let$ [x; _y] = ["x"; "_y"] in
     [usages_rel x _y; not (cannot_unbox x)] ==> to_unbox x);
    (let$ [x] = ["x"] in
     [used_pred x; not (cannot_change_representation x); not (to_unbox x)]
     ==> to_change_representation x);
    (let$ [x; _y] = ["x"; "_y"] in
     [usages_rel x _y; not (cannot_change_representation x); not (to_unbox x)]
     ==> to_change_representation x) ]

let map_from_allocation_points_to_dominated =
  (* let open! Syntax in let map_rule = let$ [x; y; z] = ["x"; "y"; "z"] in [
     sources_rel x y; sources_rel x z; not_equal y z ] ==>
     multiple_allocation_points x in let dominator_rule = let$ [x; y] = ["x";
     "y"] in [ sources_rel x y; not (multiple_allocation_points x) ] ==>
     dominator y x in [ map_rule; dominator_rule ] *)
  let open! Syntax in
  let sources_query =
    compile ["x"; "y"] (fun [x; y] -> where [sources_rel x y] (yield [x; y]))
  in
  fun db ->
    let h = Hashtbl.create 17 in
    Cursor.iter
      ~f:(fun [x; y] ->
        if Hashtbl.mem h x
        then Hashtbl.replace h x None
        else Hashtbl.add h x (Some y))
      sources_query db;
    Hashtbl.fold
      (fun id elt acc ->
        match elt with
        | None -> acc
        | Some elt ->
          Code_id_or_name.Map.update elt
            (function
              | None -> Some (Code_id_or_name.Set.singleton id)
              | Some set -> Some (Code_id_or_name.Set.add id set))
            acc)
      h Code_id_or_name.Map.empty

let rec mapi_unboxed_fields (not_unboxed : 'a -> 'b -> 'c)
    (unboxed : Field.t -> 'a -> 'a) (acc : 'a) (uf : 'b unboxed_fields) :
    'c unboxed_fields =
  match uf with
  | Not_unboxed x -> Not_unboxed (not_unboxed acc x)
  | Unboxed f ->
    Unboxed
      (Field.Map.mapi
         (fun field uf ->
           mapi_unboxed_fields not_unboxed unboxed (unboxed field acc) uf)
         f)

let map_unboxed_fields f uf =
  mapi_unboxed_fields (fun () x -> f x) (fun _ () -> ()) () uf

(* Note that this depends crucially on the fact that the poison value is not
   nullable. If it was, we could instead keep the subkind but erase the
   nullability part instead. *)
let[@inline] erase kind =
  Flambda_kind.With_subkind.create
    (Flambda_kind.With_subkind.kind kind)
    Flambda_kind.With_subkind.Non_null_value_subkind.Anything
    (Flambda_kind.With_subkind.nullable kind)

let rec rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind =
  (* CR ncourant: rewrite changed representation, or at least replace with Top.
     Not needed while we don't change representation of blocks. *)
  match Flambda_kind.With_subkind.non_null_value_subkind kind with
  | Anything -> kind
  | Tagged_immediate ->
    kind (* Always correct, since poison is a tagged immediate *)
  | Boxed_float32 | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
  | Boxed_vec128 | Float_block _ | Float_array | Immediate_array | Value_array
  | Generic_array | Unboxed_float32_array | Unboxed_int32_array
  | Unboxed_int64_array | Unboxed_nativeint_array | Unboxed_vec128_array
  | Unboxed_product_array ->
    (* For all these subkinds, we don't track fields (for now). Thus, being in
       this case without being top or bottom means that we never use this
       particular value, but that it syntactically looks like it could be used.
       We probably could keep the subkind info, but as this value should not be
       used, it is best to delete it. *)
    erase kind
  | Variant { consts; non_consts } ->
    (* CR ncourant: we should make sure poison is in the consts! *)
    let usages = get_all_usages db flow_to in
    let fields = get_fields db usages in
    let non_consts =
      Tag.Scannable.Map.map
        (fun (shape, kinds) ->
          let kinds =
            List.mapi
              (fun i kind ->
                let field =
                  Global_flow_graph.Field.Block
                    (i, Flambda_kind.With_subkind.kind kind)
                in
                match Field.Map.find_opt field fields with
                | None -> (* maybe poison *) erase kind
                | Some None -> (* top *) kind
                | Some (Some flow_to) ->
                  rewrite_kind_with_subkind_not_top_not_bottom db flow_to kind)
              kinds
          in
          shape, kinds)
        non_consts
    in
    Flambda_kind.With_subkind.create Flambda_kind.value
      (Flambda_kind.With_subkind.Non_null_value_subkind.Variant
         { consts; non_consts })
      (Flambda_kind.With_subkind.nullable kind)

let rewrite_kind_with_subkind uses var kind =
  let db = uses.db in
  let var = Code_id_or_name.name var in
  if is_top db var
  then kind
  else if not (has_use db var)
  then erase kind
  else
    rewrite_kind_with_subkind_not_top_not_bottom db
      (Code_id_or_name.Map.singleton var ())
      kind

let debug = Sys.getenv_opt "REAPERDBG" <> None

let rec mk_unboxed_fields ~has_to_be_unboxed ~mk db usages name_prefix =
  let fields = get_fields db usages in
  Field.Map.filter_map
    (fun field field_use ->
      match field with
      | Function_slot _ -> assert false
      | Apply _ | Code_of_closure -> None
      | Block _ | Value_slot _ | Is_int | Get_tag -> (
        let new_name =
          Flambda_colours.without_colours ~f:(fun () ->
              Format.asprintf "%s_field_%a" name_prefix Field.print field)
        in
        let[@local] default () =
          Some (Not_unboxed (mk (Field.kind field) new_name))
        in
        match field_use with
        | None -> default ()
        | Some flow_to ->
          if Code_id_or_name.Map.is_empty flow_to
          then Misc.fatal_errorf "Empty set in [get_fields]";
          if Code_id_or_name.Map.for_all
               (fun k () -> has_to_be_unboxed k)
               flow_to
          then
            Some
              (Unboxed
                 (mk_unboxed_fields ~has_to_be_unboxed ~mk db
                    (get_all_usages db flow_to)
                    new_name))
          else if Code_id_or_name.Map.exists
                    (fun k () -> has_to_be_unboxed k)
                    flow_to
          then
            Misc.fatal_errorf
              "Field %a of %s flows to both unboxed and non-unboxed variables"
              Field.print field name_prefix
          else default ()))
    fields

let fixpoint (graph : Global_flow_graph.graph) =
  let datalog = Global_flow_graph.to_datalog graph in
  let stats = Datalog.Schedule.create_stats () in
  let db = Datalog.Schedule.run ~stats datalog_schedule datalog in
  let db =
    List.fold_left
      (fun db rule ->
        Datalog.Schedule.run ~stats (Datalog.Schedule.saturate [rule]) db)
      db datalog_rules
  in
  if debug then Format.eprintf "%a@." Datalog.Schedule.print_stats stats;
  if Sys.getenv_opt "DUMPDB" <> None then Format.eprintf "%a@." Datalog.print db;
  let dominated_by_allocation_points =
    map_from_allocation_points_to_dominated db
  in
  let allocation_point_dominator =
    Code_id_or_name.Map.fold
      (fun alloc_point dominated acc ->
        Code_id_or_name.Set.fold
          (fun dom acc -> Code_id_or_name.Map.add dom alloc_point acc)
          dominated acc)
      dominated_by_allocation_points Code_id_or_name.Map.empty
  in
  let unboxed : unboxed Code_id_or_name.Map.t ref =
    ref Code_id_or_name.Map.empty
  in
  let query_to_unbox =
    Datalog.(compile ["X"] (fun [x] -> where [to_unbox x] (yield [x])))
  in
  let query_to_change_representation =
    Datalog.(
      compile ["X"] (fun [x] -> where [to_change_representation x] (yield [x])))
  in
  let to_unbox = Hashtbl.create 17 in
  let to_change_representation = Hashtbl.create 17 in
  Datalog.Cursor.iter query_to_unbox db ~f:(fun [u] ->
      Hashtbl.replace to_unbox u ());
  Datalog.Cursor.iter query_to_change_representation db ~f:(fun [u] ->
      Hashtbl.replace to_change_representation u ());
  let has_to_be_unboxed code_or_name =
    match
      Code_id_or_name.Map.find_opt code_or_name allocation_point_dominator
    with
    | None -> false
    | Some alloc_point -> Hashtbl.mem to_unbox alloc_point
  in
  Hashtbl.iter
    (fun code_or_name () ->
      (* Format.eprintf "%a@." Code_id_or_name.print code_or_name; *)
      let to_patch =
        match
          Code_id_or_name.Map.find_opt code_or_name
            dominated_by_allocation_points
        with
        | None -> Code_id_or_name.Set.empty
        | Some x -> x
      in
      Code_id_or_name.Set.iter
        (fun to_patch ->
          (* CR-someday ncourant: produce ghost makeblocks/set of closures for
             debugging *)
          let new_name =
            Flambda_colours.without_colours ~f:(fun () ->
                Format.asprintf "%a_into_%a" Code_id_or_name.print code_or_name
                  Code_id_or_name.print to_patch)
          in
          let fields =
            mk_unboxed_fields ~has_to_be_unboxed
              ~mk:(fun _kind name -> Variable.create name)
              db
              (get_all_usages db (Code_id_or_name.Map.singleton to_patch ()))
              new_name
          in
          unboxed := Code_id_or_name.Map.add to_patch fields !unboxed)
        to_patch)
    to_unbox;
  if debug
  then
    Format.printf "new vars: %a"
      (Code_id_or_name.Map.print
         (Field.Map.print (pp_unboxed_elt Variable.print)))
      !unboxed;
  let changed_representation = ref Code_id_or_name.Map.empty in
  Hashtbl.iter
    (fun code_id_or_name () ->
      if Code_id_or_name.Map.mem code_id_or_name !changed_representation
      then ()
      else
        let add_to_s repr c =
          Code_id_or_name.Set.iter
            (fun c ->
              changed_representation
                := Code_id_or_name.Map.add c repr !changed_representation)
            (match
               Code_id_or_name.Map.find_opt c dominated_by_allocation_points
             with
            | None -> Code_id_or_name.Set.empty
            | Some s -> s)
        in
        match get_set_of_closures_def db code_id_or_name with
        | Not_a_set_of_closures ->
          let r = ref ~-1 in
          let mk _kind _name =
            (* XXX fixme, disabled for now *)
            (* TODO depending on the kind, use two counters; then produce a
               mixed block; map_unboxed_fields should help with that *)
            incr r;
            ( !r,
              Flambda_primitive.(
                Block_access_kind.Values
                  { tag = Unknown;
                    size = Unknown;
                    field_kind = Block_access_field_kind.Any_value
                  }) )
          in
          let uses =
            get_all_usages db (Code_id_or_name.Map.singleton code_id_or_name ())
          in
          let repr = mk_unboxed_fields ~has_to_be_unboxed ~mk db uses "" in
          add_to_s (Block_representation (repr, !r + 1)) code_id_or_name
        | Set_of_closures l ->
          let mk kind name =
            Value_slot.create (Compilation_unit.get_current_exn ()) ~name kind
          in
          let uses =
            get_all_usages db
              (List.fold_left
                 (fun acc (_, x) -> Code_id_or_name.Map.add x () acc)
                 Code_id_or_name.Map.empty l)
          in
          let repr =
            mk_unboxed_fields ~has_to_be_unboxed ~mk db uses "unboxed"
          in
          let fss =
            List.fold_left
              (fun acc (fs, _) ->
                Function_slot.Map.add fs
                  (Function_slot.create
                     (Compilation_unit.get_current_exn ())
                     ~name:(Function_slot.name fs) Flambda_kind.value)
                  acc)
              Function_slot.Map.empty l
          in
          List.iter
            (fun (fs, f) -> add_to_s (Closure_representation (repr, fss, fs)) f)
            l)
    to_change_representation;
  if debug
  then
    Format.eprintf "@.TO_CHG: %a@."
      (Code_id_or_name.Map.print pp_changed_representation)
      !changed_representation;
  { db;
    unboxed_fields = !unboxed;
    changed_representation =
      !changed_representation
      (* unboxed_fields = Code_id_or_name.Map.empty ; changed_representation =
         Code_id_or_name.Map.empty *)
  }

let print_color { db; unboxed_fields; changed_representation } v =
  if Code_id_or_name.Map.mem v unboxed_fields
  then "#dd2233"
  else if Code_id_or_name.Map.mem v changed_representation
  then "#2255ff"
  else if exists_with_parameters used_pred_query [v] db
  then "#a7a7a7"
  else if has_use db v
  then "#f1c40f"
  else "white"

let get_unboxed_fields uses cn =
  Code_id_or_name.Map.find_opt cn uses.unboxed_fields

let get_changed_representation uses cn =
  Code_id_or_name.Map.find_opt cn uses.changed_representation

let has_use uses v = has_use uses.db v

let field_used uses v f = field_used uses.db v f

let has_source uses v = has_source uses.db v

let not_local_field_has_source uses v f = not_local_field_has_source uses.db v f
