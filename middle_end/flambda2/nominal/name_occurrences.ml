(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

[@@@ocaml.warning "-55"]

(* CR mshinwell: Remove this alias *)
module Kind = Name_mode

module For_one_variety_of_names (N : sig
  include Container_types.S

  val apply_renaming : t -> Renaming.t -> t
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val empty : t

  val is_empty : t -> bool

  val singleton : N.t -> Kind.t -> t

  val add : t -> N.t -> Kind.t -> t

  val apply_renaming : t -> Renaming.t -> t

  val diff : t -> t -> t

  val union : t -> t -> t

  val keys : t -> N.Set.t

  val subset_domain : t -> t -> bool

  val inter_domain_is_non_empty : t -> t -> bool

  val mem : t -> N.t -> bool

  val remove : t -> N.t -> t

  val remove_one_occurrence : t -> N.t -> Kind.t -> t

  val count : t -> N.t -> Num_occurrences.t

  val count_normal : t -> N.t -> Num_occurrences.t

  val greatest_name_mode : t -> N.t -> Kind.Or_absent.t

  val downgrade_occurrences_at_strictly_greater_kind : t -> Kind.t -> t

  val fold : t -> init:'a -> f:('a -> N.t -> 'a) -> 'a

  val for_all : t -> f:(N.t -> bool) -> bool

  val filter : t -> f:(N.t -> bool) -> t
end = struct
  module For_one_name : sig
    type t

    val print : Format.formatter -> t -> unit

    val equal : t -> t -> bool

    val one_occurrence : Kind.t -> t

    val add : t -> Kind.t -> t

    type remove_one_occurrence_result = private
      | No_more_occurrences
      | One_remaining_occurrence of Kind.t
      | Multiple_remaining_occurrences of t

    val remove_one_occurrence : t -> Kind.t -> remove_one_occurrence_result

    val num_occurrences : t -> int

    val num_occurrences_normal : t -> int

    val downgrade_occurrences_at_strictly_greater_kind : t -> Kind.t -> t

    val max_kind_opt : t -> Kind.t option

    val union : t -> t -> t
  end = struct
    (* CR mshinwell: Provide 32-bit implementation. *)

    type t = int

    let num_occurrences_normal t = t land 0xfffff

    let num_occurrences_in_types t =
      (* The constant is computed to avoid problems when the host system is 32
         bit. *)
      (t land (0xfffff lsl 20)) lsr 20

    let num_occurrences_phantom t = (t land (0xfffff lsl 40)) lsr 40

    let encode_normal_occurrences num =
      assert (num >= 0 && num <= 0xfffff);
      (* CR mshinwell: proper error *)
      num

    let encode_in_types_occurrences num =
      assert (num >= 0 && num <= 0xfffff);
      num lsl 20

    let encode_phantom_occurrences num =
      assert (num >= 0 && num <= 0xfffff);
      num lsl 40

    let without_normal_occurrences num = num land lnot 0xfffff

    let without_in_types_occurrences num = num land lnot (0xfffff lsl 20)

    let without_phantom_occurrences num = num land lnot (0xfffff lsl 40)

    let to_map t =
      Kind.Map.empty
      |> Kind.Map.add Kind.normal (num_occurrences_normal t)
      |> Kind.Map.add Kind.in_types (num_occurrences_in_types t)
      |> Kind.Map.add Kind.phantom (num_occurrences_phantom t)

    let [@ocamlformat "disable"] print ppf t =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(by_kind %a)@]\
          )@]"
        (Kind.Map.print Format.pp_print_int) (to_map t)

    let equal t1 t2 = t1 == t2

    let num_occurrences t =
      num_occurrences_normal t + num_occurrences_in_types t
      + num_occurrences_phantom t

    let one_occurrence kind =
      match Kind.descr kind with
      | Normal -> encode_normal_occurrences 1
      | In_types -> encode_in_types_occurrences 1
      | Phantom -> encode_phantom_occurrences 1

    let add t kind =
      match Kind.descr kind with
      | Normal ->
        encode_normal_occurrences (1 + num_occurrences_normal t)
        lor without_normal_occurrences t
      | In_types ->
        encode_in_types_occurrences (1 + num_occurrences_in_types t)
        lor without_in_types_occurrences t
      | Phantom ->
        encode_phantom_occurrences (1 + num_occurrences_phantom t)
        lor without_phantom_occurrences t

    type remove_one_occurrence_result =
      | No_more_occurrences
      | One_remaining_occurrence of Kind.t
      | Multiple_remaining_occurrences of t

    let remove_one_occurrence t kind =
      let t =
        match Kind.descr kind with
        | Normal ->
          let num_occurrences =
            let num = num_occurrences_normal t in
            if num > 0 then num - 1 else 0
          in
          encode_normal_occurrences num_occurrences
          lor without_normal_occurrences t
        | In_types ->
          let num_occurrences =
            let num = num_occurrences_in_types t in
            if num > 0 then num - 1 else 0
          in
          encode_in_types_occurrences num_occurrences
          lor without_in_types_occurrences t
        | Phantom ->
          let num_occurrences =
            let num = num_occurrences_phantom t in
            if num > 0 then num - 1 else 0
          in
          encode_phantom_occurrences num_occurrences
          lor without_phantom_occurrences t
      in
      match num_occurrences t with
      | 0 -> No_more_occurrences
      | 1 ->
        if num_occurrences_normal t = 1
        then One_remaining_occurrence Kind.normal
        else if num_occurrences_phantom t = 1
        then One_remaining_occurrence Kind.phantom
        else if num_occurrences_in_types t = 1
        then One_remaining_occurrence Kind.in_types
        else assert false
      | _ -> Multiple_remaining_occurrences t

    (* CR mshinwell: Add -strict-sequence to the build *)

    let downgrade_occurrences_at_strictly_greater_kind t max_kind =
      match Kind.descr max_kind with
      | Normal -> t
      | In_types ->
        encode_in_types_occurrences
          (num_occurrences_in_types t + num_occurrences_normal t)
      | Phantom ->
        encode_phantom_occurrences
          (num_occurrences_phantom t + num_occurrences_normal t)

    (* CR mshinwell: this is relying on implementation of Name_mode *)
    let max_kind_opt t =
      let num_normal = num_occurrences_normal t in
      if num_normal > 0
      then Some Kind.normal
      else
        let num_in_types = num_occurrences_in_types t in
        if num_in_types > 0
        then Some Kind.in_types
        else
          let num_phantom = num_occurrences_phantom t in
          if num_phantom > 0 then Some Kind.phantom else None

    let union t1 t2 =
      encode_normal_occurrences
        (num_occurrences_normal t1 + num_occurrences_normal t2)
      lor encode_in_types_occurrences
            (num_occurrences_in_types t1 + num_occurrences_in_types t2)
      lor encode_phantom_occurrences
            (num_occurrences_phantom t1 + num_occurrences_phantom t2)
  end

  (* CR mshinwell: This type is a pain, see if it's really worth it *)
  type t =
    | Empty
    | One of N.t * Kind.t
    | Potentially_many of For_one_name.t N.Map.t

  let map t =
    match t with
    | Empty -> N.Map.empty
    | One (name, kind) ->
      let for_one_name = For_one_name.one_occurrence kind in
      N.Map.singleton name for_one_name
    | Potentially_many map -> map

  let [@ocamlformat "disable"] print ppf t =
    N.Map.print For_one_name.print ppf (map t)

  let equal t1 t2 =
    match t1, t2 with
    | Empty, Empty -> true
    | One (n1, kind1), One (n2, kind2) ->
      N.equal n1 n2 && Kind.equal kind1 kind2
    | Potentially_many map1, Potentially_many map2 ->
      N.Map.equal For_one_name.equal map1 map2
    | Empty, Potentially_many map | Potentially_many map, Empty ->
      N.Map.is_empty map
    | One (n1, kind1), Potentially_many map
    | Potentially_many map, One (n1, kind1) -> begin
      match N.Map.get_singleton map with
      | None -> false
      | Some (n2, for_one_name2) ->
        let for_one_name1 = For_one_name.one_occurrence kind1 in
        N.equal n1 n2 && For_one_name.equal for_one_name1 for_one_name2
    end
    | (Empty | One _), _ -> false

  let empty = Empty

  let is_empty t =
    match t with
    | Empty -> true
    | One _ -> false
    | Potentially_many map -> N.Map.is_empty map

  let singleton name kind = One (name, kind)

  let add t name kind =
    match t with
    | Empty -> singleton name kind
    | One (name', kind') ->
      if N.equal name name'
      then
        let for_one_name =
          For_one_name.add (For_one_name.one_occurrence kind') kind
        in
        Potentially_many (N.Map.singleton name for_one_name)
      else
        let map =
          N.Map.empty
          |> N.Map.add name (For_one_name.one_occurrence kind)
          |> N.Map.add name' (For_one_name.one_occurrence kind')
        in
        Potentially_many map
    | Potentially_many map ->
      let map =
        N.Map.update name
          (function
            | None -> Some (For_one_name.one_occurrence kind)
            | Some for_one_name -> Some (For_one_name.add for_one_name kind))
          map
      in
      Potentially_many map

  let apply_renaming t perm =
    match t with
    | Empty -> Empty
    | One (name, kind) ->
      let name' = N.apply_renaming name perm in
      if name == name' then t else One (name', kind)
    | Potentially_many map ->
      let map =
        N.Map.fold
          (fun name for_one_name result ->
            let name = N.apply_renaming name perm in
            N.Map.add name for_one_name result)
          map N.Map.empty
      in
      Potentially_many map

  let diff t1 t2 =
    match t1, t2 with
    | (Empty | One _ | Potentially_many _), Empty -> t1
    | Empty, (One _ | Potentially_many _) -> Empty
    | One (name1, _), One (name2, _) ->
      if N.equal name1 name2 then Empty else t1
    | One (name1, _), Potentially_many map2 ->
      if N.Map.mem name1 map2 then Empty else t1
    | Potentially_many map1, One (name2, _) ->
      (* CR mshinwell: This and the next case could go back to [Empty] *)
      let map = N.Map.remove name2 map1 in
      if N.Map.is_empty map then Empty else Potentially_many map
    | Potentially_many map1, Potentially_many map2 ->
      let map = N.Map.diff_domains map1 map2 in
      if N.Map.is_empty map then Empty else Potentially_many map

  let union t1 t2 =
    match t1, t2 with
    | Empty, Empty -> Empty
    | Empty, (One _ | Potentially_many _) -> t2
    | (One _ | Potentially_many _), Empty -> t1
    | One (name1, kind1), One (name2, kind2) ->
      let map =
        if N.equal name1 name2
        then
          N.Map.empty
          |> N.Map.add name1
               (For_one_name.add (For_one_name.one_occurrence kind1) kind2)
        else
          N.Map.empty
          |> N.Map.add name1 (For_one_name.one_occurrence kind1)
          |> N.Map.add name2 (For_one_name.one_occurrence kind2)
      in
      Potentially_many map
    | One (name, kind), Potentially_many map
    | Potentially_many map, One (name, kind) ->
      let map =
        N.Map.update name
          (function
            | None -> Some (For_one_name.one_occurrence kind)
            | Some for_one_name -> Some (For_one_name.add for_one_name kind))
          map
      in
      Potentially_many map
    | Potentially_many map1, Potentially_many map2 ->
      let map =
        N.Map.union
          (fun _name for_one_name1 for_one_name2 ->
            Some (For_one_name.union for_one_name1 for_one_name2))
          map1 map2
      in
      Potentially_many map

  let keys t =
    match t with
    | Empty -> N.Set.empty
    | One (name, _) -> N.Set.singleton name
    | Potentially_many map -> N.Map.keys map

  let subset_domain t1 t2 =
    match t1, t2 with
    | Empty, (Empty | One _ | Potentially_many _) -> true
    | (One _ | Potentially_many _), Empty -> false
    | One (name1, _), One (name2, _) -> N.equal name1 name2
    | One (name1, _), Potentially_many map2 -> N.Map.mem name1 map2
    | Potentially_many map1, One (name2, _) -> (
      N.Map.is_empty map1
      ||
      match N.Map.get_singleton map1 with
      | Some (name1, _) -> N.equal name1 name2
      | None -> false)
    | Potentially_many map1, Potentially_many map2 ->
      N.Set.subset (N.Map.keys map1) (N.Map.keys map2)

  let inter_domain_is_non_empty t1 t2 =
    match t1, t2 with
    | Empty, (Empty | One _ | Potentially_many _)
    | (One _ | Potentially_many _), Empty ->
      false
    | One (name1, _), One (name2, _) -> N.equal name1 name2
    | One (name, _), Potentially_many map | Potentially_many map, One (name, _)
      ->
      N.Map.mem name map
    | Potentially_many map1, Potentially_many map2 ->
      N.Map.inter_domain_is_non_empty map1 map2

  let mem t name =
    match t with
    | Empty -> false
    | One (name', _) -> N.equal name name'
    | Potentially_many map -> N.Map.mem name map

  let remove t name =
    match t with
    | Empty -> Empty
    | One (name', _) -> if N.equal name name' then Empty else t
    | Potentially_many map ->
      let map = N.Map.remove name map in
      if N.Map.is_empty map then Empty else Potentially_many map

  let remove_one_occurrence t name kind =
    match t with
    | Empty -> Empty
    | One (name', kind') ->
      if N.equal name name' && Kind.equal kind kind' then Empty else t
    | Potentially_many map -> (
      match N.Map.find name map with
      | exception Not_found -> Empty
      | for_one_name -> (
        match For_one_name.remove_one_occurrence for_one_name kind with
        | No_more_occurrences -> Empty
        | One_remaining_occurrence kind -> One (name, kind)
        | Multiple_remaining_occurrences for_one_name ->
          let map = N.Map.add name for_one_name map in
          Potentially_many map))

  let count t name : Num_occurrences.t =
    match t with
    | Empty -> Zero
    | One (name', _) -> if N.equal name name' then One else Zero
    | Potentially_many map -> (
      match N.Map.find name map with
      | exception Not_found -> Zero
      | for_one_name ->
        let num_occurrences = For_one_name.num_occurrences for_one_name in
        assert (num_occurrences >= 0);
        if num_occurrences = 0
        then Zero
        else if num_occurrences = 1
        then One
        else More_than_one)

  let count_normal t name : Num_occurrences.t =
    match t with
    | Empty -> Zero
    | One (name', mode) ->
      if N.equal name name' && Name_mode.is_normal mode then One else Zero
    | Potentially_many map -> (
      match N.Map.find name map with
      | exception Not_found -> Zero
      | for_one_name ->
        let num_occurrences =
          For_one_name.num_occurrences_normal for_one_name
        in
        assert (num_occurrences >= 0);
        if num_occurrences = 0
        then Zero
        else if num_occurrences = 1
        then One
        else More_than_one)

  let greatest_name_mode t name : Kind.Or_absent.t =
    match t with
    | Empty -> Kind.Or_absent.absent
    | One (name', kind) ->
      if N.equal name name'
      then Kind.Or_absent.present kind
      else Kind.Or_absent.absent
    | Potentially_many map -> (
      match N.Map.find name map with
      | exception Not_found -> Kind.Or_absent.absent
      | for_one_name -> (
        match For_one_name.max_kind_opt for_one_name with
        | None -> Kind.Or_absent.absent
        | Some kind -> Kind.Or_absent.present kind))

  let fold t ~init ~f =
    match t with
    | Empty -> init
    | One (name, _kind) -> f init name
    | Potentially_many map ->
      N.Map.fold (fun name _kind acc -> f acc name) map init

  let downgrade_occurrences_at_strictly_greater_kind t max_kind =
    (* CR-someday mshinwell: This can be condensed when the compiler removes the
       closure allocation if [max_kind] is captured. *)
    match Kind.descr max_kind with
    | Normal -> t
    | Phantom -> begin
      match t with
      | Empty -> Empty
      | One (name, kind) -> begin
        match Kind.descr kind with
        | Normal | Phantom -> One (name, Kind.phantom)
        | In_types ->
          Misc.fatal_errorf "Cannot downgrade [In_types] to [Phantom]:@ %a"
            print t
      end
      | Potentially_many map ->
        let map =
          N.Map.map
            (fun for_one_name ->
              For_one_name.downgrade_occurrences_at_strictly_greater_kind
                for_one_name Kind.phantom)
            map
        in
        Potentially_many map
    end
    | In_types -> begin
      match t with
      | Empty -> Empty
      | One (name, kind) -> begin
        match Kind.descr kind with
        | Normal | In_types -> One (name, Kind.in_types)
        | Phantom ->
          Misc.fatal_errorf "Cannot downgrade [Phantom] to [In_types]:@ %a"
            print t
      end
      | Potentially_many map ->
        let map =
          N.Map.map
            (fun for_one_name ->
              For_one_name.downgrade_occurrences_at_strictly_greater_kind
                for_one_name Kind.in_types)
            map
        in
        Potentially_many map
    end

  let for_all t ~f =
    match t with
    | Empty -> true
    | One (name, _) -> f name
    | Potentially_many map -> N.Map.for_all (fun name _ -> f name) map

  let filter t ~f =
    match t with
    | Empty -> t
    | One (name, _) -> if f name then t else Empty
    | Potentially_many map ->
      let map = N.Map.filter (fun name _ -> f name) map in
      if N.Map.is_empty map then Empty else Potentially_many map
end

[@@@inlined always]

module For_names = For_one_variety_of_names (struct
  include Name

  let apply_renaming t perm = Renaming.apply_name perm t
end)

module For_continuations = For_one_variety_of_names (struct
  include Continuation

  let apply_renaming t perm = Renaming.apply_continuation perm t
end)

module For_closure_vars = For_one_variety_of_names (struct
  include Var_within_closure

  (* We never bind [Var_within_closure]s using [Name_abstraction]. *)
  let apply_renaming t _perm = t
end)

module For_code_ids = For_one_variety_of_names (struct
  include Code_id

  (* We never bind [Code_id]s using [Name_abstraction]. *)
  let apply_renaming t perm = Renaming.apply_code_id perm t
end)

type t =
  { names : For_names.t;
    continuations : For_continuations.t;
    continuations_with_traps : For_continuations.t;
    continuations_in_trap_actions : For_continuations.t;
    closure_vars : For_closure_vars.t;
    code_ids : For_code_ids.t;
    newer_version_of_code_ids : For_code_ids.t
        (* [newer_version_of_code_ids] tracks those code IDs that occur in
           "newer version of" fields (e.g. in
           [Flambda_static.Static_part.code]). *)
  }

let empty =
  { names = For_names.empty;
    continuations = For_continuations.empty;
    continuations_with_traps = For_continuations.empty;
    continuations_in_trap_actions = For_continuations.empty;
    closure_vars = For_closure_vars.empty;
    code_ids = For_code_ids.empty;
    newer_version_of_code_ids = For_code_ids.empty
  }

let [@ocamlformat "disable"] print ppf ({ names; continuations; continuations_with_traps;
                 continuations_in_trap_actions;
                 closure_vars; code_ids; newer_version_of_code_ids; } as t) =
  if t = empty then
    Format.fprintf ppf "no_occurrences"
  else
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(names %a)@]@ \
      @[<hov 1>(continuations %a)@]@ \
      @[<hov 1>(continuations_with_traps %a)@]@ \
      @[<hov 1>(continuations_in_trap_actions %a)@]@ \
      @[<hov 1>(closure_vars %a)@]@ \
      @[<hov 1>(code_ids %a)@] \
      @[<hov 1>(newer_version_of_code_ids %a)@]@ \
      @]"
    For_names.print names
    For_continuations.print continuations
    For_continuations.print continuations_with_traps
    For_continuations.print continuations_in_trap_actions
    For_closure_vars.print closure_vars
    For_code_ids.print code_ids
    For_code_ids.print newer_version_of_code_ids

let singleton_continuation cont =
  { empty with continuations = For_continuations.singleton cont Kind.normal }

let singleton_continuation_in_trap_action cont =
  { empty with
    continuations_in_trap_actions = For_continuations.singleton cont Kind.normal
  }

let add_continuation t cont ~has_traps =
  let continuations = For_continuations.add t.continuations cont Kind.normal in
  let continuations_with_traps =
    if has_traps
    then For_continuations.add t.continuations_with_traps cont Kind.normal
    else t.continuations_with_traps
  in
  { t with continuations; continuations_with_traps }

let add_continuation_in_trap_action t cont =
  { t with
    continuations_in_trap_actions =
      For_continuations.add t.continuations_in_trap_actions cont Kind.normal
  }

let count_continuation t cont = For_continuations.count t.continuations cont

let continuation_is_applied_with_traps t cont =
  For_continuations.mem t.continuations_with_traps cont

let count_variable t var = For_names.count t.names (Name.var var)

let count_variable_normal_mode t var =
  For_names.count_normal t.names (Name.var var)

let singleton_variable var kind =
  { empty with names = For_names.singleton (Name.var var) kind }

let add_variable t var kind =
  { t with names = For_names.add t.names (Name.var var) kind }

let add_symbol t sym kind =
  { t with names = For_names.add t.names (Name.symbol sym) kind }

let add_name t name kind = { t with names = For_names.add t.names name kind }

let add_closure_var t clos_var kind =
  { t with closure_vars = For_closure_vars.add t.closure_vars clos_var kind }

let add_code_id t id kind =
  { t with code_ids = For_code_ids.add t.code_ids id kind }

let singleton_code_id id kind = add_code_id empty id kind

let add_newer_version_of_code_id t id kind =
  { t with
    newer_version_of_code_ids =
      For_code_ids.add t.newer_version_of_code_ids id kind
  }

let singleton_symbol sym kind =
  { empty with names = For_names.singleton (Name.symbol sym) kind }

let singleton_name name kind =
  { empty with names = For_names.singleton name kind }

let create_variables vars kind =
  let names =
    Variable.Set.fold
      (fun var names -> For_names.add names (Name.var var) kind)
      vars For_names.empty
  in
  { empty with names }

let create_variables' name_mode vars = create_variables vars name_mode

let create_names names kind =
  let names =
    Name.Set.fold
      (fun name names -> For_names.add names name kind)
      names For_names.empty
  in
  { empty with names }

let create_closure_vars clos_vars =
  let closure_vars =
    Var_within_closure.Set.fold
      (fun clos_var closure_vars ->
        For_closure_vars.add closure_vars clos_var Name_mode.normal)
      clos_vars For_closure_vars.empty
  in
  { empty with closure_vars }

let binary_conjunction ~for_names ~for_continuations ~for_closure_vars
    ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      closure_vars = closure_vars1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      closure_vars = closure_vars2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  for_names names1 names2
  && for_continuations continuations1 continuations2
  && for_continuations continuations_with_traps1 continuations_with_traps2
  && for_continuations continuations_in_trap_actions1
       continuations_in_trap_actions2
  && for_closure_vars closure_vars1 closure_vars2
  && for_code_ids code_ids1 code_ids2
  && for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_disjunction ~for_names ~for_continuations ~for_closure_vars
    ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      closure_vars = closure_vars1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      closure_vars = closure_vars2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  for_names names1 names2
  || for_continuations continuations1 continuations2
  || for_continuations continuations_with_traps1 continuations_with_traps2
  || for_continuations continuations_in_trap_actions1
       continuations_in_trap_actions2
  || for_closure_vars closure_vars1 closure_vars2
  || for_code_ids code_ids1 code_ids2
  || for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_op ~for_names ~for_continuations ~for_closure_vars ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      closure_vars = closure_vars1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      closure_vars = closure_vars2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  let names = for_names names1 names2 in
  let continuations = for_continuations continuations1 continuations2 in
  let continuations_with_traps =
    for_continuations continuations_with_traps1 continuations_with_traps2
  in
  let continuations_in_trap_actions =
    for_continuations continuations_in_trap_actions1
      continuations_in_trap_actions2
  in
  let closure_vars = for_closure_vars closure_vars1 closure_vars2 in
  let code_ids = for_code_ids code_ids1 code_ids2 in
  let newer_version_of_code_ids =
    for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    closure_vars;
    code_ids;
    newer_version_of_code_ids
  }

let diff
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      closure_vars = closure_vars1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      closure_vars = closure_vars2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  let names = For_names.diff names1 names2 in
  let continuations = For_continuations.diff continuations1 continuations2 in
  let continuations_with_traps =
    For_continuations.diff continuations_with_traps1 continuations_with_traps2
  in
  let continuations_in_trap_actions =
    For_continuations.diff continuations_in_trap_actions1
      continuations_in_trap_actions2
  in
  let closure_vars = For_closure_vars.diff closure_vars1 closure_vars2 in
  let code_ids = For_code_ids.diff code_ids1 code_ids2 in
  let newer_version_of_code_ids =
    For_code_ids.diff newer_version_of_code_ids1
      (* Note special case here: *)
      (For_code_ids.union newer_version_of_code_ids2 code_ids2)
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    closure_vars;
    code_ids;
    newer_version_of_code_ids
  }

let union t1 t2 =
  binary_op ~for_names:For_names.union
    ~for_continuations:For_continuations.union
    ~for_closure_vars:For_closure_vars.union ~for_code_ids:For_code_ids.union t1
    t2

let equal t1 t2 =
  binary_conjunction ~for_names:For_names.equal
    ~for_continuations:For_continuations.equal
    ~for_closure_vars:For_closure_vars.equal ~for_code_ids:For_code_ids.equal t1
    t2

let is_empty t = equal t empty

(* CR mshinwell: It may be worth caching this or similar *)
let no_variables t =
  For_names.for_all t.names ~f:(fun var -> not (Name.is_var var))

let no_continuations
    { names = _;
      continuations;
      continuations_with_traps = _;
      continuations_in_trap_actions;
      closure_vars = _;
      code_ids = _;
      newer_version_of_code_ids = _
    } =
  (* Note: continuations_with_traps is included in continuations *)
  For_continuations.is_empty continuations
  && For_continuations.is_empty continuations_in_trap_actions

(* let has_only_symbols_and_code_ids ({ names = _; continuations;
   continuations_in_trap_actions; closure_vars; code_ids = _;
   newer_version_of_code_ids = _; } as t) = no_variables t &&
   For_continuations.is_empty continuations && For_continuations.is_empty
   continuations_in_trap_actions && For_closure_vars.is_empty closure_vars *)

let subset_domain t1 t2 =
  binary_conjunction ~for_names:For_names.subset_domain
    ~for_continuations:For_continuations.subset_domain
    ~for_closure_vars:For_closure_vars.subset_domain
    ~for_code_ids:For_code_ids.subset_domain t1 t2

let inter_domain_is_non_empty t1 t2 =
  binary_disjunction ~for_names:For_names.inter_domain_is_non_empty
    ~for_continuations:For_continuations.inter_domain_is_non_empty
    ~for_closure_vars:For_closure_vars.inter_domain_is_non_empty
    ~for_code_ids:For_code_ids.inter_domain_is_non_empty t1 t2

let rec union_list ts =
  match ts with [] -> empty | t :: ts -> union t (union_list ts)

let closure_vars t = For_closure_vars.keys t.closure_vars

let symbols t = For_names.keys t.names |> Name.set_to_symbol_set

let continuations t = For_continuations.keys t.continuations

let continuations_with_traps t =
  For_continuations.keys t.continuations_with_traps

let continuations_including_in_trap_actions t =
  Continuation.Set.union
    (For_continuations.keys t.continuations)
    (For_continuations.keys t.continuations_in_trap_actions)

let code_ids t = For_code_ids.keys t.code_ids

let newer_version_of_code_ids t = For_code_ids.keys t.newer_version_of_code_ids

let code_ids_and_newer_version_of_code_ids t =
  Code_id.Set.union (code_ids t) (newer_version_of_code_ids t)

let only_newer_version_of_code_ids t =
  Code_id.Set.diff (newer_version_of_code_ids t) (code_ids t)

let mem_name t name = For_names.mem t.names name

let mem_var t var = For_names.mem t.names (Name.var var)

let mem_symbol t symbol = For_names.mem t.names (Name.symbol symbol)

let mem_code_id t code_id = For_code_ids.mem t.code_ids code_id

let mem_newer_version_of_code_id t code_id =
  For_code_ids.mem t.newer_version_of_code_ids code_id

let mem_closure_var t closure_var =
  For_closure_vars.mem t.closure_vars closure_var

let closure_var_is_used_or_imported t closure_var =
  Var_within_closure.is_imported closure_var
  || For_closure_vars.mem t.closure_vars closure_var

let remove_var t var =
  if For_names.is_empty t.names
  then t
  else
    let names = For_names.remove t.names (Name.var var) in
    { t with names }

let remove_symbol t symbol =
  if For_names.is_empty t.names
  then t
  else
    let names = For_names.remove t.names (Name.symbol symbol) in
    { t with names }

let remove_code_id t code_id =
  if For_code_ids.is_empty t.code_ids
     && For_code_ids.is_empty t.newer_version_of_code_ids
  then t
  else
    let code_ids = For_code_ids.remove t.code_ids code_id in
    let newer_version_of_code_ids =
      For_code_ids.remove t.newer_version_of_code_ids code_id
    in
    { t with code_ids; newer_version_of_code_ids }

let remove_code_id_or_symbol t (cis : Code_id_or_symbol.t) =
  match cis with
  | Code_id code_id -> remove_code_id t code_id
  | Symbol symbol -> remove_symbol t symbol

let remove_continuation t k =
  if For_continuations.is_empty t.continuations
     && For_continuations.is_empty t.continuations_in_trap_actions
  then t
  else
    let continuations = For_continuations.remove t.continuations k in
    let continuations_with_traps =
      For_continuations.remove t.continuations_with_traps k
    in
    let continuations_in_trap_actions =
      For_continuations.remove t.continuations_in_trap_actions k
    in
    { t with
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions
    }

let remove_one_occurrence_of_closure_var t closure_var name_mode =
  if For_closure_vars.is_empty t.closure_vars
  then t
  else
    let closure_vars =
      For_closure_vars.remove_one_occurrence t.closure_vars closure_var
        name_mode
    in
    { t with closure_vars }

let greatest_name_mode_var t var =
  For_names.greatest_name_mode t.names (Name.var var)

let downgrade_occurrences_at_strictly_greater_kind
    { names;
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions;
      closure_vars;
      code_ids;
      newer_version_of_code_ids
    } max_kind =
  (* CR mshinwell: Don't reallocate the record if nothing changed *)
  let names =
    For_names.downgrade_occurrences_at_strictly_greater_kind names max_kind
  in
  let continuations =
    For_continuations.downgrade_occurrences_at_strictly_greater_kind
      continuations max_kind
  in
  let continuations_with_traps =
    For_continuations.downgrade_occurrences_at_strictly_greater_kind
      continuations_with_traps max_kind
  in
  let continuations_in_trap_actions =
    For_continuations.downgrade_occurrences_at_strictly_greater_kind
      continuations_in_trap_actions max_kind
  in
  let closure_vars =
    For_closure_vars.downgrade_occurrences_at_strictly_greater_kind closure_vars
      max_kind
  in
  let code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_kind code_ids
      max_kind
  in
  let newer_version_of_code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_kind
      newer_version_of_code_ids max_kind
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    closure_vars;
    code_ids;
    newer_version_of_code_ids
  }

let with_only_variables { names; _ } =
  let names = For_names.filter names ~f:Name.is_var in
  { empty with names }

let with_only_names_and_code_ids_promoting_newer_version_of
    { names; code_ids; newer_version_of_code_ids; _ } =
  let code_ids = For_code_ids.union code_ids newer_version_of_code_ids in
  { empty with names; code_ids }

let without_names_or_continuations
    { names = _;
      continuations = _;
      continuations_with_traps = _;
      continuations_in_trap_actions = _;
      closure_vars;
      code_ids;
      newer_version_of_code_ids
    } =
  { empty with closure_vars; code_ids; newer_version_of_code_ids }

let without_code_ids t =
  { t with
    code_ids = For_code_ids.empty;
    newer_version_of_code_ids = For_code_ids.empty
  }

let without_closure_vars t = { t with closure_vars = For_closure_vars.empty }

let fold_names t ~init ~f = For_names.fold t.names ~init ~f

let fold_variables t ~init ~f =
  For_names.fold t.names ~init ~f:(fun acc name ->
      Name.pattern_match name ~var:(fun var -> f acc var) ~symbol:(fun _ -> acc))

let fold_continuations_including_in_trap_actions t ~init ~f =
  (* Note: continuations_with_traps is included in continuations *)
  let init = For_continuations.fold t.continuations ~init ~f in
  For_continuations.fold t.continuations_in_trap_actions ~init ~f

let filter_names t ~f =
  let names = For_names.filter t.names ~f in
  { t with names }

let fold_code_ids t ~init ~f = For_code_ids.fold t.code_ids ~init ~f

let apply_renaming
    ({ names;
       continuations;
       continuations_with_traps;
       continuations_in_trap_actions;
       closure_vars;
       code_ids;
       newer_version_of_code_ids
     } as t) renaming =
  if Renaming.is_empty renaming
  then t
  else
    let names = For_names.apply_renaming names renaming in
    let continuations =
      For_continuations.apply_renaming continuations renaming
    in
    let continuations_with_traps =
      For_continuations.apply_renaming continuations_with_traps renaming
    in
    let continuations_in_trap_actions =
      For_continuations.apply_renaming continuations_in_trap_actions renaming
    in
    let closure_vars = For_closure_vars.apply_renaming closure_vars renaming in
    let code_ids = For_code_ids.apply_renaming code_ids renaming in
    let newer_version_of_code_ids =
      For_code_ids.apply_renaming newer_version_of_code_ids renaming
    in
    { names;
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions;
      closure_vars;
      code_ids;
      newer_version_of_code_ids
    }

let restrict_to_closure_vars
    { names = _;
      continuations = _;
      continuations_with_traps = _;
      continuations_in_trap_actions = _;
      closure_vars;
      code_ids = _;
      newer_version_of_code_ids = _
    } =
  { empty with closure_vars }
