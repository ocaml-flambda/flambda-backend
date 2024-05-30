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

module For_one_variety_of_names (N : sig
  include Container_types.S

  val apply_renaming : t -> Renaming.t -> t
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val empty : t

  val is_empty : t -> bool

  val singleton : N.t -> Name_mode.t -> t

  val add : t -> N.t -> Name_mode.t -> t

  val apply_renaming : t -> Renaming.t -> t

  val affected_by_renaming : t -> Renaming.t -> bool

  val diff : t -> t -> t

  val union : t -> t -> t

  val keys : t -> N.Set.t

  val subset_domain : t -> t -> bool

  val inter_domain_is_non_empty : t -> t -> bool

  val mem : t -> N.t -> bool

  val remove : t -> N.t -> t

  val count : t -> N.t -> Num_occurrences.t

  val count_normal : t -> N.t -> Num_occurrences.t

  val greatest_name_mode : t -> N.t -> Name_mode.Or_absent.t

  val downgrade_occurrences_at_strictly_greater_name_mode :
    t -> Name_mode.t -> t

  val fold : t -> init:'a -> f:('a -> N.t -> 'a) -> 'a

  val fold_with_mode : t -> init:'a -> f:('a -> N.t -> Name_mode.t -> 'a) -> 'a

  val for_all : t -> f:(N.t -> bool) -> bool

  val filter : t -> f:(N.t -> bool) -> t

  val increase_counts : t -> t
end = struct
  module For_one_name : sig
    type t

    val print : Format.formatter -> t -> unit

    val equal : t -> t -> bool

    val one_occurrence : Name_mode.t -> t

    val add : t -> Name_mode.t -> t

    val num_occurrences : t -> int

    val num_occurrences_normal : t -> int

    val downgrade_occurrences_at_strictly_greater_name_mode :
      t -> Name_mode.t -> t

    val max_name_mode_opt : t -> Name_mode.t option

    val union : t -> t -> t

    val increase_count : t -> t
  end = struct
    (* CR mshinwell: Provide 32-bit implementation? Probably not worth it now I
       suppose. *)

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
      Name_mode.Map.empty
      |> Name_mode.Map.add Name_mode.normal (num_occurrences_normal t)
      |> Name_mode.Map.add Name_mode.in_types (num_occurrences_in_types t)
      |> Name_mode.Map.add Name_mode.phantom (num_occurrences_phantom t)

    let [@ocamlformat "disable"] print ppf t =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(by_name_mode %a)@]\
          )@]"
        (Name_mode.Map.print Format.pp_print_int) (to_map t)

    let equal t1 t2 = t1 == t2

    let num_occurrences t =
      num_occurrences_normal t + num_occurrences_in_types t
      + num_occurrences_phantom t

    let one_occurrence (name_mode : Name_mode.t) =
      match name_mode with
      | Normal -> encode_normal_occurrences 1
      | In_types -> encode_in_types_occurrences 1
      | Phantom -> encode_phantom_occurrences 1

    let add t (name_mode : Name_mode.t) =
      match name_mode with
      | Normal ->
        encode_normal_occurrences (1 + num_occurrences_normal t)
        lor without_normal_occurrences t
      | In_types ->
        encode_in_types_occurrences (1 + num_occurrences_in_types t)
        lor without_in_types_occurrences t
      | Phantom ->
        encode_phantom_occurrences (1 + num_occurrences_phantom t)
        lor without_phantom_occurrences t

    let downgrade_occurrences_at_strictly_greater_name_mode t
        (max_name_mode : Name_mode.t) =
      match max_name_mode with
      | Normal -> t
      | In_types ->
        encode_in_types_occurrences
          (num_occurrences_in_types t + num_occurrences_normal t)
      | Phantom ->
        encode_phantom_occurrences
          (num_occurrences_phantom t + num_occurrences_normal t)

    let max_name_mode_opt t =
      let num_normal = num_occurrences_normal t in
      if num_normal > 0
      then Some Name_mode.normal
      else
        let num_in_types = num_occurrences_in_types t in
        if num_in_types > 0
        then Some Name_mode.in_types
        else
          let num_phantom = num_occurrences_phantom t in
          if num_phantom > 0 then Some Name_mode.phantom else None

    let union t1 t2 =
      encode_normal_occurrences
        (num_occurrences_normal t1 + num_occurrences_normal t2)
      lor encode_in_types_occurrences
            (num_occurrences_in_types t1 + num_occurrences_in_types t2)
      lor encode_phantom_occurrences
            (num_occurrences_phantom t1 + num_occurrences_phantom t2)

    let increase_count t =
      let increase_if_not_zero n = if n = 0 then n else succ n in
      encode_normal_occurrences
        (increase_if_not_zero (num_occurrences_normal t))
      lor encode_in_types_occurrences
            (increase_if_not_zero (num_occurrences_in_types t))
      lor encode_phantom_occurrences
            (increase_if_not_zero (num_occurrences_phantom t))
  end

  type t = For_one_name.t N.Map.t

  let [@ocamlformat "disable"] print ppf t =
    N.Map.print For_one_name.print ppf t

  let equal t1 t2 = N.Map.equal For_one_name.equal t1 t2

  let empty = N.Map.empty

  let is_empty t = N.Map.is_empty t

  let singleton name name_mode =
    N.Map.singleton name (For_one_name.one_occurrence name_mode)

  let add t name name_mode =
    N.Map.update name
      (function
        | None -> Some (For_one_name.one_occurrence name_mode)
        | Some for_one_name -> Some (For_one_name.add for_one_name name_mode))
      t

  let apply_renaming t renaming =
    N.Map.map_keys (fun name -> N.apply_renaming name renaming) t

  let affected_by_renaming t renaming =
    (* CR lmaurer: This is ultimately just [N.Map.inter_domain_is_not_equal]. *)
    N.Map.exists
      (fun name _name_mode ->
        not (N.equal name (N.apply_renaming name renaming)))
      t

  let diff t1 t2 = N.Map.diff_domains t1 t2

  let union t1 t2 =
    N.Map.union
      (fun _name for_one_name1 for_one_name2 ->
        Some (For_one_name.union for_one_name1 for_one_name2))
      t1 t2

  let keys t = N.Map.keys t

  let subset_domain t1 t2 =
    (* CR lmaurer: Add this operation to [Patricia_tree]. ([N.Map.keys] being
       O(n lg n) makes this especially painful.) *)
    N.Set.subset (N.Map.keys t1) (N.Map.keys t2)

  let inter_domain_is_non_empty t1 t2 = N.Map.inter_domain_is_non_empty t1 t2

  let mem t name = N.Map.mem name t

  let remove t name = N.Map.remove name t

  let count t name : Num_occurrences.t =
    match N.Map.find name t with
    | exception Not_found -> Zero
    | for_one_name ->
      let num_occurrences = For_one_name.num_occurrences for_one_name in
      assert (num_occurrences >= 0);
      if num_occurrences = 0
      then Zero
      else if num_occurrences = 1
      then One
      else More_than_one

  let count_normal t name : Num_occurrences.t =
    match N.Map.find name t with
    | exception Not_found -> Zero
    | for_one_name ->
      let num_occurrences = For_one_name.num_occurrences_normal for_one_name in
      assert (num_occurrences >= 0);
      if num_occurrences = 0
      then Zero
      else if num_occurrences = 1
      then One
      else More_than_one

  let greatest_name_mode t name : Name_mode.Or_absent.t =
    match N.Map.find name t with
    | exception Not_found -> Name_mode.Or_absent.absent
    | for_one_name -> (
      match For_one_name.max_name_mode_opt for_one_name with
      | None -> Name_mode.Or_absent.absent
      | Some name_mode -> Name_mode.Or_absent.present name_mode)

  let fold t ~init ~f =
    N.Map.fold (fun name _name_mode acc -> f acc name) t init

  let fold_with_mode t ~init ~f =
    N.Map.fold
      (fun name name_mode acc ->
        match For_one_name.max_name_mode_opt name_mode with
        | Some name_mode -> f acc name name_mode
        | None -> acc)
      t init

  let downgrade_occurrences_at_strictly_greater_name_mode t
      (max_name_mode : Name_mode.t) =
    (* CR-someday mshinwell: This can be condensed when the compiler removes the
       closure allocation if [max_name_mode] is captured. *)
    match max_name_mode with
    | Normal -> t
    | Phantom ->
      N.Map.map_sharing
        (fun for_one_name ->
          For_one_name.downgrade_occurrences_at_strictly_greater_name_mode
            for_one_name Name_mode.phantom)
        t
    | In_types ->
      N.Map.map_sharing
        (fun for_one_name ->
          For_one_name.downgrade_occurrences_at_strictly_greater_name_mode
            for_one_name Name_mode.in_types)
        t

  let for_all t ~f = N.Map.for_all (fun name _ -> f name) t

  let filter t ~f = N.Map.filter (fun name _ -> f name) t

  let increase_counts t = N.Map.map For_one_name.increase_count t
end
[@@inline always]

module For_names = For_one_variety_of_names (struct
  include Name

  let apply_renaming t renaming = Renaming.apply_name renaming t
end)

module For_continuations = For_one_variety_of_names (struct
  include Continuation

  let apply_renaming t renaming = Renaming.apply_continuation renaming t
end)

module For_function_slots = For_one_variety_of_names (struct
  include Function_slot

  (* We never bind [Function_slot]s using [Name_abstraction] and they do not
     participate in [Ids_for_export]. *)
  let apply_renaming t _renaming = t
end)

module For_value_slots = For_one_variety_of_names (struct
  include Value_slot

  (* We never bind [Value_slot]s using [Name_abstraction] and they do not
     participate in [Ids_for_export]. *)
  let apply_renaming t _renaming = t
end)

module For_code_ids = For_one_variety_of_names (struct
  include Code_id

  let apply_renaming t renaming = Renaming.apply_code_id renaming t
end)

type t =
  { names : For_names.t;
    continuations : For_continuations.t;
    continuations_with_traps : For_continuations.t;
    continuations_in_trap_actions : For_continuations.t;
    function_slots_in_projections : For_function_slots.t;
    value_slots_in_projections : For_value_slots.t;
    function_slots_in_declarations : For_function_slots.t;
    value_slots_in_declarations : For_value_slots.t;
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
    function_slots_in_projections = For_function_slots.empty;
    value_slots_in_projections = For_value_slots.empty;
    function_slots_in_declarations = For_function_slots.empty;
    value_slots_in_declarations = For_value_slots.empty;
    code_ids = For_code_ids.empty;
    newer_version_of_code_ids = For_code_ids.empty
  }

let singleton_continuation cont =
  { empty with
    continuations = For_continuations.singleton cont Name_mode.normal
  }

let singleton_continuation_in_trap_action cont =
  { empty with
    continuations_in_trap_actions =
      For_continuations.singleton cont Name_mode.normal
  }

let add_continuation t cont ~has_traps =
  let continuations =
    For_continuations.add t.continuations cont Name_mode.normal
  in
  let continuations_with_traps =
    if has_traps
    then For_continuations.add t.continuations_with_traps cont Name_mode.normal
    else t.continuations_with_traps
  in
  { t with continuations; continuations_with_traps }

let count_continuation t cont = For_continuations.count t.continuations cont

let continuation_is_applied_with_traps t cont =
  For_continuations.mem t.continuations_with_traps cont

let count_variable t var = For_names.count t.names (Name.var var)

let count_variable_normal_mode t var =
  For_names.count_normal t.names (Name.var var)

let singleton_variable var name_mode =
  { empty with names = For_names.singleton (Name.var var) name_mode }

let add_variable t var name_mode =
  { t with names = For_names.add t.names (Name.var var) name_mode }

let add_symbol t sym name_mode =
  { t with names = For_names.add t.names (Name.symbol sym) name_mode }

let add_name t name name_mode =
  { t with names = For_names.add t.names name name_mode }

let add_function_slot_in_projection t clos_id name_mode =
  { t with
    function_slots_in_projections =
      For_function_slots.add t.function_slots_in_projections clos_id name_mode
  }

let add_value_slot_in_projection t clos_var name_mode =
  { t with
    value_slots_in_projections =
      For_value_slots.add t.value_slots_in_projections clos_var name_mode
  }

let add_function_slot_in_declaration t clos_id name_mode =
  { t with
    function_slots_in_declarations =
      For_function_slots.add t.function_slots_in_declarations clos_id name_mode
  }

let add_value_slot_in_declaration t clos_var name_mode =
  { t with
    value_slots_in_declarations =
      For_value_slots.add t.value_slots_in_declarations clos_var name_mode
  }

let add_function_slot_in_types t clos_id =
  { t with
    function_slots_in_declarations =
      For_function_slots.add t.function_slots_in_declarations clos_id
        Name_mode.in_types;
    function_slots_in_projections =
      For_function_slots.add t.function_slots_in_projections clos_id
        Name_mode.in_types
  }

let add_value_slot_in_types t clos_var =
  { t with
    value_slots_in_declarations =
      For_value_slots.add t.value_slots_in_declarations clos_var
        Name_mode.in_types;
    value_slots_in_projections =
      For_value_slots.add t.value_slots_in_projections clos_var
        Name_mode.in_types
  }

let add_code_id t id name_mode =
  { t with code_ids = For_code_ids.add t.code_ids id name_mode }

let singleton_code_id id name_mode = add_code_id empty id name_mode

let add_newer_version_of_code_id t id name_mode =
  { t with
    newer_version_of_code_ids =
      For_code_ids.add t.newer_version_of_code_ids id name_mode
  }

let singleton_symbol sym name_mode =
  { empty with names = For_names.singleton (Name.symbol sym) name_mode }

let singleton_name name name_mode =
  { empty with names = For_names.singleton name name_mode }

let create_variables vars name_mode =
  let names =
    Variable.Set.fold
      (fun var names -> For_names.add names (Name.var var) name_mode)
      vars For_names.empty
  in
  { empty with names }

let create_names names name_mode =
  let names =
    Name.Set.fold
      (fun name names -> For_names.add names name name_mode)
      names For_names.empty
  in
  { empty with names }

let binary_conjunction ~for_names ~for_continuations ~for_function_slots
    ~for_value_slots ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      function_slots_in_projections = function_slots_in_projections1;
      value_slots_in_projections = value_slots_in_projections1;
      function_slots_in_declarations = function_slots_in_declarations1;
      value_slots_in_declarations = value_slots_in_declarations1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      function_slots_in_projections = function_slots_in_projections2;
      value_slots_in_projections = value_slots_in_projections2;
      function_slots_in_declarations = function_slots_in_declarations2;
      value_slots_in_declarations = value_slots_in_declarations2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  for_names names1 names2
  && for_continuations continuations1 continuations2
  && for_continuations continuations_with_traps1 continuations_with_traps2
  && for_continuations continuations_in_trap_actions1
       continuations_in_trap_actions2
  && for_function_slots function_slots_in_projections1
       function_slots_in_projections2
  && for_value_slots value_slots_in_projections1 value_slots_in_projections2
  && for_function_slots function_slots_in_declarations1
       function_slots_in_declarations2
  && for_value_slots value_slots_in_declarations1 value_slots_in_declarations2
  && for_code_ids code_ids1 code_ids2
  && for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_disjunction ~for_names ~for_continuations ~for_function_slots
    ~for_value_slots ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      function_slots_in_projections = function_slots_in_projections1;
      value_slots_in_projections = value_slots_in_projections1;
      function_slots_in_declarations = function_slots_in_declarations1;
      value_slots_in_declarations = value_slots_in_declarations1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      function_slots_in_projections = function_slots_in_projections2;
      value_slots_in_projections = value_slots_in_projections2;
      function_slots_in_declarations = function_slots_in_declarations2;
      value_slots_in_declarations = value_slots_in_declarations2;
      code_ids = code_ids2;
      newer_version_of_code_ids = newer_version_of_code_ids2
    } =
  for_names names1 names2
  || for_continuations continuations1 continuations2
  || for_continuations continuations_with_traps1 continuations_with_traps2
  || for_continuations continuations_in_trap_actions1
       continuations_in_trap_actions2
  || for_function_slots function_slots_in_projections1
       function_slots_in_projections2
  || for_value_slots value_slots_in_projections1 value_slots_in_projections2
  || for_function_slots function_slots_in_declarations1
       function_slots_in_declarations2
  || for_value_slots value_slots_in_declarations1 value_slots_in_declarations2
  || for_code_ids code_ids1 code_ids2
  || for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_op ~for_names ~for_continuations ~for_function_slots ~for_value_slots
    ~for_code_ids
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      function_slots_in_projections = function_slots_in_projections1;
      value_slots_in_projections = value_slots_in_projections1;
      function_slots_in_declarations = function_slots_in_declarations1;
      value_slots_in_declarations = value_slots_in_declarations1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    { names = names2;
      continuations = continuations2;
      continuations_with_traps = continuations_with_traps2;
      continuations_in_trap_actions = continuations_in_trap_actions2;
      function_slots_in_projections = function_slots_in_projections2;
      value_slots_in_projections = value_slots_in_projections2;
      function_slots_in_declarations = function_slots_in_declarations2;
      value_slots_in_declarations = value_slots_in_declarations2;
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
  let function_slots_in_projections =
    for_function_slots function_slots_in_projections1
      function_slots_in_projections2
  in
  let value_slots_in_projections =
    for_value_slots value_slots_in_projections1 value_slots_in_projections2
  in
  let function_slots_in_declarations =
    for_function_slots function_slots_in_declarations1
      function_slots_in_declarations2
  in
  let value_slots_in_declarations =
    for_value_slots value_slots_in_declarations1 value_slots_in_declarations2
  in
  let code_ids = for_code_ids code_ids1 code_ids2 in
  let newer_version_of_code_ids =
    for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations;
    code_ids;
    newer_version_of_code_ids
  }

let diff
    { names = names1;
      continuations = continuations1;
      continuations_with_traps = continuations_with_traps1;
      continuations_in_trap_actions = continuations_in_trap_actions1;
      function_slots_in_projections = function_slots_in_projections1;
      value_slots_in_projections = value_slots_in_projections1;
      function_slots_in_declarations = function_slots_in_declarations1;
      value_slots_in_declarations = value_slots_in_declarations1;
      code_ids = code_ids1;
      newer_version_of_code_ids = newer_version_of_code_ids1
    }
    ~without:
      { names = names2;
        continuations = continuations2;
        continuations_with_traps = continuations_with_traps2;
        continuations_in_trap_actions = continuations_in_trap_actions2;
        function_slots_in_projections = function_slots_in_projections2;
        value_slots_in_projections = value_slots_in_projections2;
        function_slots_in_declarations = function_slots_in_declarations2;
        value_slots_in_declarations = value_slots_in_declarations2;
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
  let function_slots_in_projections =
    For_function_slots.diff function_slots_in_projections1
      function_slots_in_projections2
  in
  let value_slots_in_projections =
    For_value_slots.diff value_slots_in_projections1 value_slots_in_projections2
  in
  let function_slots_in_declarations =
    For_function_slots.diff function_slots_in_declarations1
      function_slots_in_declarations2
  in
  let value_slots_in_declarations =
    For_value_slots.diff value_slots_in_declarations1
      value_slots_in_declarations2
  in
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
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations;
    code_ids;
    newer_version_of_code_ids
  }

let union t1 t2 =
  binary_op ~for_names:For_names.union
    ~for_continuations:For_continuations.union
    ~for_function_slots:For_function_slots.union
    ~for_value_slots:For_value_slots.union ~for_code_ids:For_code_ids.union t1
    t2

let equal t1 t2 =
  binary_conjunction ~for_names:For_names.equal
    ~for_continuations:For_continuations.equal
    ~for_function_slots:For_function_slots.equal
    ~for_value_slots:For_value_slots.equal ~for_code_ids:For_code_ids.equal t1
    t2

let is_empty t = equal t empty

let no_variables t =
  For_names.for_all t.names ~f:(fun var -> not (Name.is_var var))

let no_continuations
    { names = _;
      continuations;
      continuations_with_traps = _;
      continuations_in_trap_actions;
      function_slots_in_projections = _;
      value_slots_in_projections = _;
      function_slots_in_declarations = _;
      value_slots_in_declarations = _;
      code_ids = _;
      newer_version_of_code_ids = _
    } =
  (* Note: continuations_with_traps is included in continuations *)
  For_continuations.is_empty continuations
  && For_continuations.is_empty continuations_in_trap_actions

let subset_domain t1 t2 =
  binary_conjunction ~for_names:For_names.subset_domain
    ~for_continuations:For_continuations.subset_domain
    ~for_function_slots:For_function_slots.subset_domain
    ~for_value_slots:For_value_slots.subset_domain
    ~for_code_ids:For_code_ids.subset_domain t1 t2

let inter_domain_is_non_empty t1 t2 =
  binary_disjunction ~for_names:For_names.inter_domain_is_non_empty
    ~for_continuations:For_continuations.inter_domain_is_non_empty
    ~for_function_slots:For_function_slots.inter_domain_is_non_empty
    ~for_value_slots:For_value_slots.inter_domain_is_non_empty
    ~for_code_ids:For_code_ids.inter_domain_is_non_empty t1 t2

let rec union_list ts =
  match ts with [] -> empty | t :: ts -> union t (union_list ts)

let function_slots_in_normal_projections t =
  For_function_slots.fold_with_mode t.function_slots_in_projections
    ~init:Function_slot.Set.empty ~f:(fun acc function_slot name_mode ->
      if Name_mode.is_normal name_mode
      then Function_slot.Set.add function_slot acc
      else acc)

let all_function_slots t =
  Function_slot.Set.union
    (For_function_slots.keys t.function_slots_in_projections)
    (For_function_slots.keys t.function_slots_in_declarations)

let value_slots_in_normal_projections t =
  For_value_slots.fold_with_mode t.value_slots_in_projections
    ~init:Value_slot.Set.empty ~f:(fun acc value_slot name_mode ->
      if Name_mode.is_normal name_mode
      then Value_slot.Set.add value_slot acc
      else acc)

let all_value_slots t =
  Value_slot.Set.union
    (For_value_slots.keys t.value_slots_in_projections)
    (For_value_slots.keys t.value_slots_in_declarations)

let variables t = For_names.keys t.names |> Name.set_to_var_set

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

let mem_name t name = For_names.mem t.names name

let mem_var t var = For_names.mem t.names (Name.var var)

let mem_symbol t symbol = For_names.mem t.names (Name.symbol symbol)

let mem_code_id t code_id = For_code_ids.mem t.code_ids code_id

let value_slot_is_used_or_imported t value_slot =
  Value_slot.is_imported value_slot
  || For_value_slots.mem t.value_slots_in_projections value_slot

let remove_var t ~var =
  if For_names.is_empty t.names
  then t
  else
    let names = For_names.remove t.names (Name.var var) in
    { t with names }

let remove_symbol t ~symbol =
  if For_names.is_empty t.names
  then t
  else
    let names = For_names.remove t.names (Name.symbol symbol) in
    { t with names }

let remove_code_id t ~code_id =
  if For_code_ids.is_empty t.code_ids
     && For_code_ids.is_empty t.newer_version_of_code_ids
  then t
  else
    let code_ids = For_code_ids.remove t.code_ids code_id in
    let newer_version_of_code_ids =
      For_code_ids.remove t.newer_version_of_code_ids code_id
    in
    { t with code_ids; newer_version_of_code_ids }

let remove_code_id_or_symbol t ~(code_id_or_symbol : Code_id_or_symbol.t) =
  Code_id_or_symbol.pattern_match code_id_or_symbol
    ~code_id:(fun code_id -> remove_code_id t ~code_id)
    ~symbol:(fun symbol -> remove_symbol t ~symbol)

let remove_continuation t ~continuation =
  if For_continuations.is_empty t.continuations
     && For_continuations.is_empty t.continuations_in_trap_actions
  then t
  else
    let continuations = For_continuations.remove t.continuations continuation in
    let continuations_with_traps =
      For_continuations.remove t.continuations_with_traps continuation
    in
    let continuations_in_trap_actions =
      For_continuations.remove t.continuations_in_trap_actions continuation
    in
    { t with
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions
    }

let greatest_name_mode_var t var =
  For_names.greatest_name_mode t.names (Name.var var)

let downgrade_occurrences_at_strictly_greater_name_mode
    { names;
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions;
      function_slots_in_projections;
      value_slots_in_projections;
      function_slots_in_declarations;
      value_slots_in_declarations;
      code_ids;
      newer_version_of_code_ids
    } max_name_mode =
  (* CR mshinwell: Don't reallocate the record if nothing changed *)
  let names =
    For_names.downgrade_occurrences_at_strictly_greater_name_mode names
      max_name_mode
  in
  let continuations =
    For_continuations.downgrade_occurrences_at_strictly_greater_name_mode
      continuations max_name_mode
  in
  let continuations_with_traps =
    For_continuations.downgrade_occurrences_at_strictly_greater_name_mode
      continuations_with_traps max_name_mode
  in
  let continuations_in_trap_actions =
    For_continuations.downgrade_occurrences_at_strictly_greater_name_mode
      continuations_in_trap_actions max_name_mode
  in
  let function_slots_in_projections =
    For_function_slots.downgrade_occurrences_at_strictly_greater_name_mode
      function_slots_in_projections max_name_mode
  in
  let value_slots_in_projections =
    For_value_slots.downgrade_occurrences_at_strictly_greater_name_mode
      value_slots_in_projections max_name_mode
  in
  let function_slots_in_declarations =
    For_function_slots.downgrade_occurrences_at_strictly_greater_name_mode
      function_slots_in_declarations max_name_mode
  in
  let value_slots_in_declarations =
    For_value_slots.downgrade_occurrences_at_strictly_greater_name_mode
      value_slots_in_declarations max_name_mode
  in
  let code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_name_mode code_ids
      max_name_mode
  in
  let newer_version_of_code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_name_mode
      newer_version_of_code_ids max_name_mode
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations;
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
      function_slots_in_projections;
      value_slots_in_projections;
      function_slots_in_declarations;
      value_slots_in_declarations;
      code_ids;
      newer_version_of_code_ids
    } =
  { empty with
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations;
    code_ids;
    newer_version_of_code_ids
  }

let without_code_ids t =
  { t with
    code_ids = For_code_ids.empty;
    newer_version_of_code_ids = For_code_ids.empty
  }

let fold_names t ~init ~f = For_names.fold t.names ~init ~f

let fold_variables t ~init ~f =
  For_names.fold t.names ~init ~f:(fun acc name ->
      Name.pattern_match name ~var:(fun var -> f acc var) ~symbol:(fun _ -> acc))

let fold_continuations_including_in_trap_actions t ~init ~f =
  (* Note: continuations_with_traps is included in continuations *)
  let init = For_continuations.fold t.continuations ~init ~f in
  For_continuations.fold t.continuations_in_trap_actions ~init ~f

let fold_code_ids t ~init ~f = For_code_ids.fold t.code_ids ~init ~f

let apply_renaming
    ({ names;
       continuations;
       continuations_with_traps;
       continuations_in_trap_actions;
       function_slots_in_projections;
       value_slots_in_projections;
       function_slots_in_declarations;
       value_slots_in_declarations;
       code_ids;
       newer_version_of_code_ids
     } as t) renaming =
  if Renaming.is_identity renaming
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
    let function_slots_in_projections =
      For_function_slots.apply_renaming function_slots_in_projections renaming
    in
    let value_slots_in_projections =
      For_value_slots.apply_renaming value_slots_in_projections renaming
    in
    let function_slots_in_declarations =
      For_function_slots.apply_renaming function_slots_in_declarations renaming
    in
    let value_slots_in_declarations =
      For_value_slots.apply_renaming value_slots_in_declarations renaming
    in
    let code_ids = For_code_ids.apply_renaming code_ids renaming in
    let newer_version_of_code_ids =
      For_code_ids.apply_renaming newer_version_of_code_ids renaming
    in
    { names;
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions;
      function_slots_in_projections;
      value_slots_in_projections;
      function_slots_in_declarations;
      value_slots_in_declarations;
      code_ids;
      newer_version_of_code_ids
    }

let affected_by_renaming
    { names;
      continuations;
      continuations_with_traps = _;
      continuations_in_trap_actions;
      function_slots_in_projections = _;
      value_slots_in_projections = _;
      function_slots_in_declarations = _;
      value_slots_in_declarations = _;
      code_ids;
      newer_version_of_code_ids
    } renaming =
  For_names.affected_by_renaming names renaming
  || For_continuations.affected_by_renaming continuations renaming
  || For_continuations.affected_by_renaming continuations_in_trap_actions
       renaming
  || For_code_ids.affected_by_renaming code_ids renaming
  || For_code_ids.affected_by_renaming newer_version_of_code_ids renaming

let restrict_to_value_slots_and_function_slots
    { names = _;
      continuations = _;
      continuations_with_traps = _;
      continuations_in_trap_actions = _;
      function_slots_in_projections;
      value_slots_in_projections;
      function_slots_in_declarations;
      value_slots_in_declarations;
      code_ids = _;
      newer_version_of_code_ids = _
    } =
  { empty with
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations
  }

let ids_for_export
    ({ names = _;
       continuations;
       continuations_with_traps;
       continuations_in_trap_actions;
       function_slots_in_projections = _;
       value_slots_in_projections = _;
       function_slots_in_declarations = _;
       value_slots_in_declarations = _;
       code_ids;
       newer_version_of_code_ids
     } as t) =
  let variables = variables t in
  let symbols = symbols t in
  let continuations =
    Continuation.Set.union_list
      [ For_continuations.keys continuations;
        For_continuations.keys continuations_with_traps;
        For_continuations.keys continuations_in_trap_actions ]
  in
  let code_ids =
    Code_id.Set.union
      (For_code_ids.keys code_ids)
      (For_code_ids.keys newer_version_of_code_ids)
  in
  Ids_for_export.create ~variables ~symbols ~code_ids ~continuations ()

let increase_counts
    { names;
      continuations;
      continuations_with_traps;
      continuations_in_trap_actions;
      function_slots_in_projections;
      value_slots_in_projections;
      function_slots_in_declarations;
      value_slots_in_declarations;
      code_ids;
      newer_version_of_code_ids
    } =
  let names = For_names.increase_counts names in
  let continuations = For_continuations.increase_counts continuations in
  let continuations_with_traps =
    For_continuations.increase_counts continuations_with_traps
  in
  let continuations_in_trap_actions =
    For_continuations.increase_counts continuations_in_trap_actions
  in
  let function_slots_in_projections =
    For_function_slots.increase_counts function_slots_in_projections
  in
  let value_slots_in_projections =
    For_value_slots.increase_counts value_slots_in_projections
  in
  let function_slots_in_declarations =
    For_function_slots.increase_counts function_slots_in_declarations
  in
  let value_slots_in_declarations =
    For_value_slots.increase_counts value_slots_in_declarations
  in
  let code_ids = For_code_ids.increase_counts code_ids in
  let newer_version_of_code_ids =
    For_code_ids.increase_counts newer_version_of_code_ids
  in
  { names;
    continuations;
    continuations_with_traps;
    continuations_in_trap_actions;
    function_slots_in_projections;
    value_slots_in_projections;
    function_slots_in_declarations;
    value_slots_in_declarations;
    code_ids;
    newer_version_of_code_ids
  }

let [@ocamlformat "disable"] print ppf
      ({ names;
         continuations;
         continuations_with_traps;
         continuations_in_trap_actions;
         function_slots_in_projections;
         value_slots_in_projections;
         function_slots_in_declarations;
         value_slots_in_declarations;
         code_ids;
         newer_version_of_code_ids } as t) =
  if is_empty t then
    Format.fprintf ppf "no_occurrences"
  else
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(names %a)@]@ \
      @[<hov 1>(continuations %a)@]@ \
      @[<hov 1>(continuations_with_traps %a)@]@ \
      @[<hov 1>(continuations_in_trap_actions %a)@]@ \
      @[<hov 1>(function_slots_in_projections %a)@]@ \
      @[<hov 1>(value_slots_in_projections %a)@]@ \
      @[<hov 1>(function_slots_in_declarations %a)@]@ \
      @[<hov 1>(value_slots_in_declarations %a)@]@ \
      @[<hov 1>(code_ids %a)@] \
      @[<hov 1>(newer_version_of_code_ids %a)@]@ \
      @]"
    For_names.print names
    For_continuations.print continuations
    For_continuations.print continuations_with_traps
    For_continuations.print continuations_in_trap_actions
    For_function_slots.print function_slots_in_projections
    For_value_slots.print value_slots_in_projections
    For_function_slots.print function_slots_in_declarations
    For_value_slots.print value_slots_in_declarations
    For_code_ids.print code_ids
    For_code_ids.print newer_version_of_code_ids
