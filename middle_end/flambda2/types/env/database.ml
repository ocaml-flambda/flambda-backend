module RWC = Reg_width_const

(** {2 Meet}

    We use a simplified version of [TE.meet_return_value] for the database: we
    only care about whether the result of a [meet] is identical to the value
    that was previously stored in the database, or if it needs to be updated.

    We choose a left-biased approach: the existing value should always be passed
    as the first argument of the [meet] function. *)

type 'a left_meet_return_value =
  | Left_input
  | New_result of 'a

let left_meet_return_value res left =
  if res == left then Left_input else New_result res

let extract_value res left =
  match res with Left_input -> left | New_result value -> value

(** {2 Row-like}

    Generic row-like functor, used for disjunctions. *)

module Row_like = struct
  module Make (Branch : Container_types.S) = struct
    type 'a t =
      { known : 'a Branch.Map.t;
        other : 'a Or_bottom.t
      }

    let create ?default known =
      let other : _ Or_bottom.t =
        match default with None -> Bottom | Some other -> Ok other
      in
      { known; other }

    let print_known pp ppf known = Branch.Map.print pp ppf known

    let print pp ppf { known; other } =
      Format.fprintf ppf
        "@[<hov 1>(@[<hov 1>(known@ %a)@]@ @[<hov 1>(other@ %a)@]@]"
        (print_known pp) known (Or_bottom.print pp) other

    let is_bottom { other; known } =
      match other with Bottom -> Branch.Map.is_empty known | Ok _ -> false

    let merge f { known = known1; other = other1 }
        { known = known2; other = other2 } =
      let known =
        Branch.Map.merge
          (fun _ case1 case2 ->
            let case1 : _ Or_bottom.t =
              match case1 with None -> other1 | Some case1 -> Ok case1
            in
            let case2 : _ Or_bottom.t =
              match case2 with None -> other2 | Some case2 -> Ok case2
            in
            match (f case1 case2 : _ Or_bottom.t) with
            | Bottom -> None
            | Ok case -> Some case)
          known1 known2
      in
      let other = f other1 other2 in
      { known; other }

    let left_meet ~meet t1 t2 : _ Or_bottom.t =
      let result_is_left = ref true in
      let t =
        merge
          (fun case1 case2 ->
            match case1, case2 with
            | Bottom, _ -> Bottom
            | Ok _, Bottom ->
              result_is_left := false;
              Bottom
            | Ok case1, Ok case2 -> (
              match (meet case1 case2 : _ Or_bottom.t) with
              | Ok Left_input -> Ok case1
              | Ok (New_result case) ->
                result_is_left := false;
                Ok case
              | Bottom -> Bottom))
          t1 t2
      in
      if is_bottom t
      then Bottom
      else if !result_is_left
      then Ok Left_input
      else Ok (New_result t)

    let find const { other; known } : _ Or_bottom.t =
      match Branch.Map.find_opt const known with
      | Some arm -> Ok arm
      | None -> other
  end

  module For_const = Make (RWC)
end

(** {2 Extensions}

    Extensions are treated as inert by the database (i.e. we can't learn new
    aliases or new properties recursively when meeting extensions), so we can
    implement basic extension-related functions without dependencies on the
    database. *)

module Extension_id : sig
  include Container_types.S

  val create : unit -> t
end = struct
  type t = int

  let create =
    let next_stamp = ref 0 in
    fun () ->
      incr next_stamp;
      !next_stamp

  module T0 = struct
    let print ppf n = Format.fprintf ppf "ext%d" n

    let equal = Int.equal

    let compare = Int.compare

    let hash (x : t) = Hashtbl.hash x
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map
end

module Extension_set = Container_types.Shared_set (Extension_id)

type switch = Extension_set.t Row_like.For_const.t

let print_switch ppf switch =
  Row_like.For_const.print Extension_set.print ppf switch

let empty_switch =
  Row_like.For_const.create ~default:Extension_set.empty RWC.Map.empty

let is_empty_switch ({ other; known } : switch) =
  match other with
  | Bottom -> false
  | Ok extension_ids ->
    Extension_set.is_empty extension_ids
    && RWC.Map.for_all
         (fun _ extension_ids -> Extension_set.is_empty extension_ids)
         known

let left_meet_switch switch1 (switch2 : Extension_id.Set.t Row_like.For_const.t)
    =
  Row_like.For_const.left_meet
    ~meet:(fun ext1 ext2 : _ Or_bottom.t ->
      let ext = Extension_set.union_set ext1 ext2 in
      Ok (left_meet_return_value ext ext1))
    switch1 switch2

(** {2 Relations} *)

module Function : sig
  type t

  include Container_types.S_plus_iterator with type t := t

  type descr =
    | Is_null
    | Is_int
    | Get_tag
    | Untag_imm
    | Tag_imm

  val descr : t -> descr

  val is_null : t

  val is_int : t

  val get_tag : t

  val untag_imm : t

  val tag_imm : t

  val inverse : t -> t option

  val of_const : t -> RWC.t -> RWC.t Or_unknown_or_bottom.t
end = struct
  type t = int

  module T0 = struct
    let is_null = 0

    let is_int = 1

    let get_tag = 2

    let untag_imm = 3

    let tag_imm = 4

    let equal = Int.equal

    let compare = Int.compare

    let hash (t : t) = Hashtbl.hash t

    type descr =
      | Is_null
      | Is_int
      | Get_tag
      | Untag_imm
      | Tag_imm

    let grand_table_of_functions =
      [| Is_null; Is_int; Get_tag; Untag_imm; Tag_imm |]

    let descr fn = grand_table_of_functions.(fn)

    let print ppf fn =
      match descr fn with
      | Is_null -> Format.fprintf ppf "is_null"
      | Is_int -> Format.fprintf ppf "is_int"
      | Get_tag -> Format.fprintf ppf "get_tag"
      | Untag_imm -> Format.fprintf ppf "untag_int"
      | Tag_imm -> Format.fprintf ppf "tag_imm"
  end

  include T0

  module T = struct
    type nonrec t = t

    include T0
  end

  module Tree = Patricia_tree.Make (struct
    let print = print
  end)

  module Set = Tree.Set
  module Map = Tree.Map

  let inverse fn =
    match descr fn with
    | Is_null | Is_int | Get_tag -> None
    | Untag_imm -> Some tag_imm
    | Tag_imm -> Some untag_imm

  let of_const fn const : _ Or_unknown_or_bottom.t =
    let[@inline] return b : _ Or_unknown_or_bottom.t =
      if b then Ok RWC.untagged_const_true else Ok RWC.untagged_const_false
    in
    match descr fn, RWC.descr const with
    (* is_null *)
    | Is_null, Null -> return true
    | Is_null, Tagged_immediate _ -> return false
    | Is_null, Naked_immediate _ -> Bottom
    (* is_int *)
    | Is_int, Tagged_immediate _ -> return true
    | Is_int, (Naked_immediate _ | Null) -> Bottom
    (* get_tag *)
    | Get_tag, _ -> Bottom
    (* untag_imm *)
    | Untag_imm, Tagged_immediate imm -> Ok (RWC.naked_immediate imm)
    | Untag_imm, (Naked_immediate _ | Null) -> Bottom
    (* tag_imm *)
    | Tag_imm, Naked_immediate imm -> Ok (RWC.tagged_immediate imm)
    | Tag_imm, (Tagged_immediate _ | Null) -> Bottom
    (* others *)
    | ( _,
        ( Naked_float32 _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ | Naked_vec128 _ ) ) ->
      Bottom
end

module Function_map = Container_types.Shared_map (Function)
module Name_map = Container_types.Shared_map (Name)
module Const_map = Container_types.Shared_map (RWC)

let canonicalise aliases simple =
  Simple.pattern_match simple
    ~const:(fun _ -> simple)
    ~name:(fun name ~coercion ->
      Simple.apply_coercion_exn
        (Aliases.get_canonical_ignoring_name_mode aliases name)
        coercion)

type simple_or_switch =
  | Simple of Simple.t
  | Switch of switch

let print_simple_or_switch ppf = function
  | Simple simple -> Simple.print ppf simple
  | Switch switch -> print_switch ppf switch

type properties_of_name =
  { properties : simple_or_switch Function_map.t;
        (** Maps unary properties on the name to their value.

        Partial properties (i.e. properties that do not necessarily exist on all
        names) are supported:

          - If the value is a [Simple.t], the property exists.
          - If the value is a switch, the property is not guaranteed to exist;
            however, when it is added to the database (i.e. it is given a
            [Simple.t] value), the switch will apply to its value. *)
    inverses : Coercion.t Name_map.t Function_map.t;
        (** Inverse map from property values to their arguments.

            {b Note}: This map only contains {b non-inversible} properties. *)
    switch : switch
        (** Switches are the way we deal with disjunction in the database.

            A switch on a name contains a mapping from possible values for that
            name to extensions. When the name becomes equal to a value, the
            corresponding extensions are activated. *)
  }

let print_properties_of_name ppf { properties; switch; _ } =
  let needs_sep = ref false in
  let print_sep ppf () =
    if !needs_sep then Format.pp_print_space ppf ();
    needs_sep := true
  in
  let print_properties ppf properties =
    print_sep ppf ();
    Format.fprintf ppf "@[<hov 1>(properties@ %a)@]"
      (Function_map.print print_simple_or_switch)
      properties
  in
  let print_switch ppf switch =
    print_sep ppf ();
    Format.fprintf ppf "@[<hov 1>(switch@ %a)@]" print_switch switch
  in
  Format.fprintf ppf "@[<hov 1>(";
  if not (Function_map.is_empty properties) then print_properties ppf properties;
  if not (is_empty_switch switch) then print_switch ppf switch;
  Format.fprintf ppf ")@]"

let properties_of_name ?(properties = Function_map.empty)
    ?(inverses = Function_map.empty) ?(switch = empty_switch) () =
  { properties; inverses; switch }

let empty_properties_of_name =
  { properties = Function_map.empty;
    inverses = Function_map.empty;
    switch = empty_switch
  }

let is_empty_properties_of_name { properties; inverses; switch } =
  Function_map.is_empty properties
  && Function_map.is_empty inverses
  && is_empty_switch switch

type t =
  { properties_of_names : properties_of_name Name_map.t;
    (* Map from names to their known properties.

       {b Note}: The *keys* of this map is kept in canonical form, but not the
       values. *)
    inverses_of_consts : Name.Set.t Const_map.t Function_map.t;
    active_extensions : Extension_set.t
  }

let print ppf t =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(properties@ %a)@]@ @[<hov 1>(extensions@ %a)@])@]"
    (Name_map.print print_properties_of_name)
    t.properties_of_names Extension_set.print t.active_extensions

let empty =
  { properties_of_names = Name_map.empty;
    inverses_of_consts = Function_map.empty;
    active_extensions = Extension_set.empty
  }

let is_empty { properties_of_names; inverses_of_consts; active_extensions } =
  Name_map.is_empty properties_of_names
  && Function_map.is_empty inverses_of_consts
  && Extension_set.is_empty active_extensions

let active_extensions { active_extensions; _ } =
  (active_extensions :> Extension_id.Set.t)

let get name t =
  match Name_map.find_opt name t.properties_of_names with
  | None -> empty_properties_of_name
  | Some properties_of_name -> properties_of_name

let get_property property properties =
  match Function_map.find_opt property properties with
  | None -> Switch empty_switch
  | Some simple_or_switch -> simple_or_switch

let set name properties t =
  let properties_of_names =
    if is_empty_properties_of_name properties
    then Name_map.remove name t.properties_of_names
    else Name_map.add name properties t.properties_of_names
  in
  { t with properties_of_names }

let activate_extensions t extension_ids =
  let active_extensions =
    Extension_set.union_set t.active_extensions extension_ids
  in
  { t with active_extensions }

let add_alias ~binding_time_resolver ~binding_times_and_modes aliases canonical1
    canonical2 : _ Or_bottom.t =
  if Simple.equal canonical1 canonical2
  then Ok aliases
  else
    match
      Aliases.add ~binding_time_resolver aliases ~binding_times_and_modes
        ~canonical_element1:canonical1 ~canonical_element2:canonical2
    with
    | Bottom -> Bottom
    | Ok { demoted_name = _; canonical_element = _; t = aliases } -> Ok aliases

module Meet_env = struct
  type add_alias =
    | Add_alias :
        { binding_time_resolver : Name.t -> Binding_time.With_name_mode.t;
          binding_times_and_modes :
            ('a * Binding_time.With_name_mode.t) Name.Map.t
        }
        -> add_alias

  type equation =
    | Add_switch_on_name of Name.t * switch
    | Activate_extensions of Extension_set.t

  type t =
    { add_alias : add_alias;
      aliases : Aliases.t;
      equations : equation list
    }

  let create ~binding_time_resolver ~binding_times_and_modes aliases =
    let add_alias =
      Add_alias { binding_time_resolver; binding_times_and_modes }
    in
    { add_alias; aliases; equations = [] }

  let get_canonical env simple = canonicalise env.aliases simple

  let add_alias_between_canonicals t canonical1 canonical2 : _ Or_bottom.t =
    let (Add_alias { binding_time_resolver; binding_times_and_modes }) =
      t.add_alias
    in
    match
      add_alias ~binding_time_resolver ~binding_times_and_modes t.aliases
        canonical1 canonical2
    with
    | Bottom -> Bottom
    | Ok aliases -> Ok { t with aliases }

  let add_alias t simple1 simple2 =
    let canonical1 = get_canonical t simple1 in
    let canonical2 = get_canonical t simple2 in
    add_alias_between_canonicals t canonical1 canonical2

  let add_equation t eqn = { t with equations = eqn :: t.equations }

  let activate_extensions env extensions =
    add_equation env (Activate_extensions extensions)

  let add_switch env simple switch =
    Simple.pattern_match simple
      ~const:(fun const : _ Or_bottom.t ->
        match Row_like.For_const.find const switch with
        | Ok extensions -> Ok (activate_extensions env extensions)
        | Bottom -> Bottom)
      ~name:(fun name ~coercion : _ Or_bottom.t ->
        assert (Coercion.is_id coercion);
        Ok (add_equation env (Add_switch_on_name (name, switch))))

  let rebuild ~add_switch ~activate_extensions
      { aliases; equations; add_alias = _ } db : _ Or_bottom.t =
    let exception Is_bottom in
    let early_exit (db_ob : _ Or_bottom.t) =
      match db_ob with Ok db -> db | Bottom -> raise Is_bottom
    in
    match
      List.fold_left
        (fun db eqn ->
          match eqn with
          | Add_switch_on_name (name, switch) ->
            let canonical = canonicalise aliases (Simple.name name) in
            early_exit
              (add_switch db canonical
                 (switch :> Extension_id.Set.t Row_like.For_const.t))
          | Activate_extensions extension_ids ->
            activate_extensions db (extension_ids :> Extension_id.Set.t))
        db equations
    with
    | db -> Ok (db, aliases)
    | exception Is_bottom -> Bottom
end

let left_meet_simple_or_switch env sos1 sos2 : _ Or_bottom.t =
  let open Or_bottom.Let_syntax in
  match sos1, sos2 with
  | Simple simple1, Simple simple2 ->
    let<+ env = Meet_env.add_alias env simple1 simple2 in
    Left_input, env
  | Simple simple, Switch switch ->
    let<+ env = Meet_env.add_switch env simple switch in
    Left_input, env
  | Switch switch, Simple simple ->
    let<+ env = Meet_env.add_switch env simple switch in
    New_result (Simple simple), env
  | Switch switch1, Switch switch2 -> (
    match
      left_meet_switch switch1
        (switch2 :> Extension_id.Set.t Row_like.For_const.t)
    with
    | Ok Left_input -> Ok (Left_input, env)
    | Ok (New_result switch) -> Ok (New_result (Switch switch), env)
    | Bottom -> Bottom)

let left_meet_properties env properties1 properties2 : _ Or_bottom.t =
  let exception Is_bottom in
  let env_ref = ref env in
  let result_is_left = ref true in
  match
    Function_map.union
      (fun _ sos1 sos2 ->
        match left_meet_simple_or_switch !env_ref sos1 sos2 with
        | Bottom -> raise Is_bottom
        | Ok (meet_sos, env) -> (
          env_ref := env;
          match meet_sos with
          | Left_input -> Some sos1
          | New_result sos ->
            result_is_left := false;
            Some sos))
      properties1 properties2
  with
  | properties ->
    let properties =
      if !result_is_left then Left_input else New_result properties
    in
    Ok (properties, !env_ref)
  | exception Is_bottom -> Bottom

let left_meet_inverses env inverses1 inverses2 : _ Or_bottom.t =
  let inverses =
    Function_map.union
      (fun _ property_inverses1 property_inverses2 ->
        let property_inverses =
          Name_map.union
            (fun _ coercion1 coercion2 ->
              assert (Coercion.equal coercion1 coercion2);
              Some coercion1)
            property_inverses1 property_inverses2
        in
        Some property_inverses)
      inverses1 inverses2
  in
  Ok (left_meet_return_value inverses inverses1, env)

let left_meet_inverses_of_consts env ioc1 ioc2 : _ Or_bottom.t =
  let ioc =
    Function_map.union
      (fun _ property_inverses1 property_inverses2 ->
        let property_inverses =
          Const_map.union
            (fun _ set1 set2 -> Some (Name.Set.union set1 set2))
            property_inverses1 property_inverses2
        in
        Some property_inverses)
      ioc1 ioc2
  in
  Ok (left_meet_return_value ioc ioc1, env)

let left_meet_properties_of_name env pn1 pn2 : _ Or_bottom.t =
  let open Or_bottom.Let_syntax in
  let<* properties, env =
    left_meet_properties env pn1.properties pn2.properties
  in
  let<* inverses, env = left_meet_inverses env pn1.inverses pn2.inverses in
  let<* switch =
    left_meet_switch pn1.switch
      (pn2.switch :> Extension_id.Set.t Row_like.For_const.t)
  in
  let meet_pn =
    match properties, inverses, switch with
    | Left_input, Left_input, Left_input -> Left_input
    | (Left_input | New_result _), _, _ ->
      let properties = extract_value properties pn1.properties in
      let inverses = extract_value inverses pn1.inverses in
      let switch = extract_value switch pn1.switch in
      New_result { properties; inverses; switch }
  in
  Ok (meet_pn, env)

let add_inverses_of_consts env inverses_of_consts t : _ Or_bottom.t =
  match
    left_meet_inverses_of_consts env t.inverses_of_consts inverses_of_consts
  with
  | Ok (Left_input, env) -> Ok (t, env)
  | Ok (New_result inverses_of_consts, env) ->
    Ok ({ t with inverses_of_consts }, env)
  | Bottom -> Bottom

let add_switch_on_const t const switch : _ Or_bottom.t =
  match Row_like.For_const.find const switch with
  | Ok extensions -> Ok (activate_extensions t extensions)
  | Bottom -> Bottom

let add_switch t simple switch =
  Simple.pattern_match simple
    ~const:(fun const -> add_switch_on_const t const switch)
    ~name:(fun name ~coercion : _ Or_bottom.t ->
      assert (Coercion.is_id coercion);
      let pn = get name t in
      match left_meet_switch pn.switch switch with
      | Ok Left_input -> Ok t
      | Ok (New_result switch) -> Ok (set name { pn with switch } t)
      | Bottom -> Bottom)

let add_properties_on_const env const properties_on_const t : _ Or_bottom.t =
  let open Or_bottom.Let_syntax in
  let exception Is_bottom in
  let { properties; inverses; switch } = properties_on_const in
  try
    let t, env =
      Function_map.fold
        (fun property value (t, env) ->
          let env_ob : _ Or_bottom.t =
            match Function.of_const property const with
            | Bottom -> Bottom
            | Unknown -> Ok (t, env)
            | Ok property_of_const -> (
              match value with
              | Switch switch ->
                let<+ t =
                  add_switch_on_const t property_of_const
                    (switch :> Extension_id.Set.t Row_like.For_const.t)
                in
                t, env
              | Simple value ->
                let property_of_const = Simple.const property_of_const in
                let<+ env = Meet_env.add_alias env property_of_const value in
                t, env)
          in
          match env_ob with Bottom -> raise Is_bottom | Ok (t, env) -> t, env)
        properties (t, env)
    in
    let t =
      match
        add_switch_on_const t const
          (switch :> Extension_id.Set.t Row_like.For_const.t)
      with
      | Ok t -> t
      | Bottom -> raise Is_bottom
    in
    let inverses =
      Function_map.map_unshare
        (fun (inverses_of_property : Coercion.t Name_map.t) ->
          Const_map.singleton const
            (Name.Map.keys (inverses_of_property :> Coercion.t Name.Map.t)))
        inverses
    in
    add_inverses_of_consts env inverses t
  with Is_bottom -> Bottom

let add_properties_on_name env name properties_on_name t : _ Or_bottom.t =
  match left_meet_properties_of_name env (get name t) properties_on_name with
  | Bottom -> Bottom
  | Ok (Left_input, env) -> Ok (t, env)
  | Ok (New_result properties_of_name, env) ->
    Ok (set name properties_of_name t, env)

let add_properties env simple properties_on_simple t =
  Simple.pattern_match simple
    ~const:(fun const ->
      add_properties_on_const env const properties_on_simple t)
    ~name:(fun name ~coercion ->
      assert (Coercion.is_id coercion);
      add_properties_on_name env name properties_on_simple t)

let add_inverse env property name value t : _ Or_bottom.t =
  match Function.inverse property with
  | Some inverse_property ->
    add_properties env value
      (properties_of_name
         ~properties:
           (Function_map.singleton inverse_property (Simple (Simple.name name)))
         ())
      t
  | None ->
    Simple.pattern_match value
      ~const:(fun const ->
        add_inverses_of_consts env
          (Function_map.singleton property
             (Const_map.singleton const (Name.Set.singleton name)))
          t)
      ~name:(fun value_name ~coercion : _ Or_bottom.t ->
        assert (Coercion.is_id coercion);
        add_properties_on_name env value_name
          (properties_of_name
             ~inverses:
               (Function_map.singleton property
                  (Name_map.singleton name coercion))
             ())
          t)

let add_property ~binding_time_resolver ~binding_times_and_modes aliases t
    property ~arg ~result =
  let open Or_bottom.Let_syntax in
  Simple.pattern_match (canonicalise aliases arg)
    ~const:(fun const : _ Or_bottom.t ->
      match Function.of_const property const with
      | Bottom -> Bottom
      | Unknown -> Ok (t, aliases)
      | Ok property_of_const ->
        let property_of_const = Simple.const property_of_const in
        let<+ aliases =
          add_alias ~binding_time_resolver ~binding_times_and_modes aliases
            property_of_const result
        in
        t, aliases)
    ~name:(fun name ~coercion ->
      assert (Coercion.is_id coercion);
      (* If there is already a [Simple] value for the property, we simply need
         to add an alias to the existing [Simple.t].

         Otherwise, this is the first time we learn that the property exists. We
         need to add any pending switch onto the [result], and we also need to
         record an inverse from [result] to [arg]. *)
      let pn = get name t in
      match get_property property pn.properties with
      | Simple existing_value ->
        let<+ aliases =
          add_alias ~binding_time_resolver ~binding_times_and_modes aliases
            (canonicalise aliases existing_value)
            (canonicalise aliases result)
        in
        t, aliases
      | Switch switch ->
        let result = canonicalise aliases result in
        let properties =
          Function_map.add property (Simple result) pn.properties
        in
        let t = set name { pn with properties } t in
        let<* t =
          if is_empty_switch switch
          then Ok t
          else
            add_switch t result
              (switch :> Extension_id.Set.t Row_like.For_const.t)
        in
        let env =
          Meet_env.create ~binding_time_resolver ~binding_times_and_modes
            aliases
        in
        let<* t, env = add_inverse env property name result t in
        Meet_env.rebuild ~add_switch ~activate_extensions env t)

let find_property t simple property =
  Simple.pattern_match simple
    ~const:(fun const : _ Or_unknown_or_bottom.t ->
      match Function.of_const property const with
      | Bottom -> Bottom
      | Unknown -> Unknown
      | Ok property_of_const -> Ok (Simple.const property_of_const))
    ~name:(fun name ~coercion : _ Or_unknown_or_bottom.t ->
      let properties_of_name = get name t in
      match get_property property properties_of_name.properties with
      | Switch _ -> Unknown
      | Simple value ->
        (* simple <-- [coercion] name
         *
         * value <-- [property] name
         *
         * coercion(value) <-- [property] simple
         *)
        Ok (Simple.apply_coercion_exn value coercion))

let rebuild ~binding_time_resolver ~binding_times_and_modes aliases t demotions
    : _ Or_bottom.t =
  let env =
    Meet_env.create ~binding_time_resolver ~binding_times_and_modes aliases
  in
  let to_rebuild = ref [] in
  let properties_of_names =
    Name_map.diff_map
      (fun _to_be_demoted properties_of_to_be_demoted
           (canonical, coercion_to_canonical) ->
        assert (Coercion.is_id coercion_to_canonical);
        to_rebuild := (canonical, properties_of_to_be_demoted) :: !to_rebuild;
        None)
      t.properties_of_names demotions
  in
  let t = { t with properties_of_names } in
  let exception Is_bottom in
  match
    List.fold_left
      (fun (t, env) (simple, properties) ->
        let canonical = Meet_env.get_canonical env simple in
        match add_properties env canonical properties t with
        | Ok (t, env) -> t, env
        | Bottom -> raise Is_bottom)
      (t, env) !to_rebuild
  with
  | t, env -> Meet_env.rebuild ~add_switch ~activate_extensions env t
  | exception Is_bottom -> Bottom

let shortcut_aliases ~canonicalise
    { properties_of_names; inverses_of_consts; active_extensions } =
  let properties_of_names =
    Name_map.map
      (fun pn ->
        let properties =
          Function_map.map
            (function
              | Simple simple -> Simple (canonicalise simple)
              | Switch _ as sos -> sos)
            pn.properties
        in
        let inverses =
          Function_map.map
            (fun inverses ->
              Name_map.fold
                (fun name coercion0 inverses ->
                  let simple = canonicalise (Simple.name name) in
                  Simple.pattern_match simple
                    ~const:(fun _ -> inverses)
                    ~name:(fun name ~coercion ->
                      (* coercion0(coercion(name)) = fn(...) *)
                      let coercion =
                        Coercion.compose_exn coercion ~then_:coercion0
                      in
                      Name_map.add name coercion inverses))
                inverses Name_map.empty)
            pn.inverses
        in
        if properties == pn.properties && inverses == pn.inverses
        then pn
        else { properties; inverses; switch = pn.switch })
      properties_of_names
  in
  let inverses_of_consts =
    Function_map.map
      (fun inv_fn_of_consts ->
        Const_map.map
          (fun inverses ->
            Name.Set.filter_map
              (fun name ->
                let simple = canonicalise (Simple.name name) in
                match Simple.must_be_name simple with
                | Some (name, _) -> Some name
                | None -> None)
              inverses)
          inv_fn_of_consts)
      inverses_of_consts
  in
  { properties_of_names; inverses_of_consts; active_extensions }

(** {2 Switches} *)

let add_switch_on_canonical simple ?default ~arms t =
  add_switch t simple (Row_like.For_const.create ?default arms)

let add_switch_on_property_of_name ~aliases t property name switch :
    _ Or_bottom.t =
  let pn = get name t in
  match get_property property pn.properties with
  | Simple value -> add_switch t (canonicalise aliases value) switch
  | Switch existing_switch -> (
    match left_meet_switch existing_switch switch with
    | Ok Left_input -> Ok t
    | Ok (New_result switch) ->
      let properties =
        Function_map.add property (Switch switch) pn.properties
      in
      Ok (set name { pn with properties } t)
    | Bottom -> Bottom)

let add_switch_on_property property arg ?default ~arms t ~aliases =
  let switch = Row_like.For_const.create ?default arms in
  Simple.pattern_match arg
    ~const:(fun const : _ Or_bottom.t ->
      match Function.of_const property const with
      | Bottom -> Bottom
      | Unknown -> Ok t
      | Ok property_of_const -> add_switch_on_const t property_of_const switch)
    ~name:(fun name ~coercion : _ Or_bottom.t ->
      assert (Coercion.is_id coercion);
      add_switch_on_property_of_name ~aliases t property name switch)

let switch_on_scrutinee t ~scrutinee =
  Simple.pattern_match scrutinee
    ~const:(fun const ->
      RWC.Map.singleton const Extension_id.Set.empty, Or_bottom.Bottom)
    ~name:(fun name ~coercion ->
      assert (Coercion.is_id coercion);
      let properties_of_name = get name t in
      match properties_of_name.switch with
      | { other; known } ->
        ( (known :> Extension_id.Set.t RWC.Map.t),
          (other :> Extension_id.Set.t Or_bottom.t) ))

(** {2 Differential interface} *)

type level =
  { new_properties_of_names : properties_of_name Name.Map.t Lazy.t;
    new_inverses_of_consts : Name.Set.t Const_map.t Function.Map.t Lazy.t;
    new_active_extensions : Extension_id.Set.t Lazy.t
  }

let properties_of_name_opt ?properties ?inverses ?switch () =
  let properties_of_name =
    properties_of_name ?properties ?inverses ?switch ()
  in
  if is_empty_properties_of_name properties_of_name
  then None
  else Some properties_of_name

let diff_properties_of_names t cut_after =
  Name_map.diff
    (fun _ properties cut_after_properties ->
      let switch = properties.switch in
      let cut_after_switch = cut_after_properties.switch in
      let switch = if switch == cut_after_switch then None else Some switch in
      let properties = properties.properties in
      let cut_after_properties = cut_after_properties.properties in
      let properties =
        Function_map.diff
          (fun _ value _ -> Some value)
          properties cut_after_properties
      in
      let properties = Function_map.create properties in
      properties_of_name_opt ~properties ?switch ())
    t cut_after

let diff_inverses_of_consts ic1 ic2 =
  Function_map.diff
    (fun _ property_inverses1 property_inverses2 ->
      let property_inverses =
        Const_map.diff
          (fun _ inverses1 inverses2 ->
            let inverses = Name.Set.diff inverses1 inverses2 in
            if Name.Set.is_empty inverses then None else Some inverses)
          property_inverses1 property_inverses2
      in
      if RWC.Map.is_empty property_inverses
      then None
      else Some (Const_map.create property_inverses))
    ic1 ic2

let cut t ~cut_after : level =
  let properties_of_names = t.properties_of_names in
  let old_properties_of_names = cut_after.properties_of_names in
  let new_properties_of_names =
    lazy (diff_properties_of_names properties_of_names old_properties_of_names)
  in
  let inverses_of_consts = t.inverses_of_consts in
  let old_inverses_of_consts = cut_after.inverses_of_consts in
  let new_inverses_of_consts =
    lazy (diff_inverses_of_consts inverses_of_consts old_inverses_of_consts)
  in
  let active_extensions = t.active_extensions in
  let old_active_extensions = cut_after.active_extensions in
  let new_active_extensions =
    lazy (Extension_set.diff active_extensions old_active_extensions)
  in
  { new_properties_of_names; new_inverses_of_consts; new_active_extensions }

module Level = struct
  type t = level

  let print ppf { new_properties_of_names; _ } =
    Name.Map.print print_properties_of_name ppf
      (Lazy.force new_properties_of_names)

  let empty =
    { new_properties_of_names = lazy Name.Map.empty;
      new_inverses_of_consts = lazy Function.Map.empty;
      new_active_extensions = lazy Extension_id.Set.empty
    }

  let is_empty
      { new_properties_of_names; new_inverses_of_consts; new_active_extensions }
      =
    Name.Map.is_empty (Lazy.force new_properties_of_names)
    && Function.Map.is_empty (Lazy.force new_inverses_of_consts)
    && Extension_id.Set.is_empty (Lazy.force new_active_extensions)

  let fold_properties f { new_properties_of_names; _ } acc =
    Name.Map.fold
      (fun name { properties = properties_of_name; _ } acc ->
        Function_map.fold
          (fun property value acc ->
            match value with
            | Switch _ -> acc
            | Simple value -> f property name value ~coercion:Coercion.id acc)
          properties_of_name acc)
      (Lazy.force new_properties_of_names)
      acc

  let fold_constant_properties f { new_inverses_of_consts; _ } acc =
    let new_inverses_of_consts = Lazy.force new_inverses_of_consts in
    Function.Map.fold
      (fun property inverses_of_consts acc ->
        Const_map.fold
          (fun const inverses acc ->
            Name.Set.fold
              (fun inverse acc -> f property inverse const acc)
              inverses acc)
          inverses_of_consts acc)
      new_inverses_of_consts acc

  let activated_extensions { new_active_extensions; _ } =
    (Lazy.force new_active_extensions :> Extension_id.Set.t)
end

type extension = Simple.t Function.Map.t Name.Map.t

module Extension = struct
  type t = extension

  let fold f ext acc =
    Name.Map.fold
      (fun arg fns acc ->
        Function.Map.fold (fun fn result acc -> f fn ~arg ~result acc) fns acc)
      ext acc

  let empty = Name.Map.empty

  let is_empty = Name.Map.is_empty

  let print ppf ext = Name.Map.print (Function.Map.print Simple.print) ppf ext

  let union ext1 ext2 =
    Name.Map.union
      (fun _ fns1 fns2 -> Some (Function.Map.disjoint_union fns1 fns2))
      ext1 ext2

  let add_property ext property ~arg ~result =
    union ext (Name.Map.singleton arg (Function.Map.singleton property result))

  let free_names ext =
    Name.Map.fold
      (fun name fns free_names ->
        let free_names =
          Name_occurrences.add_name free_names name Name_mode.in_types
        in
        Function.Map.fold
          (fun _ simple free_names ->
            Name_occurrences.union free_names
              (Simple.free_names_in_types simple))
          fns free_names)
      ext Name_occurrences.empty

  let apply_renaming ext renaming =
    let changed = ref false in
    let ext' =
      Name.Map.fold
        (fun name fns acc ->
          let fns' =
            Function.Map.map_sharing (Renaming.apply_simple renaming) fns
          in
          let name' = Renaming.apply_name renaming name in
          if not (fns' == fns && name == name') then changed := true;
          Name.Map.add name' fns' acc)
        ext Name.Map.empty
    in
    if !changed then ext' else ext

  let project_variables_out ~to_project ext =
    Name.Map.filter_map_sharing
      (fun name fns ->
        let keep_fns () =
          let fns' =
            Function.Map.filter_map_sharing
              (fun _fn simple ->
                match Simple.must_be_var simple with
                | Some (var, _coercion) ->
                  if Variable.Set.mem var to_project then None else Some simple
                | None -> Some simple)
              fns
          in
          if Function.Map.is_empty fns' then None else Some fns'
        in
        Name.pattern_match name
          ~symbol:(fun _ -> keep_fns ())
          ~var:(fun var ->
            if Variable.Set.mem var to_project then None else keep_fns ()))
      ext

  let disjoint_union ext1 ext2 = Name.Map.disjoint_union ext1 ext2
end

(** {2 Contains names} *)

let free_names_simple_or_switch = function
  | Simple simple -> Simple.free_names_in_types simple
  | Switch _ -> Name_occurrences.empty

let free_names_properties properties =
  Function_map.fold
    (fun _ sos free_names ->
      Name_occurrences.union free_names (free_names_simple_or_switch sos))
    properties Name_occurrences.empty

let free_names_inverses inverses =
  Function_map.fold
    (fun _ inverses free_names ->
      Name_map.fold
        (fun name _ free_names ->
          Name_occurrences.add_name free_names name Name_mode.in_types)
        inverses free_names)
    inverses Name_occurrences.empty

let free_names t =
  let from_properties =
    Name_map.fold
      (fun name { properties; inverses; switch = _ } free_names ->
        Name_occurrences.union_list
          [ free_names_properties properties;
            free_names_inverses inverses;
            Name_occurrences.add_name free_names name Name_mode.in_types ])
      t.properties_of_names Name_occurrences.empty
  in
  Function_map.fold
    (fun _ inverses free_names ->
      Const_map.fold
        (fun _ inverses free_names ->
          Name.Set.fold
            (fun name free_names ->
              Name_occurrences.add_name free_names name Name_mode.in_types)
            inverses free_names)
        inverses free_names)
    t.inverses_of_consts from_properties

let apply_renaming t renaming =
  let rename_name = Renaming.apply_name renaming in
  let rename_simple = Renaming.apply_simple renaming in
  let rename_simple_or_switch = function
    | Simple simple -> Simple (rename_simple simple)
    | Switch _ as sos -> sos
  in
  let properties_of_names =
    Name_map.fold
      (fun name { properties; inverses; switch } properties_of_name ->
        let properties =
          Function_map.map_unshare rename_simple_or_switch properties
        in
        let inverses =
          Function_map.map_unshare
            (Name_map.map_keys_unshare rename_name)
            inverses
        in
        Name_map.add (rename_name name)
          { properties; inverses; switch }
          properties_of_name)
      t.properties_of_names Name_map.empty
  in
  let inverses_of_consts =
    Function_map.map_unshare
      (fun inverses ->
        Const_map.map_unshare (Name.Set.map rename_name) inverses)
      t.inverses_of_consts
  in
  { t with properties_of_names; inverses_of_consts }
