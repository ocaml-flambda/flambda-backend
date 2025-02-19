let fold_map_option f acc o =
  match o with
  | None -> acc, None
  | Some x ->
    let acc, x = f acc x in
    acc, Some x

let rec fold_left_map_alist f acc l =
  match l with
  | [] -> acc, []
  | (k, x) :: rest ->
    let acc, x = f acc x in
    let acc, rest = fold_left_map_alist f acc rest in
    acc, (k, x) :: rest

module Stamp : sig
  (** [t] is the type of stamps. Stamps have no structure, only a notion
      of identity. *)
  type t

  (** [fresh ()] creates a fresh stamp, unequal to any other. *)
  val fresh : unit -> t

  (** [compare] is a total ordering on stamps. *)
  val compare : t -> t -> int

  (** [to_string] prints a representation of the stamp. Each stamp
      has a different representation. *)
  val to_string : t -> string
end = struct
  type t = int

  external fresh : unit -> int = "caml_fresh_oo_id" [@@noalloc]

  let compare = Int.compare

  let to_string = string_of_int
end

module Loc = struct
  type t =
    | Unknown
    | Known of
        { file : string;
          start_line : int;
          start_col : int;
          end_line : int;
          end_col : int
        }

  let unknown = Unknown

  let known ~file ~start_line ~start_col ~end_line ~end_col =
    Known { file; start_line; start_col; end_line; end_col }

  let print fmt = function
    | Unknown -> Format.printf "<unknown>"
    | Known { file; start_line; start_col; end_line; end_col } ->
      if start_line = end_line
      then
        Format.printf "File %s, line %d, characters %d-%d" file start_line
          start_col end_col
      else
        Format.printf "File %s, lines %d-%d, characters %d-%d" file start_line
          end_line start_col end_col
end

module Level : sig
  (** [t] is the type of binding levels. All variable bindings have an
      associated level and all bindings of the same level have the same
      scope. Each level is associated with a point on the call
      stack. Each level has a location attached for error messages. *)
  type t

  (** [with_fresh loc f] creates a fresh level not equal to any others,
      associated with the current point in the call stack, with [loc] as
      the attached location. *)
  val with_fresh : Loc.t -> (t -> 'a) -> 'a

  (** [check t] is [true] if the point on the call stack associated with
      [t] is still present, otherwise it is [false]. *)
  val check : t -> bool

  (** [compare] is a total order on levels. *)
  val compare : t -> t -> int

  (** [loc t] is the location attached to [t]. *)
  val loc : t -> Loc.t
end = struct
  type t =
    { stamp : Stamp.t;
      mutable valid : bool;
      loc : Loc.t
    }

  let fresh loc =
    let stamp = Stamp.fresh () in
    let valid = true in
    { stamp; valid; loc }

  let with_fresh loc f =
    let level = fresh loc in
    try
      let r = f level in
      level.valid <- false;
      r
    with e ->
      level.valid <- false;
      raise e

  let check t = t.valid

  let compare t1 t2 = Stamp.compare t1.stamp t2.stamp

  let loc t = t.loc
end

module Name = struct
  type t = string

  let mk t = t
end

module Var : sig
  (** [t] is the type of variables of any sort *)
  type t

  type var := t

  module Module : sig
    (** [t] is the type of module variables *)
    type t

    (** [generate lv n] creates a fresh module variable bound at [lv]
        whose name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t
  end

  module Value : sig
    (** [t] is the type of ordinary variables *)
    type t

    (** [generate lv n] creates a fresh variable bound at [lv] whose
        name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t
  end

  module Type : sig
    (** [t] is the type of type constructor variables *)
    type t

    (** [generate lv n] creates a fresh type constructor variable
        bound at [lv] whose name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t
  end

  (** [name t] is the name of [t]. *)
  val name : t -> Name.t

  (** [level t] is the level of [t]. *)
  val level : t -> Level.t

  (** [loc t] is [Level.loc (level t)]. *)
  val loc : t -> Loc.t

  (** [valid t] is [true] if [t]'s binding is still open, and [false] otherwise. Once [t]'s binding has
      been closed there is no valid way to use [t]. *)
  val valid : t -> bool

  module Set : sig
    (** [t] is the type of sets of variables. *)
    type t

    (** [empty] is the empty set. *)
    val empty : t

    (** [singleton v] is the set containing just [v]. *)
    val singleton : var -> t

    (** [add v t] is the set containing [v] and the elements of [t]. *)
    val add : var -> t -> t

    (** [union ~shared t1 t2] is the union of [t1] and [t2]. [shared] is run on all
        the variables that are in both [t1] and [t2]. *)
    val union : shared:(var -> unit) -> t -> t -> t

    (** [inter ~left_only ~right_only t1 t2] is the intersection of [t1]
        and [t2]. [left_only] is run on all the variables that are in
        [t1] and not in [t2]. [right_only] is run on all the variables
        that are in [t2] and not in [t1]. *)
    val inter :
      left_only:(var -> unit) -> right_only:(var -> unit) -> t -> t -> t

    (** [iter f t] runs [f] on every variable in [t]. *)
    val iter : (var -> unit) -> t -> unit

    (** [choose t] is [None] if [t] is equal to [empty] and [Some v],
        where [v] is an element of [t], otherwise. *)
    val choose : t -> var option

    (** [differences ~left_only ~right_only t1 t2] runs [left_only] on every element of [t1]
        that is not an element of [t2] and [right_only] on every element of [t2] that is
        not an element of [t1]. *)
    val differences :
      left_only:(var -> unit) -> right_only:(var -> unit) -> t -> t -> unit
  end

  module Map : sig
    (** ['a t] is the type of maps from variables to ['a] values. *)
    type 'a t

    (** [empty] is the empty set. *)
    val empty : 'a t

    (** [singleton v data] is the map containing just a binding of [v] to [data]. *)
    val singleton : var -> 'a -> 'a t

    (** [add v data t] is the map containing the same bindings as [t]
        plus a binding of [v] to [data]. *)
    val add : var -> 'a -> 'a t -> 'a t

    (** [union ~join t1 t2] is the union of [t1] and [t2]. [join] is run on all
        the variables that are in both [t1] and [t2]. *)
    val union : (var -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

    (** [remove_level l t] is the map containing the bindings of [t] whose
        variables's level is not [l]. *)
    val remove_level : Level.t -> 'a t -> 'a t

    (** [iter f t] runs [f] on every variable in [t]. *)
    val iter : (var -> 'a -> unit) -> 'a t -> unit

    (** [choose t] is [None] if [t] is equal to [empty] and [Some (v, data)],
        where [v] is bound to [data] in [t], otherwise. *)
    val choose : 'a t -> (var * 'a) option
  end
end = struct
  type t =
    { name : string;
      stamp : Stamp.t;
      level : Level.t
    }

  module Set = struct
    type var = t

    module Stamp_map = Map.Make (Stamp)

    type t = var Stamp_map.t

    let empty = Stamp_map.empty

    let singleton v = Stamp_map.singleton v.stamp v

    let add v t = Stamp_map.add v.stamp v t

    let union ~shared t1 t2 =
      Stamp_map.union
        (fun x v1 v2 ->
          shared v2;
          Some v1)
        t1 t2

    let inter ~left_only ~right_only t1 t2 =
      Stamp_map.merge
        (fun _ v1 v2 ->
          match v1, v2 with
          | None, None -> None
          | Some v1, None ->
            left_only v1;
            None
          | None, Some v2 ->
            right_only v2;
            None
          | Some v1, Some _ -> Some v1)
        t1 t2

    let choose t =
      match Stamp_map.choose_opt t with None -> None | Some (_, v) -> Some v

    let iter f t = Stamp_map.iter (fun _ v -> f v) t

    let differences ~left_only ~right_only t1 t2 =
      ignore
        (Stamp_map.merge
           (fun _ v1 v2 ->
             match v1, v2 with
             | None, None -> None
             | Some v, None ->
               left_only v;
               None
             | None, Some v ->
               right_only v;
               None
             | Some v, Some _ -> None)
           t1 t2)
  end

  module Map = struct
    type var = t

    module Var_map = Map.Make (struct
      type t = var

      let compare t1 t2 = Stamp.compare t1.stamp t2.stamp
    end)

    module Level_map = Map.Make (Level)

    type 'a t = 'a Var_map.t Level_map.t

    let empty = Level_map.empty

    let singleton v data =
      Level_map.singleton v.level (Var_map.singleton v data)

    let add v data t =
      let update prev =
        let prev =
          match prev with None -> Var_map.empty | Some prev -> prev
        in
        let next = Var_map.add v data prev in
        Some next
      in
      Level_map.update v.level update t

    let remove_level = Level_map.remove

    let union combine t1 t2 =
      Level_map.union
        (fun x m1 m2 ->
          let m =
            Var_map.union
              (fun v data1 data2 ->
                let data = combine v data1 data2 in
                Some data)
              m1 m2
          in
          Some m)
        t1 t2

    let rec choose t =
      match Level_map.choose_opt t with
      | None -> None
      | Some (l, m) -> (
        match Var_map.choose_opt m with
        | None -> choose (Level_map.remove l t)
        | Some _ as res -> res)

    let iter f t =
      Level_map.iter (fun _ m -> Var_map.iter (fun v data -> f v data) m) t
  end

  let generate level name =
    let stamp = Stamp.fresh () in
    let name = name ^ "_" ^ Stamp.to_string stamp in
    { name; stamp; level }

  let name t = t.name

  let loc t = Level.loc t.level

  let level t = t.level

  let valid t = Level.check t.level

  module Module = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let name = name
  end

  module Value = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let name = name
  end

  module Type = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let name = name
  end
end

module With_bound_vars : sig
  (** ['a t] is the type of ['a]s paired with a set of variables that
      are bound by the ['a]. *)
  type 'a t

  (** [mk vars x] is [x] paired with [vars]. *)
  val mk : Var.Set.t -> 'a -> 'a t

  (** [return x] is [mk Var.Set.empty x]. *)
  val return : 'a -> 'a t

  (** [var v] is [mk (Var.Set.singleton v) v]. *)
  val var : Var.t -> Var.t t

  (** [value_var v] is [mk (Var.Set.singleton (Var.Value.generic v)) v]. *)
  val value_var : Var.Value.t -> Var.Value.t t

  (** [module_var v] is [mk (Var.Set.singleton (Var.Module.generic v)) v]. *)
  val module_var : Var.Module.t -> Var.Module.t t

  (** [map f t] is [f v], where [v] is the value of [t], paired with the
      same variables as [t]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [meet t1 t2] is [(v1, v2)], where [v1] and [v2] are the values of
      [t1] and [t2] respectively, paired with the union of the variables
      from [t1] and [t2] which are expected to be disjoint. If [t1] and
      [t2] share any variables then [duplicated] will be run on the
      shared variables.*)
  val meet : duplicated:(Var.t -> unit) -> 'a t -> 'b t -> ('a * 'b) t

  (** [join ~left_only ~right_only t1 t2] is [(v1, v2)], where [v1] and
      [v2] are the values of [t1] and [t2] respectively, paired with the
      intersection of the variables from [t1] and [t2] which are
      expected to be equal. If any variables in [t1] are missing from
      [t2] then [left_only] with be run on them. If any variables in
      [t2] are missing from [t1] then [right_only] will be run on
      them. *)
  val join :
    left_only:(Var.t -> unit) ->
    right_only:(Var.t -> unit) ->
    'a t ->
    'b t ->
    ('a * 'b) t

  (** [meet_all ~duplicated [t1; t2; ...]] is [[v1; v2; ...]], where
      each [vn] is the value of [tn], paired with the union of the
      variables from all the [ts] which are expected to be disjoint. If
      the [ts] share any variables then [duplicated] will be run on the
      shared variables. *)
  val meet_all : duplicated:(Var.t -> unit) -> 'a t list -> 'a list t

  (** [optional tm] is [Some v], where [v] is the value of [t], paired
      with the variables from [t] if [tm] is [Some t]. It is [return
      None] if [tm] is [None]. *)
  val optional : 'a t option -> 'a option t

  (** [value ~extra ~missing t vs] is [t]'s value. [t] is expected to
      bind exactly the variables [vs]. [extra] is run on any additional
      variables that are bound. [missing] is run on any variables that
      are missing. *)
  val value :
    extra:(Var.t -> unit) -> missing:(Var.t -> unit) -> 'a t -> Var.t list -> 'a

  (** [value_nonbinding ~extra t] is [t]'s value. [t] is expected to
      bind no variables. [extra] is run on any additional variables that
      are bound. *)
  val value_nonbinding : extra:(Var.t -> unit) -> 'a t -> 'a
end = struct
  type 'a t =
    { vars : Var.Set.t;
      v : 'a
    }

  let mk vars v = { vars; v }

  let return v = mk Var.Set.empty v

  let var v = mk (Var.Set.singleton v) v

  let value_var v = mk (Var.Set.singleton (Var.Value.generic v)) v

  let module_var v = mk (Var.Set.singleton (Var.Module.generic v)) v

  let map f { vars; v } =
    let v = f v in
    { vars; v }

  let meet ~duplicated { vars = vars1; v = v1 } { vars = vars2; v = v2 } =
    let vars = Var.Set.union ~shared:duplicated vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let join ~left_only ~right_only { vars = vars1; v = v1 }
      { vars = vars2; v = v2 } =
    let vars = Var.Set.inter ~left_only ~right_only vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let meet_all ~duplicated l =
    let vars, v =
      List.fold_left_map
        (fun acc { vars; v } ->
          let acc = Var.Set.union ~shared:duplicated acc vars in
          acc, v)
        Var.Set.empty l
    in
    { vars; v }

  let optional = function None -> return None | Some t -> map Option.some t

  let value ~extra ~missing t vs =
    let expected =
      List.fold_left (fun acc v -> Var.Set.add v acc) Var.Set.empty vs
    in
    Var.Set.differences ~left_only:extra ~right_only:missing t.vars expected;
    t.v

  let value_nonbinding ~extra t =
    Var.Set.iter extra t.vars;
    t.v
end

module With_free_vars : sig
  (** ['a t] is the type of ['a]s paired with a set of variables that
      are free in the ['a] along with their associated locations. *)
  type 'a t

  (** [mk vars x] is [x] paired with [vars]. *)
  val mk : Loc.t Var.Map.t -> 'a -> 'a t

  (** [return x] is [mk Var.Map.empty x]. *)
  val return : 'a -> 'a t

  (** [map f t] is [f v], where [v] is the value of [t], paired with the
      same variables as [t]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [both t1 t2] is [(v1, v2)], where [v1] and [v2] are the values of
      [t1] and [t2] respectively, paired with the union of the variables
      from [t1] and [t2]. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** [all [t1; t2; ...]] is [[v1; v2; ...]], where each [vn] is the
      value of [tn], paired with the union of the variables from all the
      [ts]. *)
  val all : 'a t list -> 'a list t

  (** [optional tm] is [Some t], where [v] is the value of [t], paired
      with the variables from [t] if [tm] is [Some t]. It is [return
      None] if [tm] is [None]. *)
  val optional : 'a t option -> 'a option t

  (** [check ~invalid t] runs [invalid] on any variables paired with [t]
      that are not valid. *)
  val check : invalid:(Var.t -> Loc.t -> unit) -> 'a t -> unit

  (** [value ~free t] is the value of [t]. [t] is expected to have no
      free variables. [free] is run on any variables paired with [t]. *)
  val value : free:(Var.t -> Loc.t -> unit) -> 'a t -> 'a

  val value_nonbinding : extra:(Var.t -> Loc.t -> unit) -> 'a t -> 'a

  val value_binding : Loc.t -> Name.t -> (Var.Value.t -> 'a t) -> 'a t

  val type_binding : Loc.t -> Name.t -> (Var.Type.t -> 'a t) -> 'a t

  val type_bindings : Loc.t -> Name.t list -> (Var.Type.t list -> 'a t) -> 'a t

  val module_binding : Loc.t -> Name.t -> (Var.Module.t -> 'a t) -> 'a t

  val value_bindings :
    Loc.t -> Name.t list -> (Var.Value.t list -> 'a t) -> 'a t

  val complex_bindings :
    Loc.t ->
    extra:(Var.t -> unit) ->
    missing:(Var.t -> unit) ->
    bound_values:Name.t list ->
    bound_modules:Name.t list ->
    (Var.Value.t list -> Var.Module.t list -> 'a With_bound_vars.t t * 'b t) ->
    ('a * 'b) t
end = struct
  type 'a t =
    { vars : Loc.t Var.Map.t;
      v : 'a
    }

  let mk vars v = { vars; v }

  let return v = mk Var.Map.empty v

  let map f { vars; v } =
    let v = f v in
    { vars; v }

  let both { vars = vars1; v = v1 } { vars = vars2; v = v2 } =
    let vars = Var.Map.union (fun _ loc1 _ -> loc1) vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let all ts =
    let vars, v =
      List.fold_left_map
        (fun acc { vars; v } ->
          let acc = Var.Map.union (fun _ loc1 _ -> loc1) acc vars in
          acc, v)
        Var.Map.empty ts
    in
    { vars; v }

  let optional = function None -> return None | Some t -> map Option.some t

  let check ~invalid { vars; v } =
    Var.Map.iter
      (fun var loc -> if not (Var.valid var) then invalid var loc)
      vars

  let value ~free t =
    Var.Map.iter free t.vars;
    t.v

  let value_nonbinding ~extra t =
    Var.Map.iter extra t.vars;
    t.v

  let value_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Value.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Type.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_bindings loc names f =
    Level.with_fresh loc (fun level ->
        let fresh_vars =
          List.map (fun name -> Var.Type.generate level name) names
        in
        let { vars; v } = f fresh_vars in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let module_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Module.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let value_bindings loc names f =
    Level.with_fresh loc (fun level ->
        let fresh_vars =
          List.map (fun name -> Var.Value.generate level name) names
        in
        let { vars; v } = f fresh_vars in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let complex_bindings loc ~extra ~missing ~bound_values ~bound_modules f =
    Level.with_fresh loc (fun level ->
        let fresh_value_vars =
          List.map (Var.Value.generate level) bound_values
        in
        let fresh_module_vars =
          List.map (Var.Module.generate level) bound_modules
        in
        let { vars = bindings_vars; v = bindings }, { vars; v } =
          f fresh_value_vars fresh_module_vars
        in
        let fresh_vars =
          List.map Var.Module.generic fresh_module_vars
          @ List.map Var.Value.generic fresh_value_vars
        in
        let bindings =
          With_bound_vars.value ~extra ~missing bindings fresh_vars
        in
        let vars = Var.Map.remove_level level vars in
        let vars = Var.Map.union (fun _ loc1 _ -> loc1) bindings_vars vars in
        let v = bindings, v in
        { vars; v })
end

module With_free_and_bound_vars = struct
  type 'a t = 'a With_bound_vars.t With_free_vars.t

  let return p = With_free_vars.return (With_bound_vars.return p)

  let with_no_free_vars v = With_free_vars.return v

  let with_no_bound_vars v = With_free_vars.map With_bound_vars.return v

  let bound_value_var v = with_no_free_vars (With_bound_vars.value_var v)

  let bound_module_var v = with_no_free_vars (With_bound_vars.module_var v)

  let map f t = With_free_vars.map (With_bound_vars.map f) t

  let meet ~duplicated t1 t2 =
    With_free_vars.map
      (fun (t1, t2) -> With_bound_vars.meet ~duplicated t1 t2)
      (With_free_vars.both t1 t2)

  let join ~left_only ~right_only t1 t2 =
    With_free_vars.map
      (fun (t1, t2) -> With_bound_vars.join ~left_only ~right_only t1 t2)
      (With_free_vars.both t1 t2)

  let meet_all ~duplicated ts =
    With_free_vars.map
      (With_bound_vars.meet_all ~duplicated)
      (With_free_vars.all ts)

  let optional ts =
    With_free_vars.map With_bound_vars.optional (With_free_vars.optional ts)
end

module Ident = struct
  module Module : sig
    type t

    val compilation_unit : string -> t

    val dot : t -> string -> t

    val var : Var.Module.t -> Loc.t -> t

    val free_vars : t -> Loc.t Var.Map.t

    val with_free_vars : t -> t With_free_vars.t
  end = struct
    type t =
      | Compilation_unit of string
      | Dot of t * string
      | Var of Var.Module.t * Loc.t

    let compilation_unit s = Compilation_unit s

    let dot t s = Dot (t, s)

    let var v loc = Var (v, loc)

    let rec free_vars = function
      | Compilation_unit _ -> Var.Map.empty
      | Dot (t, _) -> free_vars t
      | Var (v, loc) -> Var.Map.singleton (Var.Module.generic v) loc

    let with_free_vars t = With_free_vars.mk (free_vars t) t
  end

  module Value : sig
    type t

    val dot : Module.t -> string -> t

    val var : Var.Value.t -> Loc.t -> t

    val with_free_vars : t -> t With_free_vars.t
  end = struct
    type t =
      | Dot of Module.t * string
      | Var of Var.Value.t * Loc.t

    let dot t s = Dot (t, s)

    let var v l = Var (v, l)

    let free_vars = function
      | Dot (d, s) -> Module.free_vars d
      | Var (v, loc) -> Var.Map.singleton (Var.Value.generic v) loc

    let with_free_vars t = With_free_vars.mk (free_vars t) t
  end

  module Type : sig
    type t

    val dot : Module.t -> string -> t

    val var : Var.Type.t -> Loc.t -> t

    val with_free_vars : t -> t With_free_vars.t

    val int : t

    val char : t

    val string : t

    val bytes : t

    val float : t

    val float32 : t

    val bool : t

    val unit : t

    val exn : t

    val array : t

    val iarray : t

    val list : t

    val option : t

    val nativeint : t

    val int32 : t

    val int64 : t

    val lazy_t : t

    val extension_constructor : t

    val floatarray : t

    val lexing_position : t

    val code : t

    val unboxed_float : t

    val unboxed_nativeint : t

    val unboxed_int32 : t

    val unboxed_int64 : t

    val int8x16 : t

    val int16x8 : t

    val int32x4 : t

    val int64x2 : t

    val float32x4 : t

    val float64x2 : t
  end = struct
    type t =
      | Dot of Module.t * string
      | Var of Var.Type.t * Loc.t
      | Builtin of string

    let dot t s = Dot (t, s)

    let var v l = Var (v, l)

    let free_vars = function
      | Dot (t, _) -> Module.free_vars t
      | Var (v, loc) -> Var.Map.singleton (Var.Type.generic v) loc
      | Builtin _ -> Var.Map.empty

    let with_free_vars t = With_free_vars.mk (free_vars t) t

    let int = Builtin "int"

    let char = Builtin "char"

    let string = Builtin "string"

    let bytes = Builtin "bytes"

    let float = Builtin "float"

    let float32 = Builtin "float32"

    let bool = Builtin "bool"

    let unit = Builtin "unit"

    let exn = Builtin "exn"

    let array = Builtin "array"

    let iarray = Builtin "iarray"

    let list = Builtin "list"

    let option = Builtin "option"

    let nativeint = Builtin "nativeint"

    let int32 = Builtin "int32"

    let int64 = Builtin "int64"

    let lazy_t = Builtin "lazy_t"

    let extension_constructor = Builtin "extension_constructor"

    let floatarray = Builtin "floatarray"

    let lexing_position = Builtin "lexing_position"

    let code = Builtin "code"

    let unboxed_float = Builtin "float#"

    let unboxed_nativeint = Builtin "nativeint#"

    let unboxed_int32 = Builtin "int32#"

    let unboxed_int64 = Builtin "int64#"

    let int8x16 = Builtin "int8x16"

    let int16x8 = Builtin "int16x8"

    let int32x4 = Builtin "int32x4"

    let int64x2 = Builtin "int64x2"

    let float32x4 = Builtin "float32x4"

    let float64x2 = Builtin "float64x2"
  end

  module Module_type : sig
    type t

    val dot : Module.t -> string -> t

    val with_free_vars : t -> t With_free_vars.t
  end = struct
    type t = Dot of Module.t * string

    let dot t s = Dot (t, s)

    let free_vars = function Dot (t, _) -> Module.free_vars t

    let with_free_vars t = With_free_vars.mk (free_vars t) t
  end

  module Constructor : sig
    type t

    val dot : Module.t -> string -> t

    val with_free_vars : t -> t With_free_vars.t

    val with_free_and_bound_vars : t -> t With_free_and_bound_vars.t

    val false_ : t

    val true_ : t

    val void : t

    val nil : t

    val cons : t

    val none : t

    val some : t

    val match_failure : t

    val out_of_memory : t

    val invalid_argument : t

    val failure : t

    val not_found : t

    val sys_error : t

    val end_of_file : t

    val division_by_zero : t

    val stack_overflow : t

    val sys_blocked_io : t

    val assert_failure : t

    val undefined_recursive_module : t
  end = struct
    type t =
      | Dot of Module.t * string
      | Builtin of string

    let dot t s = Dot (t, s)

    let free_vars = function
      | Dot (t, _) -> Module.free_vars t
      | Builtin s -> Var.Map.empty

    let with_free_vars t = With_free_vars.mk (free_vars t) t

    let with_free_and_bound_vars t =
      With_free_and_bound_vars.with_no_bound_vars (with_free_vars t)

    let false_ = Builtin "false"

    let true_ = Builtin "true"

    let void = Builtin "()"

    let nil = Builtin "[]"

    let cons = Builtin "(::)"

    let none = Builtin "None"

    let some = Builtin "Some"

    let match_failure = Builtin "Match_failure"

    let out_of_memory = Builtin "Out_of_memory"

    let invalid_argument = Builtin "Invalid_argument"

    let failure = Builtin "Failure"

    let not_found = Builtin "Not_found"

    let sys_error = Builtin "Sys_error"

    let end_of_file = Builtin "End_of_file"

    let division_by_zero = Builtin "Division_by_zero"

    let stack_overflow = Builtin "Stack_overflow"

    let sys_blocked_io = Builtin "Sys_blocked_io"

    let assert_failure = Builtin "Assert_failure"

    let undefined_recursive_module = Builtin "Undefined_recursive_module"
  end

  module Field : sig
    type t

    val dot : Module.t -> string -> t

    val with_free_vars : t -> t With_free_vars.t

    val with_free_and_bound_vars : t -> t With_free_and_bound_vars.t
  end = struct
    type t = Dot of Module.t * string

    let dot t s = Dot (t, s)

    let free_vars = function Dot (t, _) -> Module.free_vars t

    let with_free_vars t = With_free_vars.mk (free_vars t) t

    let with_free_and_bound_vars t =
      With_free_and_bound_vars.with_no_bound_vars (with_free_vars t)
  end
end

module Ast = struct
  type constant =
    | Int of int
    | Char of char
    | String of string * string option
    | Float of string
    | Int32 of int32
    | Int64 of int64
    | Nativeint of nativeint

  type rec_flag =
    | Nonrecursive
    | Recursive

  type direction_flag =
    | Upto
    | Downto

  type override_flag =
    | Override
    | Fresh

  type record_flag =
    | OpenRec
    | ClosedRec

  type variant_form =
    | Fixed
    | Open
    | Closed of string list

  type arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  type tuple_label =
    | NolabelTup
    | LabelledTup of string

  type core_type =
    | AnyType
    | TypeVar of Var.Type.t
    | Arrow of arg_label * core_type * core_type
    | TupleType of (tuple_label * core_type) list
    | Constr of Ident.Constructor.t * core_type list
    | Alias of core_type * Var.Type.t
    | VariantType of row_field list * variant_form
    | Poly of Var.Type.t list * core_type
    | Package of package_type

  and row_field =
    | Tag of string * bool * core_type list
    | Inherit of core_type

  and package_type = Ident.Module.t * (fragment * core_type) list

  and fragment =
    | Name of Name.t
    | Dot of fragment * Name.t

  type type_constraint =
    | Constraint of core_type
    | Coerce of core_type option * core_type

  type pattern =
    | Any
    | Var of Var.Value.t
    | AliasPat of pattern * Var.Value.t
    | ConstantPat of constant
    | Interval of constant * constant
    | TuplePat of (tuple_label * pattern) list
    | ConstructPat of Ident.Constructor.t * pattern option
    | VariantPat of string * pattern option
    | RecordPat of (Ident.Field.t * pattern) list * record_flag
    | ArrayPat of pattern list
    | Or of pattern * pattern
    | ConstraintPat of pattern * core_type
    | LazyPat of pattern
    | UnpackPat of Var.Module.t
    | Exception of pattern

  type expression_attribute =
    | Inline
    | Inlined
    | Specialise
    | Specialised
    | Unrolled
    | Nontail
    | Tail
    | Poll
    | Loop
    | Tail_mod_cons
    | Quotation

  type expression =
    { desc : expression_desc;
      attributes : expression_attribute list
    }

  and expression_desc =
    | Ident of Ident.Value.t
    | Constant of constant
    | Let of rec_flag * value_binding list * expression
    | Fun of function_
    | Apply of expression * (arg_label * expression) list
    | Match of expression * case list
    | Try of expression * case list
    | Tuple of (tuple_label * expression) list
    | Construct of Ident.Constructor.t * expression option
    | Variant of string * expression option
    | Record of (Ident.Field.t * expression) list * expression option
    | Field of expression * Ident.Field.t
    | Setfield of expression * Ident.Field.t * expression
    | Array of expression list
    | Ifthenelse of expression * expression * expression option
    | Sequence of expression * expression
    | While of expression * expression
    | For of pattern * expression * expression * direction_flag * expression
    | ConstraintExp of expression * core_type
    | CoerceExp of expression * core_type option * core_type
    | Letmodule of Var.Module.t * module_expr * expression
    | Assert of expression
    | Lazy of expression
    | Pack of module_expr
    | OpenExp of override_flag * Ident.Module.t * expression
    | Quote of expression
    | Escape of expression
    | Splice of expression * Loc.t

  and case =
    { lhs : pattern;
      guard : expression option;
      rhs : expression option
    }

  and value_binding =
    { pat : pattern;
      expr : expression
    }

  and function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list

  and function_param =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of Var.Type.t

  and function_ =
    { params : function_param list;
      constraint_ : type_constraint option;
      body : function_body
    }

  and module_expr =
    | ModuleIdent of Ident.Module.t
    | ModuleApply of module_expr * module_expr
    | Apply_unit of module_expr
    | Unpack of expression
end

module Label = struct
  type t = Ast.arg_label

  module Nonoptional = struct
    type t = Ast.tuple_label

    let no_label = Ast.NolabelTup

    let labelled s = Ast.LabelledTup s
  end

  let nonoptional = function
    | Ast.NolabelTup -> Ast.Nolabel
    | Ast.LabelledTup s -> Ast.Labelled s

  let no_label = Ast.Nolabel

  let labelled s = Ast.Labelled s

  let optional s = Ast.Optional s
end

module Fragment = struct
  type t = Ast.fragment
end

module Constant = struct
  type t = Ast.constant

  let int i = Ast.Int i

  let char c = Ast.Char c

  let string s id = Ast.String (s, id)

  let float f = Ast.Float f

  let int32 i = Ast.Int32 i

  let int64 i = Ast.Int64 i

  let nativeint i = Ast.Nativeint i
end

module Variant = struct
  type t = (Ast.row_field list * Ast.variant_form) With_free_vars.t
end

module BindingError = struct
  let unexpected_binding_error var loc : unit =
    let msg =
      Format.asprintf
        "Unexpected binding detected for the identifier %s from %a at %a"
        (Var.name var) Loc.print (Var.loc var) Loc.print loc
    in
    failwith msg

  let unexpected_binding_error_at_loc loc var : unit =
    unexpected_binding_error var loc

  let missing_binding_error var : unit =
    let msg =
      Format.asprintf "Missing binding for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let unbound_var loc var use : unit =
    let msg =
      Format.asprintf
        "The quotation built at %a has unbound variables: variable %s used at \
         %a is not bound."
        Loc.print loc (Var.name var) Loc.print use
    in
    failwith msg

  let duplicate_binding_error var =
    let msg =
      Format.asprintf "Duplicate bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let mismatch_binding_error var =
    let msg =
      Format.asprintf "Mismatched bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg
end

module Pat = struct
  open With_free_and_bound_vars

  type t = Ast.pattern With_free_and_bound_vars.t

  let ( let+ ) x f = map f x

  let ( and+ ) t1 t2 =
    meet ~duplicated:BindingError.duplicate_binding_error t1 t2

  let all ts = meet_all ~duplicated:BindingError.duplicate_binding_error ts

  let join t1 t2 =
    join ~left_only:BindingError.mismatch_binding_error
      ~right_only:BindingError.mismatch_binding_error t1 t2

  let any = return Ast.Any

  let var v =
    let+ var = bound_value_var v in
    Ast.Var var

  let alias t v =
    let+ p = t and+ v = bound_value_var v in
    Ast.AliasPat (p, v)

  let constant const = return (Ast.ConstantPat const)

  let interval const1 const2 = return (Ast.Interval (const1, const2))

  let tuple ts =
    let ps =
      List.map
        (fun (lab, e) ->
          let+ v = e in
          lab, v)
        ts
    in
    let+ ps = all ps in
    Ast.TuplePat ps

  let construct lid tm =
    let+ pm = optional tm in
    Ast.ConstructPat (lid, pm)

  let variant label tm =
    let+ pm = optional tm in
    Ast.VariantPat (label, pm)

  let record fields closed =
    let closed_ast = if closed then Ast.ClosedRec else Ast.OpenRec in
    let ts =
      List.map
        (fun (lid, t) ->
          let+ lbl = Ident.Field.with_free_and_bound_vars lid and+ p = t in
          lbl, p)
        fields
    in
    let+ ps = all ts in
    Ast.RecordPat (ps, closed_ast)

  let array ts =
    let+ ps = all ts in
    Ast.ArrayPat ps

  let or_ t1 t2 =
    let+ p1, p2 = join t1 t2 in
    Ast.Or (p1, p2)

  let lazy_ t =
    let+ p = t in
    Ast.LazyPat p

  let unpack v =
    let+ v = bound_module_var v in
    Ast.UnpackPat v

  let exception_ t =
    let+ p = t in
    Ast.Exception p
end

module Type = struct
  type t = Ast.core_type With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let all = With_free_vars.all

  let any = With_free_vars.return Ast.AnyType

  let var v = With_free_vars.return (Ast.TypeVar v)

  let arrow lab lhs rhs =
    let+ l = lhs and+ r = rhs in
    Ast.Arrow (lab, l, r)

  let tuple ts =
    let w =
      List.map
        (fun (lab, t) ->
          let+ t = t in
          lab, t)
        ts
    in
    let+ e = all w in
    Ast.TupleType e

  let constr cons typs =
    let+ ts = all typs in
    Ast.Constr (cons, ts)

  let alias typ tv =
    let+ t = typ in
    Ast.Alias (t, tv)

  let variant variant =
    let+ row_fields, form = variant in
    Ast.VariantType (row_fields, form)

  let poly loc names f =
    With_free_vars.type_bindings loc names (fun tvs ->
        let+ t = f tvs in
        Ast.Poly (tvs, t))

  let package m spec =
    let s =
      List.map
        (fun (frag, typ) ->
          let+ t = typ in
          frag, t)
        spec
    in
    let+ specs = all s in
    Ast.Package (m, specs)
end

module Case = struct
  type t = Ast.case With_free_vars.t

  let mk lhs guard rhs : Ast.case = { lhs; guard; rhs }

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let nonbinding loc (pat : Pat.t) exp =
    let+ pat = pat and+ rhs = exp in
    let lhs =
      With_bound_vars.value_nonbinding
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        pat
    in
    mk lhs None (Some rhs)

  let simple loc name f =
    With_free_vars.value_binding loc name (fun var ->
        let+ rhs = f var in
        mk (Var var) None (Some rhs))

  let pattern loc ~bound_values ~bound_modules f =
    let+ lhs, rhs =
      With_free_vars.complex_bindings loc
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        ~missing:BindingError.missing_binding_error ~bound_values ~bound_modules
        f
    in
    mk lhs None (Some rhs)

  let guarded loc ~bound_values ~bound_modules f =
    let+ lhs, (guard, rhs) =
      With_free_vars.complex_bindings loc
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        ~missing:BindingError.missing_binding_error ~bound_values ~bound_modules
        (fun value_vars module_vars ->
          let lhs, guard, rhs = f value_vars module_vars in
          lhs, With_free_vars.both guard rhs)
    in
    mk lhs (Some guard) (Some rhs)

  let refutation loc ~bound_values ~bound_modules f =
    let+ lhs, _ =
      With_free_vars.complex_bindings loc
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        ~missing:BindingError.missing_binding_error ~bound_values ~bound_modules
        (fun v m -> f v m, With_free_vars.return ())
    in
    mk lhs None None
end

module Type_constraint = struct
  type t = Ast.type_constraint With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let constraint_ typ =
    let+ typ = typ in
    Ast.Constraint typ

  let coercion typ1 typ2 =
    let+ typ1 = With_free_vars.optional typ1 and+ typ2 = typ2 in
    Ast.Coerce (typ1, typ2)
end

module Function = struct
  type t = Ast.function_ With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let mk params constraint_ body : Ast.function_ = { params; constraint_; body }

  let body e =
    let+ b = e in
    mk [] None (Ast.Pfunction_body b)

  let cases cl =
    let+ c = With_free_vars.all cl in
    mk [] None (Ast.Pfunction_cases c)

  let param_nonbinding loc lab exp pat rest =
    let+ ({ params; constraint_; body } : Ast.function_) = rest
    and+ pat = pat
    and+ e = With_free_vars.optional exp in
    let p =
      With_bound_vars.value_nonbinding
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        pat
    in
    let param = Ast.Pparam_val (Ast.Nolabel, e, p) in
    mk (param :: params) constraint_ body

  let param_simple lab exp loc name rest =
    With_free_vars.value_binding loc name (fun var ->
        let+ ({ params; constraint_; body } : Ast.function_) = rest var
        and+ e = With_free_vars.optional exp in
        mk (Ast.Pparam_val (lab, e, Ast.Var var) :: params) constraint_ body)

  let newtype loc name f =
    With_free_vars.type_binding loc name (fun typ ->
        let+ ({ params; constraint_; body } : Ast.function_) = f typ in
        mk (Ast.Pparam_newtype typ :: params) constraint_ body)
end

module Code = struct
  type code_rep =
    { exp : Ast.expression;
      loc : Loc.t
    }

  type t = code_rep With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let of_exp exp loc =
    let+ exp = exp in
    { exp; loc }

  let of_exp_with_type_vars loc names body =
    let+ exp = With_free_vars.type_bindings loc names body in
    { exp; loc }

  module Closed = struct
    type exp = t

    type t = code_rep

    let close code =
      With_free_vars.value
        ~free:(fun var loc ->
          let msg =
            Format.asprintf
              "The code built at %a is not closed: identifier %s bound at %a \
               is free"
              Loc.print loc (Var.name var) Loc.print (Var.loc var)
          in
          failwith (Format.flush_str_formatter ()))
        code

    let open_ = With_free_vars.return
  end
end

module Exp = struct
  type t = Ast.expression With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let all = With_free_vars.all

  let optional = With_free_vars.optional

  let return = With_free_vars.return

  let mk_vb pat expr : Ast.value_binding = { pat; expr }

  let mk desc attributes : Ast.expression = { desc; attributes }

  let ident id =
    let+ id = Ident.Value.with_free_vars id in
    mk (Ident id) []

  let constant (const : Constant.t) = return (mk (Constant const) [])

  let let_nonbinding loc pat def body =
    let+ def = def and+ body = body and+ pat = pat in
    let p =
      With_bound_vars.value_nonbinding
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        pat
    in
    let vbs = [mk_vb p def] in
    mk (Let (Nonrecursive, vbs, body)) []

  let let_simple loc name def f =
    let+ def = def
    and+ pat, body =
      With_free_vars.value_binding loc name (fun var ->
          let+ body = f var in
          let pat = Ast.Var var in
          pat, body)
    in
    let vbs = [mk_vb pat def] in
    mk (Let (Nonrecursive, vbs, body)) []

  let let_rec_simple loc names f =
    With_free_vars.value_bindings loc names (fun vars ->
        let defs, body = f vars in
        let+ defs = all defs and+ body = body in
        let vbs_rev =
          List.rev_map2
            (fun var def ->
              let pat = Ast.Var var in
              mk_vb pat def)
            vars defs
        in
        let vbs = List.rev vbs_rev in
        mk (Let (Recursive, vbs, body)) [])

  let let_ loc names def f =
    With_free_vars.value_bindings loc names (fun vars ->
        let pat, body = f vars in
        let+ pat, body = With_free_vars.both pat body and+ def = def in
        let pat =
          With_bound_vars.value
            ~extra:(BindingError.unexpected_binding_error_at_loc loc)
            ~missing:BindingError.missing_binding_error pat
            (List.map Var.Value.generic vars)
        in
        let vbs = [mk_vb pat def] in
        mk (Let (Nonrecursive, vbs, body)) [])

  let function_ fn =
    let+ fn = fn in
    mk (Fun fn) []

  let apply fn args =
    let args =
      List.map
        (fun (lab, arg) ->
          let+ exp = arg in
          lab, exp)
        args
    in
    let+ args = all args and+ fn = fn in
    mk (Apply (fn, args)) []

  let match_ exp cases =
    let+ cases = all cases and+ exp = exp in
    mk (Match (exp, cases)) []

  let try_ exp cases =
    let+ cases = all cases and+ exp = exp in
    mk (Try (exp, cases)) []

  let tuple fs =
    let entries =
      List.map
        (fun (lab, exp) ->
          let+ exp = exp in
          lab, exp)
        fs
    in
    let+ entries = all entries in
    mk (Tuple entries) []

  let construct cons exp =
    let+ exp = optional exp in
    mk (Construct (cons, exp)) []

  let variant v exp =
    let+ exp = optional exp in
    mk (Variant (v, exp)) []

  let record entries exp =
    let entries =
      List.map
        (fun (field, e) ->
          let+ e = e in
          field, e)
        entries
    in
    let+ entries = all entries and+ exp = optional exp in
    mk (Record (entries, exp)) []

  let field exp field =
    let+ exp = exp in
    mk (Field (exp, field)) []

  let setfield exp field value =
    let+ exp = exp and+ value = value in
    mk (Setfield (exp, field, value)) []

  let array elements =
    let+ elements = all elements in
    mk (Array elements) []

  let ifthenelse cond then_ else_ =
    let+ cond = cond and+ then_ = then_ and+ else_ = optional else_ in
    mk (Ifthenelse (cond, then_, else_)) []

  let sequence lhs rhs =
    let+ lhs = lhs and+ rhs = rhs in
    mk (Sequence (lhs, rhs)) []

  let while_ cond body =
    let+ cond = cond and+ body = body in
    mk (While (cond, body)) [Loop]

  let for_nonbinding loc pat begin_ end_ dir body =
    let dir = if dir then Ast.Upto else Ast.Downto in
    let+ body = body and+ pat = pat and+ begin_ = begin_ and+ end_ = end_ in
    let pat =
      With_bound_vars.value_nonbinding
        ~extra:(BindingError.unexpected_binding_error_at_loc loc)
        pat
    in
    mk (For (pat, begin_, end_, dir, body)) [Loop]

  let for_simple loc name begin_ end_ dir body =
    let dir = if dir then Ast.Upto else Ast.Downto in
    let+ var, body =
      With_free_vars.value_binding loc name (fun var ->
          let+ body = body var in
          var, body)
    and+ begin_ = begin_
    and+ end_ = end_ in
    mk (For (Var var, begin_, end_, dir, body)) [Loop]

  let constraint_ exp constr =
    let+ exp = exp and+ constr = constr in
    let desc =
      match constr with
      | Ast.Constraint typ -> Ast.ConstraintExp (exp, typ)
      | Ast.Coerce (typ1, typ2) -> Ast.CoerceExp (exp, typ1, typ2)
    in
    mk desc []

  let assert_ exp =
    let+ exp = exp in
    mk (Assert exp) []

  let lazy_ exp =
    let+ exp = exp in
    mk (Lazy exp) []

  let open_ override m rhs =
    let flag = if override then Ast.Override else Ast.Fresh in
    let+ exp = rhs in
    mk (OpenExp (flag, m, exp)) []

  let quote exp =
    let+ exp = exp in
    mk (Quote exp) [Quotation]

  let escape exp =
    let+ exp = exp in
    mk (Escape exp) []

  let splice code =
    let+ ({ exp; loc } : Code.code_rep) = code in
    mk (Splice (exp, loc)) []
end
