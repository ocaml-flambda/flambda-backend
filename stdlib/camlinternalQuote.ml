module Stamp : sig
  (** [t] is the type of stamps. Stamps have no structure, only a notion
      of identity. *)
  type t

  (** [fresh ()] creates a fresh stamp, unequal to any other. *)
  val fresh : unit -> t

  (** [compare] is a total ordering on stamps. *)
  val compare : t -> t -> int
end = struct
  type t = int

  external fresh : unit -> int = "caml_fresh_oo_id" [@@noalloc]

  let compare = Int.compare
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
    | Unknown -> Format.fprintf fmt "<unknown>"
    | Known { file; start_line; start_col; end_line; end_col } ->
      if start_line = end_line
      then
        Format.fprintf fmt "File %s, line %d, characters %d-%d" file start_line
          start_col end_col
      else
        Format.fprintf fmt "File %s, lines %d-%d, characters %d-%d" file
          start_line end_line start_col end_col
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

let let_prefixes = ["let"; "and"]

module Name = struct
  type t = string

  let mk t = t

  let base_name s =
    match String.index_opt s '_' with
    | None ->
      if List.for_all (fun p -> not (String.starts_with ~prefix:p s)) let_prefixes
      then s
      else "(" ^ s ^ ")"
    | Some i -> String.sub s 0 i

  let print fmt s = Format.fprintf fmt "%s" s
end

type stamp_num = (Stamp.t, int) Hashtbl.t

type name_counter = (string, int) Hashtbl.t

type env_rec =
  { value : name_counter;
    type_var : name_counter;
    type_constr : name_counter;
    module_ : name_counter
  }

type env =
  { stamps : stamp_num;
    env : env_rec
  }

let new_env () =
  { stamps = Hashtbl.create 32;
    env =
      { value = Hashtbl.create 32;
        type_var = Hashtbl.create 32;
        type_constr = Hashtbl.create 32;
        module_ = Hashtbl.create 32
      }
  }

let print_name_and_count fmt (name, count) =
  if count > 0
  then Format.fprintf fmt "%s__%d" name count
  else Format.fprintf fmt "%s" name

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

    (** [generic t] is [t] as a list of generic variables *)
    val generic_list : t list -> var list

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t

    (** [print env formatter t] prints [t] using [formatter], with [env] as the
        naming environment. *)
    val print : env -> Format.formatter -> t -> unit
  end

  module Value : sig
    (** [t] is the type of ordinary variables *)
    type t

    (** [generate lv n] creates a fresh variable bound at [lv] whose
        name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [generic t] is [t] as a list of generic variables *)
    val generic_list : t list -> var list

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t

    (** [print env formatter t] prints [t] using [formatter], with [env] as the
        naming environment. *)
    val print : env -> Format.formatter -> t -> unit
  end

  module Type_var : sig
    (** [t] is the type of type variables *)
    type t

    (** [generate lv n] creates a fresh type constructor variable
        bound at [lv] whose name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [generic_list t] is [t] as a list of generic variables *)
    val generic_list : t list -> var list

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t

    (** [print env formatter t] prints [t] using [formatter], with [env] as the
        naming environment. *)
    val print : env -> Format.formatter -> t -> unit
  end

  module Type_constr : sig
    (** [t] is the type of type constructor variables *)
    type t

    (** [generate lv n] creates a fresh type constructor variable
        bound at [lv] whose name is [n]. *)
    val generate : Level.t -> Name.t -> t

    (** [generic t] is [t] as a generic variable *)
    val generic : t -> var

    (** [generic t] is [t] as a list of generic variables *)
    val generic_list : t list -> var list

    (** [name t] is the name of [t]. *)
    val name : t -> Name.t

    (** [print env formatter t] prints [t] using [formatter], with [env] as the
        naming environment. *)
    val print : env -> Format.formatter -> t -> unit
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
        (fun _ v1 v2 ->
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
             | _ -> None)
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
        (fun _ m1 m2 ->
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
    { name; stamp; level }

  let name t = t.name

  let loc t = Level.loc t.level

  let level t = t.level

  let valid t = Level.check t.level

  let print_var_env stamp_env name_env fmt var =
    match Hashtbl.find_opt stamp_env var.stamp with
    | Some count -> print_name_and_count fmt (Name.base_name var.name, count)
    | None -> (
      let dict_update_and_print new_val =
        Hashtbl.add stamp_env var.stamp new_val;
        Hashtbl.add name_env (Name.base_name var.name) new_val;
        print_name_and_count fmt (Name.base_name var.name, new_val)
      in
      match Hashtbl.find_opt name_env (Name.base_name var.name) with
      | Some v -> dict_update_and_print (v + 1)
      | None -> dict_update_and_print 0)

  module Module = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let generic_list = List.map generic

    let name = name

    let print env fmt t =
      Format.fprintf fmt "%a" (print_var_env env.stamps env.env.module_) t
  end

  module Value = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let generic_list = List.map generic

    let name = name

    let print env fmt t =
      Format.fprintf fmt "%a" (print_var_env env.stamps env.env.value) t
  end

  module Type_var = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let generic_list = List.map generic

    let name = name

    let print env fmt t =
      Format.fprintf fmt "%a" (print_var_env env.stamps env.env.type_var) t
  end

  module Type_constr = struct
    type var = t

    type t = var

    let generate = generate

    let generic t = t

    let generic_list = List.map generic

    let name = name

    let print env fmt t =
      Format.fprintf fmt "%a" (print_var_env env.stamps env.env.type_constr) t
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

  (** [value_binding loc name t] is the value of [t]. [t] is expected to have
      a single free variable named [name]. [t] is represented as a function
      from this variable to the term itself. *)
  val value_binding : Loc.t -> Name.t -> (Var.Value.t -> 'a t) -> 'a t

  (** [value_bindings loc names t] is the value of [t]. [t] is expected to have
      free variables whose names are given by [names]. [t] is represented as a
      function from these variables to the term itself. *)
  val value_bindings :
    Loc.t -> Name.t list -> (Var.Value.t list -> 'a t) -> 'a t

  (** [type_var_binding loc name t] is the value of [t]. [t] is expected to have
      a single free type variable named [name]. [t] is represented as a function
      from this type variable to the term itself. *)
  val type_var_binding : Loc.t -> Name.t -> (Var.Type_var.t -> 'a t) -> 'a t

  (** [type_var_bindings loc names t] is the value of [t]. [t] is expected to
      have free type variables whose names are given by [names]. [t] is
      represented as a function from these type variables to the term itself. *)
  val type_var_bindings :
    Loc.t -> Name.t list -> (Var.Type_var.t list -> 'a t) -> 'a t

  (** [type_binding loc names t] is the value of [t]. [t] is expected to have
      a single free type constructor variable named [name]. [t] is represented
      as a function from this type constructor variable to the term itself. *)
  val type_binding : Loc.t -> Name.t -> (Var.Type_constr.t -> 'a t) -> 'a t

  (** [type_bindings loc names t] is the value of [t]. [t] is expected to have
      free type constructor variables whose names are given by [names]. [t] is
      represented as a function from these type constructor variables to the term
      itself. *)
  val type_bindings :
    Loc.t -> Name.t list -> (Var.Type_constr.t list -> 'a t) -> 'a t

  (** [module_binding loc name t] is the value of [t]. [t] is expected to have
      a single free module variable named [name]. [t] is represented as a
      function from this module variable to the term itself. *)
  val module_binding : Loc.t -> Name.t -> (Var.Module.t -> 'a t) -> 'a t

  (** [complex_bindings ~extra ~missing ~bound_values ~bound_modules t] is the
      value of [t]. [t] is expected to have free value-kinded variables (named
      [bound_values]) and free module variables (named [bound_modules]).
      [t] is expected to bind exactly these variables. [extra] is run on any
      additional variables that are bound. [missing] is run on any variables that
      are missing. *)
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

  let check ~invalid { vars; _ } =
    Var.Map.iter
      (fun var loc -> if not (Var.valid var) then invalid var loc)
      vars

  let value ~free t =
    Var.Map.iter free t.vars;
    t.v

  let value_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Value.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Type_constr.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_bindings loc names f =
    Level.with_fresh loc (fun level ->
        let fresh_vars =
          List.map (fun name -> Var.Type_constr.generate level name) names
        in
        let { vars; v } = f fresh_vars in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_var_binding loc name f =
    Level.with_fresh loc (fun level ->
        let fresh_var = Var.Type_var.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let type_var_bindings loc names f =
    Level.with_fresh loc (fun level ->
        let fresh_vars =
          List.map (fun name -> Var.Type_var.generate level name) names
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
          Var.Module.generic_list fresh_module_vars
          @ Var.Value.generic_list fresh_value_vars
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

type raw_ident_module_t =
  | Compilation_unit of string
  | MDot of raw_ident_module_t * string
  | MVar of Var.Module.t * Loc.t

type raw_ident_value_t =
  | VDot of raw_ident_module_t * string
  | VVar of Var.Value.t * Loc.t

type raw_ident_type_t =
  | TDot of raw_ident_module_t * string
  | TVar of Var.Type_constr.t * Loc.t
  | TBuiltin of string

type raw_ident_constructor_t =
  | CDot of raw_ident_module_t * string
  | CBuiltin of string

type raw_ident_module_type_t = MtDot of raw_ident_module_t * string

type raw_ident_field_t = FDot of raw_ident_module_t * string

let special_symbols =
  [ '!';
    '?';
    '~';
    '=';
    '<';
    '>';
    '@';
    '^';
    '|';
    '&';
    '+';
    '-';
    '*';
    '/';
    '$';
    '%';
    '#' ]

let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::"]

let print_op fmt s =
  if List.mem s special_infix_strings || List.mem s.[0] special_symbols
  then Format.fprintf fmt "( %s )" s
  else Format.fprintf fmt "%s" s

let rec print_raw_ident_module env fmt = function
  | Compilation_unit s -> Format.fprintf fmt "%s" s
  | MDot (m, s) -> Format.fprintf fmt "%a.%s" (print_raw_ident_module env) m s
  | MVar (vm, _) -> Format.fprintf fmt "%a" (Var.Module.print env) vm

let print_raw_ident_value env fmt = function
  | VDot (m, s) ->
    Format.fprintf fmt "%a.%a" (print_raw_ident_module env) m print_op s
  | VVar (v, _) -> Var.Value.print env fmt v

let print_raw_ident_type env fmt = function
  | TDot (m, s) -> Format.fprintf fmt "%a.%s" (print_raw_ident_module env) m s
  | TVar (v, _) -> Var.Type_constr.print env fmt v
  | TBuiltin s -> Format.fprintf fmt "%s" s

let print_raw_ident_constructor env fmt = function
  | CDot (m, s) -> Format.fprintf fmt "%a.%s" (print_raw_ident_module env) m s
  | CBuiltin s -> Format.fprintf fmt "%s" s

let print_raw_ident_module_type env fmt = function
  | MtDot (m, s) -> Format.fprintf fmt "%a.%s" (print_raw_ident_module env) m s

let print_raw_ident_field env fmt = function
  | FDot (m, s) -> Format.fprintf fmt "%a.%s" (print_raw_ident_module env) m s

let rec free_vars_module = function
  | Compilation_unit _ -> Var.Map.empty
  | MDot (t, _) -> free_vars_module t
  | MVar (v, loc) -> Var.Map.singleton (Var.Module.generic v) loc

let free_vars_value = function
  | VDot (d, s) -> free_vars_module d
  | VVar (v, loc) -> Var.Map.singleton (Var.Value.generic v) loc

let free_vars_type = function
  | TDot (t, _) -> free_vars_module t
  | TVar (v, loc) -> Var.Map.singleton (Var.Type_constr.generic v) loc
  | TBuiltin _ -> Var.Map.empty

let free_vars_module_type = function MtDot (t, _) -> free_vars_module t

let free_vars_constructor = function
  | CDot (t, _) -> free_vars_module t
  | CBuiltin s -> Var.Map.empty

let free_vars_field = function FDot (t, _) -> free_vars_module t

module Identifier = struct
  module Module = struct
    type raw = raw_ident_module_t

    type t = raw_ident_module_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let ( and+ ) = With_free_vars.both

    let return = With_free_vars.return

    let mk t = With_free_vars.mk (free_vars_module t) t

    let compilation_unit s = mk (Compilation_unit s)

    let dot t s =
      let+ t = t in
      MDot (t, s)

    let var v loc = mk (MVar (v, loc))
  end

  module Value = struct
    type raw = raw_ident_value_t

    type t = raw_ident_value_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let ( and+ ) = With_free_vars.both

    let return = With_free_vars.return

    let mk t = With_free_vars.mk (free_vars_value t) t

    let dot t s =
      let+ t = t in
      VDot (t, s)

    let var v l = mk (VVar (v, l))
  end

  module Type = struct
    type raw = raw_ident_type_t

    type t = raw_ident_type_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let ( and+ ) = With_free_vars.both

    let return = With_free_vars.return

    let mk t = With_free_vars.mk (free_vars_type t) t

    let dot t s =
      let+ t = t in
      TDot (t, s)

    let var v l = mk (TVar (v, l))

    let int = TBuiltin "int" |> mk

    let char = TBuiltin "char" |> mk

    let string = TBuiltin "string" |> mk

    let bytes = TBuiltin "bytes" |> mk

    let float = TBuiltin "float" |> mk

    let float32 = TBuiltin "float32" |> mk

    let bool = TBuiltin "bool" |> mk

    let unit = TBuiltin "unit" |> mk

    let exn = TBuiltin "exn" |> mk

    let array = TBuiltin "array" |> mk

    let iarray = TBuiltin "iarray" |> mk

    let list = TBuiltin "list" |> mk

    let option = TBuiltin "option" |> mk

    let nativeint = TBuiltin "nativeint" |> mk

    let int32 = TBuiltin "int32" |> mk

    let int64 = TBuiltin "int64" |> mk

    let lazy_t = TBuiltin "lazy_t" |> mk

    let extension_constructor = TBuiltin "extension_constructor" |> mk

    let floatarray = TBuiltin "floatarray" |> mk

    let lexing_position = TBuiltin "lexing_position" |> mk

    let code = TBuiltin "code" |> mk

    let unboxed_float = TBuiltin "float#" |> mk

    let unboxed_nativeint = TBuiltin "nativeint#" |> mk

    let unboxed_int32 = TBuiltin "int32#" |> mk

    let unboxed_int64 = TBuiltin "int64#" |> mk

    let int8x16 = TBuiltin "int8x16" |> mk

    let int16x8 = TBuiltin "int16x8" |> mk

    let int32x4 = TBuiltin "int32x4" |> mk

    let int64x2 = TBuiltin "int64x2" |> mk

    let float32x4 = TBuiltin "float32x4" |> mk

    let float64x2 = TBuiltin "float64x2" |> mk
  end


  module Module_type = struct
    type raw = raw_ident_module_type_t

    type t = raw_ident_module_type_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let dot t s =
      let+ t = t in
      MtDot (t, s)
  end

  module Constructor = struct
    type raw = raw_ident_constructor_t

    type t = raw_ident_constructor_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let mk t = With_free_vars.mk (free_vars_constructor t) t

    let dot t s =
      let+ t = t in
      CDot (t, s)

    let false_ = CBuiltin "false" |> mk

    let true_ = CBuiltin "true" |> mk

    let void = CBuiltin "()" |> mk

    let nil = CBuiltin "[]" |> mk

    let cons = CBuiltin "(::)" |> mk

    let none = CBuiltin "None" |> mk

    let some = CBuiltin "Some" |> mk

    let match_failure = CBuiltin "Match_failure" |> mk

    let out_of_memory = CBuiltin "Out_of_memory" |> mk

    let invalid_argument = CBuiltin "Invalid_argument" |> mk

    let failure = CBuiltin "Failure" |> mk

    let not_found = CBuiltin "Not_found" |> mk

    let sys_error = CBuiltin "Sys_error" |> mk

    let end_of_file = CBuiltin "End_of_file" |> mk

    let division_by_zero = CBuiltin "Division_by_zero" |> mk

    let stack_overflow = CBuiltin "Stack_overflow" |> mk

    let sys_blocked_io = CBuiltin "Sys_blocked_io" |> mk

    let assert_failure = CBuiltin "Assert_failure" |> mk

    let undefined_recursive_module = CBuiltin "Undefined_recursive_module" |> mk
  end

  module Field = struct
    type t = raw_ident_field_t With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let dot t s =
      let+ t = t in
      FDot (t, s)
  end
end

module Variant = struct
  type t = string

  let of_string s = s

  let print fmt s = Format.fprintf fmt "`%s" s
end

module Method = struct
  type t = string

  let of_string s = s

  let print fmt s = Format.fprintf fmt "%s" s
end

module Ast = struct
  type constant =
    | Int of int
    | Int32 of int32
    | Int64 of int64
    | Nativeint of nativeint
    | Char of char
    | String of string * string option
    | Float of string
    | Float32 of string
    | UnboxedFloat of string
    | UnboxedFloat32 of string
    | UnboxedInt32 of int32
    | UnboxedInt64 of int64
    | UnboxedNativeint of nativeint

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
    | VFixed
    | VOpen
    | VClosed of string list

  type object_closed_flag =
    | OClosed
    | OOpen

  type arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  type tuple_label =
    | NolabelTup
    | LabelledTup of string

  type module_type =
    | MTIdent of raw_ident_module_type_t
    | MTBasic of string

  type constr =
    | CIdent of raw_ident_constructor_t
    | CBasic of string

  type record_field =
    | FIdent of raw_ident_field_t
    | FBasic of string

  type core_type =
    | TypeAny
    | TypeVar of Var.Type_var.t
    | TypeArrow of arg_label * core_type * core_type
    | TypeTuple of (tuple_label * core_type) list
    | TypeUnboxedTuple of (tuple_label * core_type) list
    | TypeConstr of raw_ident_type_t * core_type list
    | TypeObject of object_field list * object_closed_flag
    | TypeClass of Name.t * core_type list
    | TypeAlias of core_type * Name.t
    | TypeVariant of row_field list * variant_form
    | TypePoly of Name.t list * core_type
    | TypePackage of package_type
    | TypeCallPos

  and object_field =
    | Otag of Name.t * core_type
    | Oinherit of core_type

  and row_field =
    | Vtag of Variant.t * bool * core_type list
    | Vinherit of core_type

  and package_type = module_type * (fragment * core_type) list

  and fragment =
    | Name of Name.t
    | Dot of fragment * Name.t

  type type_constraint =
    | Constraint of core_type
    | Coerce of core_type option * core_type

  type pattern =
    | PatAny
    | PatVar of Var.Value.t
    | PatAlias of pattern * Var.Value.t
    | PatConstant of constant
    | PatTuple of (tuple_label * pattern) list
    | PatUnboxedTuple of (tuple_label * pattern) list
    | PatConstruct of constr * pattern option
    | PatVariant of Variant.t * pattern option
    | PatRecord of (record_field * pattern) list * record_flag
    | PatUnboxedRecord of (record_field * pattern) list * record_flag
    | PatArray of pattern list
    | PatOr of pattern * pattern
    | PatConstraint of pattern * core_type
    | PatLazy of pattern
    | PatAnyModule
    | PatUnpack of Var.Module.t
    | PatException of pattern

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
    | Ident of raw_ident_value_t
    | Constant of constant
    | Let of rec_flag * value_binding list * expression
    | Fun of function_
    | Apply of expression * (arg_label * expression) list
    | Match of expression * case list
    | Try of expression * case list
    | Tuple of (tuple_label * expression) list
    | Construct of constr * expression option
    | Variant of string * expression option
    | Record of (record_field * expression) list * expression option
    | Field of expression * record_field
    | Setfield of expression * record_field * expression
    | Array of expression list
    | Ifthenelse of expression * expression * expression option
    | Sequence of expression * expression
    | While of expression * expression
    | For of pattern * expression * expression * direction_flag * expression
    | Send of expression * Method.t
    | ConstraintExp of expression * core_type
    | CoerceExp of expression * core_type option * core_type
    | Letmodule of Var.Module.t option * module_expr * expression
    | Assert of expression
    | Lazy of expression
    | Pack of module_expr
    | New of raw_ident_value_t
    | Unreachable
    | Src_pos
    | Stack of expression
    | Exclave of expression
    | Unboxed_tuple of (tuple_label * expression) list
    | Unboxed_record_product of
        (record_field * expression) list * expression option
    | Unboxed_field of expression * record_field
    | Let_op of raw_ident_value_t list * expression list * case
    | Let_exception of Name.t * expression
    | Extension_constructor of Name.t
    | List_comprehension of comprehension
    | Array_comprehension of comprehension
    | Quote of expression
    | Antiquote of expression
    | Probe

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
    | Pparam_newtype of Var.Type_var.t

  and function_ =
    { params : function_param list;
      constraint_ : type_constraint option;
      body : function_body
    }

  and module_expr =
    | ModuleIdent of raw_ident_module_t
    | ModuleApply of module_expr * module_expr
    | ModuleApply_unit of module_expr

  and comprehension_clause =
    | Range of Var.Value.t * expression * expression * direction_flag
    | In of pattern * expression

  and comprehension =
    | Body of expression
    | When of comprehension * expression
    | ForComp of comprehension * comprehension_clause

  let print_tuple_like delim open_sym close_sym printer fmt entries =
    Format.fprintf fmt "%s@[" open_sym;
    (match entries with
    | [] -> ()
    | e :: es ->
      printer fmt e;
      List.iter (fun e -> Format.fprintf fmt "%s@ %a" delim printer e) es);
    Format.fprintf fmt "@]%s" close_sym

  let print_label_tup printer fmt (lab, entry) =
    match lab with
    | LabelledTup s -> Format.fprintf fmt "~%s:%a" s printer entry
    | NolabelTup -> printer fmt entry

  let print_array printer fmt entries =
    print_tuple_like ";" "[|" "|]" printer fmt entries

  let print_tuple printer fmt entries =
    print_tuple_like "," "(" ")" (print_label_tup printer) fmt entries

  let rec print_vb env fmt ({ pat; expr } : value_binding) =
    Format.fprintf fmt "%a@ =@ @[%a@]" (print_pat env) pat
      (print_exp_with_parens env)
      expr

  and print_const fmt = function
    | Int n -> Format.fprintf fmt "%d" n
    | Char c -> Format.fprintf fmt "%c" c
    | String (s, id_opt) -> (
      match id_opt with
      | None -> Format.fprintf fmt "\"%s\"" s
      | Some id -> Format.fprintf fmt "{%s|@[%s@]|%s}" id s id)
    | Float s -> Format.fprintf fmt "%s" s
    | Float32 s -> Format.fprintf fmt "%ss" s
    | Int32 n -> Format.fprintf fmt "%ld" n
    | Int64 n -> Format.fprintf fmt "%Ld" n
    | Nativeint n -> Format.fprintf fmt "%nd" n
    | UnboxedFloat s -> Format.fprintf fmt "#%s" s
    | UnboxedFloat32 s -> Format.fprintf fmt "#%ss" s
    | UnboxedInt32 n -> Format.fprintf fmt "#%ldl" n
    | UnboxedInt64 n -> Format.fprintf fmt "#%LdL" n
    | UnboxedNativeint n -> Format.fprintf fmt "#%ndn" n

  and print_constr env fmt = function
    | CBasic s -> Format.fprintf fmt "%s" s
    | CIdent id -> print_raw_ident_constructor env fmt id

  and print_module_type env fmt = function
    | MTBasic s -> Format.fprintf fmt "%s" s
    | MTIdent id -> print_raw_ident_module_type env fmt id

  and print_field env fmt = function
    | FBasic s -> Format.fprintf fmt "%s" s
    | FIdent id -> print_raw_ident_field env fmt id

  and print_pat env fmt = function
    | PatAny -> Format.fprintf fmt "_"
    | PatVar v -> Var.Value.print env fmt v
    | PatAlias (pat, v) ->
      Format.fprintf fmt "%a@ as@ %a"
        (print_pat env) pat (Var.Value.print env) v
    | PatConstant c -> print_const fmt c
    | PatTuple ts -> print_tuple (print_pat env) fmt ts
    | PatUnboxedTuple ts ->
      Format.fprintf fmt "#%a" (print_tuple (print_pat env)) ts
    | PatConstruct (ident, pat_opt) -> (
      match pat_opt with
      | None -> print_constr env fmt ident
      | Some pat ->
        Format.fprintf fmt "%a@ %a" (print_constr env) ident (print_pat env) pat
      )
    | PatVariant (variant, pat_opt) -> (
      match pat_opt with
      | None -> Variant.print fmt variant
      | Some pat ->
        Format.fprintf fmt "%a@ %a" Variant.print variant (print_pat env) pat)
    | PatRecord (entries, rec_flag) ->
      Format.fprintf fmt "{@[";
      List.iter
        (fun (field, pat) ->
           Format.fprintf fmt "%a=%a;@ " (print_field env) field (print_pat env)
             pat)
        entries;
      if rec_flag = OpenRec then Format.fprintf fmt "_";
      Format.fprintf fmt "@]}"
    | PatUnboxedRecord (entries, rec_flag) ->
      Format.fprintf fmt "#{@[";
      List.iter
        (fun (field, pat) ->
          Format.fprintf fmt "%a=%a;@ " (print_field env) field (print_pat env)
            pat)
        entries;
      if rec_flag = OpenRec then Format.fprintf fmt "_";
      Format.fprintf fmt "@]}"
    | PatArray pats -> print_array (print_pat env) fmt pats
    | PatOr (pat1, pat2) ->
      Format.fprintf fmt "%a@ |@ %a" (print_pat env) pat1 (print_pat env) pat2
    | PatConstraint (pat, ty) ->
      Format.fprintf fmt "(%a@ :@ %a)" (print_pat env) pat (print_core_type env)
        ty
    | PatLazy pat -> Format.fprintf fmt "lazy@ (%a)" (print_pat env) pat
    | PatAnyModule -> Format.fprintf fmt "module _"
    | PatUnpack v -> Format.fprintf fmt "module %a" (Var.Module.print env) v
    | PatException pat ->
      Format.fprintf fmt "(exception %a)" (print_pat env) pat

  and print_type_constraint env fmt = function
    | Constraint ty ->
      Format.fprintf fmt "%@ :@ @[%a@]" (print_core_type env) ty
    | Coerce (opt_ty, ty) ->
      Format.fprintf fmt "%@ :>@ @[%a@]" (print_core_type env) ty

  and print_exp_with_parens env fmt exp =
    match exp.desc with
    | Ident _ | Constant _ | Tuple _
    | Construct (_, None)
    | Variant (_, None)
    | Record (_, None)
    | Field _ | Array _ | Send _ | Unreachable | Src_pos | Unboxed_tuple _
    | Unboxed_record_product (_, None)
    | List_comprehension _ | Array_comprehension _ | Quote _ | Antiquote _ ->
      (print_exp env) fmt exp
    | _ -> Format.fprintf fmt "(@[%a@])" (print_exp env) exp

  and print_case env fmt { lhs; guard; rhs } =
    Format.fprintf fmt " | %a" (print_pat env) lhs;
    (match guard with
    | None -> ()
    | Some guard ->
      Format.fprintf fmt "@ with@ %a" (print_exp_with_parens env) guard);
    Format.fprintf fmt "@ ->@ ";
    match rhs with
    | None -> Format.fprintf fmt "."
    | Some rhs -> print_exp env fmt rhs

  and print_row_field env with_or fmt rf =
    if with_or then Format.fprintf fmt "@ |@ ";
    match rf with
    | Vinherit ty -> print_core_type env fmt ty
    | Vtag (variant, false, []) ->
      () (* fatal_error "Invalid polymorphic variant type" *)
    | Vtag (variant, true, []) -> Format.fprintf fmt "%a" Variant.print variant
    | Vtag (variant, true, tys) ->
      Format.fprintf fmt "%a@ of" Variant.print variant;
      List.iter
        (fun ty ->
          Format.fprintf fmt "@ &@ %a" (print_core_type_with_arrow env) ty)
        tys
    | Vtag (variant, false, ty :: tys) ->
      Format.fprintf fmt "%a@ of@ %a" Variant.print variant
        (print_core_type_with_arrow env)
        ty;
      List.iter
        (fun ty ->
          Format.fprintf fmt "@ &@ %a" (print_core_type_with_arrow env) ty)
        tys

  and print_fragment fmt = function
    | Name s -> Name.print fmt s
    | Dot (frag, s) ->
      Format.fprintf fmt "%a.%a" print_fragment frag Name.print s

  and print_module_with_clause env is_first fmt (frag, ty) =
    if is_first
    then
      Format.fprintf fmt "@ with@ type@ %a@ =@ %a" print_fragment frag
        (print_core_type env) ty
    else
      Format.fprintf fmt "@ and@ type@ %a@ =@ %a" print_fragment frag
        (print_core_type env) ty

  and print_core_type_with_arrow env fmt ty =
    match ty with
    | TypeArrow _ -> Format.fprintf fmt "(@[%a@])" (print_core_type env) ty
    | _ -> print_core_type env fmt ty

  and print_core_type_with_parens env fmt ty =
    match ty with
    | TypeArrow _ | TypeTuple _ | TypeConstr _ | TypeAlias _ | TypePoly _ ->
      Format.fprintf fmt "(@[%a@])" (print_core_type env) ty
    | _ -> print_core_type env fmt ty

  and print_object_field env fmt = function
    | Oinherit ty ->
      Format.fprintf fmt "<inherit %a TODO>" (print_core_type env) ty
    | Otag (name, ty) ->
      Format.fprintf fmt "%a@ :@ %a" Name.print name (print_core_type env) ty

  and print_module_exp env fmt = function
    | ModuleIdent ident -> print_raw_ident_module env fmt ident
    | ModuleApply (module_1, module_2) ->
      Format.fprintf fmt "%a(@[%a@])" (print_module_exp env) module_1
        (print_module_exp env) module_2
    | ModuleApply_unit module_ ->
      Format.fprintf fmt "%a()" (print_module_exp env) module_

  and print_core_type env fmt = function
    | TypeAny -> Format.fprintf fmt "_"
    | TypeVar v -> Var.Type_var.print env fmt v
    | TypeArrow (arg_label, ty1, ty2) ->
      Format.fprintf fmt "%a%a@ ->@ %a" print_arg_lab arg_label
        (print_core_type_with_arrow env)
        ty1 (print_core_type env) ty2
    | TypeTuple ((tl, ty) :: ts) ->
      (match tl with
      | LabelledTup l ->
        Format.fprintf fmt "%s:%a" l (print_core_type_with_parens env) ty
      | NolabelTup -> print_core_type_with_parens env fmt ty);
      List.iter
        (fun (tl, ty) ->
          Format.fprintf fmt " * ";
          match tl with
          | LabelledTup l ->
            Format.fprintf fmt "%s:%a" l (print_core_type_with_parens env) ty
          | NolabelTup -> print_core_type_with_parens env fmt ty)
        ts
    | TypeTuple [] -> () (* fatal_error "Invalid tuple type" *)
    | TypeUnboxedTuple ((tl, ty) :: ts) ->
      (match tl with
      | LabelledTup l ->
        Format.fprintf fmt "%s:%a" l (print_core_type_with_parens env) ty
      | NolabelTup -> print_core_type_with_parens env fmt ty);
      List.iter
        (fun (tl, ty) ->
          Format.fprintf fmt " * ";
          match tl with
          | LabelledTup l ->
            Format.fprintf fmt "%s:%a" l (print_core_type_with_parens env) ty
          | NolabelTup -> print_core_type_with_parens env fmt ty)
        ts (* possibly incorrect way of displaying unboxed tuples *)
    | TypeUnboxedTuple [] -> () (* fatal_error "Invalid unboxed tuple type" *)
    | TypeConstr (ident, []) -> print_raw_ident_type env fmt ident
    | TypeConstr (ident, [ty]) ->
      Format.fprintf fmt "%a@ %a"
        (print_core_type_with_parens env)
        ty (print_raw_ident_type env) ident
    | TypeConstr (ident, ty :: tys) ->
      print_tuple_like "(" ")" "," (print_core_type env) fmt (ty :: tys);
      Format.fprintf fmt "@ %a" (print_raw_ident_type env) ident
    | TypeObject ([], closed_flag) -> Format.fprintf fmt "< >"
    | TypeObject (f :: fs, closed_flag) ->
      print_tuple_like "< " " >" "," (print_object_field env) fmt (f :: fs)
    | TypeClass (name, []) -> Name.print fmt name
    | TypeClass (name, [ty]) ->
      Format.fprintf fmt "[%a]@ %a" (print_core_type env) ty Name.print name
    | TypeClass (name, ty :: tys) ->
      print_tuple_like "[" "]" "," (print_core_type env) fmt (ty :: tys);
      Format.fprintf fmt "@ %a" Name.print name
    | TypeAlias (ty, tv) ->
      Format.fprintf fmt "%a@ as@ %a" (print_core_type env) ty Name.print tv
    | TypeVariant ([], _) -> () (* fatal_error "Invalid variant type" *)
    | TypeVariant (rf :: row_fields, variant_form) ->
      (match variant_form with
      | VFixed -> Format.fprintf fmt "[ "
      | VOpen -> Format.fprintf fmt "[> "
      | VClosed sl -> Format.fprintf fmt "[< ");
      print_row_field env false fmt rf;
      List.iter (print_row_field env true fmt) row_fields;
      Format.fprintf fmt " ]"
    | TypePoly ([], _) -> () (* fatal_error "Invalid poly-type" *)
    | TypePoly (tv :: tvs, ty) ->
      let print_tv fmt name = Format.fprintf fmt "'%s" name in
      print_tuple_like "" "" " " print_tv fmt (tv :: tvs);
      Format.fprintf fmt ".@ %a" (print_core_type env) ty
    | TypePackage (ident, []) ->
      print_module_type env fmt ident
    | TypePackage (ident, (fragment, core_type)::wcs) ->
      Format.fprintf fmt "@[%a@ with@ type@ %a@ =@ %a"
        (print_module_type env) ident
        print_fragment fragment
        (print_core_type env) core_type;
      List.iter (fun (fragment, core_type) ->
        Format.fprintf fmt "@ and@ type@ %a@ =@ %a"
          print_fragment fragment
          (print_core_type env) core_type)
        wcs;
      Format.fprintf fmt "@]"
    | TypeCallPos -> Format.fprintf fmt "call_pos"

  and print_arg_lab fmt = function
    | Nolabel -> Format.fprintf fmt ""
    | Labelled s -> Format.fprintf fmt "~%s:" s
    | Optional s -> Format.fprintf fmt "?%s:" s

  (* TODO: incorrect !! -- add logic for (let+) *)
  and print_param env fmt = function
    | Pparam_val (arg_lab, exp_opt, pat) ->
      Format.fprintf fmt "@ %a%a" print_arg_lab arg_lab (print_pat env) pat
    | Pparam_newtype ty ->
      Format.fprintf fmt "@ (type %a)" (Var.Type_var.print env) ty

  and print_record env fmt (fields, exp_opt) =
    Format.fprintf fmt "{@ @[";
    (match exp_opt with
    | None -> ()
    | Some exp -> Format.fprintf fmt "%a@ with@ " (print_exp env) exp);
    List.iter
      (fun (field, exp) ->
        Format.fprintf fmt "%a = %a; " (print_field env) field (print_exp env)
          exp)
      fields;
    Format.fprintf fmt "@]}"

  and print_exp env fmt exp =
    match exp.desc with
    | Ident id ->
      print_raw_ident_value env fmt id
    | Constant c -> print_const fmt c
    | Apply (exp, args) ->
      Format.fprintf fmt "%a@[<hov>" (print_exp env) exp;
      List.iter
        (fun (arg_lab, exp) ->
          Format.fprintf fmt "@ %a@[%a@]" print_arg_lab arg_lab
            (print_exp_with_parens env)
            exp)
        args;
      Format.fprintf fmt "@]"
    | Fun { params; constraint_; body } -> (
      match body with
      | Pfunction_body exp ->
        Format.fprintf fmt "fun";
        List.iter (print_param env fmt) params;
        Option.iter (print_type_constraint env fmt) constraint_;
        Format.fprintf fmt "@ ->@ @[%a@]" (print_exp env) exp
      | Pfunction_cases cases ->
        Format.fprintf fmt "function@[";
        List.iter (print_case env fmt) cases;
        Option.iter (print_type_constraint env fmt) constraint_;
        Format.fprintf fmt "@]")
    | Let (_, [], _) ->
      failwith "Cannot create empty let-expressions. This should not happen."
    | Let (rec_flag, vb :: vbs, body) ->
      let prefix =
        match rec_flag with Nonrecursive -> "let" | Recursive -> "let@ rec"
      in
      Format.fprintf fmt "@[@[%s@ @[%a@ @]@]" prefix (print_vb env) vb;
      List.iter
        (fun vb -> Format.fprintf fmt "@[and@ @[%a@ @]@]" (print_vb env) vb)
        vbs;
      Format.fprintf fmt "@]in@ @[%a@]" (print_exp env) body
    | Let_op ([], _, _) ->
      failwith "Cannot create empty let-expressions. This should not happen."
    | Let_op (binders, defs, case) ->
      let bind_lefts =
        (match case.lhs with
         | PatTuple l -> l
         | pat -> [(NolabelTup, pat)]) in
      let rec iter3 f xs ys zs =
        match (xs, ys, zs) with
        | ([], [], []) -> ()
        | (x::xs, y::ys, z::zs) -> f x y z; iter3 f xs ys zs
        | _ -> failwith "Lists should have equal length."
      in
      iter3
        (fun binder (_, pat_bind) def ->
           Format.fprintf fmt "@[%a@ %a@ =@ @[%a@]@]@ "
             (print_raw_ident_value env) binder
             (print_pat env) pat_bind
             (print_exp env) def)
        binders
        bind_lefts
        defs;
      (match case.rhs with
       | Some exp -> Format.fprintf fmt "@[in@ @[%a@]@]" (print_exp env) exp
       | None -> Format.fprintf fmt ".")
    | Exclave exp -> Format.fprintf fmt "exclave_@ %a" (print_exp env) exp
    | Construct (ident, exp_opt) -> (
      match exp_opt with
      | None -> print_constr env fmt ident
      | Some exp ->
        Format.fprintf fmt "%a@ %a" (print_constr env) ident
          (print_exp_with_parens env)
          exp)
    | Variant (s, exp_opt) -> (
      match exp_opt with
      | None -> Format.fprintf fmt "`%s" s
      | Some exp ->
        Format.fprintf fmt "`%s@ %a" s (print_exp_with_parens env) exp)
    | Record (fields, exp_opt) -> print_record env fmt (fields, exp_opt)
    | Field (exp, field) ->
      Format.fprintf fmt "%a.%a"
        (print_exp_with_parens env)
        exp (print_field env) field
    | Array entries -> print_array (print_exp_with_parens env) fmt entries
    | Tuple entries -> print_tuple (print_exp_with_parens env) fmt entries
    | Ifthenelse (cond, then_, else_) -> (
      Format.fprintf fmt "if@ %a@ then@ %a"
        (print_exp_with_parens env)
        cond (print_exp env) then_;
      match else_ with
      | Some else_ -> Format.fprintf fmt "@ else@ %a" (print_exp env) else_
      | None -> ())
    | Sequence (exp1, exp2) ->
      Format.fprintf fmt "%a;@ %a" (print_exp env) exp1 (print_exp env) exp2
    | While (cond, body) ->
      Format.fprintf fmt "while@ %a@ do@ %a@ done" (print_exp env) cond
        (print_exp_with_parens env)
        body
    | For (it, start, stop, dir, body) ->
      let dir = match dir with Upto -> "to" | Downto -> "downto" in
      Format.fprintf fmt "for@ %a@ =@ %a@ %s@ %a@ do@ %a@ done" (print_pat env)
        it (print_exp env) start dir (print_exp env) stop
        (print_exp_with_parens env)
        body
    | Send (exp, meth) ->
      Format.fprintf fmt "%a#@[%a@]"
        (print_exp_with_parens env)
        exp Method.print meth
    | ConstraintExp (exp, ty) ->
      Format.fprintf fmt "%a@ :@ %a" (print_exp env) exp (print_core_type env)
        ty
    | CoerceExp (exp, opt_ty, ty) ->
      (match opt_ty with
       | None ->
         Format.fprintf fmt "%a@ :>@ %a" (print_exp env) exp (print_core_type env) ty
       | Some ty_constr ->
         Format.fprintf fmt "%a@ :@ %a@ :>@ %a"
           (print_exp env) exp (print_core_type env) ty_constr (print_core_type env) ty)
    | Match (exp, cases) ->
      Format.fprintf fmt "match@ @[%a@]@ with@[" (print_exp env) exp;
      List.iter (print_case env fmt) cases;
      Format.fprintf fmt "@]"
    | Probe -> Format.fprintf fmt "[%%probe]"
    | Try (try_, with_) -> (
      Format.fprintf fmt "try@ @[%a@]" (print_exp env) try_;
      match with_ with
      | [] -> ()
      | with_ ->
        Format.fprintf fmt "@ with@ @[";
        List.iter
          (fun case -> Format.fprintf fmt "%a" (print_case env) case)
          with_;
        Format.fprintf fmt "@]")
    | Setfield (obj, field, exp) ->
      Format.fprintf fmt "%a#%a <- %a"
        (print_exp_with_parens env)
        obj (print_field env) field (print_exp env) exp
    | Letmodule (None, module_exp, exp) ->
      Format.fprintf fmt "let@ module@ _@ =@ @[%a@]@ in@ @[%a@]"
        (print_module_exp env) module_exp (print_exp env) exp
    | Letmodule (Some modvar, module_exp, exp) ->
      Format.fprintf fmt "let@ module@ %a@ =@ @[%a@]@ in@ @[%a@]"
        (Var.Module.print env) modvar (print_module_exp env) module_exp
        (print_exp env) exp
    | Assert exp -> Format.fprintf fmt "assert@ @[%a@]" (print_exp env) exp
    | Lazy exp -> Format.fprintf fmt "lazy@ @[%a@]" (print_exp env) exp
    | Pack module_exp ->
      Format.fprintf fmt "(module@ @[%a@])" (print_module_exp env) module_exp
    | New ident ->
      Format.fprintf fmt "new@ @[%a@]" (print_raw_ident_value env) ident
    | Stack exp ->
      Format.fprintf fmt "stack_@ @[%a@]" (print_exp_with_parens env) exp
    | Let_exception (name, exp) ->
      Format.fprintf fmt "let@ exception@ %s@ in@ @[%a@]" name
        (print_exp env) exp
    | Extension_constructor name -> Format.fprintf fmt "(* extension %s *)" name
    | Unboxed_tuple ts ->
      Format.fprintf fmt "#";
      print_tuple (print_exp env) fmt ts
    | Unboxed_record_product (ts, exp_opt) -> print_record env fmt (ts, exp_opt)
    | Unboxed_field (exp, rec_field) ->
      Format.fprintf fmt "#%a.%a" (print_exp env) exp (print_field env)
        rec_field
    | Quote exp -> Format.fprintf fmt "[%%quote@ @[%a@]]" (print_exp env) exp
    | Antiquote exp ->
      Format.fprintf fmt "!#@[%a@]" (print_exp_with_parens env) exp
    | List_comprehension _ | Array_comprehension _ ->
      Format.fprintf fmt "(* comprehension *)"
    | Unreachable | Src_pos -> Format.fprintf fmt "."
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

  let name s = Ast.Name s

  let dot f s = Ast.Dot (f, s)
end

module Constant = struct
  type t = Ast.constant

  let int i = Ast.Int i

  let char c = Ast.Char c

  let string s id = Ast.String (s, id)

  let float f = Ast.Float f

  let float32 f = Ast.Float32 f

  let int32 i = Ast.Int32 i

  let int64 i = Ast.Int64 i

  let nativeint i = Ast.Nativeint i

  let unboxed_int32 i = Ast.UnboxedInt32 i

  let unboxed_int64 i = Ast.UnboxedInt64 i

  let unboxed_nativeint i = Ast.UnboxedNativeint i

  let unboxed_float f = Ast.UnboxedFloat f

  let unboxed_float32 f = Ast.UnboxedFloat32 f
end

module Binding_error = struct
  let unexpected var loc : unit =
    let msg =
      Format.asprintf
        "Unexpected binding detected for the identifier %s from %a at %a"
        (Var.name var) Loc.print (Var.loc var) Loc.print loc
    in
    failwith msg

  let unexpected_at_loc loc var : unit = unexpected var loc

  let missing var : unit =
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

  let duplicate var =
    let msg =
      Format.asprintf "Duplicate bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let mismatch var =
    let msg =
      Format.asprintf "Mismatched bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let code_not_closed var loc =
    let msg =
      Format.asprintf
        "The code built at %a is not closed: identifier %s bound at %a is free"
        Loc.print loc (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg
end

module Module_type = struct
  type t = Ast.module_type With_free_vars.t

  let (let+) x f = With_free_vars.map f x

  let return = With_free_vars.return

  let ident id =
    let+ id = id in Ast.MTIdent id

  let of_string s = return (Ast.MTBasic s)
end

module Constructor = struct
  type t = Ast.constr With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let return = With_free_vars.return

  let ident id =
    let+ id = id in
    Ast.CIdent id

  let of_string s = return (Ast.CBasic s)
end

module Field = struct
  type t = Ast.record_field With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let return = With_free_vars.return

  let ident id =
    let+ id = id in
    Ast.FIdent id

  let of_string s = return (Ast.FBasic s)
end

module Object_field = struct
  type t = Ast.object_field With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let inherit_ ty =
    let+ ty = ty in
    Ast.Oinherit ty

  let tag name ty =
    let+ ty = ty in
    Ast.Otag (name, ty)
end

module Pat = struct
  open With_free_and_bound_vars

  type t = Ast.pattern With_free_and_bound_vars.t

  let ( let+ ) x f = map f x

  let ( and+ ) t1 t2 = meet ~duplicated:Binding_error.duplicate t1 t2

  let all ts = meet_all ~duplicated:Binding_error.duplicate ts

  let join t1 t2 =
    join ~left_only:Binding_error.mismatch ~right_only:Binding_error.mismatch t1
      t2

  let any = return Ast.PatAny

  let var v =
    let+ var = bound_value_var v in
    Ast.PatVar var

  let alias t v =
    let+ p = t and+ v = bound_value_var v in
    Ast.PatAlias (p, v)

  let constant const = return (Ast.PatConstant const)

  let tuple ts =
    let ps =
      List.map
        (fun (lab, e) ->
          let+ v = e in
          lab, v)
        ts
    in
    let+ ps = all ps in
    Ast.PatTuple ps

  let unboxed_tuple ts =
    let ps =
      List.map
        (fun (lab, e) ->
          let+ v = e in
          lab, v)
        ts
    in
    let+ ps = all ps in
    Ast.PatUnboxedTuple ps

  let construct lid tm =
    let+ pm = optional tm and+ id = with_no_bound_vars lid in
    Ast.PatConstruct (id, pm)

  let variant label tm =
    let+ pm = optional tm in
    Ast.PatVariant (Variant.of_string label, pm)

  let record fields closed =
    let closed_ast = if closed then Ast.ClosedRec else Ast.OpenRec in
    let ts =
      List.map
        (fun (lid, t) ->
          let+ lbl = with_no_bound_vars lid and+ p = t in
          lbl, p)
        fields
    in
    let+ ps = all ts in
    Ast.PatRecord (ps, closed_ast)

  let unboxed_record fields closed =
    let closed_ast = if closed then Ast.ClosedRec else Ast.OpenRec in
    let ts =
      List.map
        (fun (lid, t) ->
          let+ lbl = with_no_bound_vars lid and+ p = t in
          lbl, p)
        fields
    in
    let+ ps = all ts in
    Ast.PatUnboxedRecord (ps, closed_ast)

  let array ts =
    let+ ps = all ts in
    Ast.PatArray ps

  let or_ t1 t2 =
    let+ p1, p2 = join t1 t2 in
    Ast.PatOr (p1, p2)

  let lazy_ t =
    let+ p = t in
    Ast.PatLazy p

  let any_module = return Ast.PatAnyModule

  let unpack v =
    let+ v = bound_module_var v in
    Ast.PatUnpack v

  let exception_ t =
    let+ p = t in
    Ast.PatException p

  let constraint_ pat ty =
    let+ pat = pat and+ ty = with_no_bound_vars ty in
    Ast.PatConstraint (pat, ty)
end

module Variant_type = struct
  module Variant_form = struct
    type t =
      | Fixed
      | Open
      | Closed of string list

    let fixed = Fixed

    let open_ = Open

    let closed s = Closed s

    let to_ast_variant_form = function
      | Fixed -> Ast.VFixed
      | Open -> Ast.VOpen
      | Closed l -> Ast.VClosed l
  end

  module Row_field = struct
    type t = Ast.row_field With_free_vars.t

    let ( let+ ) m f = With_free_vars.map f m

    let inherit_ ty =
      let+ ty = ty in
      Ast.Vinherit ty

    let tag variant has_empty types =
      let+ types = With_free_vars.all types in
      Ast.Vtag (variant, has_empty, types)
  end

  type t = Ast.row_field list With_free_vars.t * Ast.variant_form

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let of_row_fields_list row_fields variant_form =
    With_free_vars.all row_fields, Variant_form.to_ast_variant_form variant_form
end

module Type = struct
  type t = Ast.core_type With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let all = With_free_vars.all

  let var = function
    | None -> With_free_vars.return Ast.TypeAny
    | Some tv -> With_free_vars.return (Ast.TypeVar tv)

  let arrow lab lhs rhs =
    let+ l = lhs and+ r = rhs in
    Ast.TypeArrow (lab, l, r)

  let tuple ts =
    let w =
      List.map
        (fun (lab, t) ->
          let+ t = t in
          lab, t)
        ts
    in
    let+ e = all w in
    Ast.TypeTuple e

  let unboxed_tuple ts =
    let w =
      List.map
        (fun (lab, t) ->
          let+ t = t in
          lab, t)
        ts
    in
    let+ e = all w in
    Ast.TypeUnboxedTuple e

  let constr cons typs =
    let+ ts = all typs and+ cons = cons in
    Ast.TypeConstr (cons, ts)

  let object_ object_fields is_closed =
    let closed_flag = if is_closed then Ast.OClosed else Ast.OOpen in
    let+ object_fields = all object_fields in
    Ast.TypeObject (object_fields, closed_flag)

  let class_ name tys =
    let+ tys = all tys in
    Ast.TypeClass (name, tys)

  let alias typ tv =
    let+ t = typ in
    Ast.TypeAlias (t, Var.Type_var.name tv)

  let variant vty =
    let rfs, form = vty in
    let+ rfs = rfs in
    Ast.TypeVariant (rfs, form)

  let poly loc names f =
    With_free_vars.type_bindings loc names (fun tvs ->
        let+ t = f tvs in
        Ast.TypePoly (names, t))

  let package m spec =
    let s =
      List.map
        (fun (frag, typ) ->
          let+ t = typ in
          frag, t)
        spec
    in
    let+ specs = all s and+ m = m in
    Ast.TypePackage (m, specs)

  let call_pos = With_free_vars.return Ast.TypeCallPos
end

module Module = struct
  type t = Ast.module_expr With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let ident id =
    let+ id = id in
    Ast.ModuleIdent id

  let apply func arg =
    let+ func = func and+ arg = arg in
    Ast.ModuleApply (func, arg)

  let apply_unit func =
    let+ func = func in
    Ast.ModuleApply_unit func
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
        ~extra:(Binding_error.unexpected_at_loc loc)
        pat
    in
    mk lhs None (Some rhs)

  let simple loc name f =
    With_free_vars.value_binding loc name (fun var ->
        let+ rhs = f var in
        mk (Ast.PatVar var) None (Some rhs))

  let pattern loc ~bound_values ~bound_modules f =
    let+ lhs, rhs =
      With_free_vars.complex_bindings loc
        ~extra:(Binding_error.unexpected_at_loc loc)
        ~missing:Binding_error.missing ~bound_values ~bound_modules f
    in
    mk lhs None (Some rhs)

  let guarded loc ~bound_values ~bound_modules f =
    let+ lhs, (guard, rhs) =
      With_free_vars.complex_bindings loc
        ~extra:(Binding_error.unexpected_at_loc loc)
        ~missing:Binding_error.missing ~bound_values ~bound_modules
        (fun value_vars module_vars ->
          let lhs, guard, rhs = f value_vars module_vars in
          lhs, With_free_vars.both guard rhs)
    in
    mk lhs (Some guard) (Some rhs)

  let refutation loc ~bound_values ~bound_modules f =
    let+ lhs, _ =
      With_free_vars.complex_bindings loc
        ~extra:(Binding_error.unexpected_at_loc loc)
        ~missing:Binding_error.missing ~bound_values ~bound_modules (fun v m ->
          f v m, With_free_vars.return ())
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

  let body e constraint_ =
    let+ b = e and+ constraint_ = With_free_vars.optional constraint_ in
    mk [] constraint_ (Ast.Pfunction_body b)

  let cases cl constraint_ =
    let+ c = With_free_vars.all cl
    and+ constraint_ = With_free_vars.optional constraint_ in
    mk [] constraint_ (Ast.Pfunction_cases c)

  let param lab exp loc names rest =
    With_free_vars.value_bindings loc names (fun vars ->
        let pat, fn = rest vars in
        let+ pat = pat
        and+ ({ params; constraint_; body } : Ast.function_) = fn
        and+ e = With_free_vars.optional exp in
        let pat =
          With_bound_vars.value
            ~extra:(Binding_error.unexpected_at_loc loc)
            ~missing:Binding_error.missing pat
            (Var.Value.generic_list vars)
        in
        mk (Ast.Pparam_val (lab, e, pat) :: params) constraint_ body)

  let param_module_nonbinding lab loc pat fn =
    let+ pat = pat
    and+ ({params; constraint_; body} : Ast.function_) = fn in
    let pat =
      With_bound_vars.value_nonbinding ~extra:(Binding_error.unexpected_at_loc loc) pat
    in
    mk (Ast.Pparam_val (lab, None, pat) :: params) constraint_ body

  let param_module lab loc name rest =
    With_free_vars.module_binding loc name (fun var ->
      let pat, fn = rest var in
      let+ ({ params; constraint_; body } : Ast.function_) = fn
      and+ pat = pat in
      let pat =
        With_bound_vars.value
          ~extra:(Binding_error.unexpected_at_loc loc)
          ~missing:Binding_error.missing pat
          [Var.Module.generic var]
      in
      mk (Ast.Pparam_val (lab, None, pat) :: params) constraint_ body)

  let newtype loc name f =
    With_free_vars.type_var_binding loc name (fun typ ->
        let+ ({ params; constraint_; body } : Ast.function_) = f typ in
        mk (Ast.Pparam_newtype typ :: params) constraint_ body)
end

module Comprehension = struct
  type t = Ast.comprehension With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let ( and+ ) = With_free_vars.both

  let body exp =
    let+ exp = exp in
    Ast.Body exp

  let when_clause exp compr =
    let+ exp = exp and+ compr = compr in
    Ast.When (compr, exp)

  let for_range loc name start stop direction compr =
    With_free_vars.value_binding loc name (fun var ->
        let+ start = start and+ stop = stop and+ compr = compr var in
        let dir = if direction then Ast.Upto else Ast.Downto in
        Ast.ForComp (compr, Ast.Range (var, start, stop, dir)))

  let for_in loc exp names compr =
    With_free_vars.value_bindings loc names (fun vars ->
        let pat, compr = compr vars in
        let+ pat = pat and+ compr = compr and+ exp = exp in
        let p =
          With_bound_vars.value
            ~extra:(Binding_error.unexpected_at_loc loc)
            ~missing:Binding_error.missing pat
            (Var.Value.generic_list vars)
        in
        Ast.ForComp (compr, Ast.In (p, exp)))
end

module Code = struct
  type code_rep =
    { exp : Ast.expression;
      loc : Loc.t
    }

  type t = code_rep With_free_vars.t

  let ( let+ ) m f = With_free_vars.map f m

  let to_exp code =
    let+ {exp; _} = code
    in exp

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
      With_free_vars.value ~free:Binding_error.code_not_closed code

    let open_ = With_free_vars.return
  end

  let print fmt c =
    let ast_exp = With_free_vars.value ~free:(fun _ _ -> ()) c in
    Format.fprintf fmt "[%%quote@ @[%a@]]"
      (Ast.print_exp (new_env ()))
      ast_exp.exp
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
    let+ id = id in
    mk (Ident id) []

  let constant (const : Constant.t) = return (mk (Constant const) [])

  let let_rec_simple loc names f =
    With_free_vars.value_bindings loc names (fun vars ->
        let defs, body = f vars in
        let+ defs = all defs and+ body = body in
        let vbs_rev =
          List.rev_map2
            (fun var def ->
              let pat = Ast.PatVar var in
              mk_vb pat def)
            vars defs
        in
        let vbs = List.rev vbs_rev in
        mk (Let (Recursive, vbs, body)) [])

  let let_ loc names_values names_modules defs f =
    let+ defs = all defs
    and+ pats, body =
      With_free_vars.complex_bindings loc ~extra:Binding_error.duplicate
        ~missing:Binding_error.missing ~bound_values:names_values
        ~bound_modules:names_modules f
    in
    match pats with
    | Ast.PatTuple pats ->
      let vbs = List.map2 (fun (_, pat) def -> mk_vb pat def) pats defs in
      mk (Let (Nonrecursive, vbs, body)) []
    | _ -> failwith "Cannot use non-tuple patterns in building let-expressions."

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
    let+ exp = optional exp and+ cons = cons in
    mk (Construct (cons, exp)) []

  let variant v exp =
    let+ exp = optional exp in
    mk (Variant (v, exp)) []

  let record entries exp =
    let entries =
      List.map
        (fun (field, e) ->
          let+ e = e and+ field = field in
          field, e)
        entries
    in
    let+ entries = all entries and+ exp = optional exp in
    mk (Record (entries, exp)) []

  let field exp field =
    let+ exp = exp and+ field = field in
    mk (Field (exp, field)) []

  let setfield exp field value =
    let+ exp = exp and+ field = field and+ value = value in
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
        ~extra:(Binding_error.unexpected_at_loc loc)
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
    mk (For (Ast.PatVar var, begin_, end_, dir, body)) [Loop]

  let send exp meth =
    let+ exp = exp in
    mk (Ast.Send (exp, meth)) []

  let assert_ exp =
    let+ exp = exp in
    mk (Assert exp) []

  let lazy_ exp =
    let+ exp = exp in
    mk (Lazy exp) []

  let letmodule_nonbinding mod_exp body =
    let+ mod_exp = mod_exp and+ body = body in
    mk (Letmodule (None, mod_exp, body)) []

  let letmodule loc name mod_exp body =
    let+ mod_exp = mod_exp
    and+ var, body =
      With_free_vars.module_binding loc name (fun m ->
          let+ body = body m in
          m, body)
    in
    mk (Letmodule (Some var, mod_exp, body)) []

  let constraint_ exp constr =
    let+ exp = exp and+ constr = constr in
    match constr with
    | Ast.Coerce (ty_opt, ty_to) -> mk (Ast.CoerceExp (exp, ty_opt, ty_to)) []
    | Ast.Constraint ty -> mk (Ast.ConstraintExp (exp, ty)) []

  let new_ class_id =
    let+ class_id = class_id in
    mk (Ast.New class_id) []

  let pack m =
    let+ m = m in
    mk (Ast.Pack m) []

  let unreachable = return (mk Ast.Unreachable [])

  let src_pos = return (mk Ast.Src_pos [])

  let exclave exp =
    let+ exp = exp in
    mk (Ast.Exclave exp) []

  let list_comprehension compr =
    let+ compr = compr in
    mk (Ast.List_comprehension compr) []

  let array_comprehension compr =
    let+ compr = compr in
    mk (Ast.Array_comprehension compr) []

  let unboxed_tuple fs =
    let entries =
      List.map
        (fun (lab, exp) ->
          let+ exp = exp in
          lab, exp)
        fs
    in
    let+ entries = all entries in
    mk (Ast.Unboxed_tuple entries) []

  let unboxed_record_product entries exp =
    let entries =
      List.map
        (fun (field, e) ->
          let+ e = e and+ field = field in
          field, e)
        entries
    in
    let+ entries = all entries and+ exp = optional exp in
    mk (Ast.Unboxed_record_product (entries, exp)) []

  let unboxed_field exp field =
    let+ exp = exp and+ field = field in
    mk (Ast.Unboxed_field (exp, field)) []

  let extension_constructor name =
    return (mk (Ast.Extension_constructor name) [])

  let let_exception name exp =
    let+ exp = exp in
    mk (Ast.Let_exception (name, exp)) []

  let let_op idents defs case =
    let+ idents = all idents and+ defs = all defs and+ case = case in
    mk (Ast.Let_op (idents, defs, case)) []

  let stack exp =
    let+ exp = exp in
    mk (Stack exp) []

  let quote exp =
    let+ exp = exp in
    mk (Quote exp) [Quotation]

  let antiquote exp =
    let+ exp = exp in
    mk (Antiquote exp) []

  let splice (code : Code.t) =
    let+ code = code in code.exp

  let print fmt exp =
    let ast = With_free_vars.value ~free:(fun _ _ -> ()) exp in
    Ast.print_exp (new_env ()) fmt ast
end
