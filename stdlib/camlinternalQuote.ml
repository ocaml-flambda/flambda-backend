# 2 "../home/lpw25/Repositories/flambda-backend/stdlib/camlinternalQuote.ml"
let fold_map_option f acc o =
  match o with
  | None -> (acc, None)
  | Some x ->
      let acc, x = f acc x in
        (acc, Some x)

let rec fold_left_map_alist f acc l =
  match l with
  | [] -> (acc, [])
  | (k, x) :: rest ->
      let acc, x = f acc x in
      let acc, rest = fold_left_map_alist f acc rest in
        (acc, (k, x) :: rest)

module Stamp : sig

  type t
  (** [t] is the type of stamps. Stamps have no structure, only a notion
      of identity. *)

  val fresh : unit -> t
  (** [fresh ()] creates a fresh stamp, unequal to any other. *)

  val compare : t -> t -> int
  (** [compare] is a total ordering on stamps. *)

  val to_string : t -> string
  (** [to_string] prints a representation of the stamp. Each stamp
      has a different representation. *)

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
        { file: string;
          start_line:int;
          start_col:int;
          end_line:int;
          end_col:int; }

  let unknown = Unknown

  let known ~file ~start_line ~start_col ~end_line ~end_col =
    Known { file; start_line; start_col; end_line; end_col; }

end

module Level : sig

  type t
  (** [t] is the type of binding levels. All variable bindings have an
      associated level and all bindings of the same level have the same
      scope. Each level is associated with a point on the call
      stack. Each level has a location attached for error messages. *)

  val with_fresh : Loc.t -> (t -> 'a) -> 'a
  (** [with_fresh loc f] creates a fresh level not equal to any others,
      associated with the current point in the call stack, with [loc] as
      the attached location. *)

  val check : t -> bool
  (** [check t] is [true] if the point on the call stack associated with
      [t] is still present, otherwise it is [false]. *)

  val compare : t -> t -> int
  (** [compare] is a total order on levels. *)

  val loc : t -> Loc.t
  (** [loc t] is the location attached to [t]. *)

end = struct

  type t =
    { stamp : Stamp.t;
      mutable valid : bool;
      loc : Loc.t; }

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

  type t
  (** [t] is the type of variables of any sort *)

  type var := t

  module Module : sig

    type t
    (** [t] is the type of module variables *)

    val generate : Level.t -> Name.t -> t
    (** [generate lv n] creates a fresh module variable bound at [lv]
        whose name is [n]. *)

    val generic : t -> var
    (** [generic t] is [t] as a generic variable *)

    val name : t -> Name.t
    (** [name t] is the name of [t]. *)

  end

  module Value : sig

    type t
    (** [t] is the type of ordinary variables *)

    val generate : Level.t -> Name.t -> t
    (** [generate lv n] creates a fresh variable bound at [lv] whose
        name is [n]. *)

    val generic : t -> var
    (** [generic t] is [t] as a generic variable *)

    val name : t -> Name.t
    (** [name t] is the name of [t]. *)

  end

  module Type : sig

    type t
    (** [t] is the type of type constructor variables *)

    val generate : Level.t -> Name.t -> t
    (** [generate lv n] creates a fresh type constructor variable
        bound at [lv] whose name is [n]. *)

    val generic : t -> var
    (** [generic t] is [t] as a generic variable *)

    val name : t -> Name.t
    (** [name t] is the name of [t]. *)

  end

  val name : t -> Name.t
  (** [name t] is the name of [t]. *)

  val level : t -> Level.t
  (** [level t] is the level of [t]. *)

  val loc : t -> Loc.t
  (** [loc t] is [Level.loc (level t)]. *)

  val valid : t -> bool
  (** [valid t] is [true] if [t]'s binding is still open, and [false] otherwise. Once [t]'s binding has
      been closed there is no valid way to use [t]. *)

  module Set : sig

    type t
    (** [t] is the type of sets of variables. *)

    val empty : t
    (** [empty] is the empty set. *)

    val singleton : var -> t
    (** [singleton v] is the set containing just [v]. *)

    val add : var -> t -> t
    (** [add v t] is the set containing [v] and the elements of [t]. *)

    val union : shared:(var -> unit) -> t -> t -> t
    (** [union ~shared t1 t2] is the union of [t1] and [t2]. [shared] is run on all
        the variables that are in both [t1] and [t2]. *)

    val inter : left_only:(var -> unit) -> right_only:(var -> unit) -> t -> t -> t
    (** [inter ~left_only ~right_only t1 t2] is the intersection of [t1]
        and [t2]. [left_only] is run on all the variables that are in
        [t1] and not in [t2]. [right_only] is run on all the variables
        that are in [t2] and not in [t1]. *)

    val iter : (var -> unit) -> t -> unit
    (** [iter f t] runs [f] on every variable in [t]. *)

    val choose : t -> var option
    (** [choose t] is [None] if [t] is equal to [empty] and [Some v],
        where [v] is an element of [t], otherwise. *)

    val differences :
      left_only:(var -> unit) -> right_only:(var -> unit) -> t -> t -> unit
    (** [differences ~left_only ~right_only t1 t2] runs [left_only] on every element of [t1]
        that is not an element of [t2] and [right_only] on every element of [t2] that is
        not an element of [t1]. *)

  end

  module Map : sig

    type 'a t
    (** ['a t] is the type of maps from variables to ['a] values. *)

    val empty : 'a t
    (** [empty] is the empty set. *)

    val singleton : var -> 'a -> 'a t
    (** [singleton v data] is the map containing just a binding of [v] to [data]. *)

    val add : var -> 'a -> 'a t -> 'a t
    (** [add v data t] is the map containing the same bindings as [t]
        plus a binding of [v] to [data]. *)

    val union : (var -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (** [union ~join t1 t2] is the union of [t1] and [t2]. [join] is run on all
        the variables that are in both [t1] and [t2]. *)

    val remove_level : Level.t -> 'a t -> 'a t
    (** [remove_level l t] is the map containing the bindings of [t] whose
        variables's level is not [l]. *)

    val iter : (var -> 'a -> unit) -> 'a t -> unit
    (** [iter f t] runs [f] on every variable in [t]. *)

    val choose : 'a t -> (var * 'a) option
    (** [choose t] is [None] if [t] is equal to [empty] and [Some (v, data)],
        where [v] is bound to [data] in [t], otherwise. *)

  end

end = struct

  type t =
    { name : string;
      stamp : Stamp.t;
      level : Level.t; }

  module Set = struct

    type var = t

    module Stamp_map = Map.Make(Stamp)

    type t = var Stamp_map.t

    let empty = Stamp_map.empty

    let singleton v =
      Stamp_map.singleton v.stamp v

    let add v t =
      Stamp_map.add v.stamp v t

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
          | Some v1, None -> left_only v1; None
          | None, Some v2 -> right_only v2; None
          | Some v1, Some _  -> Some v1)
        t1 t2

    let choose t =
      match Stamp_map.choose_opt t with
      | None -> None
      | Some (_, v) -> Some v

    let iter f t =
      Stamp_map.iter (fun _ v -> f v) t

    let differences ~left_only ~right_only t1 t2 =
      ignore
        (Stamp_map.merge
           (fun _ v1 v2 ->
             match v1, v2 with
             | None, None -> None
             | Some v, None -> left_only v; None
             | None, Some v -> right_only v; None
             | Some v, Some _  -> None)
           t1 t2);

  end

  module Map = struct

    type var = t

    module Var_map = Map.Make(struct
        type t = var
        let compare t1 t2 = Stamp.compare t1.stamp t2.stamp
      end)

    module Level_map = Map.Make(Level)

    type 'a t = 'a Var_map.t Level_map.t

    let empty = Level_map.empty

    let singleton v data =
      Level_map.singleton v.level
        (Var_map.singleton v data)

    let add v data t =
      let update prev =
        let prev =
          match prev with
          | None -> Var_map.empty
          | Some prev -> prev
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
      | Some (l, m) ->
          match Var_map.choose_opt m with
          | None -> choose (Level_map.remove l t)
          | Some _ as res -> res

    let iter f t =
      Level_map.iter
        (fun _ m -> Var_map.iter (fun v data -> f v data) m)
        t

  end

  let generate level name =
    let stamp = Stamp.fresh () in
    let name = name ^ "_" ^ (Stamp.to_string stamp) in
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

  type 'a t
  (** ['a t] is the type of ['a]s paired with a set of variables that
      are bound by the ['a]. *)

  val mk : Var.Set.t -> 'a -> 'a t
  (** [mk vars x] is [x] paired with [vars]. *)

  val return : 'a -> 'a t
  (** [return x] is [mk Var.Set.empty x]. *)

  val var : Var.t -> Var.t t
  (** [var v] is [mk (Var.Set.singleton v) v]. *)

  val value_var : Var.Value.t -> Var.Value.t t
  (** [value_var v] is [mk (Var.Set.singleton (Var.Value.generic v)) v]. *)

  val module_var : Var.Module.t -> Var.Module.t t
  (** [module_var v] is [mk (Var.Set.singleton (Var.Module.generic v)) v]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] is [f v], where [v] is the value of [t], paired with the
      same variables as [t]. *)

  val meet : 
    duplicated:(Var.t -> unit)
    -> 'a t
    -> 'b t
    -> ('a * 'b)t
  (** [meet t1 t2] is [(v1, v2)], where [v1] and [v2] are the values of
      [t1] and [t2] respectively, paired with the union of the variables
      from [t1] and [t2] which are expected to be disjoint. If [t1] and
      [t2] share any variables then [duplicated] will be run on the
      shared variables.*)

  val join :
    left_only:(Var.t -> unit)
    -> right_only:(Var.t -> unit)
    -> 'a t -> 'b t -> ('a * 'b) t
  (** [join ~left_only ~right_only t1 t2] is [(v1, v2)], where [v1] and
      [v2] are the values of [t1] and [t2] respectively, paired with the
      intersection of the variables from [t1] and [t2] which are
      expected to be equal. If any variables in [t1] are missing from
      [t2] then [left_only] with be run on them. If any variables in
      [t2] are missing from [t1] then [right_only] will be run on
      them. *)

  val meet_all :
    duplicated:(Var.t -> unit)
    -> 'a t list    
    -> 'a list t
  (** [meet_all ~duplicated [t1; t2; ...]] is [[v1; v2; ...]], where
      each [vn] is the value of [tn], paired with the union of the
      variables from all the [ts] which are expected to be disjoint. If
      the [ts] share any variables then [duplicated] will be run on the
      shared variables. *)

  val optional : 'a t option -> 'a option t
  (** [optional tm] is [Some v], where [v] is the value of [t], paired
      with the variables from [t] if [tm] is [Some t]. It is [return
      None] if [tm] is [None]. *)

  val value :
    extra:(Var.t -> unit)
    -> missing:(Var.t -> unit)
    -> 'a t
    -> Var.t list
    -> 'a
  (** [value ~extra ~missing t vs] is [t]'s value. [t] is expected to
      bind exactly the variables [vs]. [extra] is run on any additional
      variables that are bound. [missing] is run on any variables that
      are missing. *)

  val value_nonbinding :
    extra:(Var.t -> unit)
    -> 'a t
    -> 'a
  (** [value_nonbinding ~extra t] is [t]'s value. [t] is expected to
      bind no variables. [extra] is run on any additional variables that
      are bound. *)

end = struct

  type 'a t =
    { vars : Var.Set.t;
      v : 'a; }

  let mk vars v = { vars; v }

  let return v = mk Var.Set.empty v

  let var v = mk (Var.Set.singleton v) v

  let value_var v = mk (Var.Set.singleton (Var.Value.generic v)) v

  let module_var v = mk (Var.Set.singleton (Var.Module.generic v)) v

  let map f { vars; v } =
    let v = f v in
    { vars; v }

  let meet ~duplicated {vars = vars1; v = v1} {vars = vars2; v = v2} =
    let vars = Var.Set.union ~shared:duplicated vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let join ~left_only ~right_only {vars = vars1; v = v1} {vars = vars2; v = v2} =
    let vars = Var.Set.inter ~left_only ~right_only vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let meet_all ~duplicated l =
    let vars, v =
      List.fold_left_map
        (fun acc {vars; v} ->
          let acc = Var.Set.union ~shared:duplicated acc vars in
          (acc, v))
        Var.Set.empty l
    in
    { vars; v }

  let optional = function
    | None -> return None
    | Some t -> map Option.some t

  let value ~extra ~missing t vs =
    let expected = List.fold_left (fun acc v -> Var.Set.add v acc) Var.Set.empty vs in
    Var.Set.differences ~left_only:extra ~right_only:missing t.vars expected;
    t.v

  let value_nonbinding ~extra t =
    Var.Set.iter extra t.vars;
    t.v
end

module With_free_vars : sig

  type 'a t
  (** ['a t] is the type of ['a]s paired with a set of variables that
      are free in the ['a] along with their associated locations. *)

  val mk : Loc.t Var.Map.t -> 'a -> 'a t
  (** [mk vars x] is [x] paired with [vars]. *)

  val return : 'a -> 'a t
  (** [return x] is [mk Var.Map.empty x]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f t] is [f v], where [v] is the value of [t], paired with the
      same variables as [t]. *)

  val both : 'a t -> 'b t -> ('a * 'b) t
  (** [both t1 t2] is [(v1, v2)], where [v1] and [v2] are the values of
      [t1] and [t2] respectively, paired with the union of the variables
      from [t1] and [t2]. *)

  val all : 'a t list -> 'a list t
  (** [all [t1; t2; ...]] is [[v1; v2; ...]], where each [vn] is the
      value of [tn], paired with the union of the variables from all the
      [ts]. *)

  val optional : 'a t option -> 'a option t
  (** [optional tm] is [Some t], where [v] is the value of [t], paired
      with the variables from [t] if [tm] is [Some t]. It is [return
      None] if [tm] is [None]. *)

  val check : invalid:(Var.t -> Loc.t -> unit) -> 'a t -> unit
  (** [check ~invalid t] runs [invalid] on any variables paired with [t]
      that are not valid. *)

  val value : free:(Var.t -> Loc.t -> unit) -> 'a t -> 'a
  (** [value ~free t] is the value of [t]. [t] is expected to have no
      free variables. [free] is run on any variables paired with [t]. *)

  val value_binding :
    Loc.t -> Name.t -> (Var.Value.t -> 'a t) -> 'a t

  val module_binding :
    Loc.t -> Name.t -> (Var.Module.t -> 'a t) -> 'a t

  val value_bindings :
    Loc.t -> Name.t list -> (Var.Value.t list -> 'a t) -> 'a t

  val complex_bindings :
    Loc.t
    -> extra:(Var.t -> unit)
    -> missing:(Var.t -> unit)
    -> bound_values:Name.t list
    -> bound_modules:Name.t list
    -> (Var.Value.t list -> Var.Module.t list -> 'a With_bound_vars.t t * 'b t)
    -> ('a * 'b) t

end  = struct

  type 'a t =
    { vars : Loc.t Var.Map.t;
      v : 'a; }

  let mk vars v = { vars; v }

  let return v = mk Var.Map.empty v

  let map f { vars; v } =
    let v = f v in
    { vars; v }

  let both f {vars = vars1; v = v1} {vars = vars2; v = v2} =
    let vars = Var.Map.union (fun _ loc1 _ -> loc1) vars1 vars2 in
    let v = v1, v2 in
    { vars; v }

  let all ts =
    let vars, v =
      List.fold_left_map
        (fun acc {vars; v} ->
          let acc = Var.Map.union (fun _ loc1 _ -> loc1) acc vars in
          (acc, v))
        Var.Map.empty ts
    in
    { vars; v }

  let optional = function
    | None -> return None
    | Some t -> map Option.some t

  let check ~invalid { vars; v } =
    Var.Map.iter
      (fun var loc -> if not (Var.valid var) then invalid var loc)
      vars

  let value = ()

  let value_binding loc name f =
    Level.with_fresh loc
      (fun level ->
        let fresh_var = Var.Value.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let module_binding loc name f =
    Level.with_fresh loc
      (fun level ->
        let fresh_var = Var.Module.generate level name in
        let { vars; v } = f fresh_var in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let value_bindings loc names f =
    Level.with_fresh loc
      (fun level ->
        let fresh_vars =
          List.map
            (fun name -> Var.Value.generate level name)
            names
        in
        let { vars; v } = f fresh_vars in
        let vars = Var.Map.remove_level level vars in
        { vars; v })

  let complex_bindings loc ~extra ~missing ~bound_values ~bound_modules f =
    Level.with_fresh loc
      (fun level ->
        let fresh_value_vars =
          List.map (Var.Value.generate level) bound_values
        in
        let fresh_module_vars =
          List.map (Var.Module.generate level) bound_modules
        in
        let { vars = bindings_vars; v = bindings}, { vars; v } =
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

  let return p =
    With_free_vars.return
      (With_bound_vars.return p)

  let with_no_free_vars v =
    With_free_vars.return v

  let with_no_bound_vars v =
    With_free_vars.map With_bound_vars.return v

  let bound_value_var v =
    with_no_free_vars (With_bound_vars.value_var v)

  let bound_module_var v =
    with_no_free_vars (With_bound_vars.module_var v)

  let map f t =
    With_free_vars.map
      (With_bound_vars.map f)
      t

  let meet ~dupicated t1 t2 =
    With_free_vars.map
      (fun (t1, t2) -> With_bound_vars.meet ~duplicated t1 t2)
      (With_free_vars.both t1 t2)

  let join ~left_only ~right_only t1 t2 =
    With_free_vars.map
      (fun (t1, t2) -> With_bound_vars.join ~left_only ~right_only t1 t2)
      (With_free_vars.both t1 t2)

  let meet_all ~duplicated ts =
    With_free_vars.map
      (With_bound_vars.all ~duplicated)
      (With_free_vars.all ts)

  let optional ts =
    With_free_vars.map
      With_bound_vars.optional
      (With_free_vars.optional ts)

end

module Ident = struct

  module Module : sig

    type t

    val compilation_unit : string -> t

    val dot : t -> string -> t

    val var : Var.Module.t -> Loc.t -> t

    val with_free_vars : t -> t With_free_vars.t

  end = struct

    type t =
      | Compilation_unit of string
      | Dot of t * string
      | Var of Var.Module.t * Loc.t

    let compilation_unit s = Compilation_unit s
    let dot t s = Dot(t, s)
    let var v loc = Var(v, loc)

    let rec free_vars = function
      | Compilation_unit _ -> Var.Map.empty
      | Dot(t, _) -> free_vars t
      | Var(v, loc) -> Var.Map.singleton (Var.generic v) loc

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

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

    let dot t s = Dot(v, s)
    let var v = Var v
    let free_vars = function
      | Dot(t, _) -> Module.free_vars t
      | Var(v, loc) -> Var.Map.singleton (Var.Value.generic v) loc

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

  end

  module Type : sig

    type t

    val dot : Module.t -> string -> t

    val var : Var.Value.t -> Loc.t -> t

    val with_free_vars : t -> t With_free_vars.t

    val int: t
    val char: t
    val string: t
    val bytes: t
    val float: t
    val float32: t
    val bool: t
    val unit: t
    val exn: t
    val array: t -> t
    val iarray: t -> t
    val list: t -> t
    val option: t -> t
    val nativeint: t
    val int32: t
    val int64: t
    val lazy_t: t -> t
    val extension_constructor:t
    val floatarray:t
    val lexing_position:t
    val code: t -> t
    val unboxed_float:t
    val unboxed_nativeint:t
    val unboxed_int32:t
    val unboxed_int64:t
    val int8x16: t
    val int16x8: t
    val int32x4: t
    val int64x2: t
    val float32x4: t
    val float64x2: t

  end = struct

    type t =
      | Dot of Module.t * string
      | Var of Var.Type.t * Loc.t
      | Builtin of string

    let dot t s = Dot(v, s)
    let var v = Var v
    let free_vars = function
      | Dot(t, _) -> Module.free_vars t
      | Var(v, loc) -> Var.Map.singleton (Var.Type.generic v) loc

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

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

    val dot : t -> string -> t

    val with_free_vars : t -> t With_free_vars.t

  end = struct

    type t =
      | Dot of t * string

    let dot t s = Dot(t, s)

    let free_vars = function
      | Dot(t, _) -> Module.free_vars t

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

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

    let dot t s = Dot(t, s)

    let free_vars = function
      | Dot(t, _) -> Module.free_vars t

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

    let with_free_and_bound_vars t =
      With_free_and_bound_vars.no_bound_vars (with_free_vars t)

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

    type t =
      | Dot of Module.t * string

    let dot t s = Dot(t, s)

    let free_vars = function
      | Dot(t, _) -> Module.free_vars t

    let with_free_vars t =
      With_free_vars.mk (free_vars t) t

    let with_free_and_bound_vars t =
      With_free_and_bound_vars.no_bound_vars (with_free_vars t)

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

  type rec_flag = Nonrecursive | Recursive

  type direction_flag = Upto | Downto

  type override_flag = Override | Fresh

  type closed_flag = Closed | Open

  type arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  type tuple_label =
    | Nolabel
    | Labelled of label

  type core_type =
    | Any
    | Var of Name.t
    | Arrow of arg_label * core_type * core_type
    | Tuple of (tuple_label * core_type) list
    | Constr of Ident.Type.t * core_type list
    | Alias of core_type * Name.t
    | Variant of row_field list * closed_flag * string list option
    | Poly of Name.t list * core_type
    | Package of package_type

  and row_field =
    | Tag of string * bool * core_type list
    | Inherit of core_type

  and package_type = Ident.Module_type.t * (fragment * core_type) list

  and fragment =
    | Name of Name.t
    | Dot of fragment * Name.t

  type type_constraint =
    | Constraint of core_type
    | Coerce of core_type option * core_type

  type pattern =
    | Any
    | Var of Var.Value.t
    | Alias of pattern * Var.Value.t
    | Constant of constant
    | Interval of constant * constant
    | Tuple of pattern list
    | Construct of Ident.Constructor.t * pattern option
    | Variant of string * pattern option
    | Record of (Ident.Field.t * pattern) list * closed_flag
    | Array of pattern list
    | Or of pattern * pattern
    | Constraint of pattern * core_type
    | Lazy of pattern
    | Unpack of Var.Module.t
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
    {
      desc: expression_desc;
      attributes: expression_attribute list;
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
    | For of
        pattern * expression
        * expression * direction_flag
        * expression
    | Constraint of expression * core_type
    | Coerce of expression * core_type option * core_type
    | Letmodule of Var.Module.t * module_expr * expression
    | Assert of expression
    | Lazy of expression
    | Pack of module_expr
    | Open of override_flag * Ident.Module.t * expression
    | Quote of expression
    | Escape of expression
    | Splice of expression * Loc.t

  and case =
    {
      lhs: pattern;
      guard: expression option;
      rhs: expression option;
    }

  and value_binding =
    {
      pat: pattern;
      expr: expression;
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
      body : function_body; }

  and module_expr =
    | Ident of Ident.Module.t
    | Apply of module_expr * module_expr
    | Apply_unit of module_expr
    | Unpack of expression

end

module Constant = struct

  type t = Ast.constant

  let int i = Int i
  let char c = Char c
  let string s = String s
  let float f = Float f
  let int32 i = Int32 i
  let int64 i = Int64 i
  let nativeint i = Nativeint i

end

module Label = struct

  type t = arg_label

  let no_label = Nolabel
  let labelled s = Labelled s
  let optional s = Optional s

end

module Variant = struct

  type t = string

  let of_string (s : string) : t = s

end

module Pat = struct

  open With_free_and_bound_vars

  type t = Ast.pattern With_free_and_bound_vars.t

  let duplicate_binding_error var =
    let msg =
      Format.asprintf
        "Duplicate bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let mismatch_binding_error var =
    let msg =
      Format.asprintf Format.str_formatter
        "Mismatched bindings detected for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let (let+) = map
  let (and+) t1 t2 =
    meet ~dupicated:duplicate_binding_error t1 t2
  let all ~duplicated ts =
    meet_all ~duplicated:duplicate_binding_error ts

  let join t1 t2 =
    join
      ~left_only:mismatch_binding_error
      ~right_only:mismatch_binding_error
      t1 t2

  let any = return Any

  let var v =
    let+ v = bound_value_var v in
    Var v

  let alias t v =
    let+ p = t
    and+ v = bound_value_var v in
    Alias(p, v)

  let constant const =
    return (Constant const)

  let interval const1 const2 =
    return (Interval(const1, const2))

  let tuple ts =
    let+ ps = all ts in
    Tuple ps

  let construct lid tm =
    let+ pm = optional tm
    and+ lid = Ident.Constructor.with_free_and_bound_vars lid in
    Construct(lid, pm)

  let variant label tm =
    let+ pm = optional tm in
    Variant(label, pm)

  let record fields closed =
    let closed = if closed then AST.Closed else AST.Open in
    let ts =
      List.map
        (fun (lbl, t) ->
          let+ lbl = Ident.Field.with_free_and_bound_vars lid
          and+ p = t in
          lbl, t)
        fields
    in
    let+ ps = all ts in
    Record(ps, closed)

  let array ts =
    let+ ps = all ts in
    Array ps

  let or_ t1 t2 =
    let+ (p1, p2) = join t1 t2 in
    Or(pat1, pat2)

  let lazy_ t =
    let+ p = t in
    Lazy p

  let module_ v =
    let+ v = bound_module_var v in
    Module v

  let exception_ t =
    let+ p = t in
    Exception p

end

module Case = struct

  type t = Ast.case With_free_vars.t

  let unexpected_binding_error loc var =
    let msg =
      Format.asprintf Format.str_formatter
        "Unexpected binding detected for the identifier %s from %a at %a"
        (Var.name var) Loc.print (Var.loc var) Loc.print loc
    in
    failwith msg

  let missing_binding_error var =
    let msg =
      Format.asprintf Format.str_formatter
        "Missing binding for the identifier %s at %a"
        (Var.name var) Loc.print (Var.loc var)
    in
    failwith msg

  let mk lhs guard rhs =
    { lhs; guard; rhs; }

  let (let+) = With_free_vars.map
  let (and+) = With_free_vars.both

  let nonbinding loc lhs rhs =
    let+ lhs = value_nonbinding ~extra:(unexpected_binding_error loc) lhs
    and+ rhs = rhs in
    mk lhs None (Some rhs)

  let simple loc name f =
    With_free_vars.binding loc name
      (fun var ->
        let+ rhs = f var in
        mk (Var var) None (Some rhs))

  let pattern loc ~bound_vars ~bound_modules f =
    let+ lhs, rhs =
      With_free_vars.complex_bindings loc
        ~extra:(unexpected_binding_error loc) ~missing:missing_binding_error
        ~bound_vars ~bound_modules f
    in
    mk lhs None (Some rhs)

  let guarded loc ~bound_vars ~bound_modules f =
    let+ lhs, (guard, rhs) =
      With_free_vars.complex_bindings loc
        ~extra:(unexpected_binding_error loc) ~missing:missing_binding_error
        ~bound_vars ~bound_modules
        (fun value_vars module_vars ->
            let lhs, guard, rhs = f value_vars module_vars in
            lhs, both guard rhs)
    in
    mk lhs (Some guard) (Some rhs)

  let refutation loc ~bound_vars ~bound_modules f =
    let+ lhs =
      With_free_vars.complex_bindings loc
        ~extra:(unexpected_binding_error loc) ~missing:missing_binding_error
        ~bound_vars ~bound_modules f
    in
    mk lhs None None

end

module Type_constraint = struct

  type t = Ast.type_constraint With_free_vars.t

  open With_free_vars

  let (let+) = With_free_vars.map
  let (and+) = With_free_vars.both

  let constraint_ typ =
    let+ typ = typ in
    Constraint typ

  let coercion typ1 typ2 =
    let+ typ1 = typ1
    and+ typ2 = typ2 in
    Coercion(typ1, typ2)
  
end

module Function_ = struct

  type t = function_ With_free_vars.t

  open With_free_vars

  let (let+) = With_free_vars.map
  let (and+) = With_free_vars.both

  val body : Exp.t -> t

  val cases : Case.t list -> t

  val param_nonbinding : Label.t -> Exp.t option -> Pat.t -> t -> t

  val param_simple :
    Label.t
    -> Exp.t option
    -> Name.t
    -> (Var.Value.t -> t)
    -> t

  val param :
    Label.t
    -> Exp.t option
    -> Name.t list
    -> (Var.Value.t list -> Pat.t * t)
    -> t

  val newtype : Name.t -> (Var.Type.t -> t) -> t

end

module Exp = struct

  type t = Ast.expression With_free_vars.t

  open With_free_vars

  let unbound_var loc var use =
    let msg =
      Format.asprintf Format.str_formatter
        "The quotation built at %a has unbound variables: \
         variable %s used at %a is not bound."
        Loc.print loc (Var.txt var)
        Loc.print use
    in
    failwith msg

  let unexpected_binding_error loc var =
    let msg =
      Format.asprintf Format.str_formatter
        "Unexpected binding detected for the identifier %s from %a at %a"
        (Var.name var) Loc.print (Var.loc var) Loc.print loc
    in
    failwith msg

  let mk desc attributes : Ast.expression =
    { desc; attributes; }

  let (let+) = With_free_vars.map
  let (and+) = With_free_vars.both

  let mk_vb pat expr : Ast.value_binding =
    {pat; expr}

  let ident id =
    let+ id = Ident.Value.with_free_vars id in
    mk (Ident id) []

  let constant const =
    return (mk (Constant const) [])

  let let_nonbinding loc pat def body =
    let+ pat = pat
    and+ def = def
    and+ body = body in
    let pat =
      With_bound_vars.value_nonbinding ~extra:(unexpected_binding_error loc) pat
    in
    let vbs = [mk_vb pat def] in
    mk (Let(Nonrec, vbs, body)) []

  let let_simple loc name def f =
    let+ def = def
    and+ (pat, body) =
      value_binding loc name
        (fun var ->
          let+ body = f var in
          let pat = Var var in
          pat, body)
    in
    let vbs = [mk_vb pat def] in
    mk (Let(Nonrec, vbs, body)) []

  let let_rec_simple loc names f =
    value_bindings loc names
      (fun vars ->
        let defs, body = f vars in
        let+ defs = defs
        and+ body = body in
        let vbs_rev =
          List.rev_map2
            (fun var def ->
              let pat = Var var in
              mk_vb pat def)
            vars defs
        in
        let vbs = List.rev vbs_rev in
        mk (Let(Rec, vbs, body))
        vbs, body)

  let let_ loc names def f =
    let+ def = def
    and+ (pat, body) = value_binding loc name f in
    let vbs = [mk_vb pat def] in
    mk (Let(Nonrec, vbs, body)) []

  let fun_nonbinding loc params exp =
    let+ params = params
    and+ exp = exp in
    let constraint_ = None in
    let body = Pfunction_body exp in
    let fn = { params; constraint_; body } in
    mk (Fun fn)

  let fun_simple loc name label default f =
    let heap, pat, exp = Binding.simple loc name f in
    let heap, default = fold_map_option merge loc heap default in
      mk loc heap (Pexp_fun (label, default, pat, exp))

  let fun_ loc names label default f =
    let heap, pat, exp = Binding.pattern loc names f in
    let heap, default = fold_map_option merge loc heap default in
      mk loc heap (Pexp_fun(label, default, pat, exp))

  let function_ loc cases =
    let heap, cases = List.fold_left_map CaseRepr.merge loc Level_map.empty cases in
      mk loc heap (Pexp_function cases)

  let apply loc fn args =
    let heap, fn = merge loc Level_map.empty fn in
    let heap, args = fold_left_map_alist merge loc heap args in
      mk loc heap (Pexp_apply (fn, args))

  let match_ loc exp cases =
    let heap, exp = merge loc Level_map.empty exp in
    let heap, cases = List.fold_left_map CaseRepr.merge loc heap cases in
      mk loc heap (Pexp_match(exp, cases))

  let try_ loc exp cases =
    let heap, exp = merge loc Level_map.empty exp in
    let heap, cases = List.fold_left_map CaseRepr.merge loc heap cases in
      mk loc heap (Pexp_try(exp, cases))

  let tuple loc exps =
    let heap, exps = List.fold_left_map merge loc Level_map.empty exps in
      mk loc heap (Pexp_tuple exps)

  let construct loc lid argo =
    let heap, argo = fold_map_option merge loc Level_map.empty argo in
      mk loc heap (Pexp_construct (lid, argo))

  let variant loc label argo =
    let heap, argo = fold_map_option merge loc Level_map.empty argo in
      mk loc heap (Pexp_variant (label, argo))

  let record loc defs orig =
    let heap, defs = fold_left_map_alist merge loc Level_map.empty defs in
    let heap, orig = fold_map_option merge loc heap orig in
      mk loc heap (Pexp_record (defs, orig))

  let field loc rcd lid =
    let heap, rcd = merge loc Level_map.empty rcd in
      mk loc heap (Pexp_field (rcd, lid))

  let setfield loc rcd lid def =
    let heap, rcd = merge loc Level_map.empty rcd in
    let heap, def = merge loc heap def in
      mk loc heap (Pexp_setfield (rcd, lid, def))

  let array loc args =
    let heap, args = List.fold_left_map merge loc Level_map.empty args in
      mk loc heap (Pexp_array args)

  let ifthenelse loc cond tr fs =
    let heap, cond = merge loc Level_map.empty cond in
    let heap, tr = merge loc heap tr in
    let heap, fs = fold_map_option merge loc heap fs in
      mk loc heap (Pexp_ifthenelse (cond, tr, fs))

  let sequence loc exp1 exp2 =
    let heap, exp1 = merge loc Level_map.empty exp1 in
    let heap, exp2 = merge loc heap exp2 in
      mk loc heap (Pexp_sequence(exp1, exp2))

  let while_ loc cond body =
    let heap, cond = merge loc Level_map.empty cond in
    let heap, body = merge loc heap body in
      mk loc heap (Pexp_while(cond, body))

  let for_nonbinding loc pat low high dir body =
    let dir = if dir then CamlinternalAST.Upto else CamlinternalAST.Downto in
    let pat = PatRepr.nonbinding loc pat in
    let heap, low = merge loc Level_map.empty low in
    let heap, high = merge loc heap high in
    let heap, body = merge loc heap body in
      mk loc heap (Pexp_for (pat, low, high, dir, body))

  let for_simple loc name low high dir f =
    let dir = if dir then CamlinternalAST.Upto else CamlinternalAST.Downto in
    let heap, pat, body = Binding.simple loc name f in
    let heap, low = merge loc heap low in
    let heap, high = merge loc heap high in
      mk loc heap (Pexp_for (pat, low, high, dir, body))

  let assert_ loc exp =
    let heap, exp = merge loc Level_map.empty exp in
      mk loc heap (Pexp_assert exp)

  let lazy_ loc exp =
    let heap, exp = merge loc Level_map.empty exp in
      mk loc heap (Pexp_lazy exp)

  let open_ loc ovr lid exp =
    let ovr =
      if ovr then CamlinternalAST.Override else CamlinternalAST.Fresh
    in
    let heap, exp = merge loc Level_map.empty exp in
      mk loc heap (Pexp_open(ovr, lid, exp))

  let quote loc exp =
    let heap, exp = merge loc Level_map.empty exp in
      mk loc heap (Pexp_quote exp)

  let escape loc exp =
    let heap, exp = merge loc Level_map.empty exp in
      mk loc heap (Pexp_escape exp)

end

module Code = struct

  module Closed = struct

    type exp = t

    type t = expression

    let close_delay_check exp =
        match Level_map.choose exp.heap with
        | None -> (exp.exp, fun () -> ())
        | Some var ->
          (exp.exp, fun () ->
            Format.fprintf Format.str_formatter
            "The code built at %a is not closed: \
             identifier %s bound at %a is free"
            Loc.print exp.exp.pexp_loc (Var.txt var)
            Loc.print (Var.loc var);
            failwith (Format.flush_str_formatter ()))

    let close exp =
      let exp, check = close_delay_check exp in
      check (); exp

    let open_ exp =
      let heap = Level_map.empty in
        { heap; exp }

  end

end
