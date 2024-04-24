(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2022-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

let debug = false

module String = Misc.Stdlib.String

module Witness = struct
  type kind =
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo
        }
    | Indirect_call
    | Indirect_tailcall
    | Direct_call of { callee : string }
    | Direct_tailcall of { callee : string }
    | Extcall of { callee : string }
    | Arch_specific
    | Probe of
        { name : string;
          handler_code_sym : string
        }

  type t =
    { dbg : Debuginfo.t;
      kind : kind
    }

  let create dbg kind = { dbg; kind }

  let compare { dbg = dbg1; kind = kind1 } { dbg = dbg2; kind = kind2 } =
    (* compare by [dbg] first to print the errors in the order they appear in
       the source file. *)
    let c = Debuginfo.compare dbg1 dbg2 in
    if c <> 0 then c else Stdlib.compare kind1 kind2

  let print_kind ppf kind =
    let open Format in
    match kind with
    | Alloc { bytes; dbginfo = _ } -> fprintf ppf "allocation of %d bytes" bytes
    | Indirect_call -> fprintf ppf "indirect call"
    | Indirect_tailcall -> fprintf ppf "indirect tailcall"
    | Direct_call { callee } -> fprintf ppf "direct call %s" callee
    | Direct_tailcall { callee : string } ->
      fprintf ppf "direct tailcall %s" callee
    | Extcall { callee } -> fprintf ppf "external call to %s" callee
    | Arch_specific -> fprintf ppf "arch specific operation"
    | Probe { name; handler_code_sym } ->
      fprintf ppf "probe \"%s\" handler %s" name handler_code_sym

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a {%a}@," print_kind kind Debuginfo.print_compact dbg
end

module Witnesses : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val create : Witness.kind -> Debuginfo.t -> t

  val print : Format.formatter -> t -> unit

  val elements : t -> Witness.t list

  val compare : t -> t -> int

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  val simplify : components -> components
end = struct
  include Set.Make (Witness)

  (* CR gyorsh: consider using Flambda_backend_flags.checkmach_details_cutoff to
     limit the size of this set. The downside is that it won't get tested as
     much. Only keep witnesses for functions that need checking. *)
  let join = union

  let meet = inter

  let lessequal = subset

  let create kind dbg = singleton (Witness.create dbg kind)

  let iter t ~f = iter f t

  let print ppf t = Format.pp_print_seq Witness.print ppf (to_seq t)

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  let simplify { nor; exn; div } =
    { div =
        (* don't print diverge witnesses unless they are the only ones. *)
        (if is_empty nor && is_empty exn then div else empty);
      nor;
      (* only print the exn witnesses that are not also nor witnesses. *)
      exn = diff exn nor
    }
end

module Tag = struct
  type t =
    | N
    | E
    | D

  let compare = Stdlib.compare

  let print = function N -> "nor" | E -> "exn" | D -> "div"
end

module Var : sig
  type t

  val get : string -> Tag.t -> t

  val name : t -> string

  val tag : t -> Tag.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val equal : t -> t -> bool

  module Map : Map.S with type key = t
end = struct
  module T = struct
    type t =
      { name : string;
        tag : Tag.t
      }

    let compare { tag = tag1; name = name1 } { tag = tag2; name = name2 } =
      let c = String.compare name1 name2 in
      if c = 0 then Tag.compare tag1 tag2 else c
  end

  include T
  module Map = Map.Make (T)

  let equal t1 t2 = compare t1 t2 = 0

  let name t = t.name

  let tag t = t.tag

  let get name tag = { name; tag }

  let print ppf { name; tag } =
    Format.fprintf ppf "%s.%s@ " name (Tag.print tag)
end

(** Abstract value for each component of the domain. *)
module V : sig
  type t

  (** order of the abstract domain  *)
  val lessequal : t -> t -> bool

  (** [equal] is structural equality on terms,
      not the order of the abstract domain. *)

  val join : t -> t -> t

  val meet : t -> t -> t

  val transform : t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val diff_witnesses : expected:t -> actual:t -> Witnesses.t

  val get_witnesses : t -> Witnesses.t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  val unresolved : Witnesses.t -> Var.t -> t

  val is_resolved : t -> bool

  val apply : t -> env:(Var.t -> t) -> t

  val bot : t

  val safe : t

  val top : Witnesses.t -> t

  val compare : t -> t -> int

  val match_with :
    bot:'a ->
    safe:'a ->
    top:(Witnesses.t -> 'a) ->
    unresolved:(unit -> 'a) ->
    t ->
    'a
end = struct
  (** Map of variables to witnesess, used as a helper for the normal forms below. *)

  let pp_w ~witnesses ppf w =
    if witnesses then Format.fprintf ppf "@, (%a)" Witnesses.print w else ()

  let pp_var ~witnesses ppf var w =
    Format.fprintf ppf "(%a%a)@," Var.print var (pp_w ~witnesses) w

  let pp_top ~witnesses ppf w = Format.fprintf ppf "(top%a)" (pp_w ~witnesses) w

  let meet _ _ =
    Misc.fatal_error "Meet is not implemented and shouldn't be needed."

  module Vars : sig
    type t

    val empty : t

    val compare : t -> t -> int

    (** [same_vars] compares variables ignoring witnesses *)
    val same_vars : t -> t -> bool

    val join : t -> t -> t

    val update : t -> Var.t -> Witnesses.t -> t

    val singleton : Var.t -> Witnesses.t -> t

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val print : witnesses:bool -> Format.formatter -> t -> unit

    val fold :
      f:(Var.t -> Witnesses.t -> 'acc -> 'acc) -> init:'acc -> t -> 'acc
  end = struct
    type t = Witnesses.t Var.Map.t

    let empty = Var.Map.empty

    let fold ~f ~init t = Var.Map.fold f t init

    let singleton var w = Var.Map.singleton var w

    let compare = Var.Map.compare Witnesses.compare

    let same_vars = Var.Map.equal (fun _ _ -> (* ignore witnesses *) true)

    let has_witnesses vars =
      Var.Map.exists (fun _ w -> not (Witnesses.is_empty w)) vars

    let join t1 t2 =
      Var.Map.union (fun _var w1 w2 -> Some (Witnesses.join w1 w2)) t1 t2

    let update t var witnesses =
      Var.Map.update var
        (function
          | None -> Some witnesses | Some w -> Some (Witnesses.join w witnesses))
        t

    let replace_witnesses t w = Var.Map.map (fun _ -> w) t

    let print ~witnesses ppf t =
      Var.Map.iter (fun var w -> pp_var ~witnesses ppf var w) t
  end

  (** Normal form of Transform *)
  module Transform : sig
    type t

    val var_with_top : Var.t -> var_witnesses:Witnesses.t -> Witnesses.t -> t

    val add_top : t -> Witnesses.t -> t

    val add_var : t -> Var.t -> Witnesses.t -> t

    val vars : var1:Var.t -> w1:Witnesses.t -> var2:Var.t -> w2:Witnesses.t -> t

    val print : witnesses:bool -> Format.formatter -> t -> unit

    val compare : t -> t -> int

    val equal : t -> t -> bool

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val flatten : t -> t -> t

    val get_top : t -> Witnesses.t option

    val get_vars : t -> Vars.t

    (** [same_vars] compares variables ignoring witnesses *)
    val same_vars : t -> t -> bool

    module Set : Set.S with type elt = t
  end = struct
    module T = struct
      type t =
        | Args of Vars.t
        | Args_with_top of
            { w : Witnesses.t;
              vars : Vars.t
            }

      (* The binary "transform" is commutative and associative, so a nested
         "transform" can be flattened in the normal form. The arguments are
         represented as a set of variables and optionally top. if top is absent,
         [vars] must contain at least two elements. if top is present, [vars]
         must have at least one element. This is enforced by the available
         constructors.

         We never need to represent other constants because these cases can be
         simplified to either a constant or a variable. *)

      let compare t1 t2 =
        match t1, t2 with
        | Args a1, Args a2 -> Vars.compare a1 a2
        | Args _, Args_with_top _ -> -1
        | Args_with_top _, Args _ -> 1
        | ( Args_with_top { w = w1; vars = vars1 },
            Args_with_top { w = w2; vars = vars2 } ) ->
          let c = Vars.compare vars1 vars2 in
          if c <> 0 then c else Witnesses.compare w1 w2
    end

    include T
    module Set = Set.Make (T)

    let equal t1 t2 = compare t1 t2 = 0

    let var_with_top var ~var_witnesses w =
      let vars = Vars.singleton var var_witnesses in
      Args_with_top { w; vars }

    let vars ~var1 ~w1 ~var2 ~w2 =
      assert (not (Var.equal var1 var2));
      let vars = Vars.(update (singleton var1 w1) var2 w2) in
      Args vars

    let add_top t w =
      match t with
      | Args vars -> Args_with_top { w; vars }
      | Args_with_top { w = w'; vars } ->
        Args_with_top { w = Witnesses.join w w'; vars }

    let add_var t var witnesses =
      (* CR gyorsh: future optimization is to return [t] when vars is phys equal
         to (update vars).*)
      match t with
      | Args vars -> Args (Vars.update vars var witnesses)
      | Args_with_top { w; vars } ->
        Args_with_top { w; vars = Vars.update vars var witnesses }

    let flatten tr1 tr2 =
      match tr1, tr2 with
      | ( Args_with_top { w = w1; vars = vars1 },
          Args_with_top { w = w2; vars = vars2 } ) ->
        Args_with_top { w = Witnesses.join w1 w2; vars = Vars.join vars1 vars2 }
      | Args_with_top { w; vars }, Args vars'
      | Args vars', Args_with_top { w; vars } ->
        Args_with_top { w; vars = Vars.join vars vars' }
      | Args vars1, Args vars2 -> Args (Vars.join vars1 vars2)

    let get_top t =
      match t with Args_with_top { w; vars = _ } -> Some w | Args _ -> None

    let get_vars t =
      match t with Args_with_top { w = _; vars } | Args vars -> vars

    let same_vars t1 t2 = Vars.same_vars (get_vars t1) (get_vars t2)

    let has_witnesses t =
      match t with
      | Args_with_top { w; vars } ->
        Vars.has_witnesses vars || not (Witnesses.is_empty w)
      | Args vars -> Vars.has_witnesses vars

    let replace_witnesses t w =
      match t with
      | Args vars -> Args (Vars.replace_witnesses vars w)
      | Args_with_top { w = _; vars } ->
        Args_with_top { w; vars = Vars.replace_witnesses vars w }

    let print ~witnesses ppf t =
      match t with
      | Args vars ->
        Format.fprintf ppf "(transform:@.%a)@." (Vars.print ~witnesses) vars
      | Args_with_top { w; vars } ->
        Format.fprintf ppf "(transform:@.%a@.%a)@." (pp_top ~witnesses) w
          (Vars.print ~witnesses) vars
  end

  (* CR gyorsh: treatment of vars and top is duplicated between Args and
     Transform, is there a nice way to factor it out? *)

  (** helper for Join  *)
  module Args : sig
    type t

    val add_tr : t -> Transform.t -> t

    val add_var : t -> Var.t -> Witnesses.t -> t

    val empty : t

    val join : t -> t -> t

    val get : t -> Vars.t * Transform.Set.t

    (** [transform t tr] replace each element x of [t] with "transform x tr" *)
    val transform : t -> Transform.t -> t

    (** [transform_var t var w]
        replace each element x of [t] with "transfrom x var". *)
    val transform_var : t -> Var.t -> Witnesses.t -> t

    (** [transform_top t w] replace each element x of [t] with
        "transform t (Top w)" *)
    val transform_top : t -> Witnesses.t -> t

    val transform_join : t -> t -> t

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val compare : t -> t -> int

    val print : witnesses:bool -> Format.formatter -> t -> unit
  end = struct
    type t =
      { vars : Vars.t;
        trs : Transform.Set.t
      }

    let empty = { vars = Vars.empty; trs = Transform.Set.empty }

    let get { vars; trs } = vars, trs

    let print ~witnesses ppf { vars; trs } =
      let pp_trs ppf trs =
        Transform.Set.iter (Transform.print ~witnesses ppf) trs
      in
      Format.fprintf ppf "vars=(%a)@.transforms=(%a)@," (Vars.print ~witnesses)
        vars pp_trs trs

    let add_var t var witnesses =
      (* Optimization to avoid allocation when the content hasn't changed. *)
      let vars = Vars.update t.vars var witnesses in
      if vars == t.vars then t else { t with vars }

    let add_tr t tr =
      let trs = Transform.Set.add tr t.trs in
      if trs == t.trs then t else { t with trs }

    let join ({ vars = v1; trs = trs1 } as t) ({ vars = v2; trs = trs2 } as t')
        =
      if debug
      then
        Format.fprintf Format.std_formatter "join@.%a@. %a@."
          (print ~witnesses:true) t (print ~witnesses:true) t';
      let vars = Vars.join v1 v2 in
      let trs = Transform.Set.union trs1 trs2 in
      { vars; trs }

    let transform { vars; trs } tr =
      let from_vars =
        (* add each x from [vars] to [tr] *)
        Vars.fold
          ~f:(fun var w acc ->
            Transform.Set.add (Transform.add_var tr var w) acc)
          vars ~init:Transform.Set.empty
      in
      let from_trs =
        Transform.Set.map (fun tr' -> Transform.flatten tr tr') trs
      in
      { vars = Vars.empty; trs = Transform.Set.union from_vars from_trs }

    let transform_top { vars; trs } w =
      let from_vars =
        Vars.fold
          ~f:(fun var var_witnesses acc ->
            Transform.Set.add (Transform.var_with_top var ~var_witnesses w) acc)
          vars ~init:Transform.Set.empty
      in
      let from_trs = Transform.Set.map (fun tr -> Transform.add_top tr w) trs in
      { vars = Vars.empty; trs = Transform.Set.union from_vars from_trs }

    let transform_var { vars; trs } var witnesses =
      let acc =
        Vars.fold
          ~f:(fun var1 w1 args ->
            if Var.equal var1 var
            then add_var args var witnesses
            else
              let tr = Transform.vars ~var1 ~w1 ~var2:var ~w2:witnesses in
              add_tr args tr)
          vars ~init:empty
      in
      let from_trs =
        Transform.Set.map (fun tr -> Transform.add_var tr var witnesses) trs
      in
      { acc with trs = Transform.Set.union from_trs acc.trs }

    let transform_join t ({ vars; trs } as t') =
      if debug
      then
        Format.fprintf Format.std_formatter "transform_join@.%a@. %a@."
          (print ~witnesses:true) t (print ~witnesses:true) t';
      let acc =
        Vars.fold vars ~init:empty ~f:(fun var witnesses acc ->
            join acc (transform_var t var witnesses))
      in
      Transform.Set.fold (fun tr acc -> join acc (transform t tr)) trs acc

    let has_witnesses { vars; trs } =
      Vars.has_witnesses vars
      || Transform.Set.exists Transform.has_witnesses trs

    let replace_witnesses { vars; trs } w =
      { vars = Vars.replace_witnesses vars w;
        trs = Transform.Set.map (fun tr -> Transform.replace_witnesses tr w) trs
      }

    let compare { vars = vars1; trs = trs1 } { vars = vars2; trs = trs2 } =
      let c = Vars.compare vars1 vars2 in
      if c <> 0 then c else Transform.Set.compare trs1 trs2
  end

  (** normal form of join *)
  module Join : sig
    (** normal form of Join *)
    type t

    val tr_with_safe : Transform.t -> t

    val tr_with_top : Transform.t -> Witnesses.t -> t

    val var_with_top : Var.t -> var_witnesses:Witnesses.t -> Witnesses.t -> t

    val var_with_safe : Var.t -> Witnesses.t -> t

    val var_with_tr : Var.t -> Witnesses.t -> Transform.t -> t

    val vars : var1:Var.t -> w1:Witnesses.t -> var2:Var.t -> w2:Witnesses.t -> t

    val trs : Transform.t -> Transform.t -> t

    val add_top : t -> Witnesses.t -> t

    val add_safe : t -> t

    val add_var : t -> Var.t -> Witnesses.t -> t

    val add_tr : t -> Transform.t -> t

    val flatten : t -> t -> t

    val distribute_transform_over_join : t -> Transform.t -> t

    val distribute_transform_var_over_join : t -> Var.t -> Witnesses.t -> t

    val distribute_transform_top_over_join : t -> Witnesses.t -> t

    val distribute_transform_over_joins : t -> t -> t

    val get_top : t -> Witnesses.t option

    val has_safe : t -> bool

    val get : t -> Vars.t * Transform.Set.t

    val print : witnesses:bool -> Format.formatter -> t -> unit

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val compare : t -> t -> int
  end = struct
    type t =
      | Args_with_safe of Args.t
      | Args_with_top of
          { w : Witnesses.t;
            args : Args.t
          }
      | Args of Args.t

    (* Tracks "top" to preserve witnesses. For example simplifying
     * "join (Top  w) (Var v w')" to "Top w"
     * loses the witness w' when w' is non-empty and v resolved to Top at the end.
     * Simplifying to "Top (join w w')" is wrong if v is resolved to Safe or Bot. *)

    (* Only Top or Safe are allowed not both. At least two elements must be
       present in the join: if one of the constants is present then at least one
       of vars or transforms must be non-empty. If no constants, then vars or
       transforms have at least two elements between them. The following
       constructor ensure it. *)

    let tr_with_safe tr = Args_with_safe Args.(add_tr empty tr)

    let tr_with_top tr w = Args_with_top { w; args = Args.(add_tr empty tr) }

    let var_with_top var ~var_witnesses w =
      Args_with_top { w; args = Args.(add_var empty var var_witnesses) }

    let var_with_safe var w = Args_with_safe Args.(add_var empty var w)

    let trs tr1 tr2 =
      assert (not (Transform.equal tr1 tr2));
      Args Args.(add_tr (add_tr empty tr1) tr2)

    let vars ~var1 ~w1 ~var2 ~w2 =
      assert (not (Var.equal var1 var2));
      Args Args.(add_var (add_var empty var2 w2) var1 w1)

    let var_with_tr var w tr = Args Args.(add_var (add_tr empty tr) var w)

    let add_safe t =
      match t with
      | Args_with_top _ -> t
      | Args_with_safe _ -> t
      | Args args -> Args_with_safe args

    let add_top t witnesses =
      match t with
      | Args_with_top { w; args } ->
        Args_with_top { w = Witnesses.join w witnesses; args }
      | Args_with_safe args | Args args -> Args_with_top { w = witnesses; args }

    let add_var t var witnesses =
      match t with
      | Args_with_safe args -> Args_with_safe (Args.add_var args var witnesses)
      | Args args -> Args (Args.add_var args var witnesses)
      | Args_with_top { w; args } ->
        Args_with_top { w; args = Args.add_var args var witnesses }

    let flatten t1 t2 =
      match t1, t2 with
      | Args a1, Args a2 -> Args (Args.join a1 a2)
      | Args_with_safe a1, Args_with_safe a2 -> Args_with_safe (Args.join a1 a2)
      | Args_with_top { w; args = a1 }, (Args a2 | Args_with_safe a2)
      | (Args a2 | Args_with_safe a2), Args_with_top { w; args = a1 } ->
        Args_with_top { w; args = Args.join a1 a2 }
      | Args_with_top { w = w1; args = a1 }, Args_with_top { w = w2; args = a2 }
        ->
        Args_with_top { w = Witnesses.join w1 w2; args = Args.join a1 a2 }
      | Args args1, Args_with_safe args2 | Args_with_safe args1, Args args2 ->
        Args_with_safe (Args.join args1 args2)

    let distribute_transform_over_join t tr =
      match t with
      | Args_with_safe args ->
        let args = Args.(add_tr (transform args tr) tr) in
        Args args
      | Args_with_top { w; args } ->
        let tr' = Transform.add_top tr w in
        let args = Args.(add_tr (transform args tr) tr') in
        Args args
      | Args args -> Args (Args.transform args tr)

    let distribute_transform_var_over_join t var witnesses =
      match t with
      | Args_with_safe args ->
        let args =
          Args.add_var (Args.transform_var args var witnesses) var witnesses
        in
        Args args
      | Args_with_top { w; args } ->
        let tr' = Transform.var_with_top var ~var_witnesses:witnesses w in
        let args = Args.(add_tr (transform_var args var witnesses) tr') in
        Args args
      | Args args -> Args (Args.transform_var args var witnesses)

    let distribute_transform_top_over_join t w =
      match t with
      | Args_with_safe args ->
        let args = Args.transform_top args w in
        Args_with_top { w; args }
      | Args_with_top { w = w'; args } ->
        let args = Args.(transform_top args w) in
        Args_with_top { w = Witnesses.join w' w; args }
      | Args args -> Args (Args.transform_top args w)

    let distribute_transform_over_joins t1 t2 =
      match t1, t2 with
      | Args a1, Args a2 -> Args (Args.transform_join a1 a2)
      | Args_with_safe a1, Args_with_safe a2 ->
        let new_args = Args.transform_join a1 a2 in
        Args_with_safe (Args.join a1 (Args.join a2 new_args))
      | Args_with_safe a1, Args a2 | Args a2, Args_with_safe a1 ->
        let new_args = Args.transform_join a1 a2 in
        Args (Args.join a2 new_args)
      | Args_with_top { w = w1; args = a1 }, Args_with_top { w = w2; args = a2 }
        ->
        if debug
        then
          Format.printf "distribute_transform_over_joins:@.%a@.%a@."
            (Args.print ~witnesses:true)
            a1
            (Args.print ~witnesses:true)
            a2;
        let new_args = Args.transform_join a1 a2 in
        let args_top =
          Args.join (Args.transform_top a1 w2) (Args.transform_top a2 w1)
        in
        Args_with_top
          { w = Witnesses.join w1 w2; args = Args.join new_args args_top }
      | Args_with_top { w; args = a1 }, Args a2
      | Args a2, Args_with_top { w; args = a1 } ->
        let new_args = Args.transform_join a1 a2 in
        let args_top = Args.transform_top a2 w in
        Args (Args.join new_args args_top)
      | Args_with_top { w; args = a1 }, Args_with_safe a2
      | Args_with_safe a2, Args_with_top { w; args = a1 } ->
        let new_args = Args.transform_join a1 a2 in
        let args_top = Args.transform_top a2 w in
        let args = Args.join new_args args_top in
        Args_with_top { w; args }

    let add_tr t tr =
      match t with
      | Args_with_safe args -> Args_with_safe (Args.add_tr args tr)
      | Args args -> Args (Args.add_tr args tr)
      | Args_with_top { w; args } ->
        Args_with_top { w; args = Args.add_tr args tr }

    let get_top t =
      match t with
      | Args_with_top { w; args = _ } -> Some w
      | Args _ | Args_with_safe _ -> None

    let has_safe t =
      match t with
      | Args_with_safe _ -> true
      | Args _ | Args_with_top _ -> false

    let get t =
      match t with
      | Args_with_top { w = _; args } | Args args | Args_with_safe args ->
        Args.get args

    let has_witnesses t =
      match t with
      | Args_with_safe args | Args args -> Args.has_witnesses args
      | Args_with_top { w; args } ->
        (not (Witnesses.is_empty w)) || Args.has_witnesses args

    let replace_witnesses t witnesses =
      match t with
      | Args_with_safe args ->
        Args_with_safe (Args.replace_witnesses args witnesses)
      | Args args -> Args (Args.replace_witnesses args witnesses)
      | Args_with_top { w; args } ->
        Args_with_top { w; args = Args.replace_witnesses args witnesses }

    let compare t1 t2 =
      match t1, t2 with
      | Args a1, Args a2 | Args_with_safe a1, Args_with_safe a2 ->
        Args.compare a1 a2
      | Args _, _ -> -1
      | _, Args _ -> 1
      | Args_with_safe _, _ -> -1
      | _, Args_with_safe _ -> 1
      | Args_with_top { w = w1; args = a1 }, Args_with_top { w = w2; args = a2 }
        ->
        let c = Witnesses.compare w1 w2 in
        if c <> 0 then c else Args.compare a1 a2

    let print ~witnesses ppf t =
      match t with
      | Args_with_safe args ->
        Format.fprintf ppf "(join:@.Safe@.%a)@." (Args.print ~witnesses) args
      | Args_with_top { w; args } ->
        Format.fprintf ppf "(join:@.%a@.%a)@." (pp_top ~witnesses) w
          (Args.print ~witnesses) args
      | Args args ->
        Format.fprintf ppf "(join:@.%a)@." (Args.print ~witnesses) args
  end

  type t =
    | Top of Witnesses.t
    | Safe
    | Bot
    (* unresolved *)
    | Var of
        { var : Var.t;
          witnesses : Witnesses.t
        }
    | Transform of Transform.t
    | Join of Join.t

  let unresolved witnesses var = Var { var; witnesses }

  let bot = Bot

  let safe = Safe

  let top w = Top w

  let is_resolved t =
    match t with
    | Top _ | Safe | Bot -> true
    | Var _ | Join _ | Transform _ -> false

  let print ~witnesses ppf t =
    match t with
    | Bot -> Format.fprintf ppf "bot"
    | Safe -> Format.fprintf ppf "safe"
    | Top w -> pp_top ~witnesses ppf w
    | Var { var; witnesses = w } -> pp_var ~witnesses ppf var w
    | Join j -> Format.fprintf ppf "(join %a)@," (Join.print ~witnesses) j
    | Transform tr ->
      Format.fprintf ppf "(transform %a)@," (Transform.print ~witnesses) tr

  let match_with ~bot ~safe ~top ~unresolved t =
    match t with
    | Bot -> bot
    | Safe -> safe
    | Top w -> top w
    | Var _ | Join _ | Transform _ -> unresolved ()

  let get_witnesses t =
    match t with
    | Top w -> w
    | Bot | Safe -> Witnesses.empty
    | Var _ | Transform _ | Join _ -> assert false

  (* structural *)
  let compare t1 t2 =
    match t1, t2 with
    | Bot, Bot -> 0
    | Bot, _ -> -1
    | _, Bot -> 1
    | Safe, Safe -> 0
    | Safe, _ -> -1
    | _, Safe -> 1
    | Top w1, Top w2 -> Witnesses.compare w1 w2
    | Top _, (Var _ | Transform _ | Join _) -> -1
    | (Var _ | Transform _ | Join _), Top _ -> 1
    | Var { var = v1; witnesses = w1 }, Var { var = v2; witnesses = w2 } ->
      let c = Var.compare v1 v2 in
      if c = 0 then Witnesses.compare w1 w2 else c
    | Var _, _ -> -1
    | _, Var _ -> 1
    | Transform tr1, Transform tr2 -> Transform.compare tr1 tr2
    | Transform _, _ -> -1
    | _, Transform _ -> 1
    | Join j1, Join j2 -> Join.compare j1 j2

  let equal t1 t2 = compare t1 t2 = 0

  (* This naive normal form is conceptually "dnf". Currently very inefficient,
     does not guarantee sharing, and reallocates even if the input is already in
     a normal form. Worst case exponential space (in the number of variables).
     Someday it can be optimized using hash consing and bdd-like
     representation. *)

  (* Keep [join] and [lessequal] in sync. *)
  let join t1 t2 =
    match t1, t2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top _, Bot | Top _, Safe -> t1
    | Bot, Top _ | Safe, Top _ -> t2
    | Bot, (Var _ | Transform _ | Join _) -> t2
    | (Var _ | Transform _ | Join _), Bot -> t1
    | Safe, Transform tr | Transform tr, Safe -> Join (Join.tr_with_safe tr)
    | Safe, Var { var; witnesses } | Var { var; witnesses }, Safe ->
      Join (Join.var_with_safe var witnesses)
    | (Top w as top), Var { var; witnesses }
    | Var { var; witnesses }, (Top w as top) ->
      if Witnesses.is_empty witnesses
      then top
      else Join (Join.var_with_top var ~var_witnesses:witnesses w)
    | Var { var = var1; witnesses = w1 }, Var { var = var2; witnesses = w2 } ->
      if Var.equal var1 var2
      then Var { var = var1; witnesses = Witnesses.join w1 w2 }
      else Join (Join.vars ~var1 ~w1 ~var2 ~w2)
    | Var { var; witnesses }, Transform tr
    | Transform tr, Var { var; witnesses } ->
      Join (Join.var_with_tr var witnesses tr)
    | Transform tr1, Transform tr2 ->
      if Transform.equal tr1 tr2
      then t1
      else if Transform.same_vars tr1 tr2
      then Transform (Transform.flatten tr1 tr2)
      else Join (Join.trs tr1 tr2)
    | (Top w as top), Transform tr | Transform tr, (Top w as top) ->
      (* [has_witnesses]: Don't simplify (join Top x) to x if there are any
         witnesses in x. This makes the analysis more expensive because symbolic
         summaries cannot be simiplified as much. Finding out if there are
         witnesses is also expensive (traverse the entire term). Someday, we can
         make it cheap by passing [keep_witnesses] to all operations. Only
         functions that need to be checked against a user-provided annotation at
         the end keep witnesses, unless the analysis is used to visualize all
         allocations. We can put a bound on the number of witnesses recorded,
         but it would make the resulting error messages sensitive to iteration
         order. *)
      if Transform.has_witnesses tr then Join (Join.tr_with_top tr w) else top
    | (Top w as top), Join j | Join j, (Top w as top) ->
      if Join.has_witnesses j then Join (Join.add_top j w) else top
    | Safe, Join j | Join j, Safe -> Join (Join.add_safe j)
    | Var { var; witnesses }, Join j | Join j, Var { var; witnesses } ->
      Join (Join.add_var j var witnesses)
    | Join j, Transform tr | Transform tr, Join j -> Join (Join.add_tr j tr)
    | Join j1, Join j2 -> Join (Join.flatten j1 j2)

  (* CR gyorsh: Handling of constant cases here is an optimization, instead of
     going directly to [join]. *)
  let lessequal t1 t2 =
    match t1, t2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top w1, Top w2 -> Witnesses.lessequal w1 w2
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false
    | Bot, (Var _ | Transform _ | Join _) -> true
    | (Var _ | Transform _ | Join _), Bot -> false
    | (Safe | Top _ | Var _ | Transform _ | Join _), _ ->
      (* structural equality on the normal form *)
      equal (join t1 t2) t2

  (* Abstract transformer. Commutative and Associative.

     let transform t t' = if t = V.Bot || t' = V.Bot then V.Bot else (V.join t
     t')

     The implementation is an optimized version of the above definition that
     "inlines" and "specializes" join: efficently handle definitive cases and
     preserve normal form of unresolved.

     Soundness (intuitively): If a return is unreachable from the program
     location immediately after the statement, or the statement does not return,
     then return is unreachable from the program location immediately before the
     statement. *)
  let transform t t' =
    match t, t' with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Safe, t' -> t'
    | t, Safe -> t
    | Top w, Top w' -> Top (Witnesses.join w w')
    | Top w, Transform tr | Transform tr, Top w ->
      Transform (Transform.add_top tr w)
    | Top w, Var { var; witnesses } | Var { var; witnesses }, Top w ->
      Transform (Transform.var_with_top var ~var_witnesses:witnesses w)
    | Var { var; witnesses }, Transform tr
    | Transform tr, Var { var; witnesses } ->
      Transform (Transform.add_var tr var witnesses)
    | Var { var = var1; witnesses = w1 }, Var { var = var2; witnesses = w2 } ->
      if Var.equal var1 var2
      then Var { var = var1; witnesses = Witnesses.join w1 w2 }
      else Transform (Transform.vars ~var1 ~w1 ~var2 ~w2)
    | Transform tr1, Transform tr2 -> Transform (Transform.flatten tr1 tr2)
    | Transform tr, Join j | Join j, Transform tr ->
      Join (Join.distribute_transform_over_join j tr)
    | (Top w as top), Join j | Join j, (Top w as top) ->
      if Join.has_safe j && not (Join.has_witnesses j)
      then top
      else Join (Join.distribute_transform_top_over_join j w)
    | Var { var; witnesses }, Join j | Join j, Var { var; witnesses } ->
      Join (Join.distribute_transform_var_over_join j var witnesses)
    | Join j1, Join j2 -> Join (Join.distribute_transform_over_joins j1 j2)

  let replace_witnesses w t =
    match t with
    | Top _ -> Top w
    | Bot | Safe -> t
    | Join j -> Join (Join.replace_witnesses j w)
    | Transform tr -> Transform (Transform.replace_witnesses tr w)
    | Var { var; witnesses = _ } -> Var { var; witnesses = w }

  let diff_witnesses ~expected ~actual =
    (* If [actual] is Top and [expected] is not Top then return the [actual]
       witnesses. Otherwise, return empty. *)
    match actual, expected with
    | Bot, Bot | Safe, Safe | Bot, Safe -> Witnesses.empty
    | Bot, Top w | Safe, Top w | Top _, Top w ->
      assert (Witnesses.is_empty w);
      Witnesses.empty
    | Safe, Bot -> Witnesses.empty
    | Top w, (Bot | Safe) -> w
    | (Var _ | Join _ | Transform _), _ | _, (Var _ | Join _ | Transform _) ->
      assert false

  let rec apply t ~env =
    match t with
    | Bot | Safe | Top _ -> t
    | Var { var; witnesses } -> replace_witnesses witnesses (env var)
    | Transform tr ->
      let init =
        match Transform.get_top tr with None -> Safe | Some w -> Top w
      in
      Vars.fold
        ~f:(fun var w acc ->
          let v = replace_witnesses w (env var) in
          transform v acc)
        ~init (Transform.get_vars tr)
    | Join j ->
      let init =
        match Join.get_top j with
        | None -> if Join.has_safe j then Safe else Bot
        | Some w -> Top w
      in
      let vars, trs = Join.get j in
      let acc =
        Vars.fold
          ~f:(fun var w acc ->
            let v = replace_witnesses w (env var) in
            join v acc)
          ~init vars
      in
      Transform.Set.fold
        (fun tr acc ->
          let t = Transform tr in
          join (apply t ~env) acc)
        trs acc
end

module T = Zero_alloc_utils.Make_value (Witnesses) (V)

module Value : sig
  include module type of T

  val transform : V.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val get_witnesses : t -> Witnesses.components

  val diff_witnesses : expected:t -> actual:t -> Witnesses.components

  val unresolved : string -> Witnesses.t -> t

  val is_resolved : t -> bool

  val get_component : t -> Tag.t -> V.t

  val apply : t -> (Var.t -> V.t) -> t
end = struct
  include T

  let unresolved name w =
    { nor = Var.get name Tag.N |> V.unresolved w;
      exn = Var.get name Tag.E |> V.unresolved w;
      div = Var.get name Tag.D |> V.unresolved w
    }

  let is_resolved t =
    V.is_resolved t.nor && V.is_resolved t.exn && V.is_resolved t.div

  let get_component t (tag : Tag.t) =
    match tag with N -> t.nor | E -> t.exn | D -> t.div

  let apply t env =
    { nor = V.apply t.nor ~env;
      exn = V.apply t.exn ~env;
      div = V.apply t.div ~env
    }

  let transform effect v =
    { nor = V.transform effect v.nor;
      exn = V.transform effect v.exn;
      div = V.transform effect v.div
    }

  let replace_witnesses w t =
    { nor = V.replace_witnesses w t.nor;
      exn = V.replace_witnesses w t.exn;
      div = V.replace_witnesses w t.div
    }

  let diff_witnesses ~expected ~actual =
    { Witnesses.nor = V.diff_witnesses ~expected:expected.nor ~actual:actual.nor;
      Witnesses.exn = V.diff_witnesses ~expected:expected.exn ~actual:actual.exn;
      Witnesses.div = V.diff_witnesses ~expected:expected.div ~actual:actual.div
    }

  let get_witnesses t =
    { Witnesses.nor = V.get_witnesses t.nor;
      Witnesses.exn = V.get_witnesses t.exn;
      Witnesses.div = V.get_witnesses t.div
    }
end

(**  Representation of user-provided annotations as abstract values *)
module Annotation : sig
  type t

  val get_loc : t -> Location.t

  val find :
    Cmm.codegen_option list -> Cmm.property -> string -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  (** [valid t value] returns true if and only if the [value] satisfies the annotation,
      i.e., [value] is less or equal to [expected_value a] when ignoring witnesses. *)

  val valid : t -> Value.t -> bool

  val diff_witnesses : t -> Value.t -> Witnesses.components

  val is_assume : t -> bool

  val is_strict : t -> bool

  val is_check_enabled :
    Cmm.codegen_option list -> string -> Debuginfo.t -> bool
end = struct
  (**
   ***************************************************************************
   *  [Strict] statically guarantees that all paths through the function satisfy
   *  all of the following conditions:
   *   - property holds on all primitive operations (e.g., no heap allocation)
   *   - no indirect calls (incl. no indirect tailcalls)
   *   - all direct calls (incl. tailcalls and probes) are to functions that
   *     satisfy the same conditions, i.e., they are [Strict].
   *
   *  [Relaxed] is the same as [Strict] on all paths that end in a normal return
   *  from a function, but no restrictions on diverging executions or
   *  on when a function returns with a [raise] with backtrace, which is treated
   *  as an error return (whereas [raise_no_trace] is treated as normal control flow
   *  and is subject to [Strict] requirements).
   *
   *****************************************************************************)

  type t =
    { strict : bool;  (** strict or relaxed? *)
      assume : bool;
      never_returns_normally : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let get_loc t = t.loc

  let expected_value { strict; never_returns_normally; assume = _; loc = _ } =
    Value.of_annotation ~strict ~never_returns_normally

  let valid t v =
    (* Use Value.lessequal but ignore witnesses. *)
    let expected = expected_value t in
    let actual = Value.replace_witnesses Witnesses.empty v in
    Value.lessequal actual expected

  let diff_witnesses t v =
    let expected = expected_value t in
    Value.diff_witnesses ~actual:v ~expected

  let is_assume t = t.assume

  let is_strict t = t.strict

  let find codegen_options spec fun_name dbg =
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check { property; strict; loc } when property = spec ->
            Some { strict; assume = false; never_returns_normally = false; loc }
          | Assume { property; strict; never_returns_normally; loc }
            when property = spec ->
            Some { strict; assume = true; never_returns_normally; loc }
          | Check _ | Assume _ | Reduce_code_size | No_CSE
          | Use_linscan_regalloc ->
            None)
        codegen_options
    in
    match a with
    | [] -> None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a for %s"
        Debuginfo.print_compact dbg fun_name ()

  let is_check_enabled codegen_options fun_name dbg =
    let is_enabled p =
      match find codegen_options p fun_name dbg with
      | None -> false
      | Some { assume; _ } -> not assume
    in
    List.exists is_enabled Cmm.all_properties
end

module Metadata : sig
  (* CR-someday gyorsh: propagate assert of arbitrary expressions. *)
  val assume_value :
    Debuginfo.t -> can_raise:bool -> Witnesses.t -> Value.t option
end = struct
  (* CR gyorsh: The return type of [Assume_info.get_value] is
     [Assume_info.Value.t]. It is not the same as [Checkmach.Value.t], because
     [Witnesses] in [Checkmach] depend on Debuginfo and cannot be used in
     Assume_info due to cyclic dependencies. The witnesses in Assume_info are
     always empty and the translation is trivial. Is there a better way to avoid
     duplicating [Zero_alloc_utils]? *)
  let transl w (v : Zero_alloc_utils.Assume_info.V.t) : V.t =
    match v with Top _ -> V.top w | Safe -> V.safe | Bot -> V.bot

  let transl w (v : Zero_alloc_utils.Assume_info.Value.t) : Value.t =
    { nor = transl w v.nor; exn = transl w v.exn; div = transl w v.div }

  let assume_value dbg ~can_raise w =
    (* [loc] can be obtained by [Debuginfo.to_location dbg], For now just return
       [Location.none] because it is not used. *)
    let a = Debuginfo.assume_zero_alloc dbg in
    match Zero_alloc_utils.Assume_info.get_value a with
    | None -> None
    | Some v ->
      let v = transl w v in
      let v = if can_raise then v else { v with exn = V.bot } in
      Some v
end

module Report : sig
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  val print : exn -> Location.error option
end = struct
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list * Cmm.property

  let annotation_error ~property_name t =
    (* print location of the annotation, print function name as part of the
       message. *)
    let loc = Annotation.get_loc t.a in
    let print_annotated_fun ppf () =
      let scoped_name =
        t.fun_dbg |> Debuginfo.get_dbg |> Debuginfo.Dbg.to_list
        |> List.map (fun dbg ->
               Debuginfo.(Scoped_location.string_of_scopes dbg.dinfo_scopes))
        |> String.concat ","
      in
      Format.fprintf ppf "Annotation check for %s%s failed on function %s (%s)"
        property_name
        (if Annotation.is_strict t.a then " strict" else "")
        scoped_name t.fun_name
    in
    Location.error_of_printer ~loc print_annotated_fun ()

  let error_messages ~property_name t : Location.error list =
    let pp_inlined_dbg ppf dbg =
      (* Show inlined locations, if dbg has more than one item. The first item
         will be shown at the start of the error message. *)
      if Debuginfo.Dbg.length (Debuginfo.get_dbg dbg) > 1
      then Format.fprintf ppf " (%a)" Debuginfo.print_compact dbg
    in
    let print_comballoc dbg =
      match dbg with
      | [] | [_] -> "", []
      | alloc_dbginfo ->
        (* If one Ialloc is a result of combining multiple allocations, print
           details of each location. Currently, this cannot happen because
           checkmach is before comballoc. In the future, this may be done in the
           middle-end. *)
        let msg =
          Printf.sprintf " combining %d allocations below"
            (List.length alloc_dbginfo)
        in
        let details =
          List.map
            (fun (item : Debuginfo.alloc_dbginfo_item) ->
              let pp_alloc ppf =
                Format.fprintf ppf "allocate %d words%a" item.alloc_words
                  pp_inlined_dbg item.alloc_dbg
              in
              let aloc = Debuginfo.to_location item.alloc_dbg in
              Location.mkloc pp_alloc aloc)
            alloc_dbginfo
        in
        msg, details
    in
    let print_witness (w : Witness.t) ~component =
      (* print location of the witness, print witness description. *)
      let loc = Debuginfo.to_location w.dbg in
      let component_msg =
        if String.equal "" component
        then component
        else " on a path to " ^ component
      in
      let print_main_msg, sub =
        match w.kind with
        | Alloc { bytes = _; dbginfo } ->
          let comballoc_msg, sub = print_comballoc dbginfo in
          ( Format.dprintf "%a%s%s" Witness.print_kind w.kind component_msg
              comballoc_msg,
            sub )
        | Indirect_call | Indirect_tailcall | Direct_call _ | Direct_tailcall _
        | Extcall _ ->
          ( Format.dprintf "called function may allocate%s (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
        | Arch_specific | Probe _ ->
          ( Format.dprintf "expression may allocate%s@ (%a)" component_msg
              Witness.print_kind w.kind,
            [] )
      in
      let pp ppf () =
        print_main_msg ppf;
        pp_inlined_dbg ppf w.dbg
      in
      Location.error_of_printer ~loc ~sub pp ()
    in
    let print_witnesses ws : Location.error list =
      let { Witnesses.nor; exn; div } = Witnesses.simplify ws in
      let f ws component =
        ws |> Witnesses.elements |> List.map (print_witness ~component)
      in
      List.concat [f div "diverge"; f nor ""; f exn "exceptional return"]
    in
    let details =
      match !Flambda_backend_flags.checkmach_details_cutoff with
      | No_details ->
        (* do not print witnesses. *)
        []
      | Keep_all -> print_witnesses t.witnesses
      | At_most cutoff ->
        let all = print_witnesses t.witnesses in
        if List.compare_length_with all cutoff <= 0
        then all
        else
          let result, _ = Misc.Stdlib.List.split_at cutoff all in
          result
    in
    annotation_error ~property_name t :: details

  let rec print_all msgs =
    (* Print all errors message in a compilation unit as separate messages to
       make editor integration easier. *)
    match msgs with
    | [] -> assert false
    | [last_error] ->
      (* Finally, raise Error with the last function. *)
      Some last_error
    | error :: tl ->
      Location.print_report Format.err_formatter error;
      print_all tl

  let print = function
    | Fail (reports, property) ->
      let property_name = Printcmm.property_to_string property in
      (* Sort by function's location. If debuginfo is missing, keep sorted by
         function name. *)
      let compare t1 t2 =
        let c = Debuginfo.compare t1.fun_dbg t2.fun_dbg in
        if not (Int.equal c 0)
        then c
        else String.compare t1.fun_name t2.fun_name
      in
      reports |> List.stable_sort compare
      |> List.concat_map (error_messages ~property_name)
      |> print_all
    | _ -> None
end

module Func_info : sig
  type t = private
    { name : string;  (** function name *)
      dbg : Debuginfo.t;  (** debug info associated with the function *)
      mutable value : Value.t;  (** the result of the check *)
      annotation : Annotation.t option
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
    }

  val create : string -> Value.t -> Debuginfo.t -> Annotation.t option -> t

  val print : witnesses:bool -> msg:string -> Format.formatter -> t -> unit

  val update : t -> Value.t -> unit
end = struct
  type t =
    { name : string;  (** function name *)
      dbg : Debuginfo.t;  (** debug info associated with the function *)
      mutable value : Value.t;  (** the result of the check *)
      annotation : Annotation.t option
          (** [value] must be lessequal than the expected value
          if there is user-defined annotation on this function. *)
    }

  let create name value dbg annotation = { name; dbg; value; annotation }

  let print ~witnesses ~msg ppf t =
    Format.fprintf ppf "%s %s %a@." msg t.name (Value.print ~witnesses) t.value

  let update t value = t.value <- value
end

module type Spec = sig
  (** Is the check enabled? *)
  val enabled : unit -> bool

  (** [get_value_opt f] returns the value recorded for function [f] in [Compilenv],
      either because the check passed or because of user-defined "assume" annotation.
      If [f] was compiled with checks disabled, returns None.
  *)
  val get_value_opt : string -> Value.t option

  (** [set_value f v] record the value of the function named [f] in [Compilenv]. *)
  val set_value : string -> Value.t -> unit

  (** Summary of target specific operations. *)
  val transform_specific : Witnesses.t -> Arch.specific_operation -> Value.t

  val property : Cmm.property
end
(* CR-someday gyorsh: We may also want annotations on call sites, not only on
   functions. *)

(** Information about functions that we have seen so far in the current compilation
      unit. *)
module Unit_info : sig
  (** mutable state *)
  type t

  val create : unit -> t

  val reset : t -> unit

  val find_opt : t -> string -> Func_info.t option

  (** [recod t name v dbg a] name must be in the current compilation unit,
      and not previously recorded.  *)
  val record :
    t -> string -> Value.t -> Debuginfo.t -> Annotation.t option -> unit

  val iter : t -> f:(Func_info.t -> unit) -> unit

  val fold : t -> f:(Func_info.t -> 'a -> 'a) -> init:'a -> 'a
end = struct
  (** map function name to the information about it *)
  type t = Func_info.t String.Tbl.t

  let create () = String.Tbl.create 17

  let reset t = String.Tbl.reset t

  let find_opt t name = String.Tbl.find_opt t name

  let iter t ~f = String.Tbl.iter (fun _ func_info -> f func_info) t

  let fold t ~f ~init =
    String.Tbl.fold (fun _name func_info acc -> f func_info acc) t init

  let record t name value dbg annotation =
    match String.Tbl.find_opt t name with
    | Some _ -> Misc.fatal_errorf "Duplicate symbol %s" name
    | None ->
      let func_info = Func_info.create name value dbg annotation in
      String.Tbl.replace t name func_info
end

(** The analysis involved some fixed point computations.
    Termination: [Value.t] is a finite height domain and
    [transfer] is a monotone function w.r.t. [Value.lessequal] order.
*)
module Analysis (S : Spec) : sig
  (** Check one function. *)
  val fundecl :
    Mach.fundecl ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Format.formatter ->
    unit

  (** Resolve all function summaries, check them against user-provided assertions,
      and record the summaries in Compilenv to be saved in .cmx files *)
  val record_unit : Unit_info.t -> Format.formatter -> unit
end = struct
  (** Information about the current function under analysis. *)
  type t =
    { ppf : Format.formatter;
      current_fun_name : string;
      future_funcnames : String.Set.t;
      unit_info : Unit_info.t;  (** must be the current compilation unit.  *)
      keep_witnesses : bool
    }

  let should_keep_witnesses keep =
    match !Flambda_backend_flags.checkmach_details_cutoff with
    | Keep_all -> true
    | No_details -> false
    | At_most _ -> keep

  let create ppf current_fun_name future_funcnames unit_info annot =
    let keep_witnesses = should_keep_witnesses (Option.is_some annot) in
    { ppf; current_fun_name; future_funcnames; unit_info; keep_witnesses }

  let analysis_name = Printcmm.property_to_string S.property

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_checkmach
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc
        (Value.print ~witnesses:true)
        v Debuginfo.print_compact dbg

  let report t v ~msg ~desc dbg =
    report' t.ppf v ~msg ~desc ~current_fun_name:t.current_fun_name dbg

  let is_future_funcname t callee = String.Set.mem callee t.future_funcnames

  let report_unit_info ppf unit_info ~msg =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Unit_info.iter unit_info ~f:(Func_info.print ~witnesses:true ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_checkmach
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ~witnesses:true ppf ~msg func_info

  let record_unit ppf unit_info =
    let errors = ref [] in
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        Builtin_attributes.mark_property_checked analysis_name
          (Annotation.get_loc a);
        if (not (Annotation.is_assume a))
           && S.enabled ()
           && not (Annotation.valid a func_info.value)
        then
          (* CR-soon gyorsh: keeping track of all the witnesses until the end of
             the compilation unit will be expensive. For functions that do not
             have any dependencies, we can check annotations earlier, as soon as
             the function is analyzed, or as soon as its dependencies are
             resolved, print the error, and remove the witnesses from the stored
             values. *)
          (* CR gyorsh: we can add error recovering mode where we sets the
             expected value as the actual value and continue analysis of other
             functions. *)
          let witnesses = Annotation.diff_witnesses a func_info.value in
          errors
            := { Report.a;
                 fun_name = func_info.name;
                 fun_dbg = func_info.dbg;
                 witnesses
               }
               :: !errors);
      report_func_info ~msg:"record" ppf func_info;
      S.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record;
    match !errors with
    | [] -> ()
    | errors -> raise (Report.Fail (errors, S.property))

  let[@inline always] create_witnesses t kind dbg =
    if t.keep_witnesses then Witnesses.create kind dbg else Witnesses.empty

  (* [find_callee] returns the value associated with the callee. *)
  let find_callee t callee ~desc dbg w =
    let return ~msg v =
      report t v ~msg ~desc dbg;
      (* Abstract witnesses of a call to the single witness for the callee name.
         Summary of tailcall self won't be affected because it is not set to Top
         by [find_callee]. *)
      Value.replace_witnesses w v
    in
    let unresolved v reason =
      let msg = Printf.sprintf "unresolved %s (%s)" callee reason in
      return ~msg v
    in
    let resolved v =
      assert (Value.is_resolved v);
      let msg = Printf.sprintf "resolved  %s" callee in
      return ~msg v
    in
    if is_future_funcname t callee
    then
      if !Flambda_backend_flags.disable_precise_checkmach
      then
        (* Conservatively return Top. Won't be able to prove any recursive
           functions as non-allocating. *)
        unresolved (Value.top w)
          "conservative handling of forward or recursive call\nor tailcall"
      else if String.equal callee t.current_fun_name
      then (* Self call. *)
        unresolved (Value.unresolved callee w) "self call"
      else
        (* Call is defined later in the current compilation unit. Summary of
           this callee is not yet computed. *)
        unresolved (Value.unresolved callee w) "foward call"
    else
      (* CR gyorsh: unresolved case here is impossible in the conservative
         analysis because all previous functions have been conservatively
         resolved.*)
      match Unit_info.find_opt t.unit_info callee with
      | None -> (
        (* Callee is not defined in the current compilation unit. *)
        match S.get_value_opt callee with
        | None ->
          unresolved (Value.top w)
            "missing summary: callee compiled without checks"
        | Some v -> resolved v)
      | Some callee_info ->
        (* Callee defined earlier in the same compilation unit. May have
           unresolved dependencies. *)
        if Value.is_resolved callee_info.value
        then resolved callee_info.value
        else
          unresolved
            (Value.unresolved callee w)
            "defined earlier with unresolved dependencies"

  let transform_return ~(effect : V.t) dst =
    (* Instead of calling [Value.transform] directly, first check for trivial
       cases to avoid reallocating [dst]. *)
    V.match_with effect ~bot:Value.bot ~safe:dst
      ~top:(fun _ -> Value.transform effect dst)
      ~unresolved:(fun () -> Value.transform effect dst)

  let transform_diverge ~(effect : V.t) (dst : Value.t) =
    let div = V.join effect dst.div in
    { dst with div }

  let transform t ~next ~exn ~(effect : Value.t) desc dbg =
    let next = transform_return ~effect:effect.nor next in
    let exn = transform_return ~effect:effect.exn exn in
    report t next ~msg:"transform new next" ~desc dbg;
    report t exn ~msg:"transform new exn" ~desc dbg;
    let r = Value.join next exn in
    report t r ~msg:"transform join" ~desc dbg;
    let r = transform_diverge ~effect:effect.div r in
    report t r ~msg:"transform result" ~desc dbg;
    r

  let transform_top t ~next ~exn w desc dbg =
    let effect =
      match Metadata.assume_value dbg ~can_raise:true w with
      | Some v -> v
      | None -> Value.top w
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_call t ~next ~exn callee w ~desc dbg =
    report t next ~msg:"transform_call next" ~desc dbg;
    report t exn ~msg:"transform_call exn" ~desc dbg;
    let effect =
      match Metadata.assume_value dbg ~can_raise:true w with
      | Some v -> v
      | None -> find_callee t callee ~desc dbg w
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
    | Iconst_float32 _ | Iconst_symbol _ | Iconst_vec128 _ | Iload _
    | Ifloatop _ | Ivectorcast _ | Iscalarcast _
    | Iintop_imm
        ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
          _ )
    | Iintop
        ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
    | Icsel _ ->
      assert (Mach.operation_is_pure op);
      assert (not (Mach.operation_can_raise op));
      next
    | Iname_for_debugger _ | Ivalueofint | Iintofvalue ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istackoffset _ | Iprobe_is_enabled _ | Iopaque | Ibeginregion | Iendregion
    | Iintop_atomic _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Istore _ ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ipoll _
    (* Ignore poll points even though they may trigger an allocations, because
       otherwise all loops would be considered allocating when poll insertion is
       enabled. [@poll error] should be used instead. *)
    | Ialloc { mode = Alloc_local; _ } ->
      assert (not (Mach.operation_can_raise op));
      next
    | Ialloc { mode = Alloc_heap; bytes; dbginfo } ->
      assert (not (Mach.operation_can_raise op));
      let w = create_witnesses t (Alloc { bytes; dbginfo }) dbg in
      let effect =
        match Metadata.assume_value dbg ~can_raise:false w with
        | Some effect -> effect
        | None -> Value.{ nor = V.top w; exn = V.bot; div = V.bot }
      in
      transform t ~effect ~next ~exn "heap allocation" dbg
    | Iprobe { name; handler_code_sym; enabled_at_init = __ } ->
      let desc = Printf.sprintf "probe %s handler %s" name handler_code_sym in
      let w = create_witnesses t (Probe { name; handler_code_sym }) dbg in
      transform_call t ~next ~exn handler_code_sym w ~desc dbg
    | Icall_ind ->
      let w = create_witnesses t Indirect_call dbg in
      transform_top t ~next ~exn w "indirect call" dbg
    | Itailcall_ind ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t Indirect_tailcall dbg in
      transform_top t ~next:Value.normal_return ~exn:Value.exn_escape w
        "indirect tailcall" dbg
    | Icall_imm { func = { sym_name = func; _ } } ->
      let w = create_witnesses t (Direct_call { callee = func }) dbg in
      transform_call t ~next ~exn func w ~desc:("direct call to " ^ func) dbg
    | Itailcall_imm { func = { sym_name = func; _ } } ->
      (* Sound to ignore [next] and [exn] because the call never returns. *)
      let w = create_witnesses t (Direct_tailcall { callee = func }) dbg in
      transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func w
        ~desc:("direct tailcall to " ^ func)
        dbg
    | Iextcall { alloc = false; returns = true; _ } ->
      (* Sound to ignore [exn] because external call marked as noalloc does not
         raise. *)
      next
    | Iextcall { alloc = false; returns = false; _ } ->
      (* Sound to ignore [next] and [exn] because the call never returns or
         raises. *)
      Value.bot
    | Iextcall { func; alloc = true; _ } ->
      let w = create_witnesses t (Extcall { callee = func }) dbg in
      transform_top t ~next ~exn w ("external call to " ^ func) dbg
    | Ispecific s ->
      let effect =
        let w = create_witnesses t Arch_specific dbg in
        match
          Metadata.assume_value dbg ~can_raise:(Arch.operation_can_raise s) w
        with
        | Some v -> v
        | None -> S.transform_specific w s
      in
      transform t ~next ~exn ~effect "Arch.specific_operation" dbg
    | Idls_get -> Misc.fatal_error "Idls_get not supported"

  module D = Dataflow.Backward ((Value : Dataflow.DOMAIN))

  let check_instr t body =
    let transfer (i : Mach.instruction) ~next ~exn =
      match i.desc with
      | Ireturn _ -> Value.normal_return
      | Iop op -> transform_operation t op ~next ~exn i.dbg
      | Iraise Raise_notrace ->
        (* [raise_notrace] is typically used for control flow, not for
           indicating an error. Therefore, we do not ignore any allocation on
           paths to it. The following conservatively assumes that normal and exn
           Returns are reachable. *)
        Value.join exn Value.safe
      | Iraise (Raise_reraise | Raise_regular) -> exn
      | Iend -> next
      | Iexit _ ->
        report t next ~msg:"transform" ~desc:"iexit" i.dbg;
        next
      | Iifthenelse _ | Iswitch _ -> next
      | Icatch (_rc, _ts, _, _body) ->
        report t next ~msg:"transform" ~desc:"catch" i.dbg;
        next
      | Itrywith (_body, _, (_trap_stack, _handler)) ->
        report t next ~msg:"transform" ~desc:"try-with" i.dbg;
        next
    in
    (* By default, backward analysis does not check the property on paths that
       diverge (non-terminating loops that do not reach normal or exceptional
       return). All loops must go through an (Iexit label) instruction or a
       recursive function call. If (Iexit label) is not backward reachable from
       the function's Normal or Exceptional Return, either the loop diverges or
       the Iexit instruction is not reachable from function entry.

       To check divergent loops, the initial value of "div" component of all
       Iexit labels of recurisve Icatch handlers is set to "Safe" instead of
       "Bot". *)
    D.analyze ~exnescape:Value.exn_escape ~init_rc_lbl:Value.diverges ~transfer
      body
    |> fst

  module Env : sig
    type t

    val empty : t

    val add : Func_info.t -> Value.t -> t -> t

    val get_value : string -> t -> Value.t

    val iter : t -> f:(Func_info.t -> Value.t -> unit) -> unit

    val map : t -> f:(Func_info.t -> Value.t -> Value.t) -> t

    val print : msg:string -> Format.formatter -> t -> unit
  end = struct
    type data =
      { func_info : Func_info.t;
        approx : Value.t
      }

    type t = data String.Map.t

    let empty = String.Map.empty

    let add (func_info : Func_info.t) approx t =
      let d = { func_info; approx } in
      String.Map.add func_info.name d t

    let get_value name t =
      let d = String.Map.find name t in
      d.approx

    let map t ~f =
      String.Map.map (fun d -> { d with approx = f d.func_info d.approx }) t

    let iter t ~f = String.Map.iter (fun _name d -> f d.func_info d.approx) t

    let print ~msg ppf t =
      if !Flambda_backend_flags.dump_checkmach
      then
        iter t ~f:(fun func_info approx ->
            Format.fprintf ppf "Env %s: %s: %a@." msg func_info.name
              (Value.print ~witnesses:true)
              approx)
  end

  (* CR gyorsh: do we need join in the fixpoint computation or is the function
     body analysis/summary already monotone? *)
  let fixpoint ppf unit_info =
    report_unit_info ppf unit_info ~msg:"before fixpoint";
    (* CR gyorsh: this is a really dumb iteration strategy. *)
    let found_unresolved = ref false in
    let init_env =
      (* initialize [env] with Bot for all functions on normal and exceptional
         return, and Safe for diverage component conservatively. *)
      let init_val = Value.diverges in
      Unit_info.fold unit_info ~init:Env.empty ~f:(fun func_info env ->
          let v =
            if Value.is_resolved func_info.value
            then func_info.value
            else (
              found_unresolved := true;
              init_val)
          in
          Env.add func_info v env)
    in
    let lookup env var =
      let v = Env.get_value (Var.name var) env in
      Value.get_component v (Var.tag var)
    in
    let rec loop env =
      Env.print ~msg:"computing fixpoint" ppf env;
      let changed = ref false in
      let env' =
        Env.map
          ~f:(fun func_info v ->
            let v' = Value.apply func_info.value (lookup env) in
            if !Flambda_backend_flags.dump_checkmach
            then
              Format.fprintf ppf "fixpoint after apply: %s %a@." func_info.name
                (Value.print ~witnesses:true)
                v';
            assert (Value.is_resolved v');
            if not (Value.lessequal v' v)
            then (
              changed := true;
              if !Flambda_backend_flags.dump_checkmach
              then
                Format.fprintf ppf "fixpoint update: %s %a@." func_info.name
                  (Value.print ~witnesses:true)
                  v');
            Value.join v v')
          env
      in
      if !changed then loop env' else env'
    in
    if !found_unresolved
    then (
      let env = loop init_env in
      Env.iter env ~f:(fun func_info v -> Func_info.update func_info v);
      Env.print ~msg:"after fixpoint" ppf env;
      report_unit_info ppf unit_info ~msg:"after fixpoint")

  let record_unit unit_info ppf =
    Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
      (fun () ->
        fixpoint ppf unit_info;
        record_unit ppf unit_info)

  let fundecl (f : Mach.fundecl) ~future_funcnames unit_info ppf =
    let check () =
      let fun_name = f.fun_name in
      let a =
        Annotation.find f.fun_codegen_options S.property fun_name f.fun_dbg
      in
      let t = create ppf fun_name future_funcnames unit_info a in
      let really_check () =
        let res = check_instr t f.fun_body in
        report t res ~msg:"finished" ~desc:"fundecl" f.fun_dbg;
        if (not t.keep_witnesses) && Value.is_resolved res
        then (
          let { Witnesses.nor; exn; div } = Value.get_witnesses res in
          assert (Witnesses.is_empty nor);
          assert (Witnesses.is_empty exn);
          assert (Witnesses.is_empty div));
        Unit_info.record unit_info fun_name res f.fun_dbg a;
        report_unit_info ppf unit_info ~msg:"after record"
      in
      let really_check () =
        if !Flambda_backend_flags.disable_checkmach
        then
          (* Do not analyze the body of the function, conservatively assume that
             the summary is top. *)
          Unit_info.record unit_info fun_name
            (Value.top Witnesses.empty)
            f.fun_dbg a
        else really_check ()
      in
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" f.fun_dbg;
        Unit_info.record unit_info fun_name expected_value f.fun_dbg None
      | None -> really_check ()
      | Some a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assert" ~desc:"fundecl" f.fun_dbg;
        (* Only keep witnesses for functions that need checking. *)
        really_check ()
    in
    Profile.record_call ~accumulate:true ("check " ^ analysis_name) check
end

(** Check that functions do not allocate on the heap (local allocations are ignored) *)
module Spec_zero_alloc : Spec = struct
  let property = Cmm.Zero_alloc

  let enabled () =
    (* Checkmach no longer distinguishes between opt and default checks. *)
    match !Clflags.zero_alloc_check with
    | No_check -> false
    | Check_default -> true
    | Check_all -> true
    | Check_opt_only -> true

  (* Compact the mapping from function name to Value.t to reduce size of Checks
     in cmx and memory consumption Compilenv. Different components have
     different frequencies of Top/Bot. The most likely value is encoded as None
     (i.e., not stored). *)
  let encode (v : V.t) =
    V.match_with v
      ~top:(fun _ -> 0)
      ~safe:1 ~bot:2
      ~unresolved:(fun () -> assert false)

  (* Witnesses are not used across functions and not stored in cmx. Witnesses
     that appear in a function's summary are only used for error messages about
     that function, not about its callers. Witnesses from the summary of a
     callee are ignored, and replaced by the name of the callee. *)
  let decoded_witness = Witnesses.empty

  let decode = function
    | 0 -> V.top decoded_witness
    | 1 -> V.safe
    | 2 -> V.bot
    | n -> Misc.fatal_errorf "Checkmach cannot decode %d" n

  let encode (v : Value.t) : Checks.value =
    let c = (encode v.div lsl 4) lor (encode v.exn lsl 2) lor encode v.nor in
    if c = 0 then None else Some c

  let decode : Checks.value -> Value.t = function
    | None -> Value.top decoded_witness
    | Some d ->
      if d = 0 then Misc.fatal_error "Checkmach unexpected 0 encoding";
      let nor = decode (d land 3) in
      let exn = decode ((d lsr 2) land 3) in
      let div = decode ((d lsr 4) land 3) in
      { nor; exn; div }

  let set_value s (v : Value.t) =
    let checks = (Compilenv.current_unit_infos ()).ui_checks in
    Checks.set_value checks s (encode v)

  let get_value_opt s =
    let checks = Compilenv.cached_checks in
    match Checks.get_value checks s with
    | None -> None
    | Some (c : Checks.value) -> Some (decode c)

  let transform_specific w s =
    (* Conservatively assume that operation can return normally. *)
    let nor = if Arch.operation_allocates s then V.top w else V.safe in
    let exn = if Arch.operation_can_raise s then nor else V.bot in
    (* Assume that the operation does not diverge. *)
    let div = V.bot in
    { Value.nor; exn; div }
end

module Check_zero_alloc = Analysis (Spec_zero_alloc)

(** Information about the current unit. *)
let unit_info = Unit_info.create ()

let fundecl ppf_dump ~future_funcnames fd =
  Check_zero_alloc.fundecl fd ~future_funcnames unit_info ppf_dump;
  fd

let reset_unit_info () = Unit_info.reset unit_info

let record_unit_info ppf_dump =
  Check_zero_alloc.record_unit unit_info ppf_dump;
  Compilenv.cache_checks (Compilenv.current_unit_infos ()).ui_checks

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit

let iter_witnesses f =
  Unit_info.iter unit_info ~f:(fun func_info ->
      f func_info.name
        (Value.get_witnesses func_info.value |> Witnesses.simplify))

let () = Location.register_error_of_exn Report.print

let is_check_enabled codegen_options fun_name dbg =
  Annotation.is_check_enabled codegen_options fun_name dbg
