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
    | Widen

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
    | Widen -> fprintf ppf "widen"

  let print ppf { kind; dbg } =
    Format.fprintf ppf "%a {%a}@," print_kind kind Debuginfo.print_compact dbg
end

let take_first_n t n ~to_seq ~of_seq ~cardinal =
  let len = cardinal t in
  if len <= n then t else t |> to_seq |> Seq.take n |> of_seq

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

  val size : t -> int

  val cutoff : t -> n:int -> t

  type components =
    { nor : t;
      exn : t;
      div : t
    }

  val simplify : components -> components
end = struct
  include Set.Make (Witness)

  let print ppf t = Format.pp_print_seq Witness.print ppf (to_seq t)

  let size t = cardinal t

  let cutoff t ~n = take_first_n t n ~to_seq ~of_seq ~cardinal

  let join t1 t2 =
    let res = union t1 t2 in
    match !Flambda_backend_flags.zero_alloc_checker_details_cutoff with
    | Keep_all -> res
    | No_details ->
      if not (is_empty res)
      then Misc.fatal_errorf "expected no witnesses got %a" print res;
      res
    | At_most n ->
      (* CR-someday gyorsh: current implementation is naive: first compute the
         union and then remove some elements. This can be optimized but should
         preserve the property that we keep the smallest [n] witnesses from both
         sets. This property makes user error messages more stable and
         independent of iteration order. *)
      cutoff res ~n

  let meet = inter

  let lessequal = subset

  let create kind dbg = singleton (Witness.create dbg kind)

  let iter t ~f = iter f t

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

  let compare : t -> t -> int = fun t1 t2 -> Stdlib.compare t1 t2

  let print = function N -> "nor" | E -> "exn" | D -> "div"
end

module Var : sig
  type t

  val create : string -> Tag.t -> t

  val name : t -> string

  val tag : t -> Tag.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val equal : t -> t -> bool

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
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
  module Set = Set.Make (T)

  let equal t1 t2 = compare t1 t2 = 0

  let name t = t.name

  let tag t = t.tag

  let create name tag = { name; tag }

  let print ppf { name; tag } =
    Format.fprintf ppf "%s.%s@ " name (Tag.print tag)
end

(** Abstract value for each component of the domain. *)
module V : sig
  type t

  (** order of the abstract domain  *)
  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val transform : t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val diff_witnesses : expected:t -> actual:t -> Witnesses.t

  val get_witnesses : t -> Witnesses.t

  val print : witnesses:bool -> Format.formatter -> t -> unit

  val unresolved : Witnesses.t -> Var.t -> t

  val is_resolved : t -> bool

  val get_unresolved_names : t -> String.Set.t

  val apply : t -> env:(Var.t -> t option) -> t

  val bot : t

  val safe : t

  val top : Witnesses.t -> t

  (** [compare] is structural on terms (for the use of [V.t] as a key in sets and maps),
      whereas [lessequal] is the order of the abstract domain (for fixed point checks). *)
  val compare : t -> t -> int

  val match_with :
    bot:'a ->
    safe:'a ->
    top:(Witnesses.t -> 'a) ->
    unresolved:(unit -> 'a) ->
    t ->
    'a
end = struct
  let pp_w ~witnesses ppf w =
    if witnesses then Format.fprintf ppf "@, (%a)" Witnesses.print w else ()

  let pp_var ~witnesses ppf var w =
    Format.fprintf ppf "(%a%a)@," Var.print var (pp_w ~witnesses) w

  let pp_top ~witnesses ppf w = Format.fprintf ppf "(top%a)" (pp_w ~witnesses) w

  (** Variables with witnesses *)
  module Vars : sig
    type t

    val empty : t

    val compare : t -> t -> int

    (** [same_vars] compares variables ignoring witnesses *)
    val same_vars : t -> t -> bool

    val get_vars : t -> Var.Set.t

    val get_unresolved_name : t -> String.Set.t

    val join : t -> t -> t

    val update : t -> Var.t -> Witnesses.t -> t

    val singleton : Var.t -> Witnesses.t -> t

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val print : witnesses:bool -> Format.formatter -> t -> unit

    val fold :
      f:(Var.t -> Witnesses.t -> 'acc -> 'acc) -> init:'acc -> t -> 'acc

    val exists : (Var.t -> Witnesses.t -> bool) -> t -> bool

    val cutoff : t -> int -> t

    val cutoff_witnesses : t -> int -> t
  end = struct
    type t = Witnesses.t Var.Map.t

    let empty = Var.Map.empty

    let fold ~f ~init t = Var.Map.fold f t init

    let singleton var w = Var.Map.singleton var w

    let compare t1 t2 = Var.Map.compare Witnesses.compare t1 t2

    let same_vars = Var.Map.equal (fun _ _ -> (* ignore witnesses *) true)

    let get_vars t =
      (* CR-someday gyorsh: If this is called often, it may be better to store
         the result of [get_vars t] in [t] instead of recomputing it. *)
      t |> Var.Map.to_seq |> Seq.map fst |> Var.Set.of_seq

    let get_unresolved_name t =
      t |> Var.Map.to_seq |> Seq.map fst |> Seq.map Var.name
      |> String.Set.of_seq

    let exists p t = Var.Map.exists p t

    let has_witnesses vars = exists (fun _ w -> not (Witnesses.is_empty w)) vars

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

    let cutoff_witnesses t n = Var.Map.map (Witnesses.cutoff ~n) t

    let cutoff t n =
      let t = cutoff_witnesses t n in
      take_first_n t n ~to_seq:Var.Map.to_seq ~of_seq:Var.Map.of_seq
        ~cardinal:Var.Map.cardinal
  end

  (** Normal form of Transform.
      [Transform] represents an abstract transformer of a primitive
      such as a function call. *)
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

    (** does not change the number of variables, only their witnesses.  *)
    val cutoff_witnesses : t -> n:int -> t
  end = struct
    type t =
      | Args of Vars.t
      | Args_with_top of
          { w : Witnesses.t;
            vars : Vars.t
          }

    (* The binary "transform" is commutative and associative, so a nested
       "transform" can be flattened in the normal form. The arguments are
       represented as a set of variables and optionally top. if top is absent,
       [vars] must contain at least two elements. if top is present, [vars] must
       have at least one element. This is enforced by the available
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

    let cutoff_witnesses t ~n =
      match t with
      | Args vars -> Args (Vars.cutoff_witnesses vars n)
      | Args_with_top { w; vars } ->
        Args_with_top
          { w = Witnesses.cutoff w ~n; vars = Vars.cutoff_witnesses vars n }

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

  module Transforms : sig
    type t

    val join : t -> t -> t

    val empty : t

    val add : Transform.t -> t -> t

    val compare : t -> t -> int

    val cutoff : t -> int -> t

    val iter : (Transform.t -> unit) -> t -> unit

    val map : (Transform.t -> Transform.t) -> t -> t

    val fold : (Transform.t -> 'a -> 'a) -> t -> 'a -> 'a

    val exists : (Transform.t -> bool) -> t -> bool

    val get_unresolved_names : t -> String.Set.t

    exception Widen

    exception Widen_with_witness
  end = struct
    (* Join of two Transform with the same set of vars: merged both sets of Vars
       into one Transform in normal form, without loss of precision or
       witnesses, even if one Transform has "Top" and the other does not.

       Naive implementation: map with key of type [Var.Set.t] to data of type
       [Transform.t] *)
    module M = Map.Make (Var.Set)

    type t = Transform.t M.t

    exception Widen

    exception Widen_with_witness

    let maybe_widen t =
      match !Flambda_backend_flags.zero_alloc_checker_join with
      | Keep_all -> t
      | Widen n ->
        if M.cardinal t > n
        then
          if M.exists (fun _var tr -> Transform.has_witnesses tr) t
          then raise Widen_with_witness
          else raise Widen
        else t
      | Error n ->
        if M.cardinal t > n
        then
          Misc.fatal_errorf
            "Join with %d paths is too big, use \
             -disable-precise-zero-alloc-checker"
            (M.cardinal t)
        else t

    let empty = M.empty

    let get_key tr = tr |> Transform.get_vars |> Vars.get_vars

    let get_unresolved_names t =
      (* collect the keys *)
      t |> M.to_seq |> Seq.map fst |> Seq.map Var.Set.to_seq |> Seq.concat
      |> Seq.map Var.name |> String.Set.of_seq

    let add tr t =
      let res = M.add (get_key tr) tr t in
      maybe_widen res

    let compare t1 t2 = M.compare Transform.compare t1 t2

    let iter f t = M.iter (fun _key tr -> f tr) t

    let fold f t init = M.fold (fun _key tr acc -> f tr acc) t init

    let exists f t = M.exists (fun _key tr -> f tr) t

    let join t1 t2 =
      let res =
        M.union (fun _var tr1 tr2 -> Some (Transform.flatten tr1 tr2)) t1 t2
      in
      maybe_widen res

    let map f t = M.fold (fun _key tr acc -> add (f tr) acc) t M.empty

    let cutoff t n =
      let t = map (Transform.cutoff_witnesses ~n) t in
      take_first_n t n ~to_seq:M.to_seq ~of_seq:M.of_seq ~cardinal:M.cardinal
  end

  (* CR-someday gyorsh: treatment of vars and top is duplicated between Args and
     Transform, is there a nice way to factor it out?

     For instance, Join.t could be defined as a record { args : Args.t; ... }
     with the ellipsis encoding top/safe. It may simplify a couple of functions
     in the Join module, and perhaps lead to a type like { args : Args.t; rest :
     'a; } that could then be used in other modules such as Transform. *)

  (** helper for Join  *)
  module Args : sig
    type t

    val add_tr : t -> Transform.t -> t

    val add_var : t -> Var.t -> Witnesses.t -> t

    val empty : t

    val join : t -> t -> t

    val get : t -> Vars.t * Transforms.t

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

    val cutoff : t -> int -> t

    val compare : t -> t -> int

    val print : witnesses:bool -> Format.formatter -> t -> unit
  end = struct
    type t =
      { vars : Vars.t;
        trs : Transforms.t
      }

    let empty = { vars = Vars.empty; trs = Transforms.empty }

    let get { vars; trs } = vars, trs

    let print ~witnesses ppf { vars; trs } =
      let pp_trs ppf trs =
        Transforms.iter (Transform.print ~witnesses ppf) trs
      in
      Format.fprintf ppf "vars=(%a)@.transforms=(%a)@," (Vars.print ~witnesses)
        vars pp_trs trs

    let add_var t var witnesses =
      (* Optimization to avoid allocation when the content hasn't changed. *)
      let vars = Vars.update t.vars var witnesses in
      if vars == t.vars then t else { t with vars }

    let add_tr t tr =
      let trs = Transforms.add tr t.trs in
      if trs == t.trs then t else { t with trs }

    let join ({ vars = v1; trs = trs1 } as t) ({ vars = v2; trs = trs2 } as t')
        =
      if debug
      then
        Format.fprintf Format.std_formatter "join@.%a@. %a@."
          (print ~witnesses:true) t (print ~witnesses:true) t';
      let vars = Vars.join v1 v2 in
      let trs = Transforms.join trs1 trs2 in
      { vars; trs }

    let transform { vars; trs } tr =
      let from_vars =
        (* add each x from [vars] to [tr] *)
        Vars.fold
          ~f:(fun var w acc -> Transforms.add (Transform.add_var tr var w) acc)
          vars ~init:Transforms.empty
      in
      let from_trs = Transforms.map (fun tr' -> Transform.flatten tr tr') trs in
      { vars = Vars.empty; trs = Transforms.join from_vars from_trs }

    let transform_top { vars; trs } w =
      let from_vars =
        Vars.fold
          ~f:(fun var var_witnesses acc ->
            Transforms.add (Transform.var_with_top var ~var_witnesses w) acc)
          vars ~init:Transforms.empty
      in
      let from_trs = Transforms.map (fun tr -> Transform.add_top tr w) trs in
      { vars = Vars.empty; trs = Transforms.join from_vars from_trs }

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
        Transforms.map (fun tr -> Transform.add_var tr var witnesses) trs
      in
      { acc with trs = Transforms.join from_trs acc.trs }

    let transform_join t ({ vars; trs } as t') =
      if debug
      then
        Format.fprintf Format.std_formatter "transform_join@.%a@. %a@."
          (print ~witnesses:true) t (print ~witnesses:true) t';
      let acc =
        Vars.fold vars ~init:empty ~f:(fun var witnesses acc ->
            join acc (transform_var t var witnesses))
      in
      Transforms.fold (fun tr acc -> join acc (transform t tr)) trs acc

    let has_witnesses { vars; trs } =
      Vars.has_witnesses vars || Transforms.exists Transform.has_witnesses trs

    let replace_witnesses { vars; trs } w =
      { vars = Vars.replace_witnesses vars w;
        trs = Transforms.map (fun tr -> Transform.replace_witnesses tr w) trs
      }

    let cutoff { vars; trs } n =
      { vars = Vars.cutoff vars n; trs = Transforms.cutoff trs n }

    let compare { vars = vars1; trs = trs1 } { vars = vars2; trs = trs2 } =
      let c = Vars.compare vars1 vars2 in
      if c <> 0 then c else Transforms.compare trs1 trs2
  end

  (** normal form of join *)
  module Join : sig
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

    val flatten : t -> t -> unit -> t

    val distribute_transform_over_join : t -> Transform.t -> unit -> t

    val distribute_transform_var_over_join :
      t -> Var.t -> Witnesses.t -> unit -> t

    val distribute_transform_top_over_join : t -> Witnesses.t -> unit -> t

    val distribute_transform_over_joins : t -> t -> unit -> t

    val get_top : t -> Witnesses.t option

    val has_safe : t -> bool

    val get : t -> Vars.t * Transforms.t

    val print : witnesses:bool -> Format.formatter -> t -> unit

    val has_witnesses : t -> bool

    val replace_witnesses : t -> Witnesses.t -> t

    val compare : t -> t -> int

    val get_unresolved_names : t -> String.Set.t
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

    (* [cutoff] is a heuristic to limit the size of the representation when
       tracking witnesses. Adding [@zero_alloc] assert on a function has a
       limited overhead on compilation time and memory.

       The representation may be resolved to more than [n] witnesses (e.g.,
       different variables from Args.vars and Args.trs) or less than [n]
       witnesses (when some of the variables are not resolved to Top and
       therefore don't contribute witnesses).

       Cutoff for "join with top" does not affect the precision of generated
       summaries, only the number of witnesses reported.

       Termination of fixpoint: [Args] is sorted, [Arg.cutoff] keeps the least
       [n], and [Arg.join] is used for fixpoint computation before applying the
       cutoff. *)
    let args_with_top w args =
      if not (Args.has_witnesses args)
      then
        Misc.fatal_errorf "Join Top without witnesses in args:%a"
          (Args.print ~witnesses:false)
          args;
      match !Flambda_backend_flags.zero_alloc_checker_details_cutoff with
      | Keep_all -> Args_with_top { w; args }
      | No_details ->
        Misc.fatal_errorf "unexpected: (Join (Top %a) %a) " Witnesses.print w
          (Args.print ~witnesses:true)
          args
      | At_most n ->
        (* Normal form after cutoff: args contains at least one element even
           after cutoff because [n] is guaranteed to be positive. *)
        let len = Witnesses.size w in
        if len > n
        then
          Misc.fatal_errorf "expected at most %d witnesses, got %d: %a" n len
            Witnesses.print w;
        Args_with_top { w; args = Args.cutoff args n }

    let tr_with_safe tr = Args_with_safe Args.(add_tr empty tr)

    let tr_with_top tr w = args_with_top w Args.(add_tr empty tr)

    let var_with_top var ~var_witnesses w =
      args_with_top w Args.(add_var empty var var_witnesses)

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
        args_with_top (Witnesses.join w witnesses) args
      | Args_with_safe args | Args args -> args_with_top witnesses args

    let add_var t var witnesses =
      match t with
      | Args_with_safe args -> Args_with_safe (Args.add_var args var witnesses)
      | Args args -> Args (Args.add_var args var witnesses)
      | Args_with_top { w; args } ->
        args_with_top w (Args.add_var args var witnesses)

    let flatten t1 t2 () =
      match t1, t2 with
      | Args a1, Args a2 -> Args (Args.join a1 a2)
      | Args_with_safe a1, Args_with_safe a2 -> Args_with_safe (Args.join a1 a2)
      | Args_with_top { w; args = a1 }, (Args a2 | Args_with_safe a2)
      | (Args a2 | Args_with_safe a2), Args_with_top { w; args = a1 } ->
        args_with_top w (Args.join a1 a2)
      | Args_with_top { w = w1; args = a1 }, Args_with_top { w = w2; args = a2 }
        ->
        args_with_top (Witnesses.join w1 w2) (Args.join a1 a2)
      | Args args1, Args_with_safe args2 | Args_with_safe args1, Args args2 ->
        Args_with_safe (Args.join args1 args2)

    let distribute_transform_over_join t tr () =
      match t with
      | Args_with_safe args ->
        let args = Args.(add_tr (transform args tr) tr) in
        Args args
      | Args_with_top { w; args } ->
        let tr' = Transform.add_top tr w in
        let args = Args.(add_tr (transform args tr) tr') in
        Args args
      | Args args -> Args (Args.transform args tr)

    let distribute_transform_var_over_join t var witnesses () =
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

    let distribute_transform_top_over_join t w () =
      match t with
      | Args_with_safe args ->
        let args = Args.transform_top args w in
        args_with_top w args
      | Args_with_top { w = w'; args } ->
        let args = Args.(transform_top args w) in
        args_with_top (Witnesses.join w' w) args
      | Args args -> Args (Args.transform_top args w)

    let distribute_transform_over_joins t1 t2 () =
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
        args_with_top (Witnesses.join w1 w2) (Args.join new_args args_top)
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
        args_with_top w args

    let add_tr t tr =
      match t with
      | Args_with_safe args -> Args_with_safe (Args.add_tr args tr)
      | Args args -> Args (Args.add_tr args tr)
      | Args_with_top { w; args } -> args_with_top w (Args.add_tr args tr)

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
      | Args_with_top { w = _; args } ->
        args_with_top witnesses (Args.replace_witnesses args witnesses)

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

    let get_unresolved_names t =
      match t with
      | Args_with_safe args | Args_with_top { w = _; args } | Args args ->
        let vars, trs = Args.get args in
        String.Set.union
          (Vars.get_unresolved_name vars)
          (Transforms.get_unresolved_names trs)

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

  let get_unresolved_names t =
    match t with
    | Bot | Safe | Top _ -> String.Set.empty
    | Var { var; _ } -> String.Set.singleton (Var.name var)
    | Transform tr -> Transform.get_vars tr |> Vars.get_unresolved_name
    | Join j -> Join.get_unresolved_names j

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

  let widen_witness = Witnesses.create Witness.Widen Debuginfo.none

  let bounded_join f =
    try Join (f ()) with
    | Transforms.Widen -> Top Witnesses.empty
    | Transforms.Widen_with_witness -> Top widen_witness

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
         summaries cannot be simplified as much. Finding out if there are
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
    | Join j1, Join j2 -> bounded_join (Join.flatten j1 j2)

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
     "inlines" and "specializes" join: efficiently handle definitive cases and
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
      bounded_join (Join.distribute_transform_over_join j tr)
    | (Top w as top), Join j | Join j, (Top w as top) ->
      if Join.has_safe j && not (Join.has_witnesses j)
      then top
      else bounded_join (Join.distribute_transform_top_over_join j w)
    | Var { var; witnesses }, Join j | Join j, Var { var; witnesses } ->
      bounded_join (Join.distribute_transform_var_over_join j var witnesses)
    | Join j1, Join j2 ->
      bounded_join (Join.distribute_transform_over_joins j1 j2)

  (* CR-soon xclerc for gyorsh: It may be valuable to gather the elements about
     the "constructors" (e.g. join, transform above) in one place, with the
     theoretical properties (e.g. neutral or absorbing elements, distribution),
     while keeping the comments about implementation choices and/or imperatives
     (e.g. why/how witnesses are tracked) next to the code. *)

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

  let meet t1 t2 =
    match t1, t2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.meet w1 w2)
    | Safe, Bot | Bot, Safe -> Bot
    | Top _, Bot | Bot, Top _ -> Bot
    | Top _, Safe | Safe, Top _ -> Safe
    | (Var _ | Transform _ | Join _), _ | _, (Var _ | Transform _ | Join _) ->
      Misc.fatal_error
        "Meet of unresolved is not implemented and shouldn't be needed."

  let apply t ~env =
    let get env var w =
      match env var with
      | None -> unresolved w var
      | Some v -> replace_witnesses w v
    in
    let contains_any env vars =
      Vars.exists (fun var _w -> Option.is_some (env var)) vars
    in
    (* CR-someday gyorsh: This is a naive implementation that reallocates the
       entire [t] whenever there is a change. Can be optimized to update the
       underlying sets and maps instead of using [fold]. *)
    let rec aux t =
      match t with
      | Bot | Safe | Top _ -> t
      | Var { var; witnesses } -> (
        match env var with None -> t | Some v -> replace_witnesses witnesses v)
      | Transform tr ->
        let vars = Transform.get_vars tr in
        if not (contains_any env vars)
        then t
        else
          let init =
            match Transform.get_top tr with None -> Safe | Some w -> Top w
          in
          Vars.fold
            ~f:(fun var w acc -> transform (get env var w) acc)
            ~init vars
      | Join j ->
        let vars, trs = Join.get j in
        if (not (contains_any env vars))
           && not
                (Transforms.exists
                   (fun tr -> contains_any env (Transform.get_vars tr))
                   trs)
        then t
        else
          let init =
            match Join.get_top j with
            | None -> if Join.has_safe j then Safe else Bot
            | Some w -> Top w
          in
          let acc =
            Vars.fold ~f:(fun var w acc -> join (get env var w) acc) ~init vars
          in
          Transforms.fold
            (fun tr acc ->
              let t = Transform tr in
              join (aux t) acc)
            trs acc
    in
    aux t
end

module T = Zero_alloc_utils.Make_value (Witnesses) (V)

module Value : sig
  include module type of T

  val transform : V.t -> t -> t

  val replace_witnesses : Witnesses.t -> t -> t

  val get_unresolved_names : t -> String.Set.t

  val get_witnesses : t -> Witnesses.components

  val diff_witnesses : expected:t -> actual:t -> Witnesses.components

  val unresolved : string -> Witnesses.t -> t

  val is_resolved : t -> bool

  val get_component : t -> Tag.t -> V.t

  val apply : t -> (Var.t -> V.t option) -> t
end = struct
  include T

  let unresolved name w =
    { nor = Var.create name Tag.N |> V.unresolved w;
      exn = Var.create name Tag.E |> V.unresolved w;
      div = Var.create name Tag.D |> V.unresolved w
    }

  let is_resolved t =
    V.is_resolved t.nor && V.is_resolved t.exn && V.is_resolved t.div

  let get_unresolved_names t =
    String.Set.(
      union
        (V.get_unresolved_names t.nor)
        (union (V.get_unresolved_names t.exn) (V.get_unresolved_names t.div)))

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

  val find : Cmm.codegen_option list -> string -> Debuginfo.t -> t option

  val of_cfg : Cfg.codegen_option list -> string -> Debuginfo.t -> t option

  val expected_value : t -> Value.t

  (** [valid t value] returns true if and only if the [value] satisfies the annotation,
      i.e., [value] is less or equal to [expected_value a] when ignoring witnesses. *)

  val valid : t -> Value.t -> bool

  val diff_witnesses : t -> Value.t -> Witnesses.components

  val is_assume : t -> bool

  val is_strict : t -> bool

  val is_check_enabled : t option -> bool
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
      never_raises : bool;
      loc : Location.t
          (** Source location of the annotation, used for error messages. *)
    }

  let get_loc t = t.loc

  let expected_value
      { strict; never_returns_normally; never_raises; assume = _; loc = _ } =
    Value.of_annotation ~strict ~never_returns_normally ~never_raises

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

  let find codegen_options fun_name dbg =
    let a =
      List.filter_map
        (fun (c : Cmm.codegen_option) ->
          match c with
          | Check_zero_alloc { strict; loc } ->
            Some
              { strict;
                assume = false;
                never_returns_normally = false;
                never_raises = false;
                loc
              }
          | Assume_zero_alloc
              { strict; never_returns_normally; never_raises; loc } ->
            Some
              { strict;
                assume = true;
                never_returns_normally;
                never_raises;
                loc
              }
          | Reduce_code_size | No_CSE | Use_linscan_regalloc -> None)
        codegen_options
    in
    match a with
    | [] -> None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a for %s"
        Debuginfo.print_compact dbg fun_name ()

  let of_cfg codegen_options fun_name dbg =
    let a =
      List.filter_map
        (fun (c : Cfg.codegen_option) ->
          match c with
          | Check_zero_alloc { strict; loc } ->
            Some
              { strict;
                assume = false;
                never_returns_normally = false;
                never_raises = false;
                loc
              }
          | Assume_zero_alloc
              { strict; never_returns_normally; never_raises; loc } ->
            Some
              { strict;
                assume = true;
                never_returns_normally;
                never_raises;
                loc
              }
          | Reduce_code_size | No_CSE -> None)
        codegen_options
    in
    match a with
    | [] -> None
    | [p] -> Some p
    | _ :: _ ->
      Misc.fatal_errorf "Unexpected duplicate annotation %a for %s"
        Debuginfo.print_compact dbg fun_name ()

  let is_check_enabled t =
    match t with None -> false | Some { assume; _ } -> not assume
end

module Metadata : sig
  (* CR-someday gyorsh: propagate assert of arbitrary expressions. *)
  val assume_value :
    Debuginfo.t -> can_raise:bool -> Witnesses.t -> Value.t option
end = struct
  (* CR gyorsh: The return type of [Assume_info.get_value] is
     [Assume_info.Value.t]. It is not the same as [Zero_alloc_checker.Value.t],
     because [Witnesses] in [Zero_alloc_checker] depend on Debuginfo and cannot
     be used in Assume_info due to cyclic dependencies. The witnesses in
     Assume_info are always empty and the translation is trivial. Is there a
     better way to avoid duplicating [Zero_alloc_utils]? *)
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

  exception Fail of t list

  val print : exn -> Location.error option
end = struct
  type t =
    { a : Annotation.t;
      fun_name : string;
      fun_dbg : Debuginfo.t;
      witnesses : Witnesses.components
    }

  exception Fail of t list

  let annotation_error t =
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
      Format.fprintf ppf
        "Annotation check for zero_alloc%s failed on function %s (%s)"
        (if Annotation.is_strict t.a then " strict" else "")
        scoped_name t.fun_name
    in
    Location.error_of_printer ~loc print_annotated_fun ()

  let error_messages t : Location.error list =
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
           zero_alloc_checker is before comballoc. In the future, this may be
           done in the middle-end. *)
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
        | Widen ->
          ( Format.dprintf
              "details are not available. This may be a false alarm due to \
               conservative analysis.\n\
               Hint: for more precise results, recompile this function with\n\
               \"-function-layout topological\" or \"-zero-alloc-checker-join \
               0\" flags.\n\
               The \"-zero-alloc-checker-join 0\" flag may substantially \
               increase compilation time.\n\
               (widenning applied in function %s%s)" t.fun_name component_msg,
            [] )
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
      let dbg = if Debuginfo.is_none w.dbg then t.fun_dbg else w.dbg in
      let loc = Debuginfo.to_location dbg in
      let pp ppf () =
        print_main_msg ppf;
        pp_inlined_dbg ppf dbg
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
      match !Flambda_backend_flags.zero_alloc_checker_details_cutoff with
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
    annotation_error t :: details

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
    | Fail reports ->
      (* Sort by function's location. If debuginfo is missing, keep sorted by
         function name. *)
      let compare t1 t2 =
        let c = Debuginfo.compare t1.fun_dbg t2.fun_dbg in
        if not (Int.equal c 0)
        then c
        else String.compare t1.fun_name t2.fun_name
      in
      reports |> List.stable_sort compare
      |> List.concat_map error_messages
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

  val find_exn : t -> string -> Func_info.t

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

  let find_exn t name = String.Tbl.find t name

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

module Unresolved_dependencies : sig
  type t

  val create : unit -> t

  val reset : t -> unit

  val is_empty : t -> bool

  val contains : callee:string -> t -> bool

  val get_callers : callee:string -> t -> String.Set.t

  (** removes all association of the [callee] with its direct callers. [callee] must exist
      and must not be associated with any callees of its own in [t]. *)
  val remove : callee:string -> t -> unit

  (** adds a new caller. *)
  val add : t -> caller:string -> callees:String.Set.t -> unit

  (** Ensure [caller] exists and is already associated with [callees], and remove
      association of [caller] with any other callees.  *)
  val update : t -> caller:string -> callees:String.Set.t -> unit
end = struct
  (** reverse dependencies: map from an unresolved callee to all its callers  *)
  type t = String.Set.t String.Tbl.t

  let create () = String.Tbl.create 2

  let reset t = String.Tbl.reset t

  let is_empty t = String.Tbl.length t = 0

  let contains ~callee t = String.Tbl.mem t callee

  let get_callers ~callee t = String.Tbl.find t callee

  let remove ~callee t =
    assert (contains ~callee t);
    String.Tbl.iter
      (fun _ callers -> assert (not (String.Set.mem callee callers)))
      t;
    String.Tbl.remove t callee

  let add_one t ~caller callee =
    let callers =
      match String.Tbl.find_opt t callee with
      | None -> String.Set.singleton caller
      | Some callers ->
        assert (not (String.Set.mem caller callers));
        String.Set.add caller callers
    in
    String.Tbl.replace t callee callers

  let add t ~caller ~callees = String.Set.iter (add_one t ~caller) callees

  let update t ~caller ~callees =
    (* check that all callees are present *)
    String.Set.iter
      (fun callee ->
        match String.Tbl.find_opt t callee with
        | None ->
          Misc.fatal_errorf "Unresolved dependencies: missing callee %s of %s"
            callee caller
        | Some callers ->
          if not (String.Set.mem caller callers)
          then
            Misc.fatal_errorf
              "Unresolved_dependencies: missing caller %s for callee %s in \
               unresolved "
              caller callee)
      callees;
    (* remove resolved callees of this caller *)
    let remove callee callers =
      if String.Set.mem callee callees
      then Some callers
      else
        let new_callers = String.Set.remove caller callers in
        if String.Set.is_empty new_callers then None else Some new_callers
    in
    String.Tbl.filter_map_inplace remove t
end

module Compilenv_utils : sig
  (** [get_value_opt f] returns the value recorded for function [f] in [Compilenv],
      either because the check passed or because of user-defined "assume" annotation.
      If [f] was compiled with checks disabled, returns None.
  *)
  val get_value_opt : string -> Value.t option

  (** [set_value f v] records the value of the function named [f] in [Compilenv]. *)
  val set_value : string -> Value.t -> unit
end = struct
  (* Compact the mapping from function name to Value.t to reduce size of
     Zero_alloc_info in cmx and memory consumption Compilenv. Different
     components have different frequencies of Top/Bot. The most likely value is
     encoded as None (i.e., not stored). *)
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
    | n -> Misc.fatal_errorf "Zero_alloc_checker cannot decode %d" n

  let encode (v : Value.t) : Zero_alloc_info.value option =
    let c = (encode v.div lsl 4) lor (encode v.exn lsl 2) lor encode v.nor in
    if c = 0 then None else Some c

  let decode : Zero_alloc_info.value option -> Value.t = function
    | None -> Value.top decoded_witness
    | Some d ->
      if d = 0 then Misc.fatal_error "Zero_alloc_checker unexpected 0 encoding";
      let nor = decode (d land 3) in
      let exn = decode ((d lsr 2) land 3) in
      let div = decode ((d lsr 4) land 3) in
      { nor; exn; div }

  let set_value s (v : Value.t) =
    let info = (Compilenv.current_unit_infos ()).ui_zero_alloc_info in
    match encode v with
    | None -> ()
    | Some i -> Zero_alloc_info.set_value info s i

  let get_value_opt s =
    let info = Compilenv.cached_zero_alloc_info in
    Some (decode (Zero_alloc_info.get_value info s))
end

(** The analysis involved some fixed point computations.
    Termination: [Value.t] is a finite height domain and
    [transfer] is a monotone function w.r.t. [Value.lessequal] order.
*)
module Analysis : sig
  (** Check one function. *)
  val fundecl :
    Mach.fundecl ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Unresolved_dependencies.t ->
    Format.formatter ->
    unit

  val cfg :
    Cfg.t ->
    future_funcnames:String.Set.t ->
    Unit_info.t ->
    Unresolved_dependencies.t ->
    Format.formatter ->
    unit

  (** Resolve all function summaries, check them against user-provided assertions,
      and record the summaries in Compilenv to be saved in .cmx files *)
  val record_unit :
    Unit_info.t -> Unresolved_dependencies.t -> Format.formatter -> unit
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
    match !Flambda_backend_flags.zero_alloc_checker_details_cutoff with
    | Keep_all -> true
    | No_details -> false
    | At_most _ -> keep

  let create ppf current_fun_name future_funcnames unit_info annot =
    let keep_witnesses = should_keep_witnesses (Option.is_some annot) in
    { ppf; current_fun_name; future_funcnames; unit_info; keep_witnesses }

  let analysis_name = "zero_alloc"

  (** Is the checking of "assert" annotations enabled? *)
  let enabled () =
    (* Zero_alloc_checker no longer distinguishes between opt and default
       checks. *)
    match !Clflags.zero_alloc_check with
    | No_check -> false
    | Check_default -> true
    | Check_all -> true
    | Check_opt_only -> true

  let report' ppf v ~current_fun_name ~msg ~desc dbg =
    if !Flambda_backend_flags.dump_zero_alloc
    then
      Format.fprintf ppf "*** check %s %s in %s: %s with %a (%a)\n"
        analysis_name msg current_fun_name desc
        (Value.print ~witnesses:true)
        v Debuginfo.print_compact dbg

  let report t v ~msg ~desc dbg =
    report' t.ppf v ~msg ~desc ~current_fun_name:t.current_fun_name dbg

  let is_future_funcname t callee = String.Set.mem callee t.future_funcnames

  let report_unit_info ppf unit_info ~msg =
    if !Flambda_backend_flags.dump_zero_alloc
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Unit_info.iter unit_info ~f:(Func_info.print ~witnesses:true ppf ~msg)

  let report_func_info ~msg ppf func_info =
    if !Flambda_backend_flags.dump_zero_alloc
    then
      let msg = Printf.sprintf "%s %s:" analysis_name msg in
      Func_info.print ~witnesses:true ppf ~msg func_info

  let check_and_save_unit_info ppf unit_info =
    let errors = ref [] in
    let record (func_info : Func_info.t) =
      (match func_info.annotation with
      | None -> ()
      | Some a ->
        Builtin_attributes.mark_zero_alloc_attribute_checked analysis_name
          (Annotation.get_loc a);
        if (not (Annotation.is_assume a))
           && enabled ()
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
      Compilenv_utils.set_value func_info.name func_info.value
    in
    Unit_info.iter unit_info ~f:record;
    match !errors with [] -> () | errors -> raise (Report.Fail errors)

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
      if !Flambda_backend_flags.disable_precise_zero_alloc_checker
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
        match Compilenv_utils.get_value_opt callee with
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
    report t effect ~msg:"transform effect" ~desc dbg;
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
    let v = find_callee t callee ~desc dbg w in
    let effect =
      match Metadata.assume_value dbg ~can_raise:true w with
      | Some v' ->
        assert (Value.is_resolved v');
        if Value.is_resolved v then Value.meet v v' else v'
      | None -> v
    in
    transform t ~next ~exn ~effect desc dbg

  (** Summary of target specific operations. *)
  let transform_specific t s ~next ~exn dbg =
    let desc = "Arch.specific_operation" in
    report t next ~msg:"transform_specific next" ~desc dbg;
    report t exn ~msg:"transform_specific exn" ~desc dbg;
    let can_raise = Arch.operation_can_raise s in
    let effect =
      let w = create_witnesses t Arch_specific dbg in
      match Metadata.assume_value dbg ~can_raise w with
      | Some v -> v
      | None ->
        (* Conservatively assume that operation can return normally. *)
        let nor = if Arch.operation_allocates s then V.top w else V.safe in
        let exn = if Arch.operation_can_raise s then nor else V.bot in
        (* Assume that the operation does not diverge. *)
        let div = V.bot in
        { Value.nor; exn; div }
    in
    transform t ~next ~exn ~effect desc dbg

  let transform_operation t (op : Mach.operation) ~next ~exn dbg =
    match op with
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
    | Iconst_float32 _ | Iconst_symbol _ | Iconst_vec128 _ | Iload _
    | Ifloatop _
    | Ireinterpret_cast
        ( Float32_of_float | Float_of_float32 | Float_of_int64 | Int64_of_float
        | Float32_of_int32 | Int32_of_float32 | V128_of_v128 )
    | Istatic_cast
        ( Float_of_int _ | Int_of_float _ | Float_of_float32 | Float32_of_float
        | Scalar_of_v128 _ | V128_of_scalar _ )
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
    | Iname_for_debugger _ | Ireinterpret_cast (Int_of_value | Value_of_int) ->
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
    | Ispecific s -> transform_specific t s ~next ~exn dbg
    | Idls_get -> next

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
      | Iifthenelse _ ->
        report t next ~msg:"transform" ~desc:"ifthenelse" i.dbg;
        next
      | Iswitch _ ->
        report t next ~msg:"transform" ~desc:"switch" i.dbg;
        next
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

    val singleton : Func_info.t -> Value.t -> t

    val add : Func_info.t -> Value.t -> t -> t

    val get_value_exn : string -> t -> Value.t

    val iter : t -> f:(Func_info.t -> Value.t -> unit) -> unit

    val map : t -> f:(Func_info.t -> Value.t -> Value.t) -> t

    val print : msg:string -> Format.formatter -> t -> unit

    (** initialize [env] with Bot for all functions on normal and exceptional
       return, and Safe for diverge component conservatively. *)
    val init_val : Value.t
  end = struct
    type data =
      { func_info : Func_info.t;
        approx : Value.t
      }

    type t = data String.Map.t

    let empty = String.Map.empty

    let add (func_info : Func_info.t) approx t =
      assert (Value.is_resolved approx);
      let d = { func_info; approx } in
      String.Map.add func_info.name d t

    let singleton (func_info : Func_info.t) approx = add func_info approx empty

    let get_value_exn name t =
      let d = String.Map.find name t in
      d.approx

    let map t ~f =
      String.Map.map
        (fun d ->
          let approx = f d.func_info d.approx in
          assert (Value.is_resolved approx);
          { d with approx })
        t

    let iter t ~f = String.Map.iter (fun _name d -> f d.func_info d.approx) t

    let print ~msg ppf t =
      if !Flambda_backend_flags.dump_zero_alloc
      then
        iter t ~f:(fun func_info approx ->
            Format.fprintf ppf "Env %s: %s: %a@." msg func_info.name
              (Value.print ~witnesses:true)
              approx)

    let init_val = Value.diverges
  end

  (* CR gyorsh: do we need join in the fixpoint computation or is the function
     body analysis/summary already monotone? *)
  let fixpoint ppf init_env =
    (* CR gyorsh: this is a really dumb iteration strategy. *)
    let lookup env var =
      let v = Env.get_value_exn (Var.name var) env in
      Some (Value.get_component v (Var.tag var))
    in
    let rec loop env =
      Env.print ~msg:"computing fixpoint" ppf env;
      let changed = ref false in
      let env' =
        Env.map
          ~f:(fun func_info v ->
            let v' = Value.apply func_info.value (lookup env) in
            if !Flambda_backend_flags.dump_zero_alloc
            then
              Format.fprintf ppf "fixpoint after apply: %s %a@." func_info.name
                (Value.print ~witnesses:true)
                v';
            assert (Value.is_resolved v');
            if not (Value.lessequal v' v)
            then (
              changed := true;
              if !Flambda_backend_flags.dump_zero_alloc
              then
                Format.fprintf ppf "fixpoint update: %s %a@." func_info.name
                  (Value.print ~witnesses:true)
                  v');
            Value.join v v')
          env
      in
      if !changed then loop env' else env'
    in
    let env = loop init_env in
    Env.iter env ~f:(fun func_info v ->
        assert (Value.is_resolved v);
        Func_info.update func_info v);
    Env.print ~msg:"after fixpoint" ppf env

  let record_unit unit_info unresolved_deps ppf =
    Profile.record_call ~accumulate:true ("record_unit " ^ analysis_name)
      (fun () ->
        report_unit_info ppf unit_info ~msg:"before fixpoint";
        let found_unresolved = ref false in
        let init_env =
          Unit_info.fold unit_info ~init:Env.empty ~f:(fun func_info env ->
              let v =
                if Value.is_resolved func_info.value
                then func_info.value
                else (
                  found_unresolved := true;
                  Env.init_val)
              in
              Env.add func_info v env)
        in
        if !found_unresolved
        then (
          fixpoint ppf init_env;
          report_unit_info ppf unit_info ~msg:"after fixpoint")
        else assert (Unresolved_dependencies.is_empty unresolved_deps);
        check_and_save_unit_info ppf unit_info)

  (* [fixpoint_self_rec] tries to resolve the common case of self-recursive
     functions with no other unresolved dependencies, instead of waiting until
     the end of the compilation unit to compute its fixpoint. Return the
     unresolved callees of [func_info]. *)
  let fixpoint_self_rec (func_info : Func_info.t) ppf =
    let unresolved_callees = Value.get_unresolved_names func_info.value in
    if String.Set.is_empty unresolved_callees
       || not
            (String.Set.equal unresolved_callees
               (String.Set.singleton func_info.name))
    then unresolved_callees
    else
      let init_env = Env.singleton func_info Env.init_val in
      fixpoint ppf init_env;
      report_func_info ~msg:"after self-rec fixpoint" ppf func_info;
      String.Set.empty

  (* [propagate] applies resolved values to transitive dependencies, but does
     not resolve mutually recursive loops. *)
  let rec propagate ~callee unit_info unresolved_deps ppf =
    let callee_info = Unit_info.find_exn unit_info callee in
    if Value.is_resolved callee_info.value
       && Unresolved_dependencies.contains ~callee unresolved_deps
    then (
      let callers =
        Unresolved_dependencies.get_callers ~callee unresolved_deps
      in
      assert (not (String.Set.is_empty callers));
      Unresolved_dependencies.remove ~callee unresolved_deps;
      let lookup var =
        if String.equal (Var.name var) callee
        then Some (Value.get_component callee_info.value (Var.tag var))
        else None
      in
      (* To avoid problems due to cyclic dependencies and sharing, first resolve
         everything that directly depends on [callee_info] and update
         dependencies, then propagate recursively. *)
      String.Set.iter
        (fun caller ->
          let caller_info = Unit_info.find_exn unit_info caller in
          let new_value = Value.apply caller_info.value lookup in
          Func_info.update caller_info new_value;
          let unresolved_callees = fixpoint_self_rec caller_info ppf in
          Unresolved_dependencies.update ~caller:caller_info.name
            ~callees:unresolved_callees unresolved_deps)
        callers;
      String.Set.iter
        (fun caller -> propagate ~callee:caller unit_info unresolved_deps ppf)
        callers)

  let add_unresolved_dependencies caller unit_info unresolved_deps ppf =
    let caller_info = Unit_info.find_exn unit_info caller in
    let unresolved_callees = fixpoint_self_rec caller_info ppf in
    if not (String.Set.is_empty unresolved_callees)
    then
      Unresolved_dependencies.add ~caller ~callees:unresolved_callees
        unresolved_deps;
    propagate ~callee:caller unit_info unresolved_deps ppf

  let check_fun fun_name fun_dbg a check_body ~future_funcnames unit_info
      unresolved_deps ppf =
    let check () =
      let t = create ppf fun_name future_funcnames unit_info a in
      let really_check () =
        let res = check_body t in
        report t res ~msg:"finished" ~desc:"fundecl" fun_dbg;
        if (not t.keep_witnesses) && Value.is_resolved res
        then (
          let { Witnesses.nor; exn; div } = Value.get_witnesses res in
          assert (Witnesses.is_empty nor);
          assert (Witnesses.is_empty exn);
          assert (Witnesses.is_empty div));
        Unit_info.record unit_info fun_name res fun_dbg a;
        add_unresolved_dependencies fun_name unit_info unresolved_deps t.ppf;
        report_unit_info ppf unit_info ~msg:"after record"
      in
      let really_check () =
        if !Flambda_backend_flags.disable_zero_alloc_checker
        then
          (* Do not analyze the body of the function, conservatively assume that
             the summary is top. *)
          Unit_info.record unit_info fun_name
            (Value.top Witnesses.empty)
            fun_dbg a
        else really_check ()
      in
      match a with
      | Some a when Annotation.is_assume a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assumed" ~desc:"fundecl" fun_dbg;
        Unit_info.record unit_info fun_name expected_value fun_dbg None
      | None -> really_check ()
      | Some a ->
        let expected_value = Annotation.expected_value a in
        report t expected_value ~msg:"assert" ~desc:"fundecl" fun_dbg;
        (* Only keep witnesses for functions that need checking. *)
        really_check ()
    in
    Profile.record_call ~accumulate:true ("check " ^ analysis_name) check

  let fundecl (fd : Mach.fundecl) ~future_funcnames unit_info unresolved_deps
      ppf =
    let a = Annotation.find fd.fun_codegen_options fd.fun_name fd.fun_dbg in
    let check_body t = check_instr t fd.fun_body in
    check_fun fd.fun_name fd.fun_dbg a check_body ~future_funcnames unit_info
      unresolved_deps ppf

  module Check_cfg_backward = struct
    module Value = struct
      include Value

      let less_equal t1 t2 = lessequal t1 t2
    end

    module Backward_transfer = struct
      type domain = Value.t

      type context = t

      type error = unit

      let transform_tailcall_imm t func dbg =
        (* Sound to ignore [next] and [exn] because the call never returns. *)
        let w = create_witnesses t (Direct_tailcall { callee = func }) dbg in
        transform_call t ~next:Value.normal_return ~exn:Value.exn_escape func w
          ~desc:("direct tailcall to " ^ func)
          dbg

      let operation t ~next (op : Cfg.operation) dbg =
        match op with
        | Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Load _ | Floatop _
        | Intop_imm
            ( ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
              | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ ),
              _ )
        | Intop
            ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _ | Icomp _ )
        | Reinterpret_cast
            ( Float32_of_float | Float_of_float32 | Float_of_int64
            | Int64_of_float | Float32_of_int32 | Int32_of_float32
            | V128_of_v128 )
        | Static_cast _ | Csel _ ->
          if not (Cfg.is_pure_operation op)
          then
            Misc.fatal_errorf "Expected pure operation, got %a\n"
              Cfg.dump_operation op;
          next
        | Reinterpret_cast (Int_of_value | Value_of_int)
        | Name_for_debugger _ | Stackoffset _ | Probe_is_enabled _ | Opaque
        | Begin_region | End_region | Intop_atomic _ | Store _ ->
          next
        | Poll ->
          (* Ignore poll points even though they may trigger an allocations,
             because otherwise all loops would be considered allocating when
             poll insertion is enabled. [@poll error] should be used instead. *)
          next
        | Alloc { mode = Alloc_local; _ } -> next
        | Alloc { mode = Alloc_heap; bytes; dbginfo } ->
          let w = create_witnesses t (Alloc { bytes; dbginfo }) dbg in
          let effect =
            match Metadata.assume_value dbg ~can_raise:false w with
            | Some effect -> effect
            | None -> Value.{ nor = V.top w; exn = V.bot; div = V.bot }
          in
          transform t ~effect ~next ~exn:Value.bot "heap allocation" dbg
        | Specific s -> transform_specific t s ~next ~exn:Value.bot dbg
        | Dls_get -> next

      let basic next (i : Cfg.basic Cfg.instruction) t : (domain, error) result
          =
        match i.desc with
        | Op op -> Ok (operation t ~next op i.dbg)
        | Pushtrap _ | Poptrap ->
          (* treated as no-op here, flow and handling of exceptions is
             incorporated into the blocks and edges of the CFG *)
          Ok next
        | Reloadretaddr | Prologue | Stack_check _ -> Ok next

      let terminator next ~exn (i : Cfg.terminator Cfg.instruction) t =
        let dbg = i.dbg in
        match i.desc with
        | Never -> assert false
        | Return -> Value.normal_return
        | Raise Raise_notrace ->
          (* [raise_notrace] is typically used for control flow, not for
             indicating an error. Therefore, we do not ignore any allocation on
             paths to it. The following conservatively assumes that normal and
             exn returns are reachable. *)
          Value.join exn Value.safe
        | Raise (Raise_reraise | Raise_regular) -> exn
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ ->
          assert (not (Cfg.can_raise_terminator i.desc));
          next
        | Tailcall_self _ ->
          (* CR gyorsh: show this be treated as a jump, without affecting the
             summary? *)
          transform_tailcall_imm t t.current_fun_name dbg
        | Tailcall_func (Direct { sym_name; _ }) ->
          transform_tailcall_imm t sym_name dbg
        | Tailcall_func Indirect ->
          (* Sound to ignore [next] and [exn] because the call never returns. *)
          let w = create_witnesses t Indirect_tailcall dbg in
          transform_top t ~next:Value.normal_return ~exn:Value.exn_escape w
            "indirect tailcall" dbg
        | Call_no_return { alloc = false; _ } ->
          (* Sound to ignore [next] and [exn] because the call never returns or
             raises. *)
          Value.bot
        | Call_no_return { alloc = true; func_symbol = func; _ } ->
          (* Sound to ignore [next] because the call never returns. *)
          (* CR gyorsh: we do not currently generate this, but may later. *)
          let w = create_witnesses t (Extcall { callee = func }) dbg in
          transform_top t ~next:Value.bot ~exn w
            ("external call to " ^ func)
            dbg
        | Prim { op = External { alloc = false; _ }; _ } ->
          (* Sound to ignore [exn] because external call marked as noalloc does
             not raise. *)
          next
        | Prim { op = External { alloc = true; func_symbol = func; _ }; _ } ->
          let w = create_witnesses t (Extcall { callee = func }) dbg in
          transform_top t ~next ~exn w ("external call to " ^ func) dbg
        | Prim { op = Probe { name; handler_code_sym; enabled_at_init = _ }; _ }
          ->
          let desc =
            Printf.sprintf "probe %s handler %s" name handler_code_sym
          in
          let w = create_witnesses t (Probe { name; handler_code_sym }) dbg in
          transform_call t ~next ~exn handler_code_sym w ~desc dbg
        | Call { op = Indirect; _ } ->
          let w = create_witnesses t Indirect_call dbg in
          transform_top t ~next ~exn w "indirect call" dbg
        | Call { op = Direct { sym_name = func; _ }; _ } ->
          let w = create_witnesses t (Direct_call { callee = func }) dbg in
          transform_call t ~next ~exn func w ~desc:("direct call to " ^ func)
            dbg
        | Specific_can_raise { op = s; _ } ->
          transform_specific t s ~next ~exn dbg

      let terminator next ~exn (i : Cfg.terminator Cfg.instruction) t =
        Ok (terminator next ~exn i t)

      let exception_ next _t : (domain, error) result =
        (* enter trap handler *)
        Ok next
    end

    include Cfg_dataflow.Backward (Value) (Backward_transfer)
  end

  let cfg (fd : Cfg.t) ~future_funcnames unit_info unresolved_deps ppf =
    let a = Annotation.of_cfg fd.fun_codegen_options fd.fun_name fd.fun_dbg in
    let check_body t =
      (* CR gyorsh: initialize loops with [Value.Diverges] *)
      match
        Check_cfg_backward.run fd ~init:Value.bot ~exnescape:Value.exn_escape
          ~map:Instr t
      with
      | Ok map ->
        let entry_block = Cfg.get_block_exn fd fd.entry_label in
        let res =
          Cfg_dataflow.Instr.Tbl.find map (Cfg.first_instruction_id entry_block)
        in
        report t res ~msg:"Check_cfg_backward result" ~desc:"fundecl" fd.fun_dbg;
        res
      | Aborted _ -> Misc.fatal_errorf "Analyzing function %s" fd.fun_name ()
      | Max_iterations_reached ->
        let res = Value.top Witnesses.empty in
        report t res
          ~msg:
            "Can't reach fixpoint in max iterations, conservatively return Top"
          ~desc:"fundecl" fd.fun_dbg;
        res
    in
    check_fun fd.fun_name fd.fun_dbg a check_body ~future_funcnames unit_info
      unresolved_deps ppf
end

(** Information about the current unit. *)
let unit_info = Unit_info.create ()

let unresolved_deps = Unresolved_dependencies.create ()

(* [Selectgen] sets [return] field of [Iextcall] to prevent dead code
   elimination on functions that zero_alloc checker needs to see (details in
   PR#2112). Here we can clear [returns] field to allow subsequent passes to
   eliminate dead code. *)
let update_caml_flambda_invalid (fd : Mach.fundecl) =
  let rec fixup (i : Mach.instruction) : Mach.instruction =
    match i.desc with
    | Iop (Iextcall ({ func; _ } as ext)) ->
      if String.equal func Cmm.caml_flambda2_invalid
      then
        let desc = Mach.Iop (Iextcall { ext with returns = false }) in
        { i with desc; next = Mach.end_instr () }
      else { i with next = fixup i.next }
    | Iop
        ( Imove | Ispill | Ireload | Iconst_int _ | Iconst_float32 _
        | Iconst_float _ | Iconst_symbol _ | Iconst_vec128 _ | Icall_ind
        | Icall_imm _ | Istackoffset _ | Iload _ | Istore _ | Ialloc _
        | Iintop _ | Iintop_imm _ | Iintop_atomic _ | Ifloatop _ | Icsel _
        | Ireinterpret_cast _ | Istatic_cast _ | Ispecific _
        | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
        | Ibeginregion | Iendregion | Ipoll _ | Idls_get ) ->
      { i with next = fixup i.next }
    | Iend | Ireturn _
    | Iop Itailcall_ind
    | Iop (Itailcall_imm _)
    | Iraise _ | Iexit _ ->
      i
    | Iifthenelse (tst, ifso, ifnot) ->
      let next = fixup i.next in
      { i with desc = Iifthenelse (tst, fixup ifso, fixup ifnot); next }
    | Iswitch (index, cases) ->
      let next = fixup i.next in
      { i with desc = Iswitch (index, Array.map fixup cases); next }
    | Icatch (rec_flag, ts, handlers, body) ->
      let next = fixup i.next in
      let handlers =
        List.map
          (fun (n, ts, handler, is_cold) -> n, ts, fixup handler, is_cold)
          handlers
      in
      { i with desc = Icatch (rec_flag, ts, handlers, fixup body); next }
    | Itrywith (body, kind, (ts, handler)) ->
      let next = fixup i.next in
      { i with desc = Itrywith (fixup body, kind, (ts, fixup handler)); next }
  in
  (* This condition matches the one in [Selectgen] to avoid copying [fd]
     unnecessarily. *)
  let enabled =
    Annotation.is_check_enabled
      (Annotation.find fd.fun_codegen_options fd.fun_name fd.fun_dbg)
  in
  if enabled then { fd with fun_body = fixup fd.Mach.fun_body } else fd

let update_caml_flambda_invalid_cfg cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  (* This condition matches the one in [Selectgen] to avoid copying [fd]
     unnecessarily. *)
  let enabled =
    Annotation.is_check_enabled
      (Annotation.of_cfg cfg.fun_codegen_options cfg.fun_name cfg.fun_dbg)
  in
  if enabled
  then (
    let modified = ref false in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        match block.terminator.desc with
        | Prim { op = External ({ func_symbol; _ } as ext); label_after = _ } ->
          if String.equal func_symbol Cmm.caml_flambda2_invalid
          then (
            let successors =
              Cfg.successor_labels ~normal:true ~exn:true block
            in
            block.terminator
              <- { block.terminator with desc = Call_no_return ext };
            block.exn <- None;
            block.can_raise <- false;
            (* update predecessors for successors of [block]. *)
            Label.Set.iter
              (fun successor_label ->
                let successor_block = Cfg.get_block_exn cfg successor_label in
                successor_block.predecessors
                  <- Label.Set.remove label successor_block.predecessors)
              successors;
            modified := true)
        | Prim { op = Probe _; _ }
        | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
        | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
        | Tailcall_func _ | Call_no_return _ | Call _ | Specific_can_raise _ ->
          ());
    if !modified
    then
      Profile.record ~accumulate:true "cleanup"
        (fun () ->
          Eliminate_fallthrough_blocks.run cfg_with_layout;
          Merge_straightline_blocks.run cfg_with_layout;
          Eliminate_dead_code.run_dead_block cfg_with_layout;
          Simplify_terminator.run cfg)
        ())

let fundecl ppf_dump ~future_funcnames fd =
  Analysis.fundecl fd ~future_funcnames unit_info unresolved_deps ppf_dump;
  update_caml_flambda_invalid fd

let cfg ppf_dump ~future_funcnames cl =
  let cfg = Cfg_with_layout.cfg cl in
  Analysis.cfg cfg ~future_funcnames unit_info unresolved_deps ppf_dump;
  update_caml_flambda_invalid_cfg cl;
  cl

let reset_unit_info () =
  Unit_info.reset unit_info;
  Unresolved_dependencies.reset unresolved_deps

let record_unit_info ppf_dump =
  Analysis.record_unit unit_info unresolved_deps ppf_dump;
  Compilenv.cache_zero_alloc_info
    (Compilenv.current_unit_infos ()).ui_zero_alloc_info

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit

let iter_witnesses f =
  Unit_info.iter unit_info ~f:(fun func_info ->
      f func_info.name
        (Value.get_witnesses func_info.value |> Witnesses.simplify))

let () = Location.register_error_of_exn Report.print

let is_check_enabled codegen_options fun_name dbg =
  Annotation.is_check_enabled (Annotation.find codegen_options fun_name dbg)
