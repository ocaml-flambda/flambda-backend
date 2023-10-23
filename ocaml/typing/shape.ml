(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let compare (x : t) y =
      match x, y with
      | Compilation_unit s1, Compilation_unit s2 -> String.compare s1 s2
      | Item c1, Item c2 ->
        let c = Int.compare c1.id c2.id in
        if c <> 0 then c else String.compare c1.comp_unit c2.comp_unit
      | Internal, Internal -> 0
      | Predef s1, Predef s2 -> String.compare s1 s2
      | Compilation_unit _, (Item _ | Internal | Predef _) -> -1
      | Item _, (Internal | Predef _) -> -1
      | Internal, Predef _ -> -1
      | (Item _ | Internal | Predef _), Compilation_unit _ -> 1
      | (Internal | Predef _), Item _ -> 1
      | Predef _, Internal -> 1

    let equal x y = compare x y = 0

    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      let comp_unit =
        match current_unit with
        | Some cu -> cu |> Compilation_unit.full_path_as_string
        | None -> ""
      in
      Item { comp_unit; id = !id }

  let of_compilation_unit_id id =
    Compilation_unit (id |> Compilation_unit.full_path_as_string)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true

  let rank = function
    | Value -> 0
    | Type -> 1
    | Module -> 2
    | Module_type -> 3
    | Extension_constructor -> 4
    | Class -> 5
    | Class_type -> 6

  let compare a b =
    let a = rank a in
    let b = rank b in
    Int.compare a b
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t

    let compare (sa, ka) (sb, kb) =
      let c = String.compare sa sb in
      if c <> 0 then c
      else (Sig_component_kind.compare ka kb)

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type

    let print fmt (name, ns) =
      Format.fprintf fmt "%S[%s]"
        name
        (Sig_component_kind.to_string ns)

    let hash x = Hashtbl.hash x
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
type t = { hash:int; uid: Uid.t option; desc: desc }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string

let rec equal_desc d1 d2 =
  if d1 == d2 then true else
  match d1, d2 with
  | Var v1, Var v2 -> Ident.equal v1 v2
  | Abs (v1, t1), Abs (v2, t2) ->
    if Ident.equal v1 v2 then equal t1 t2
    else false
  | App (v1, t1), App (v2, t2) ->
    if not (equal t1 t2) then false
    else equal v1 v2
  | Leaf, Leaf -> true
  | Struct t1, Struct t2 ->
    Item.Map.equal equal t1 t2
  | Proj (t1, i1), Proj (t2, i2) ->
    if Item.compare i1 i2 <> 0 then false
    else equal t1 t2
  | Comp_unit c1, Comp_unit c2 -> String.equal c1 c2
  | Var _, (Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _) -> false
  | Abs _, (Var _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _) -> false
  | App _, (Var _ | Abs _ | Struct _ | Leaf | Proj _ | Comp_unit _) -> false
  | Struct _, (Var _ | Abs _ | App _ | Leaf | Proj _ | Comp_unit _) -> false
  | Leaf, (Var _ | Abs _ | App _ | Struct _ | Proj _ | Comp_unit _) -> false
  | Proj _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Comp_unit _) -> false
  | Comp_unit _, (Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _) -> false

and equal t1 t2 =
  if t1.hash <> t2.hash then false
  else if not (Option.equal Uid.equal t1.uid t2.uid) then false
  else equal_desc t1.desc t2.desc

let print fmt =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  let rec aux fmt { uid; desc; hash = _ } =
    match desc with
    | Var id ->
        Format.fprintf fmt "%a%a" Ident.print id print_uid_opt uid
    | Abs (id, t) ->
        Format.fprintf fmt "Abs@[%a@,(@[%a,@ @[%a@]@])@]"
          print_uid_opt uid Ident.print id aux t
    | App (t1, t2) ->
        Format.fprintf fmt "@[%a(@,%a)%a@]" aux t1 aux t2
          print_uid_opt uid
    | Leaf ->
        Format.fprintf fmt "<%a>" (Format.pp_print_option Uid.print) uid
    | Proj (t, item) ->
        begin match uid with
        | None ->
            Format.fprintf fmt "@[%a@ .@ %a@]"
              aux t
              Item.print item
        | Some uid ->
            Format.fprintf fmt "@[(%a@ .@ %a)<%a>@]"
              aux t
              Item.print item
              Uid.print uid
        end
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun item t ->
              Format.fprintf fmt "@[<hv 4>%a ->@ %a;@]@,"
                Item.print item
                aux t
            )
        in
        Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
  in
  Format.fprintf fmt"@[%a@]@;" aux

let hash_var = 1
let hash_abs = 2
let hash_struct = 3
let hash_leaf = 4
let hash_proj = 5
let hash_app = 6
let hash_comp_unit = 7

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var; hash = Hashtbl.hash (hash_var, uid, var) }

let for_unnamed_functor_param = Ident.create_local "()"

let var ?uid id =
  { uid; desc = Var id; hash = Hashtbl.hash (hash_var, uid, id) }

let abs ?uid var body =
  { uid; desc = Abs (var, body); hash = Hashtbl.hash (hash_abs, uid, body.hash) }

let str ?uid map =
  let h = Item.Map.fold (fun key t acc ->
    Hashtbl.hash (acc, Item.hash key, t.hash)) map 0
  in
  { uid; desc = Struct map; hash = Hashtbl.hash (hash_struct, uid, h)  }

let leaf ?uid () =
  { uid; desc = Leaf; hash = Hashtbl.hash (hash_leaf, uid) }

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf
        as a best effort *)
      t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item);
        hash = Hashtbl.hash (hash_proj, t.hash, item) }

let app ?uid f ~arg =
      { uid; desc = App (f, arg); hash = Hashtbl.hash (hash_app, f.hash, uid, arg.hash) }

let comp_unit ?uid s =
      { uid; desc = Comp_unit s; hash = Hashtbl.hash (hash_comp_unit, uid, s) }

let decompose_abs t =
  match t.desc with
  | Abs (x, t) -> Some (x, t)
  | _ -> None

module Make_reduce(Params : sig
  type env
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
  val find_shape : env -> Ident.t -> t
end) = struct
  (* We implement a strong call-by-need reduction, following an
     evaluator from Nathanaelle Courant. *)

  type nf = { uid: Uid.t option; desc: nf_desc }
  and nf_desc =
    | NVar of var
    | NApp of nf * nf
    | NAbs of local_env * var * t * delayed_nf
    | NStruct of delayed_nf Item.Map.t
    | NProj of nf * Item.t
    | NLeaf
    | NComp_unit of string
    | NoFuelLeft of t
  (* A type of normal forms for strong call-by-need evaluation.
     The normal form of an abstraction
       Abs(x, t)
     is a closure
       NAbs(env, x, t, dnf)
     when [env] is the local environment, and [dnf] is a delayed
     normal form of [t].

     A "delayed normal form" is morally equivalent to (nf Lazy.t), but
     we use a different representation that is compatible with
     memoization (lazy values are not hashable/comparable by default
     comparison functions): we represent a delayed normal form as
     just a not-yet-computed pair [local_env * t] of a term in a
     local environment -- we could also see this as a term under
     an explicit substitution. This delayed thunked is "forced"
     by calling the normalization function as usual, but duplicate
     computations are precisely avoided by memoization.
   *)
  and delayed_nf = Thunk of local_env * t

  and local_env = delayed_nf option Ident.Map.t
  (* When reducing in the body of an abstraction [Abs(x, body)], we
     bind [x] to [None] in the environment. [Some v] is used for
     actual substitutions, for example in [App(Abs(x, body), t)], when
     [v] is a thunk that will evaluate to the normal form of [t]. *)

  let improve_uid uid (nf : nf) =
    match nf.uid with
    | Some _ -> nf
    | None -> { nf with uid }

  let rec equal_local_env t1 t2 =
    Ident.Map.equal (Option.equal equal_delayed_nf) t1 t2

  and equal_delayed_nf t1 t2 =
    match t1, t2 with
    | Thunk (l1, t1), Thunk (l2, t2) ->
      if equal t1 t2 then equal_local_env l1 l2
      else false

  and equal_nf_desc d1 d2 =
    match d1, d2 with
    | NVar v1, NVar v2 -> Ident.equal v1 v2
    | NAbs (l1, v1, t1, nf1), NAbs (l2, v2, t2, nf2) ->
      if not (Ident.equal v1 v2) then false
      else if not (equal t1 t2) then false
      else if not (equal_delayed_nf nf1 nf2) then false
      else equal_local_env l1 l2
    | NApp (v1, t1), NApp (v2, t2) ->
      if equal_nf v1 v2 then equal_nf t1 t2
      else false
    | NLeaf, NLeaf -> true
    | NoFuelLeft d1, NoFuelLeft d2 -> equal d1 d2
    | NStruct t1, NStruct t2 ->
      Item.Map.equal equal_delayed_nf t1 t2
    | NProj (t1, i1), NProj (t2, i2) ->
      if Item.compare i1 i2 <> 0 then false
      else equal_nf t1 t2
    | NComp_unit c1, NComp_unit c2 -> String.equal c1 c2
    | NVar _, (NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NoFuelLeft _)
    | NLeaf, (NVar _ | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NoFuelLeft _)
    | NApp _, (NVar _ | NLeaf | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NoFuelLeft _)
    | NAbs _, (NVar _ | NLeaf | NApp _ | NStruct _ | NProj _ | NComp_unit _ | NoFuelLeft _)
    | NStruct _, (NVar _ | NLeaf | NApp _ | NAbs _ | NProj _ | NComp_unit _ | NoFuelLeft _)
    | NProj _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NComp_unit _ | NoFuelLeft _)
    | NComp_unit _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NoFuelLeft _)
    | NoFuelLeft _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _)
    -> false

  and equal_nf t1 t2 =
    if not (Option.equal Uid.equal t1.uid t2.uid) then false
    else equal_nf_desc t1.desc t2.desc

  module ReduceMemoTable = Hashtbl.Make(struct
      type nonrec t = local_env * t

      let hash t = Hashtbl.hash t

      let equal (env1, t1) (env2, t2) =
        if equal t1 t2 then equal_local_env env1 env2
        else false
  end)

  module ReadBackMemoTable = Hashtbl.Make(struct
      type nonrec t = nf

      let hash t = Hashtbl.hash t

  let equal a b = equal_nf a b
  end)

  let in_reduce_memo_table memo_table memo_key f arg =
    match ReduceMemoTable.find memo_table memo_key with
        | res -> res
    | exception Not_found ->
        let res = f arg in
        ReduceMemoTable.replace memo_table memo_key res;
        res

  let in_read_back_memo_table memo_table memo_key f arg =
    match ReadBackMemoTable.find memo_table memo_key with
    | res -> res
    | exception Not_found ->
        let res = f arg in
        ReadBackMemoTable.replace memo_table memo_key res;
        res

  type env = {
    fuel: int ref;
    global_env: Params.env;
    local_env: local_env;
    reduce_memo_table: nf ReduceMemoTable.t;
    read_back_memo_table: t ReadBackMemoTable.t;
  }

  let bind env var shape =
    { env with local_env = Ident.Map.add var shape env.local_env }

  let rec reduce_ env t =
    let memo_key = (env.local_env, t) in
    in_reduce_memo_table env.reduce_memo_table memo_key (reduce__ env) t
  (* Memoization is absolutely essential for performance on this
     problem, because the normal forms we build can in some real-world
     cases contain an exponential amount of redundancy. Memoization
     can avoid the repeated evaluation of identical subterms,
     providing a large speedup, but even more importantly it
     implicitly shares the memory of the repeated results, providing
     much smaller normal forms (that blow up again if printed back
     as trees). A functor-heavy file from Irmin has its shape normal
     form decrease from 100Mio to 2.5Mio when memoization is enabled.

     Note: the local environment is part of the memoization key, while
     it is defined using a type Ident.Map.t of non-canonical balanced
     trees: two maps could have exactly the same items, but be
     balanced differently and therefore hash differently, reducing
     the effectivenss of memoization.
     This could in theory happen, say, with the two programs
       (fun x -> fun y -> ...)
     and
       (fun y -> fun x -> ...)
     having "the same" local environments, with additions done in
     a different order, giving non-structurally-equal trees. Should we
     define our own hash functions to provide robust hashing on
     environments?

     We believe that the answer is "no": this problem does not occur
     in practice. We can assume that identifiers are unique on valid
     typedtree fragments (identifier "stamps" distinguish
     binding positions); in particular the two program fragments above
     in fact bind *distinct* identifiers x (with different stamps) and
     different identifiers y, so the environments are distinct. If two
     environments are structurally the same, they must correspond to
     the evaluation environments of two sub-terms that are under
     exactly the same scope of binders. So the two environments were
     obtained by the same term traversal, adding binders in the same
     order, giving the same balanced trees: the environments have the
     same hash.
*)

  and reduce__ ({fuel; global_env; local_env; _} as env) (t : t) =
    let reduce env t = reduce_ env t in
    let delay_reduce env t = Thunk (env.local_env, t) in
    let force (Thunk (local_env, t)) =
      reduce { env with local_env } t in
    let return desc : nf = { uid = t.uid; desc } in
    if !fuel < 0 then return (NoFuelLeft t)
    else
      match t.desc with
      | Comp_unit unit_name ->
          begin match Params.read_unit_shape ~unit_name with
          | Some t -> reduce env t
          | None -> return (NComp_unit unit_name)
          end
      | App(f, arg) ->
          let f = reduce env f in
          begin match f.desc with
          | NAbs(clos_env, var, body, _body_nf) ->
              let arg = delay_reduce env arg in
              let env = bind { env with local_env = clos_env } var (Some arg) in
              reduce env body
              |> improve_uid t.uid
          | _ ->
              let arg = reduce env arg in
              return (NApp(f, arg))
          end
      | Proj(str, item) ->
          let str = reduce env str in
          let nored () = return (NProj(str, item)) in
          begin match str.desc with
          | NStruct (items) ->
              begin match Item.Map.find item items with
              | exception Not_found -> nored ()
              | nf ->
                  force nf
                  |> improve_uid t.uid
              end
          | _ ->
              nored ()
          end
      | Abs(var, body) ->
          let body_nf = delay_reduce (bind env var None) body in
          return (NAbs(local_env, var, body, body_nf))
      | Var id ->
          begin match Ident.Map.find id local_env with
          (* Note: instead of binding abstraction-bound variables to
             [None], we could unify it with the [Some v] case by
             binding the bound variable [x] to [NVar x].

             One reason to distinguish the situations is that we can
             provide a different [Uid.t] location; for bound
             variables, we use the [Uid.t] of the bound occurrence
             (not the binding site), whereas for bound values we use
             their binding-time [Uid.t]. *)
          | None -> return (NVar id)
          | Some def -> force def
          | exception Not_found ->
          match Params.find_shape global_env id with
          | exception Not_found -> return (NVar id)
          | res when res = t -> return (NVar id)
          | res ->
              decr fuel;
              reduce env res
          end
      | Leaf -> return NLeaf
      | Struct m ->
          let mnf = Item.Map.map (delay_reduce env) m in
          return (NStruct mnf)

  let rec read_back env (nf : nf) : t =
    in_read_back_memo_table env.read_back_memo_table nf (read_back_ env) nf
  (* The [nf] normal form we receive may contain a lot of internal
     sharing due to the use of memoization in the evaluator. We have
     to memoize here again, otherwise the sharing is lost by mapping
     over the term as a tree. *)

  and read_back_ env (nf : nf) : t =
  read_back_desc ~uid:nf.uid env nf.desc

  and read_back_desc ~uid env desc =
    let read_back nf = read_back env nf in
    let read_back_force (Thunk (local_env, t)) =
    read_back (reduce_ { env with local_env } t)
  in
  match desc with
  | NVar v ->
    var ?uid v
  | NApp (nft, nfu) ->
      let f = read_back nft in
      let arg = read_back nfu in
      app ?uid f ~arg
  | NAbs (_env, x, _t, nf) ->
    let body = read_back_force nf in
    abs ?uid x body
  | NStruct nstr ->
    let map = Item.Map.map read_back_force nstr in
    str ?uid map
  | NProj (nf, item) ->
      let t = read_back nf in
      proj ?uid t item
  | NLeaf -> leaf ?uid ()
  | NComp_unit s -> comp_unit ?uid s
  | NoFuelLeft t -> { t with uid }

  let reduce global_env t =
    let fuel = ref Params.fuel in
    let reduce_memo_table = ReduceMemoTable.create 42 in
    let read_back_memo_table = ReadBackMemoTable.create 42 in
    let local_env = Ident.Map.empty in
    let env = {
      fuel;
      global_env;
      reduce_memo_table;
      read_back_memo_table;
      local_env;
    } in
    reduce_ env t |> read_back env
end

module Local_reduce =
  (* Note: this definition with [type env = unit] is only suitable for
     reduction of toplevel shapes -- shapes of compilation units,
     where free variables are only Comp_unit names. If we wanted to
     reduce shapes inside module signatures, we would need to take
     a typing environment as parameter. *)
  Make_reduce(struct
    type env = unit
    let fuel = 10
    let read_unit_shape ~unit_name:_ = None
    let find_shape _env _id = raise Not_found
  end)

let local_reduce shape =
  Profile.record_call "shape.local_reduce" (fun () ->
    Local_reduce.reduce () shape
  )

let dummy_mod = str Item.Map.empty

let of_path ~find_shape ~namespace =
  let rec aux : Sig_component_kind.t -> Path.t -> t = fun ns -> function
    | Pident id -> find_shape ns id
    | Pdot (path, name) -> proj (aux Module path) (name, ns)
    | Papply (p1, p2) -> app (aux Module p1) ~arg:(aux Module p2)
  in
  aux namespace

let for_persistent_unit s =
  comp_unit ~uid:(Compilation_unit s) s

let leaf_for_unpack = leaf ()

let set_uid_if_none t uid =
  match t.uid with
  | None -> { t with uid = Some uid }
  | _ -> t

module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf ~uid ()) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id uid = Item.Map.add (Item.type_ id) (leaf ~uid ()) t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf ~uid ()) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id uid =
    Item.Map.add (Item.extension_constructor id) (leaf ~uid ()) t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf ~uid ()) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf ~uid ()) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
