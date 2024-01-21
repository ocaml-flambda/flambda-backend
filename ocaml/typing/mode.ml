(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Axis = struct
  type t =
    [ `Locality
    | `Regionality
    | `Uniqueness
    | `Linearity ]

  let string_of = function
    | `Locality -> "locality"
    | `Regionality -> "regionality"
    | `Uniqueness -> "uniqueness"
    | `Linearity -> "linearity"
end

module Global_flag = struct
  type t =
    | Global
    | Unrestricted

  let compare flag0 flag1 =
    match flag0, flag1 with
    | Global, Unrestricted -> -1
    | Unrestricted, Global -> 1
    | Global, Global | Unrestricted, Unrestricted -> 0
end

type 'a var =
  { mutable upper : 'a;
    mutable lower : 'a;
    mutable vlower : 'a var list;
    mutable mark : bool;
    mvid : int
  }

type changes =
  | Cnil : changes
  | Cupper : 'a var * 'a * changes -> changes
  | Clower : 'a var * 'a * changes -> changes
  | Cvlower : 'a var * 'a var list * changes -> changes

let set_lower ~log v lower =
  log := Clower (v, v.lower, !log);
  v.lower <- lower

let set_upper ~log v upper =
  log := Cupper (v, v.upper, !log);
  v.upper <- upper

let set_vlower ~log v vlower =
  log := Cvlower (v, v.vlower, !log);
  v.vlower <- vlower

let rec undo_changes = function
  | Cnil -> ()
  | Cupper (v, upper, rest) ->
    v.upper <- upper;
    undo_changes rest
  | Clower (v, lower, rest) ->
    v.lower <- lower;
    undo_changes rest
  | Cvlower (v, vlower, rest) ->
    v.vlower <- vlower;
    undo_changes rest

let change_log : (changes -> unit) ref = ref (fun _ -> ())

let is_not_nil = function
  | Cnil -> false
  | Cupper _ | Clower _ | Cvlower _ -> true

let log_changes changes = if is_not_nil changes then !change_log changes

type ('a, 'b) const_or_var =
  | Const of 'a
  | Var of 'b

type ('loc, 'u, 'lin) modes =
  { locality : 'loc;
    uniqueness : 'u;
    linearity : 'lin
  }

module type Lattice = sig
  type t

  val min : t

  val max : t

  val eq : t -> t -> bool

  val le : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end

module type Solver = sig
  type const

  type t

  type var

  val of_const : const -> t

  val min_mode : t

  val max_mode : t

  val is_const : t -> bool

  val submode : t -> t -> (unit, unit) Result.t

  val submode_exn : t -> t -> unit

  val equate : t -> t -> (unit, unit) Result.t

  val constrain_upper : t -> const

  val newvar : unit -> t

  val newvar_below : t -> t * bool

  val newvar_above : t -> t * bool

  val join : t list -> t

  val meet : t list -> t

  val constrain_lower : t -> const

  val const_or_var : t -> (const, var) const_or_var

  val check_const : t -> const option

  val print_var : Format.formatter -> var -> unit

  val print : Format.formatter -> t -> unit

  val print' : ?verbose:bool -> ?label:string -> Format.formatter -> t -> unit
end

module Solver (L : Lattice) : Solver with type const := L.t = struct
  type nonrec var = L.t var

  type t =
    | Amode of L.t
    | Amodevar of var

  let next_id = ref (-1)

  let fresh () =
    incr next_id;
    { upper = L.max; lower = L.min; vlower = []; mvid = !next_id; mark = false }

  exception NotSubmode

  let of_const c = Amode c

  let min_mode = Amode L.min

  let max_mode = Amode L.max

  let is_const = function Amode _ -> true | Amodevar _ -> false

  let submode_cv ~log m v =
    if L.le m v.lower
    then ()
    else if not (L.le m v.upper)
    then raise NotSubmode
    else
      let m = L.join v.lower m in
      set_lower ~log v m;
      if L.eq m v.upper then set_vlower ~log v []

  let rec submode_vc ~log v m =
    if L.le v.upper m
    then ()
    else if not (L.le v.lower m)
    then raise NotSubmode
    else
      let m = L.meet v.upper m in
      set_upper ~log v m;
      v.vlower
      |> List.iter (fun a ->
             (* a <= v <= m *)
             submode_vc ~log a m;
             set_lower ~log v (L.join v.lower a.lower));
      if L.eq v.lower m then set_vlower ~log v []

  let submode_vv ~log a b =
    (* Printf.printf "  %a <= %a\n" pp_v a pp_v b; *)
    if L.le a.upper b.lower
    then ()
    else if a == b || List.memq a b.vlower
    then ()
    else (
      submode_vc ~log a b.upper;
      set_vlower ~log b (a :: b.vlower);
      submode_cv ~log a.lower b)

  let rec all_equal v = function
    | [] -> true
    | v' :: rest -> if v == v' then all_equal v rest else false

  let join_vc v m =
    if L.le v.upper m
    then Amode m
    else if L.le m v.lower
    then Amodevar v
    else
      let log = ref Cnil in
      let v' = fresh () in
      submode_cv ~log m v';
      submode_vv ~log v v';
      log_changes !log;
      Amodevar v'

  let join_vsc vs m =
    match vs with
    | [] -> Amode m
    | v :: rest ->
      if all_equal v rest
      then join_vc v m
      else
        let log = ref Cnil in
        let v = fresh () in
        submode_cv ~log m v;
        List.iter (fun v' -> submode_vv ~log v' v) vs;
        log_changes !log;
        Amodevar v

  let meet_vc v m =
    if L.le m v.lower
    then Amode m
    else if L.le v.upper m
    then Amodevar v
    else
      let log = ref Cnil in
      let v' = fresh () in
      submode_vc ~log v' m;
      submode_vv ~log v' v;
      log_changes !log;
      Amodevar v'

  let meet_vsc vs m =
    match vs with
    | [] -> Amode m
    | v :: rest ->
      if all_equal v rest
      then meet_vc v m
      else
        let log = ref Cnil in
        let v = fresh () in
        submode_vc ~log v m;
        List.iter (fun v' -> submode_vv ~log v v') vs;
        log_changes !log;
        Amodevar v

  let submode a b =
    let log = ref Cnil in
    match
      match a, b with
      | Amode a, Amode b -> if not (L.le a b) then raise NotSubmode
      | Amodevar v, Amode c -> submode_vc ~log v c
      | Amode c, Amodevar v -> submode_cv ~log c v
      | Amodevar a, Amodevar b -> submode_vv ~log a b
    with
    | () ->
      log_changes !log;
      Ok ()
    | exception NotSubmode ->
      undo_changes !log;
      Error ()

  let submode_exn t1 t2 =
    match submode t1 t2 with
    | Ok () -> ()
    | Error () -> invalid_arg "submode_exn"

  let equate a b =
    match submode a b, submode b a with
    | Ok (), Ok () -> Ok ()
    | Error (), _ | _, Error () -> Error ()

  let constrain_upper = function
    | Amode m -> m
    | Amodevar v ->
      submode_exn (Amode v.upper) (Amodevar v);
      v.upper

  let newvar () = Amodevar (fresh ())

  let newvar_below = function
    | Amode c when L.eq c L.min -> min_mode, false
    | m ->
      let v = newvar () in
      submode_exn v m;
      v, true

  let newvar_above = function
    | Amode c when L.eq c L.max -> max_mode, false
    | m ->
      let v = newvar () in
      submode_exn m v;
      v, true

  let join ms =
    let rec aux vars const = function
      | [] -> join_vsc vars const
      | Amode c :: _ when L.eq c L.max -> max_mode
      | Amode c :: ms -> aux vars (L.join c const) ms
      | Amodevar v :: ms -> aux (v :: vars) const ms
    in
    aux [] L.min ms

  let meet ms =
    let rec aux vars const = function
      | [] -> meet_vsc vars const
      | Amode c :: _ when L.eq c L.min -> min_mode
      | Amode c :: ms -> aux vars (L.join c const) ms
      | Amodevar v :: ms -> aux (v :: vars) const ms
    in
    aux [] L.max ms

  exception Became_constant

  let compress_vlower v =
    let nmarked = ref 0 in
    let mark v' =
      assert (not v'.mark);
      v'.mark <- true;
      incr nmarked
    in
    let unmark v' =
      assert v'.mark;
      v'.mark <- false;
      decr nmarked
    in
    let new_lower = ref v.lower in
    let new_vlower = ref v.vlower in
    (* Ensure that each transitive lower bound of v
       is a direct lower bound of v *)
    let rec trans v' =
      if L.le v'.upper !new_lower
      then ()
      else if v'.mark
      then ()
      else (
        mark v';
        new_vlower := v' :: !new_vlower;
        trans_low v')
    and trans_low v' =
      assert (v != v');
      if not (L.le v'.lower v.upper)
      then Misc.fatal_error "compress_vlower: invalid bounds";
      if not (L.le v'.lower !new_lower)
      then (
        new_lower := L.join !new_lower v'.lower;
        if !new_lower = v.upper
        then
          (* v is now a constant, no need to keep computing bounds *)
          raise Became_constant);
      List.iter trans v'.vlower
    in
    mark v;
    List.iter mark v.vlower;
    let became_constant =
      match List.iter trans_low v.vlower with
      | () -> false
      | exception Became_constant -> true
    in
    List.iter unmark !new_vlower;
    unmark v;
    assert (!nmarked = 0);
    if became_constant then new_vlower := [];
    if !new_lower != v.lower || !new_vlower != v.vlower
    then (
      let log = ref Cnil in
      set_lower ~log v !new_lower;
      set_vlower ~log v !new_vlower;
      log_changes !log)

  let constrain_lower = function
    | Amode m -> m
    | Amodevar v ->
      compress_vlower v;
      submode_exn (Amodevar v) (Amode v.lower);
      v.lower

  let const_or_var = function
    | Amode m -> Const m
    | Amodevar v ->
      compress_vlower v;
      if L.eq v.lower v.upper then Const v.lower else Var v

  let check_const a =
    match const_or_var a with Const m -> Some m | Var _ -> None

  let print_var_id ppf v = Format.fprintf ppf "?%i" v.mvid

  let print_var ppf v =
    if v.vlower = []
    then print_var_id ppf v
    else
      Format.fprintf ppf "%a[> %a]" print_var_id v
        (Format.pp_print_list print_var_id)
        v.vlower

  let print' ?(verbose = true) ?label ppf a =
    match const_or_var a with
    | Const m -> L.print ppf m
    | Var v ->
      (match label with None -> () | Some s -> Format.fprintf ppf "%s:" s);
      if verbose then print_var ppf v else Format.fprintf ppf "?"

  let print ppf a = print' ~verbose:true ?label:None ppf a
end

module type DualLattice = sig
  include Lattice

  type dual

  val to_dual : t -> dual

  val of_dual : dual -> t
end

module type DualSolver = sig
  include Solver

  type dual

  val to_dual : t -> dual

  val of_dual : dual -> t
end

module DualSolver
    (Dual : Lattice)
    (Solver : Solver with type const := Dual.t)
    (L : DualLattice with type dual := Dual.t) :
  DualSolver with type const := L.t and type dual := Solver.t = struct
  type var = Solver.var

  type t = Solver.t

  let of_const c = Solver.of_const (L.to_dual c)

  let is_const a = Solver.is_const a

  let submode a b = Solver.submode b a

  let submode_exn a b = Solver.submode_exn b a

  let equate a b = Solver.equate b a

  let constrain_upper a = L.of_dual (Solver.constrain_lower a)

  let constrain_lower a = L.of_dual (Solver.constrain_upper a)

  let to_dual a = a

  let of_dual a = a

  let min_mode = of_dual Solver.max_mode

  let max_mode = of_dual Solver.min_mode

  let newvar () = Solver.newvar ()

  let newvar_below a =
    let a', changed = Solver.newvar_above a in
    a', changed

  let newvar_above a =
    let a', changed = Solver.newvar_below a in
    a', changed

  let join ts = Solver.meet ts

  let meet ts = Solver.join ts

  let const_or_var a =
    match Solver.const_or_var a with
    | Const c -> Const (L.of_dual c)
    | Var v -> Var v

  let check_const a =
    match Solver.check_const a with
    | Some m -> Some (L.of_dual m)
    | None -> None

  let print_var = Solver.print_var

  let print' ?(verbose = true) ?label ppf a =
    match Solver.const_or_var a with
    | Const m -> L.print ppf (L.of_dual m)
    | Var v ->
      (match label with None -> () | Some s -> Format.fprintf ppf "%s:" s);
      if verbose
      then (* caret stands for dual *)
        Format.fprintf ppf "^%a" print_var v
      else Format.fprintf ppf "?"

  let print ppf m = print' ~verbose:true ?label:None ppf m
end

module Locality = struct
  module Const = struct
    type t =
      | Global
      | Local

    let min = Global

    let max = Local

    let legacy = Global

    let le a b =
      match a, b with Global, _ | _, Local -> true | Local, Global -> false

    let eq a b =
      match a, b with
      | Global, Global | Local, Local -> true
      | Local, Global | Global, Local -> false

    let join a b =
      match a, b with Local, _ | _, Local -> Local | Global, Global -> Global

    let meet a b =
      match a, b with Global, _ | _, Global -> Global | Local, Local -> Local

    let print ppf = function
      | Global -> Format.fprintf ppf "Global"
      | Local -> Format.fprintf ppf "Local"
  end

  include Solver (Const)

  let global = of_const Const.Global

  let local = of_const Const.Local

  let legacy = global

  let constrain_legacy = constrain_lower
end

module Regionality = struct
  module Const = struct
    type t =
      | Global
      | Regional
      | Local

    let r_as_l : t -> Locality.Const.t = function
      | Local | Regional -> Local
      | Global -> Global

    let r_as_g : t -> Locality.Const.t = function
      | Local -> Local
      | Regional | Global -> Global

    let of_localities ~(r_as_l : Locality.Const.t) ~(r_as_g : Locality.Const.t)
        =
      match r_as_l, r_as_g with
      | Global, Global -> Global
      | Global, Local -> assert false
      | Local, Global -> Regional
      | Local, Local -> Local

    let print ppf t =
      let s =
        match t with
        | Global -> "Global"
        | Regional -> "Regional"
        | Local -> "Local"
      in
      Format.fprintf ppf "%s" s
  end

  type t =
    { r_as_l : Locality.t;
      r_as_g : Locality.t
    }

  let of_locality l = { r_as_l = l; r_as_g = l }

  let of_const c =
    let r_as_l, r_as_g =
      match c with
      | Const.Global -> Locality.global, Locality.global
      | Const.Regional -> Locality.local, Locality.global
      | Const.Local -> Locality.local, Locality.local
    in
    { r_as_l; r_as_g }

  let local = of_const Local

  let regional = of_const Regional

  let global = of_const Global

  let legacy = global

  let max_mode =
    let r_as_l = Locality.max_mode in
    let r_as_g = Locality.max_mode in
    { r_as_l; r_as_g }

  let min_mode =
    let r_as_l = Locality.min_mode in
    let r_as_g = Locality.min_mode in
    { r_as_l; r_as_g }

  let local_to_regional t = { t with r_as_g = Locality.global }

  let regional_to_global t = { t with r_as_l = t.r_as_g }

  let regional_to_local t = { t with r_as_g = t.r_as_l }

  let global_to_regional t = { t with r_as_l = Locality.local }

  let regional_to_global_locality t = t.r_as_g

  let regional_to_local_locality t = t.r_as_l

  type error =
    [ `Regionality
    | `Locality ]

  let submode t1 t2 =
    match Locality.submode t1.r_as_l t2.r_as_l with
    | Error () -> Error `Regionality
    | Ok () -> (
      match Locality.submode t1.r_as_g t2.r_as_g with
      | Error () -> Error `Locality
      | Ok () as ok -> ok)

  let equate a b =
    match submode a b, submode b a with
    | Ok (), Ok () -> Ok ()
    | Error e, _ | _, Error e -> Error e

  let join ts =
    let r_as_l = Locality.join (List.map (fun t -> t.r_as_l) ts) in
    let r_as_g = Locality.join (List.map (fun t -> t.r_as_g) ts) in
    { r_as_l; r_as_g }

  let constrain_upper t =
    let r_as_l = Locality.constrain_upper t.r_as_l in
    let r_as_g = Locality.constrain_upper t.r_as_g in
    Const.of_localities ~r_as_l ~r_as_g

  let constrain_lower t =
    let r_as_l = Locality.constrain_lower t.r_as_l in
    let r_as_g = Locality.constrain_lower t.r_as_g in
    Const.of_localities ~r_as_l ~r_as_g

  let newvar () =
    let r_as_l = Locality.newvar () in
    let r_as_g, _ = Locality.newvar_below r_as_l in
    { r_as_l; r_as_g }

  let newvar_below t =
    let r_as_l, changed1 = Locality.newvar_below t.r_as_l in
    let r_as_g, changed2 = Locality.newvar_below t.r_as_g in
    Locality.submode_exn r_as_g r_as_l;
    { r_as_l; r_as_g }, changed1 || changed2

  let newvar_above t =
    let r_as_l, changed1 = Locality.newvar_above t.r_as_l in
    let r_as_g, changed2 = Locality.newvar_above t.r_as_g in
    Locality.submode_exn r_as_g r_as_l;
    { r_as_l; r_as_g }, changed1 || changed2

  let check_const t =
    match Locality.check_const t.r_as_l with
    | None -> None
    | Some r_as_l -> (
      match Locality.check_const t.r_as_g with
      | None -> None
      | Some r_as_g -> Some (Const.of_localities ~r_as_l ~r_as_g))

  let print' ?(verbose = true) ?label ppf t =
    match check_const t with
    | Some l -> Const.print ppf l
    | None -> (
      match label with
      | None -> ()
      | Some l ->
        Format.fprintf ppf "%s: " l;
        Format.fprintf ppf "r_as_l=%a r_as_g=%a"
          (Locality.print' ~verbose ?label:None)
          t.r_as_l
          (Locality.print' ~verbose ?label:None)
          t.r_as_g)

  let print ppf m = print' ~verbose:true ?label:None ppf m
end

module Uniqueness = struct
  module Const = struct
    type t =
      | Unique
      | Shared

    let legacy = Shared

    let min = Unique

    let max = Shared

    let le a b =
      match a, b with Unique, _ | _, Shared -> true | Shared, Unique -> false

    let eq a b =
      match a, b with
      | Unique, Unique | Shared, Shared -> true
      | Shared, Unique | Unique, Shared -> false

    let join a b =
      match a, b with
      | Shared, _ | _, Shared -> Shared
      | Unique, Unique -> Unique

    let meet a b =
      match a, b with
      | Unique, _ | _, Unique -> Unique
      | Shared, Shared -> Shared

    let print ppf = function
      | Shared -> Format.fprintf ppf "Shared"
      | Unique -> Format.fprintf ppf "Unique"
  end

  include Solver (Const)

  let constrain_legacy = constrain_upper

  let unique = of_const Const.Unique

  let shared = of_const Const.Shared

  let legacy = shared
end

module Linearity = struct
  module Const = struct
    type t =
      | Many
      | Once

    let legacy = Many

    let min = Many

    let max = Once

    let le a b =
      match a, b with Many, _ | _, Once -> true | Once, Many -> false

    let eq a b =
      match a, b with
      | Many, Many | Once, Once -> true
      | Once, Many | Many, Once -> false

    let join a b =
      match a, b with Once, _ | _, Once -> Once | Many, Many -> Many

    let meet a b =
      match a, b with Many, _ | _, Many -> Many | Once, Once -> Once

    let print ppf = function
      | Once -> Format.fprintf ppf "Once"
      | Many -> Format.fprintf ppf "Many"

    let to_dual : t -> Uniqueness.Const.t = function
      | Once -> Unique
      | Many -> Shared

    let of_dual : Uniqueness.Const.t -> t = function
      | Unique -> Once
      | Shared -> Many
  end

  include DualSolver (Uniqueness.Const) (Uniqueness) (Const)

  let once = of_const Once

  let many = of_const Many

  let legacy = many

  let constrain_legacy = constrain_lower
end

module Alloc = struct
  module Const = struct
    type t = (Locality.Const.t, Uniqueness.Const.t, Linearity.Const.t) modes

    module Option = struct
      type some = t

      type t =
        ( Locality.Const.t option,
          Uniqueness.Const.t option,
          Linearity.Const.t option )
        modes

      let none = { locality = None; uniqueness = None; linearity = None }

      let value opt ~default =
        let locality = Option.value opt.locality ~default:default.locality in
        let uniqueness =
          Option.value opt.uniqueness ~default:default.uniqueness
        in
        let linearity = Option.value opt.linearity ~default:default.linearity in
        { locality; uniqueness; linearity }
    end

    let legacy =
      { locality = Locality.Const.legacy;
        uniqueness = Uniqueness.Const.legacy;
        linearity = Linearity.Const.legacy
      }

    let join { locality = loc1; uniqueness = u1; linearity = lin1 }
        { locality = loc2; uniqueness = u2; linearity = lin2 } =
      { locality = Locality.Const.join loc1 loc2;
        uniqueness = Uniqueness.Const.join u1 u2;
        linearity = Linearity.Const.join lin1 lin2
      }

    (** constrain uncurried function ret_mode from arg_mode *)
    let close_over arg_mode =
      let locality = arg_mode.locality in
      (* uniqueness of the returned function is not constrained *)
      let uniqueness = Uniqueness.Const.min in
      let linearity =
        Linearity.Const.join arg_mode.linearity
          (* In addition, unique argument make the returning function once.
             In other words, if argument <= unique, returning function >= once.
             That is, returning function >= (dual of argument) *)
          (Linearity.Const.of_dual arg_mode.uniqueness)
      in
      { locality; uniqueness; linearity }

    (** constrain uncurried function ret_mode from the mode of the whole
    function *)
    let partial_apply alloc_mode =
      let locality = alloc_mode.locality in
      let uniqueness = Uniqueness.Const.min in
      let linearity = alloc_mode.linearity in
      { locality; uniqueness; linearity }

    let min =
      { locality = Locality.Const.min;
        uniqueness = Uniqueness.Const.min;
        linearity = Linearity.Const.min
      }

    let min_with_uniqueness uniqueness = { min with uniqueness }
  end

  type t = (Locality.t, Uniqueness.t, Linearity.t) modes

  let of_const { locality; uniqueness; linearity } : t =
    { locality = Locality.of_const locality;
      uniqueness = Uniqueness.of_const uniqueness;
      linearity = Linearity.of_const linearity
    }

  let prod locality uniqueness linearity = { locality; uniqueness; linearity }

  let legacy =
    { locality = Locality.legacy;
      uniqueness = Uniqueness.legacy;
      linearity = Linearity.legacy
    }

  let local = { legacy with locality = Locality.local }

  let unique = { legacy with uniqueness = Uniqueness.unique }

  let local_unique = { local with uniqueness = Uniqueness.unique }

  let is_const { locality; uniqueness; linearity } =
    Locality.is_const locality
    && Uniqueness.is_const uniqueness
    && Linearity.is_const linearity

  let min_mode : t =
    { locality = Locality.min_mode;
      uniqueness = Uniqueness.min_mode;
      linearity = Linearity.min_mode
    }

  let max_mode : t =
    { locality = Locality.max_mode;
      uniqueness = Uniqueness.max_mode;
      linearity = Linearity.max_mode
    }

  let locality t = t.locality

  let uniqueness t = t.uniqueness

  let linearity t = t.linearity

  type error =
    [ `Locality
    | `Uniqueness
    | `Linearity ]

  let submode { locality = loc1; uniqueness = u1; linearity = lin1 }
      { locality = loc2; uniqueness = u2; linearity = lin2 } =
    match Locality.submode loc1 loc2 with
    | Ok () -> (
      match Uniqueness.submode u1 u2 with
      | Ok () -> (
        match Linearity.submode lin1 lin2 with
        | Ok () -> Ok ()
        | Error () -> Error `Linearity)
      | Error () -> Error `Uniqueness)
    | Error () -> Error `Locality

  let submode_exn ({ locality = loc1; uniqueness = u1; linearity = lin1 } : t)
      ({ locality = loc2; uniqueness = u2; linearity = lin2 } : t) =
    Locality.submode_exn loc1 loc2;
    Uniqueness.submode_exn u1 u2;
    Linearity.submode_exn lin1 lin2

  let equate ({ locality = loc1; uniqueness = u1; linearity = lin1 } : t)
      ({ locality = loc2; uniqueness = u2; linearity = lin2 } : t) =
    match Locality.equate loc1 loc2 with
    | Ok () -> (
      match Uniqueness.equate u1 u2 with
      | Ok () -> (
        match Linearity.equate lin1 lin2 with
        | Ok () -> Ok ()
        | Error () -> Error `Linearity)
      | Error () -> Error `Uniqueness)
    | Error () -> Error `Locality

  let join ms : t =
    { locality = Locality.join (List.map (fun (t : t) -> t.locality) ms);
      uniqueness = Uniqueness.join (List.map (fun (t : t) -> t.uniqueness) ms);
      linearity = Linearity.join (List.map (fun (t : t) -> t.linearity) ms)
    }

  let constrain_upper { locality; uniqueness; linearity } =
    { locality = Locality.constrain_upper locality;
      uniqueness = Uniqueness.constrain_upper uniqueness;
      linearity = Linearity.constrain_upper linearity
    }

  let constrain_lower { locality; uniqueness; linearity } =
    { locality = Locality.constrain_lower locality;
      uniqueness = Uniqueness.constrain_lower uniqueness;
      linearity = Linearity.constrain_lower linearity
    }

  (* constrain to the legacy modes*)
  let constrain_legacy { locality; uniqueness; linearity } =
    { locality = Locality.constrain_legacy locality;
      uniqueness = Uniqueness.constrain_legacy uniqueness;
      linearity = Linearity.constrain_legacy linearity
    }

  let newvar () =
    { locality = Locality.newvar ();
      uniqueness = Uniqueness.newvar ();
      linearity = Linearity.newvar ()
    }

  let newvar_below { locality; uniqueness; linearity } =
    let locality, changed1 = Locality.newvar_below locality in
    let uniqueness, changed2 = Uniqueness.newvar_below uniqueness in
    let linearity, changed3 = Linearity.newvar_below linearity in
    { locality; uniqueness; linearity }, changed1 || changed2 || changed3

  let newvar_below_comonadic { locality; uniqueness; linearity } =
    let locality, changed1 = Locality.newvar_below locality in
    let linearity, changed2 = Linearity.newvar_below linearity in
    { locality; uniqueness; linearity }, changed1 || changed2

  let newvar_above { locality; uniqueness; linearity } =
    let locality, changed1 = Locality.newvar_above locality in
    let uniqueness, changed2 = Uniqueness.newvar_above uniqueness in
    let linearity, changed3 = Linearity.newvar_above linearity in
    { locality; uniqueness; linearity }, changed1 || changed2 || changed3

  let of_uniqueness uniqueness =
    { locality = Locality.newvar ();
      uniqueness;
      linearity = Linearity.newvar ()
    }

  let of_locality locality =
    { locality;
      uniqueness = Uniqueness.newvar ();
      linearity = Linearity.newvar ()
    }

  let of_linearity linearity =
    { locality = Locality.newvar ();
      uniqueness = Uniqueness.newvar ();
      linearity
    }

  let with_locality locality t = { t with locality }

  let with_uniqueness uniqueness t = { t with uniqueness }

  let with_linearity linearity t = { t with linearity }

  let check_const { locality; uniqueness; linearity } =
    { locality = Locality.check_const locality;
      uniqueness = Uniqueness.check_const uniqueness;
      linearity = Linearity.check_const linearity
    }

  let print' ?(verbose = true) ppf { locality; uniqueness; linearity } =
    Format.fprintf ppf "%a, %a, %a"
      (Locality.print' ~verbose ~label:"locality")
      locality
      (Uniqueness.print' ~verbose ~label:"uniqueness")
      uniqueness
      (Linearity.print' ~verbose ~label:"linearity")
      linearity

  let print ppf m = print' ~verbose:true ppf m

  (** constrain uncurried function ret_mode from arg_mode *)
  let close_over arg_mode =
    let locality = arg_mode.locality in
    (* uniqueness of the returned function is not constrained *)
    let uniqueness = Uniqueness.of_const Uniqueness.Const.min in
    let linearity =
      Linearity.join
        [ arg_mode.linearity;
          (* In addition, unique argument make the returning function once.
             In other words, if argument <= unique, returning function >= once.
             That is, returning function >= (dual of argument) *)
          Linearity.of_dual arg_mode.uniqueness ]
    in
    { locality; uniqueness; linearity }

  (** constrain uncurried function ret_mode from the mode of the whole function
  *)
  let partial_apply alloc_mode =
    let locality = alloc_mode.locality in
    let uniqueness = Uniqueness.of_const Uniqueness.Const.min in
    let linearity = alloc_mode.linearity in
    { locality; uniqueness; linearity }
end

module Value = struct
  module Const = struct
    type t = (Regionality.Const.t, Uniqueness.Const.t, Linearity.Const.t) modes

    let r_as_l : t -> Alloc.Const.t = function
      | { locality; uniqueness; linearity } ->
        let locality = Regionality.Const.r_as_l locality in
        { locality; uniqueness; linearity }
      [@@warning "-unused-value-declaration"]

    let r_as_g : t -> Alloc.Const.t = function
      | { locality; uniqueness; linearity } ->
        let locality = Regionality.Const.r_as_g locality in
        { locality; uniqueness; linearity }
      [@@warning "-unused-value-declaration"]
  end

  type t = (Regionality.t, Uniqueness.t, Linearity.t) modes

  let legacy =
    { locality = Regionality.legacy;
      uniqueness = Uniqueness.legacy;
      linearity = Linearity.legacy
    }

  let regional = { legacy with locality = Regionality.regional }

  let local = { legacy with locality = Regionality.local }

  let unique = { legacy with uniqueness = Uniqueness.unique }

  let regional_unique = { regional with uniqueness = Uniqueness.unique }

  let local_unique = { local with uniqueness = Uniqueness.unique }

  let of_const { locality; uniqueness; linearity } =
    { locality = Regionality.of_const locality;
      uniqueness = Uniqueness.of_const uniqueness;
      linearity = Linearity.of_const linearity
    }

  let max_mode =
    let locality = Regionality.max_mode in
    let uniqueness = Uniqueness.max_mode in
    let linearity = Linearity.max_mode in
    { locality; uniqueness; linearity }

  let min_mode =
    let locality = Regionality.min_mode in
    let uniqueness = Uniqueness.min_mode in
    let linearity = Linearity.min_mode in
    { locality; uniqueness; linearity }

  let locality t = t.locality

  let uniqueness t = t.uniqueness

  let linearity t = t.linearity

  let min_with_uniqueness u = { min_mode with uniqueness = u }

  let max_with_uniqueness u = { max_mode with uniqueness = u }

  let min_with_locality locality = { min_mode with locality }

  let max_with_locality locality = { max_mode with locality }

  let min_with_linearity linearity = { min_mode with linearity }

  let with_locality locality t = { t with locality }

  let with_uniqueness uniqueness t = { t with uniqueness }

  let with_linearity linearity t = { t with linearity }

  let to_local t = { t with locality = Regionality.local }

  let to_global t = { t with locality = Regionality.global }

  let to_unique t = { t with uniqueness = Uniqueness.unique }

  let to_shared t = { t with uniqueness = Uniqueness.shared }

  let to_once t = { t with linearity = Linearity.once }

  let to_many t = { t with linearity = Linearity.many }

  let of_alloc { locality; uniqueness; linearity } =
    let locality = Regionality.of_locality locality in
    { locality; uniqueness; linearity }

  let local_to_regional t =
    { t with locality = Regionality.local_to_regional t.locality }

  let regional_to_global t =
    { t with locality = Regionality.regional_to_global t.locality }

  let regional_to_local t =
    { t with locality = Regionality.regional_to_local t.locality }

  let global_to_regional t =
    { t with locality = Regionality.global_to_regional t.locality }

  let regional_to_global_alloc t =
    { t with locality = Regionality.regional_to_global_locality t.locality }

  let regional_to_local_alloc t =
    { t with locality = Regionality.regional_to_local_locality t.locality }

  let regional_to_global_locality t =
    Regionality.regional_to_global_locality t.locality

  let regional_to_local_locality t =
    Regionality.regional_to_local_locality t.locality

  type error =
    [ `Regionality
    | `Locality
    | `Uniqueness
    | `Linearity ]

  let submode t1 t2 =
    match Regionality.submode t1.locality t2.locality with
    | Error _ as e -> e
    | Ok () -> (
      match Uniqueness.submode t1.uniqueness t2.uniqueness with
      | Error () -> Error `Uniqueness
      | Ok () -> (
        match Linearity.submode t1.linearity t2.linearity with
        | Error () -> Error `Linearity
        | Ok () as ok -> ok))

  let submode_exn t1 t2 =
    match submode t1 t2 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate ({ locality = loc1; uniqueness = u1; linearity = lin1 } : t)
      ({ locality = loc2; uniqueness = u2; linearity = lin2 } : t) =
    match Regionality.equate loc1 loc2 with
    | Ok () -> (
      match Uniqueness.equate u1 u2 with
      | Ok () -> (
        match Linearity.equate lin1 lin2 with
        | Ok () -> Ok ()
        | Error () -> Error `Linearity)
      | Error () -> Error `Uniqueness)
    | Error e -> Error e

  let rec submode_meet t = function
    | [] -> Ok ()
    | t' :: rest -> (
      match submode t t' with
      | Ok () -> submode_meet t rest
      | Error _ as err -> err)

  let join ts =
    let locality = Regionality.join (List.map (fun t -> t.locality) ts) in
    let uniqueness = Uniqueness.join (List.map (fun t -> t.uniqueness) ts) in
    let linearity = Linearity.join (List.map (fun t -> t.linearity) ts) in
    { locality; uniqueness; linearity }

  let constrain_upper t =
    let locality = Regionality.constrain_upper t.locality in
    let uniqueness = Uniqueness.constrain_upper t.uniqueness in
    let linearity = Linearity.constrain_upper t.linearity in
    { locality; uniqueness; linearity }

  let constrain_lower t =
    let locality = Regionality.constrain_lower t.locality in
    let uniqueness = Uniqueness.constrain_lower t.uniqueness in
    let linearity = Linearity.constrain_lower t.linearity in
    { locality; uniqueness; linearity }

  let newvar () =
    let locality = Regionality.newvar () in
    let uniqueness = Uniqueness.newvar () in
    let linearity = Linearity.newvar () in
    { locality; uniqueness; linearity }

  let newvar_below { locality; uniqueness; linearity } =
    let locality, changed1 = Regionality.newvar_below locality in
    let uniqueness, changed2 = Uniqueness.newvar_below uniqueness in
    let linearity, changed3 = Linearity.newvar_below linearity in
    { locality; uniqueness; linearity }, changed1 || changed2 || changed3

  let newvar_above { locality; uniqueness; linearity } =
    let locality, changed1 = Regionality.newvar_above locality in
    let uniqueness, changed2 = Uniqueness.newvar_above uniqueness in
    let linearity, changed3 = Linearity.newvar_above linearity in
    { locality; uniqueness; linearity }, changed1 || changed2 || changed3

  let check_const t =
    let locality = Regionality.check_const t.locality in
    let uniqueness = Uniqueness.check_const t.uniqueness in
    let linearity = Linearity.check_const t.linearity in
    { locality; uniqueness; linearity }

  let print' ?(verbose = true) ppf t =
    Format.fprintf ppf "%a, %a, %a"
      (Regionality.print' ~verbose ~label:"locality")
      t.locality
      (Uniqueness.print' ~verbose ~label:"uniqueness")
      t.uniqueness
      (Linearity.print' ~verbose ~label:"linearity")
      t.linearity

  let print ppf t = print' ~verbose:true ppf t
end
