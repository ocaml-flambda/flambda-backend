(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Solver_intf

module Magic_allow_disallow (X : Allow_disallow) :
  Allow_disallow with type ('a, 'b, 'd) sided = ('a, 'b, 'd) X.sided = struct
  type ('a, 'b, 'd) sided = ('a, 'b, 'd) X.sided

  let disallow_right :
      type a b l r. (a, b, l * r) sided -> (a, b, l * disallowed) sided =
    Obj.magic

  let disallow_left :
      type a b l r. (a, b, l * r) sided -> (a, b, disallowed * r) sided =
    Obj.magic

  let allow_right :
      type a b l r. (a, b, l * allowed) sided -> (a, b, l * r) sided =
    Obj.magic

  let allow_left :
      type a b l r. (a, b, allowed * r) sided -> (a, b, l * r) sided =
    Obj.magic
end
[@@inline]

type 'a error =
  { left : 'a;
    right : 'a
  }

(** Map the function to the list, and returns the first [Error] found;
    Returns [Ok ()] if no error. *)
let rec find_error (f : 'x -> ('a, 'b) Result.t) : 'x list -> ('a, 'b) Result.t
    = function
  | [] -> Ok ()
  | x :: rest -> (
    match f x with Ok () -> find_error f rest | Error _ as e -> e)

module Solver_mono (C : Lattices_mono) = struct

  type 'a var =
    { mutable level : int;
      (** The level of the variable. This has the same meaning as
          the level field of a [type_expr]. *)
      mutable vlower : 'a lmorphvar list;
      (** A list of variables directly under the current variable.
          Each is a pair [f] [v], and we have [f v <= u] where [u] is the current
          variable. All variables in this list have a level that is less than
          or equal to the current variable.
          TODO: consider using hashset for quicker deduplication *)
      mutable vupper : 'a rmorphvar list;
      (** A list of variables directly above the current variable.
          Each is a pair [f] [v], and we have [u <= f v] where [u] is the current
          variable. *)
      mutable lower : 'a;
      (** The *conservative* lower bound of the variable.  Why
          conservative: if a user calls [submode c u] where [c] is some
          constant and [u] some variable, we can modify [u.lower] of
          course.  Idealy we should also modify all [v.lower] where [v]
          is variable above [u]. However, we only have either [vlower]
          or [vupper], not both. Therefore, the [lower] of some higher
          variables are not updated immediately, and hence
          conservative. Those [lower] of higher variables can be made
          precise later on demand, see [zap_to_floor_var_aux]. *)
      mutable upper : 'a;
      (** The conservative upper bound of the variable. It is conservative
          for similar reasons to [lower]. *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], [u.level <= v.level]
         - For any [v] and [f u \in v.vupper], [u.level < v.level]
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper],
           but not necessarily [f u.lower <= v.lower].
         - For any [v] and [f u \in v.vupper], we have [v.lower <= f u.lower],
           but not necessarily [v.upper <= f u.upper].
         - for all [f u \in v.vlower] and [g w \in v.vupper] we
           have either [g'f \in w.vlower] or [f'g \in u.vupper]. *)
      id : int;  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and 'b rmorphvar = ('b, right_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar : 'a var * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a -> change
    | Clower : 'a var * 'a -> change
    | Cvlower : 'a var * 'a lmorphvar list -> change
    | Cvupper : 'a var * 'a rmorphvar list -> change
    | Clevel : 'a var * int -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper) -> v.upper <- upper
    | Clower (v, lower) -> v.lower <- lower
    | Cvlower (v, vlower) -> v.vlower <- vlower
    | Cvupper (v, vupper) -> v.vupper <- vupper
    | Clevel (v, level) -> v.level <- level

  let empty_changes = []

  let undo_changes l = List.iter undo_change l

  (** [append_changes l0 l1] returns a log that's equivalent to [l0] followed by
  [l1]. *)
  let append_changes l0 l1 = l1 @ l0

  type ('a, 'd) mode =
    | Amode : 'a -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a * ('a, 'l * disallowed) morphvar list
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a [mv0, mv1, ..]] represents [a join mv0 join mv1 join ..] *)
    | Amodemeet :
        'a * ('a, disallowed * 'r) morphvar list
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a [mv0, mv1, ..]] represents [a meet mv0 meet mv1 meet ..]. *)

  (** Prints a mode variable, including the set of variables related to it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice containing three elements A = {0, 1, 2} with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2

      Note that f has a left adjoint, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      f x <= x
      this would result in adding (f, x) into the [vlower] of [x]. That is,
      there will be a self-loop on [x].
      *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
     Format.fprintf ppf "%x<%d>" v.id v.level;
     if (not (C.le obj v.lower (C.min obj)))
        || (not (C.le obj (C.max obj) v.upper)) then
       Format.fprintf ppf "[%a--%a]"
         (C.print obj) v.lower (C.print obj) v.upper;
     match traversed with
     | None -> ()
     | Some traversed ->
       if VarSet.mem v.id traversed
       then ()
       else if v.vlower <> [] || v.vupper <> [] then begin
         let traversed = VarSet.add v.id traversed in
         let pp_mv ppf m = print_morphvar ~traversed obj ppf m in
         Format.fprintf ppf "{%a--%a}" (Format.pp_print_list pp_mv) v.vlower
           (Format.pp_print_list pp_mv) v.vupper
       end

  and print_morphvar :
      type a d. ?traversed:VarSet.t -> a C.obj -> _ -> (a, d) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f)) ->
    let src = C.src dst f in
    Format.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src)
      v

  let print_raw :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = false) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else None in
    match m with
    | Amode a -> C.print obj ppf a
    | Amodevar mv -> print_morphvar ?traversed obj ppf mv
    | Amodejoin (a, mvs) ->
      Format.fprintf ppf "join(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        mvs
    | Amodemeet (a, mvs) ->
      Format.fprintf ppf "meet(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        mvs

  module Morphvar = Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) morphvar constraint 'd = 'l * 'r

    let allow_left :
        type a l r. (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_left m)

    let allow_right :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m) -> Amorphvar (v, C.allow_right m)

    let disallow_left :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_left m)

    let disallow_right :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m) -> Amorphvar (v, C.disallow_right m)
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, mvs) -> Amodejoin (c, List.map Morphvar.allow_left mvs)

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, mvs) -> Amodemeet (c, List.map Morphvar.allow_right mvs)

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, mvs) -> Amodejoin (c, List.map Morphvar.disallow_left mvs)
      | Amodemeet (c, mvs) -> Amodemeet (c, List.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode c -> Amode c
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, mvs) -> Amodejoin (c, List.map Morphvar.disallow_right mvs)
      | Amodemeet (c, mvs) -> Amodemeet (c, List.map Morphvar.disallow_right mvs)
  end)

  let mlower dst (Amorphvar (var, morph)) = C.apply dst morph (var).lower

  let mupper dst (Amorphvar (var, morph)) = C.apply dst morph (var).upper

  let min (type a) (obj : a C.obj) = Amode (C.min obj)

  let max (type a) (obj : a C.obj) = Amode (C.max obj)

  let of_const a = Amode a

  let apply_morphvar dst morph (Amorphvar (var, morph')) =
    Amorphvar (var, C.compose dst morph morph')

  let apply :
      type a b l r.
      b C.obj -> (a, b, l * r) C.morph -> (a, l * r) mode -> (b, l * r) mode =
   fun dst morph m ->
    match m with
    | Amode a -> Amode (C.apply dst morph a)
    | Amodevar mv -> Amodevar (apply_morphvar dst morph mv)
    | Amodejoin (a, vs) ->
      Amodejoin (C.apply dst morph a, List.map (apply_morphvar dst morph) vs)
    | Amodemeet (a, vs) ->
      Amodemeet (C.apply dst morph a, List.map (apply_morphvar dst morph) vs)

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower) :: !log);
    v.lower <- C.join obj v.lower a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper) :: !log);
    v.upper <- C.meet obj v.upper a

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  (** When called, graph must be fixed so maintain INVARIANT *)
  let set_level ~log v level =
    (match log with
    | None -> ()
    | Some log -> log := Clevel (v, v.level) :: !log);
    v.level <- level

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vupper ~log v vupper =
    (match log with
    | None -> ()
    | Some log -> log := Cvupper (v, v.vupper) :: !log);
    v.vupper <- vupper

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly lower) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_cv : type a. log:_ -> a C.obj -> a -> a var -> (unit, a) Result.t
      =
    fun (type a) ~log (obj : a C.obj) a' v ->
     if C.le obj a' v.lower
     then Ok ()
     else if not (C.le obj a' v.upper)
     then Error v.upper
     else (
       update_lower ~log obj v a';
       let r =
         v.vupper
         |> find_error (fun mu ->
                let r = submode_cmv ~log obj a' mu in
                (if Result.is_ok r && v.vlower = []
                then
                  (* Optimization: update [v.upper] based on [mupper u].*)
                  let mu_upper = mupper obj mu in
                  if not (C.le obj mu_upper v.upper)
                  then update_upper ~log obj v mu_upper);
                r)
       in
       if C.le obj v.upper v.lower then begin
         set_vlower ~log v [];
         set_vupper ~log v [];
       end;
       r)

  and submode_cmv :
      type a l.
      log:_ -> a C.obj -> a -> (a, l * allowed) morphvar -> (unit, a) Result.t =
   fun ~log obj a (Amorphvar (v, f) as mv) ->
    let mlower = mlower obj mv in
    let mupper = mupper obj mv in
    if C.le obj a mlower
    then Ok ()
    else if not (C.le obj a mupper)
    then Error mupper
    else
      (* At this point we know [a <= f v], therefore [a] is in the downward
         closure of [f]'s image. Therefore, asking [a <= f v] is equivalent to
         asking [f' a <= v]. *)
      let f' = C.left_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_cv ~log src a' v with
      | Ok () -> Ok ()
      | Error e -> Error (C.apply obj f e)

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly higher) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_vc :
      type a. log:_ -> a C.obj -> a var -> a -> (unit, a) Result.t =
    fun (type a) ~log (obj : a C.obj) v a' ->
     if C.le obj v.upper a'
     then Ok ()
     else if not (C.le obj v.lower a')
     then Error v.lower
     else (
       update_upper ~log obj v a';
       let r =
         v.vlower
         |> find_error (fun mu ->
                let r = submode_mvc ~log obj mu a' in
                (if Result.is_ok r && v.vupper = []
                then
                  (* Optimization: update [v.lower] based on [mlower u].*)
                  let mu_lower = mlower obj mu in
                  if not (C.le obj mu_lower v.lower)
                  then update_lower ~log obj v mu_lower);
                r)
       in
       if C.le obj v.upper v.lower then begin
         set_vlower ~log v [];
         set_vupper ~log v []
       end;
       r)

  and submode_mvc :
        'a 'r.
        log:change list ref option ->
        'a C.obj ->
        ('a, allowed * 'r) morphvar ->
        'a ->
        (unit, 'a) Result.t =
   fun ~log obj (Amorphvar (v, f) as mv) a ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    let mupper = mupper obj mv in
    let mlower = mlower obj mv in
    if C.le obj mupper a
    then Ok ()
    else if not (C.le obj mlower a)
    then Error mlower
    else
      let f' = C.right_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~log src v a' with
      | Ok () -> Ok ()
      | Error e -> Error (C.apply obj f e)

  let eq_morphvar :
      type a l0 r0 l1 r1.
        a C.obj -> (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool
      =
   fun dst (Amorphvar (v0, f0) as mv0) (Amorphvar (v1, f1) as mv1) ->
    (* To align l0/l1, r0/r1; The existing disallow_left/right] is for [mode],
       not [morphvar]. *)
    Morphvar.(
      disallow_left (disallow_right mv0) == disallow_left (disallow_right mv1))
    || match C.eq_morph dst f0 f1 with None -> false | Some Refl -> v0 == v1

  let exists dst mu mvs = List.exists (fun mv -> eq_morphvar dst mv mu) mvs

  let rec submode_mvmv
          : type a l r.
                 log:_ -> a C.obj
                 -> (a, allowed * r) morphvar -> (a, l * allowed) morphvar
                 -> (_, a * a) result
    = fun ~log dst (Amorphvar (v, f) as mv) (Amorphvar (u, g) as mu) ->
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar dst mv mu
    then Ok ()
    else
      (* The call f v <= g u translates to the following step:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. If v.level <= u.level, for all (h w) in u.vupper, g' (f v) <= h w
         4. If v.level <= u.level adding g' (f v) to u.vlower, where g' is the left adjoint of g
         5. If v.level > u.level, for all (h w) in v.vlower, h w <= f' (g u)
         6. If v.level > u.level adding f' (g u) to v.vupper, where f' is the right adjoint of f
         Steps 3 and 4 are implemented by [add_vlower], steps 5 and 6 by [add_vupper].
      *)
      match submode_mvc ~log dst mv (mupper dst mu) with
      | Error a -> Error (a, mupper dst mu)
      | Ok () ->
          match submode_cmv ~log dst (mlower dst mv) mu with
          | Error a -> Error (mlower dst mv, a)
          | Ok () ->
              if v.level <= u.level then
                add_vlower ~log dst v f mv u g
              else
                add_vupper ~log dst v f u g mu

  (* Add a vlower entry for the relationship [f v <= g u] if necessary. *)
  and add_vlower
      : type a b c l r. log:_ -> b C.obj
             -> a var -> (a, b, allowed * r) C.morph
             -> (b, allowed * r) morphvar
             -> c var -> (c, b, l * allowed) C.morph
             -> (_, b * b) result =
    fun ~log dst v f mv u g ->
      let g' = C.left_adjoint dst g in
      let src = C.src dst g in
      let g'f = C.compose src g' (C.disallow_right f) in
      let x = Amorphvar (v, g'f) in
      if exists src x u.vlower then Ok ()
      else begin
        set_vlower ~log u (x :: u.vlower);
        find_error
          (fun (Amorphvar(w, h)) ->
            let gh = C.compose dst (C.disallow_left g) h in
            let y = Amorphvar (w, gh) in
            submode_mvmv ~log dst mv y)
          u.vupper
      end

  (* Add a vupper entry for the relationship [f v <= g u] if necessary. *)
  and add_vupper
      : type a b c l r. log:_ -> b C.obj
             -> a var -> (a, b, allowed * r) C.morph
             -> c var -> (c, b, l * allowed) C.morph
             -> (b, l * allowed) morphvar
             -> (_, b * b) result =
    fun ~log dst v f u g mu ->
      let f' = C.right_adjoint dst f in
      let src = C.src dst f in
      let f'g = C.compose src f' (C.disallow_left g) in
      let x = Amorphvar (u, f'g) in
      if exists src x v.vupper then Ok ()
      else begin
        set_vupper ~log v (x :: v.vupper);
        find_error
          (fun (Amorphvar(w, h)) ->
             let fh = C.compose dst (C.disallow_right f) h in
             let y = Amorphvar (w, fh) in
             submode_mvmv ~log dst y mu)
          v.vlower
      end

  (* Tighten the lower bound of [u] based on the lower bound of [f' v].
  No recursion into [u.vuppers] *)
  let push_lower_bound :
    type a b l r. log:_ -> b C.obj -> a var
          -> (a, b, (l * r)) C.morph
          -> b var
          -> unit =
    fun ~log dst v f' u ->
      let mlower = mlower dst (Amorphvar (v, f')) in
      let ulower = C.join dst u.lower mlower in
      update_lower ~log dst u ulower

  (* Tighten the upper bound of [u] based on the upper bound of [f' v].
  No recursion into [u.vlowers] *)
  let push_upper_bound :
    type a b l r. log:_ -> b C.obj -> a var
          -> (a, b, (l * r)) C.morph
          -> b var
          -> unit =
    fun ~log dst v f' u ->
      let mupper = mupper dst (Amorphvar (v, f')) in
      let uupper = C.meet dst u.upper mupper in
      update_upper ~log dst u uupper

  let add_vlower_nocheck :
    type a b r. log:_ -> a C.obj -> a var
          -> b var -> (b, a, (allowed * r)) C.morph
          -> unit =
    fun ~log dst v u f ->
      let x = Amorphvar (u, (C.disallow_right f)) in
      if exists dst x v.vlower then ()
      else set_vlower ~log v (x :: v.vlower)


  let add_vupper_nocheck :
    type a b l. log:_ -> a C.obj -> a var
          -> b var -> (b, a, (l * allowed)) C.morph
          -> unit =
    fun ~log dst v u f ->
      let x = Amorphvar (u, C.disallow_left f) in
      if exists dst x v.vupper then ()
      else set_vupper ~log v (x :: v.vupper)

  (* Add a vlower entry for the relation [f u <= v], tighten the upper bound of [u],
  and recursively add relations to maintain invariant.
  The lower and upper bounds of [u] and [v] are not checked, upper bound is not pushed
  down [u.vlower] *)
  let rec add_vlower_reversed :
    type a b r. log:_ -> a C.obj -> a var
          -> b var -> (b, a, (allowed * r)) C.morph
          -> unit =
    fun ~log dst v u f ->
      let x = Amorphvar (u, (C.disallow_right f)) in
      if exists dst x v.vlower then ()
      else begin
        let src = C.src dst f in
        let f' = C.right_adjoint dst f in
        push_upper_bound ~log src v f' u;
        set_vlower ~log v (x :: v.vlower);
        List.iter
          (fun (Amorphvar(w, h)) ->
            if w.level < u.level then begin
              let f'h = C.compose src f' h in
              add_vupper_nocheck ~log src u w f'h
            end else begin
              let src = C.src dst h in
              let h' = C.left_adjoint dst h in
              let h'f = C.compose src h' (C.disallow_right f) in
              add_vlower_reversed ~log src w u h'f
            end
          )
          v.vupper
        end

    (* Add a vupper entry for the relation [v <= f u], tighten the lower bound of [u],
    and recursively add relations to maintain invariant.
    The lower and upper bounds of [u] and [v] are not checked, lower bound is not pushed
    down [u.vupper] *)
    let rec add_vupper_reversed :
      type a b l. log:_ -> a C.obj -> a var
            -> b var -> (b, a, (l * allowed)) C.morph
            -> unit =
      fun ~log dst v u f ->
        let x = Amorphvar (u, C.disallow_left f) in
        if exists dst x v.vupper then ()
        else begin
          let src = C.src dst f in
          let f' = C.left_adjoint dst f in
          push_lower_bound ~log src v f' u;
          set_vupper ~log v (x :: v.vupper);
          List.iter
            (fun (Amorphvar(w, h)) ->
              if u.level < w.level then begin
                let src = C.src dst h in
                let h' = C.right_adjoint dst h in
                let h'f = C.compose src h' (C.disallow_left f) in
                add_vupper_reversed ~log src w u h'f
              end else begin
                let f'h = C.compose src f' h in
                add_vlower_nocheck ~log src u w f'h
              end
            )
            v.vlower
          end

  let update_level_v
      : type a. log:_ -> a C.obj -> int -> a var -> unit =
    fun ~log dst level u ->
    if u.level > level then begin
      let (vupper_lt, vupper_ge) =
        List.partition (fun (Amorphvar(v, _)) -> v.level < level) u.vupper
      in
      let (vlower_le, vlower_gt) =
        List.partition (fun (Amorphvar(v, _)) -> v.level <= level) u.vlower
      in
      set_vlower ~log u vlower_le;
      set_vupper ~log u vupper_lt;
      set_level ~log u level;
      List.iter
        (fun (Amorphvar(v, f)) ->
          let f' = C.right_adjoint dst f in
          let src = C.src dst f in
          add_vupper_reversed ~log src v u f')
        vlower_gt;
      List.iter
        (fun (Amorphvar(v, f)) ->
          let f' = C.left_adjoint dst f in
          let src = C.src dst f in
          add_vlower_reversed ~log src v u f')
        vupper_ge;
      (* optimization: if lower = upper, we can remove vuppers and vlowers since the
        information is as precise as it can get *)
      if u.lower = u.upper then begin
        set_vlower ~log u [];
        set_vupper ~log u [];
      end
    end

  let cnt_id = ref 0

  let fresh ?upper ?lower ?vlower ?vupper ?level obj =
    let id = !cnt_id in
    cnt_id := id + 1;
    let level = Option.value level ~default:0 in
    let upper = Option.value upper ~default:(C.max obj) in
    let lower = Option.value lower ~default:(C.min obj) in
    let vlower = Option.value vlower ~default:[] in
    let vupper = Option.value vupper ~default:[] in
    { level; upper; lower; vlower; vupper; id }


  (* Moves every reachable variable from [u] such that
  [current_level] < [u.level] < [generic_level] to
  [generic_level + (u.level - current_level)], preserving the exact topology *)
  let rec generalize_topology
      : type a. log:_ -> a C.obj -> current_level:int ->
        generic_level:int -> a var -> unit =
    fun ~log dst ~current_level ~generic_level u ->
      if u.level <= current_level || u.level >= generic_level then ()
      else begin
        let new_level = generic_level + (u.level - current_level) in
        set_level ~log u new_level;
        let do_gen (Amorphvar (v, f)) =
          let src = C.src dst f in
          generalize_topology ~log src ~current_level ~generic_level v
        in
        List.iter do_gen u.vupper;
        List.iter do_gen u.vlower
      end

  (* Behaves like [generalize_topology], and additionally creates a copy of each reachable
  variable [u] such that [u] < [copy] and [copy] < [u] via submode, and lowers the [copy]
  to [current_level]. *)
  let rec generalize_topology_struct
      : type a. log:_ -> a C.obj -> current_level:int ->
        generic_level:int -> a var -> unit =
    fun ~log dst ~current_level ~generic_level u ->
      if u.level <= current_level || u.level >= generic_level then ()
      else begin
        let new_level = generic_level + (u.level - current_level) in
        set_level ~log u new_level;
        let do_gen (Amorphvar (v, f)) =
          let src = C.src dst f in
          generalize_topology_struct ~log src ~current_level ~generic_level v
        in
        List.iter do_gen u.vupper;
        List.iter do_gen u.vlower;
        (* If bounds are already tight, there is no need to create a copy *)
        if not (C.le dst u.upper u.lower) then begin
          let copy = fresh ~upper:u.upper ~lower:u.lower ~level:u.level dst in
          let ok1 = submode_mvmv ~log dst (Amorphvar (copy, C.id)) (Amorphvar (u, C.id)) in
          let ok2 = submode_mvmv ~log dst (Amorphvar (u, C.id)) (Amorphvar (copy, C.id)) in
          assert (Result.is_ok ok1 && Result.is_ok ok2);
          update_level_v ~log dst current_level copy
        end
      end

  let generalize_v
      : type a. log:_ -> a C.obj -> current_level:int ->
        generic_level:int -> a var -> unit =
    fun ~log dst ~current_level ~generic_level u ->
      generalize_topology ~log dst ~current_level ~generic_level u;
      update_level_v ~log dst generic_level u

  (* generalize_structure has three cases:
    (1) if bounds are tight, the var is moved to generic_level
    (2) if bounds are fully open, do nothing --- we consider only the case where bounds
        are precise by virtue of having no vupper/vlower
    (3) if bounds are non-trivial (neither tight nor fully open) we make a copy, move the
        original var to generic_level, and mark the two variables as equivalent by adding
        constraint arrows via [add_vlower] and [add_vupper]
  *)
  let generalize_structure_v
      : type a. log:_ -> a C.obj -> current_level:int ->
        generic_level:int -> a var -> unit =
    fun ~log dst ~current_level ~generic_level u ->
      if C.le dst u.upper u.lower then begin
        (* the bounds are tight *)
        generalize_topology ~log dst ~current_level ~generic_level u;
        update_level_v ~log dst generic_level u
      end else if
        C.le dst (C.max dst) u.upper &&
        C.le dst u.lower (C.min dst) &&
        u.vlower = [] && u.vupper = [] then
        (* the bounds are fully open *)
        update_level_v ~log dst current_level u
      else begin
        (* the bounds are non-trivial *)
        generalize_topology_struct ~log dst ~current_level ~generic_level u;
        update_level_v ~log dst generic_level u
      end

  let generalize (type a l r) ~current_level ~generic_level (obj : a C.obj)
      (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f)) ->
      let obj = C.src obj f in
      generalize_v ~log obj ~current_level ~generic_level v
    | Amode _ -> ()
    | Amodejoin (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          generalize_v ~log obj ~current_level ~generic_level v)
        mvs
    | Amodemeet (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          generalize_v ~log obj ~current_level ~generic_level v)
        mvs

  let generalize_structure (type a l r) ~current_level ~generic_level (obj : a C.obj)
      (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f)) ->
      let obj = C.src obj f in
      generalize_structure_v ~log obj ~current_level ~generic_level v
    | Amode _ -> ()
    | Amodejoin (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          generalize_structure_v ~log obj ~current_level ~generic_level v)
        mvs
    | Amodemeet (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          generalize_structure_v ~log obj ~current_level ~generic_level v)
        mvs


  let update_level (type a l r) (level : int) (obj : a C.obj) (a : (a, l * r) mode) ~log =
    match a with
    | Amodevar (Amorphvar (v, f)) ->
      let obj = C.src obj f in
      update_level_v ~log obj level v
    | Amode _ -> ()
    | Amodejoin (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          update_level_v ~log obj level v)
        mvs
    | Amodemeet (_, mvs) ->
      List.iter
        (fun (Amorphvar (v, f)) ->
          let obj = C.src obj f in
          update_level_v ~log obj level v)
        mvs

  let submode (type a r l) (obj : a C.obj) (a : (a, allowed * r) mode)
      (b : (a, l * allowed) mode) ~log =
    let submode_cc ~log:_ obj left right =
      if C.le obj left right then Ok () else Error { left; right }
    in
    let submode_mvc ~log obj v right =
      Result.map_error
        (fun left -> { left; right })
        (submode_mvc ~log obj v right)
    in
    let submode_cmv ~log obj left v =
      Result.map_error
        (fun right -> { left; right })
        (submode_cmv ~log obj left v)
    in
    let submode_mvmv ~log obj v u =
      Result.map_error
        (fun (left, right) -> { left; right })
        (submode_mvmv ~log obj v u)
    in
    match a, b with
    | Amode left, Amode right -> submode_cc ~log obj left right
    | Amodevar v, Amode right -> submode_mvc ~log obj v right
    | Amode left, Amodevar v -> submode_cmv ~log obj left v
    | Amodevar v, Amodevar u -> submode_mvmv ~log obj v u
    | Amode a, Amodemeet (b, mvs) ->
      Result.bind (submode_cc ~log obj a b) (fun () ->
          find_error (fun mv -> submode_cmv ~log obj a mv) mvs)
    | Amodevar mv, Amodemeet (b, mvs) ->
      Result.bind (submode_mvc ~log obj mv b) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv mv') mvs)
    | Amodejoin (a, mvs), Amode b ->
      Result.bind (submode_cc ~log obj a b) (fun () ->
          find_error (fun mv' -> submode_mvc ~log obj mv' b) mvs)
    | Amodejoin (a, mvs), Amodevar mv ->
      Result.bind (submode_cmv ~log obj a mv) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv' mv) mvs)
    | Amodejoin (a, mvs), Amodemeet (b, mus) ->
      (* TODO: mabye create a intermediate variable? *)
      Result.bind (submode_cc ~log obj a b) (fun () ->
          Result.bind
            (find_error (fun mv -> submode_mvc ~log obj mv b) mvs)
            (fun () ->
              Result.bind
                (find_error (fun mu -> submode_cmv ~log obj a mu) mus)
                (fun () ->
                  find_error
                    (fun mu ->
                      find_error (fun mv -> submode_mvmv ~log obj mv mu) mvs)
                    mus)))

  let cons_dedup dst x xs = if exists dst x xs then xs else x :: xs

  (* Similar to [List.rev_append] but dedup the result (assuming both inputs are
     deduped) *)
  let rev_append_dedup dst l0 l1 =
    let rec loop rest acc =
      match rest with [] -> acc | x :: xs -> loop xs (cons_dedup dst x acc)
    in
    loop l0 l1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, allowed * disallowed) morphvar list ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a mvs rest ->
      if C.le obj (C.max obj) a
      then Amode (C.max obj)
      else
        match rest with
        | [] -> Amodejoin (a, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode b -> loop (C.join obj a b) mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode] *)
          | Amodevar mv ->
            loop (C.join obj a (mlower obj mv)) (cons_dedup obj mv mvs) xs
          | Amodejoin (b, mvs') ->
            loop (C.join obj a b) (rev_append_dedup obj mvs' mvs) xs)
    in
    loop (C.min obj) [] l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, disallowed * allowed) morphvar list ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a mvs rest ->
      if C.le obj a (C.min obj)
      then Amode (C.min obj)
      else
        match rest with
        | [] -> Amodemeet (a, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode b -> loop (C.meet obj a b) mvs xs
          (* some minor optimization: if [a] is higher than [mupper mv], we
              should keep the latter instead. This helps to fail early in
             [submode_log] *)
          | Amodevar mv ->
            loop (C.meet obj a (mupper obj mv)) (cons_dedup obj mv mvs) xs
          | Amodemeet (b, mvs') ->
            loop (C.meet obj a b) (rev_append_dedup obj mvs' mvs) xs)
    in
    loop (C.max obj) [] l

  let get_conservative_ceil : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> mupper obj mv
    | Amodemeet (a, mvs) ->
      List.fold_left (fun acc mv -> C.meet obj acc (mupper obj mv)) a mvs
    | Amodejoin (a, mvs) ->
      List.fold_left (fun acc mv -> C.join obj acc (mupper obj mv)) a mvs

  let get_conservative_floor : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> mlower obj mv
    | Amodejoin (a, mvs) ->
      List.fold_left (fun acc mv -> C.join obj acc (mupper obj mv)) a mvs
    | Amodemeet (a, mvs) ->
      List.fold_left (fun acc mv -> C.meet obj acc (mlower obj mv)) a mvs

  (** Zap [mv] to its lower bound. Returns the [log] of the zapping, in
      case the caller are only interested in the lower bound and wants to
      reverse the zapping.

      As mentioned in [var], [mlower mv] is not precise; to get the precise
      lower bound of [mv], we call [submode mv (mlower mv)]. This will propagate
      to all its children, which might fail because some children's lower bound
      [a] is more up-to-date than [mv]. In that case, we call [submode mv a]. We
      repeat this process until no failure, and we will get the precise lower
      bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite.
      *)
  let zap_to_floor_morphvar_aux (type a r) (obj : a C.obj)
      (mv : (a, allowed * r) morphvar) =
    let rec loop lower =
      let log = ref empty_changes in
      let r = submode_mvc ~log:(Some log) obj mv lower in
      match r with
      | Ok () -> !log, lower
      | Error a ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop (mlower obj mv)

  (** Zap [mv] to its upper bound. Returns the [log] of the zapping, in
      case the caller are only interested in the lower bound and wants to
      reverse the zapping.

      [mupper mv] is not precise; to get the precise upper bound of
      [mv], we call [submode (mupper mv) mv ]. This will propagate to all
      its children, which might fail because some children's upper bound
      [a] is more up-to-date than [mv]. In that case, we call [submode
      a mv]. We repeat this process until no failure, and we will get
      the precise lower bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite.
      *)
  let zap_to_ceil_morphvar_aux (type a l) (obj : a C.obj)
      (mv : (a, l * allowed) morphvar) =
    let rec loop upper =
      let log = ref empty_changes in
      let r = submode_cmv ~log:(Some log) obj upper mv in
      match r with
      | Ok () -> !log, upper
      | Error a ->
        undo_changes !log;
        loop (C.meet obj a upper)
    in
    loop (mupper obj mv)

  (** Zaps a morphvar to its floor and returns the floor. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the floor without zapping. *)
  let zap_to_floor_morphvar obj mv ~commit =
    let log_, lower = zap_to_floor_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    lower

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:log
    | Amodejoin (a, mvs) ->
        let floor =
          List.fold_left
            (fun acc mv ->
               C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
            a mvs
        in
        List.iter
          (fun mv ->
             let ok = submode_mvc obj mv floor ~log in
             assert (Result.is_ok ok))
          mvs;
        floor

  (** Zaps a morphvar to its ceiling and returns the ceiling. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the ceiling without zapping. *)
  let zap_to_ceil_morphvar obj mv ~commit =
    let log_, upper = zap_to_ceil_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    upper

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:log
    | Amodemeet (a, mvs) ->
        let ceil =
          List.fold_left
            (fun acc mv ->
               C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
            a mvs
        in
        List.iter
          (fun mv ->
             let ok = submode_cmv obj ceil mv ~log in
             assert (Result.is_ok ok))
          mvs;
        ceil

  let get_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:None
    | Amodejoin (a, mvs) ->
      List.fold_left
        (fun acc mv ->
          C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
        a mvs

  let get_ceil : type a l. a C.obj -> (a, l * allowed) mode -> a =
   fun obj m ->
    match m with
    | Amode a -> a
    | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:None
    | Amodemeet (a, mvs) ->
      List.fold_left
        (fun acc mv ->
          C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
        a mvs

  let print :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?verbose (obj : a C.obj) ppf m ->
    let ceil = get_conservative_ceil obj m in
    let floor = get_conservative_floor obj m in
    if C.le obj ceil floor
    then C.print obj ppf ceil
    else print_raw ?verbose obj ppf m

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode a ->
      if C.le obj (C.max obj) a
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~lower:a obj, C.id)), true
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      let ok = submode_mvmv obj ~log:None mv mu in
      assert (Result.is_ok ok);
      allow_left (Amodevar mu), true
    | Amodejoin (a, mvs) ->
      let u = fresh ~lower:a obj in
      let mu = Amorphvar (u, C.id) in
      List.iter
        (fun mv ->
          let ok = submode_mvmv obj ~log:None mv mu in
          assert (Result.is_ok ok))
        mvs;
      allow_left (Amodevar mu), true

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode a ->
      if C.le obj a (C.min obj)
      then Amode a, false
      else Amodevar (Amorphvar (fresh ~upper:a obj, C.id)), true
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id) in
      let ok = submode_mvmv obj ~log:None mu mv in
      assert (Result.is_ok ok);
      allow_left (Amodevar mu), true
    | Amodemeet (a, mvs) ->
      let u = fresh ~upper:a obj in
      let mu = Amorphvar (u, C.id) in
      List.iter
        (fun mv ->
          let ok = submode_mvmv obj ~log:None mu mv in
          assert (Result.is_ok ok))
        mvs;
      allow_left (Amodevar mu), true
end
[@@inline always]

module Solvers_polarized (C : Lattices_mono) = struct
  module S = Solver_mono (C)

  type changes = S.changes

  let empty_changes = S.empty_changes

  let undo_changes = S.undo_changes

  module type Solver_polarized =
    Solver_polarized
      with type ('a, 'b, 'd) morph := ('a, 'b, 'd) C.morph
       and type 'a obj := 'a C.obj
       and type 'a error := 'a error
       and type changes := changes

  module rec Positive :
    (Solver_polarized
      with type 'd polarized = 'd pos
       and type ('a, 'd) mode_op = ('a, 'd) Negative.mode) = struct
    type 'd polarized = 'd pos

    type ('a, 'd) mode_op = ('a, 'd) Negative.mode

    type ('a, 'd) mode = ('a, 'd) S.mode constraint 'd = 'l * 'r

    include Magic_allow_disallow (S)

    let newvar = S.newvar

    let submode = S.submode

    let update_level = S.update_level

    let generalize = S.generalize

    let generalize_structure = S.generalize_structure

    let join = S.join

    let meet = S.meet

    let of_const _ = S.of_const

    let min = S.min

    let max = S.max

    let zap_to_floor = S.zap_to_floor

    let zap_to_ceil = S.zap_to_ceil

    let newvar_above = S.newvar_above

    let newvar_below = S.newvar_below

    let get_ceil = S.get_ceil

    let get_floor = S.get_floor

    let get_conservative_ceil = S.get_conservative_ceil

    let get_conservative_floor = S.get_conservative_floor

    let print ?(verbose = false) = S.print ~verbose

    let via_monotone = S.apply

    let via_antitone = S.apply
  end

  and Negative :
    (Solver_polarized
      with type 'd polarized = 'd neg
       and type ('a, 'd) mode_op = ('a, 'd) Positive.mode) = struct
    type 'd polarized = 'd neg

    type ('a, 'd) mode_op = ('a, 'd) Positive.mode

    type ('a, 'd) mode = ('a, 'r * 'l) S.mode constraint 'd = 'l * 'r

    include Magic_allow_disallow (struct
      type ('a, _, 'd) sided = ('a, 'd) mode

      let disallow_right = S.disallow_left

      let disallow_left = S.disallow_right

      let allow_right = S.allow_left

      let allow_left = S.allow_right
    end)

    let newvar = S.newvar

    let submode obj m0 m1 ~log =
      Result.map_error
        (fun { left; right } -> { left = right; right = left })
        (S.submode obj m1 m0 ~log)

    let update_level = S.update_level

    let generalize = S.generalize

    let generalize_structure = S.generalize_structure

    let join = S.meet

    let meet = S.join

    let of_const _ = S.of_const

    let min = S.max

    let max = S.min

    let zap_to_floor = S.zap_to_ceil

    let zap_to_ceil = S.zap_to_floor

    let newvar_above = S.newvar_below

    let newvar_below = S.newvar_above

    let get_ceil = S.get_floor

    let get_floor = S.get_ceil

    let get_conservative_ceil = S.get_conservative_floor

    let get_conservative_floor = S.get_conservative_ceil

    let print ?(verbose = false) = S.print ~verbose

    let via_monotone = S.apply

    let via_antitone = S.apply
  end

  (* Definitions to show that this solver works over a category. *)
  module Category = struct
    type 'a obj = 'a C.obj

    type ('a, 'b, 'd) morph = ('a, 'b, 'd) C.morph

    type ('a, 'd) mode =
      | Positive of ('a, 'd pos) Positive.mode
      | Negative of ('a, 'd neg) Negative.mode

    let apply_into_positive :
        type a b l r.
        b obj ->
        (a, b, l * r) morph ->
        (a, l * r) mode ->
        (b, l * r) Positive.mode =
     fun obj morph -> function
      | Positive mode -> Positive.via_monotone obj morph mode
      | Negative mode -> Positive.via_antitone obj morph mode

    let apply_into_negative :
        type a b l r.
        b obj ->
        (a, b, l * r) morph ->
        (a, l * r) mode ->
        (b, r * l) Negative.mode =
     fun obj morph -> function
      | Positive mode -> Negative.via_antitone obj morph mode
      | Negative mode -> Negative.via_monotone obj morph mode
  end
end
[@@inline always]
