open Solver_intf

(** Error returned by failed [submode a b]. [left] will be the lowest mode [a]
   can be, and [right] will be the highest mode [b] can be. And [left <= right]
   will be false, which is why the submode failed. *)
type 'a error =
  { left : 'a;
    right : 'a
  }

let rec find_error f = function
  | [] -> Ok ()
  | x :: rest -> (
    match f x with Ok () -> find_error f rest | Error _ as e -> e)

module VarSet = Set.Make (Int)

module Solver_mono (C : Lattices_mono) = struct
  type 'a var =
    { mutable upper : 'a;
      mutable lower : 'a;
      (* TODO: consider using hashset for quicker deduplication *)
      mutable vlower : 'a lmorphvar list
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar : 'a var * ('a, 'b, 'd) C.morph -> ('b, 'd) morphvar

  type change =
    | Cupper : 'a var * 'a -> change
    | Clower : 'a var * 'a -> change
    | Cvlower : 'a var * 'a lmorphvar list -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper) -> v.upper <- upper
    | Clower (v, lower) -> v.lower <- lower
    | Cvlower (v, vlower) -> v.vlower <- vlower

  let undo_changes l = List.iter undo_change l

  (* To be filled in *)
  let append_changes : (changes ref -> unit) ref = ref (fun _ -> assert false)

  type ('a, 'd) mode =
    | Amode : 'a -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a * ('a, allowed * disallowed) morphvar list
        -> ('a, allowed * disallowed) mode
    | Amodemeet :
        'a * ('a, disallowed * allowed) morphvar list
        -> ('a, disallowed * allowed) mode

  let address_of : 'a var -> int = Obj.magic

  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Format.fprintf ppf "%x[%a,%a]" (address_of v) (C.print obj) v.lower
      (C.print obj) v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem (address_of v) traversed
      then ()
      else
        let traversed = VarSet.add (address_of v) traversed in
        let p = print_morphvar ~traversed obj in
        Format.fprintf ppf "{%a}" (Format.pp_print_list p) v.vlower

  and print_morphvar :
      type a d. ?traversed:VarSet.t -> a C.obj -> _ -> (a, d) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f)) ->
    let src = C.src dst f in
    Format.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src)
      v

  let print_raw :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = true) (obj : a C.obj) ppf m ->
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

  let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode =
    Obj.magic

  let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode =
    Obj.magic

  let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
    Obj.magic

  let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
    Obj.magic

  let mlower dst (Amorphvar (var, morph)) = C.apply dst morph var.lower

  let mupper dst (Amorphvar (var, morph)) = C.apply dst morph var.upper

  let min (type a) (obj : a C.obj) = Amode (C.min obj)

  let max (type a) (obj : a C.obj) = Amode (C.max obj)

  let of_const a = Amode a

  let apply_morphvar dst morph (Amorphvar (var, morph')) =
    Amorphvar (var, C.compose dst morph morph')

  let apply :
      type a b l r.
      b C.obj -> (a, b, l * r) C.morph -> (a, l * r) mode -> (b, l * r) mode =
   fun dst morph -> function
    | Amode a -> Amode (C.apply dst morph a)
    | Amodevar mv -> Amodevar (apply_morphvar dst morph mv)
    | Amodejoin (a, vs) ->
      Amodejoin (C.apply dst morph a, List.map (apply_morphvar dst morph) vs)
    | Amodemeet (a, vs) ->
      Amodemeet (C.apply dst morph a, List.map (apply_morphvar dst morph) vs)

  let update_lower (type a) ?log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower) :: !log);
    v.lower <- C.join obj v.lower a;
    if not (C.le obj v.lower v.upper)
    then (
      Format.eprintf "range insane after update_lower: %a\n" (print_var obj) v;
      assert false)

  let update_upper (type a) ?log (obj : a C.obj) v a =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper) :: !log);
    v.upper <- C.meet obj v.upper a;
    if not (C.le obj v.lower v.upper)
    then (
      Format.eprintf "range insane after update_lower: %a\n" (print_var obj) v;
      assert false)

  let set_vlower ?log v vlower =
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  let submode_cv : type a. ?log:_ -> a C.obj -> a -> a var -> (unit, a) Result.t
      =
    fun (type a) ?log (obj : a C.obj) a' v ->
     if C.le obj a' v.lower
     then Ok ()
     else if not (C.le obj a' v.upper)
     then Error v.upper
     else (
       update_lower ?log obj v a';
       if C.le obj v.upper v.lower then set_vlower ?log v [];
       Ok ())

  let submode_cmv :
      type a l.
      ?log:_ -> a C.obj -> a -> (a, l * allowed) morphvar -> (unit, a) Result.t
      =
   fun ?log obj a (Amorphvar (v, f) as mv) ->
    (* Want a <= f v, therefore f' a <= v. Ideally the two are equivalent.
       However, [f v] could have been implicitly injected into a larger lattice,
       which means [a] could be outside of [f]'s co-domain, which is also [f']'s
       domain - so applying [f'] to [a] could be invalid.

       The following (seemingly redundant) several lines prevents that:
       - If a <= (f v).lower, immediately succeed
       - If not (a <= (f v).upper), immediately fail
       - Note that at this point, we still can't ensure that a >= (f v).lower.
         (We don't assume total ordering for generality.)
        Therefore, we set a = join a (f v).lower. This operation has no effect
        in terms of submoding, but we have now ensured that a is within the
        range of f v, and thus a is within the co-domain of f, and thus its
        safe to apply f' to a. *)
    if C.le obj a (mlower obj mv)
    then Ok ()
    else if not (C.le obj a (mupper obj mv))
    then Error (mupper obj mv)
    else
      let a = C.join obj a (mlower obj mv) in
      let f' = C.left_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      assert (Result.is_ok (submode_cv ?log src a' v));
      Ok ()

  (* In the following, functions might take two logs: [log] records changes
     caused by the operation, [optim] records optimizing changes not caused by the
     operation. Semantically, [optim] happens before [log] and doesn't rely on [log].
     On the other hand, some changes in [log] might rely on [optim]. *)

  (** Returns [None] if success; [Some x] if failed, and [x] is the next best
     guess to replace the constant argument that MIGHT succeed. *)
  let rec submode_vc :
      type a. ?log:_ -> a C.obj -> a var -> a -> (unit, a) Result.t =
    fun (type a) ?log (obj : a C.obj) v a' ->
     if C.le obj v.upper a'
     then Ok ()
     else if not (C.le obj v.lower a')
     then Error v.lower
     else (
       update_upper ?log obj v a';
       let r =
         v.vlower
         |> find_error (fun mu ->
                let r = submode_mvc ?log obj mu a' in
                (if Result.is_ok r
                then
                  (* Optimization: update [v.lower] based on [mlower u].*)
                  let mu_lower = mlower obj mu in
                  if not (C.le obj mu_lower v.lower)
                  then update_lower ?log obj v mu_lower);
                r)
       in
       if C.le obj v.upper v.lower then set_vlower ?log v [];
       r)

  and submode_mvc :
        'a 'r.
        ?log:change list ref ->
        'a C.obj ->
        ('a, allowed * 'r) morphvar ->
        'a ->
        (unit, 'a) Result.t =
   fun ?log obj (Amorphvar (v, f) as mv) a ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    if C.le obj (mupper obj mv) a
    then Ok ()
    else if not (C.le obj (mlower obj mv) a)
    then Error (mlower obj mv)
    else
      let a = C.meet obj a (mupper obj mv) in
      let f' = C.right_adjoint obj f in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      (* cannot assert [Option.is_none] because [mlower] is imprecise. *)
      Result.map_error (C.apply obj f) (submode_vc ?log src v a')

  (** Calculate the precise lower bound *)
  let constrain_lower_try (type a) (obj : a C.obj) (v : a var) =
    let rec loop lower =
      let log = ref [] in
      let r = submode_vc ~log obj v lower in
      match r with
      | Ok () -> log, lower
      | Error a ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop v.lower

  let constrain_mlower_try dst (Amorphvar (v, f)) =
    let src = C.src dst f in
    let log, lower = constrain_lower_try src v in
    log, C.apply dst f lower

  let eq_morphvar :
      type a l0 r0 l1 r1.
      a C.obj -> (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool =
   fun dst (Amorphvar (v0, f0)) (Amorphvar (v1, f1)) ->
    let obj0 = C.src dst f0 in
    let obj1 = C.src dst f1 in
    match C.eq_obj obj0 obj1 with
    | None -> false
    | Some Refl ->
      v0 == v1
      && C.disallow_left (C.disallow_right f0)
         = C.disallow_left (C.disallow_right f1)

  let submode_mvmv (type a) ?log (dst : a C.obj) (Amorphvar (v, f) as mv)
      (Amorphvar (u, g) as mu) =
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar dst mv mu
    then Ok ()
    else
      (* we have f v <= g u which gives g' (f v) <= u. On the other hand, we
         also have v <= f' (g u) *)
      match submode_mvc ?log dst mv (mupper dst mu) with
      | Error a -> Error (a, mupper dst mu)
      | Ok () -> (
        match submode_cmv ?log dst (mlower dst mv) mu with
        | Error a -> Error (mlower dst mv, a)
        | Ok () ->
          let g' = C.left_adjoint dst g in
          let src = C.src dst g in
          let g'f = C.compose src g' (C.disallow_right f) in
          let x = Amorphvar (v, g'f) in
          if not (List.exists (fun mv -> eq_morphvar src mv x) u.vlower)
          then set_vlower ?log u (x :: u.vlower);
          Ok ())

  let fresh (type a) (obj : a C.obj) =
    { upper = C.max obj; lower = C.min obj; vlower = [] }

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id))

  let submode_try (type a r l) ?(logging = true) (obj : a C.obj)
      (a : (a, allowed * r) mode) (b : (a, l * allowed) mode) =
    let log = if logging then Some (ref []) else None in
    let submode_cc left right =
      if C.le obj left right then Ok () else Error { left; right }
    in
    let submode_mvc v right =
      Result.map_error
        (fun left ->
          assert (not (C.le obj left right));
          { left; right })
        (submode_mvc ?log obj v right)
    in
    let submode_cmv left v =
      Result.map_error
        (fun right ->
          assert (not (C.le obj left right));
          { left; right })
        (submode_cmv ?log obj left v)
    in
    let submode_mvmv v u =
      Result.map_error
        (fun (left, right) ->
          assert (not (C.le obj left right));
          { left; right })
        (submode_mvmv ?log obj v u)
    in
    let r =
      match a, b with
      | Amode left, Amode right -> submode_cc left right
      | Amodevar v, Amode right -> submode_mvc v right
      | Amode left, Amodevar v -> submode_cmv left v
      | Amodevar v, Amodevar u -> submode_mvmv v u
      | Amode a, Amodemeet (b, mvs) ->
        Result.bind (submode_cc a b) (fun () ->
            find_error (fun mv -> submode_cmv a mv) mvs)
      | Amodevar mv, Amodemeet (b, mvs) ->
        Result.bind (submode_mvc mv b) (fun () ->
            find_error (fun mv' -> submode_mvmv mv mv') mvs)
      | Amodejoin (a, mvs), Amode b ->
        Result.bind (submode_cc a b) (fun () ->
            find_error (fun mv' -> submode_mvc mv' b) mvs)
      | Amodejoin (a, mvs), Amodevar mv ->
        Result.bind (submode_cmv a mv) (fun () ->
            find_error (fun mv' -> submode_mvmv mv' mv) mvs)
      | Amodejoin (a, mvs), Amodemeet (b, mus) ->
        (* TODO: mabye create a intermediate variable? *)
        Result.bind (submode_cc a b) (fun () ->
            Result.bind
              (find_error (fun mv -> submode_mvc mv b) mvs)
              (fun () ->
                Result.bind
                  (find_error (fun mu -> submode_cmv a mu) mus)
                  (fun () ->
                    find_error
                      (fun mu -> find_error (fun mv -> submode_mvmv mv mu) mvs)
                      mus)))
    in
    match r with
    | Ok () -> Ok log
    | Error e ->
      (* we mutated some states and found conflict;
         need to revert those mutation to keep the state consistent.
         A nice by-effect is that this function doesn't mutate state in failure
      *)
      Option.iter (fun log -> undo_changes !log) log;
      Error e

  let submode obj a b =
    match submode_try obj a b with
    | Ok log ->
      Option.iter !append_changes log;
      Ok ()
    | Error _ as e -> e

  let constrain_upper_morphvar obj mv =
    assert (submode obj (Amode (mupper obj mv)) (Amodevar mv) |> Result.is_ok);
    mupper obj mv

  let constrain_upper : type a l. a C.obj -> (a, l * allowed) mode -> a =
   fun obj -> function
    | Amode m -> m
    | Amodevar mv -> constrain_upper_morphvar obj mv
    | Amodemeet (a, mvs) ->
      List.fold_left
        (fun acc mv -> C.meet obj acc (constrain_upper_morphvar obj mv))
        a mvs

  let join :
      type a r.
      a C.obj -> (a, allowed * r) mode list -> (a, allowed * disallowed) mode =
   fun obj l ->
    let rec loop a mvs =
      if C.le obj (C.max obj) a
      then fun _ -> Amode (C.max obj)
      else
        function
        | [] -> Amodejoin (a, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode b -> loop (C.join obj a b) mvs xs
          | Amodevar mv -> loop a (mv :: mvs) xs
          | Amodejoin (b, mvs') -> loop (C.join obj a b) (mvs' @ mvs) xs)
    in
    loop (C.min obj) [] l

  let meet :
      type a l.
      a C.obj -> (a, l * allowed) mode list -> (a, disallowed * allowed) mode =
   fun obj l ->
    let rec loop a mvs =
      if C.le obj a (C.min obj)
      then fun _ -> Amode (C.min obj)
      else
        function
        | [] -> Amodemeet (a, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode b -> loop (C.meet obj a b) mvs xs
          | Amodevar mv -> loop a (mv :: mvs) xs
          | Amodemeet (b, mvs') -> loop (C.meet obj a b) (mvs' @ mvs) xs)
    in
    loop (C.max obj) [] l

  let constrain_lower_morphvar ~commit obj mv =
    let log, lower = constrain_mlower_try obj mv in
    if commit then !append_changes log else undo_changes !log;
    lower

  let constrain_lower : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj -> function
    | Amode a -> a
    | Amodevar mv -> constrain_lower_morphvar ~commit:true obj mv
    | Amodejoin (a, mvs) ->
      List.fold_left
        (fun acc mv ->
          C.join obj acc (constrain_lower_morphvar ~commit:true obj mv))
        a mvs

  (* because lower bound conservative, this check is also conservative.
     if it returns Some, then definitely a constant.
     if it returns None, then we don't know anything *)
  let check_const : type a l r. a C.obj -> (a, l * r) mode -> a option =
   fun obj -> function
    | Amode a -> Some a
    | Amodevar mv ->
      let lower = constrain_lower_morphvar ~commit:false obj mv in
      if C.le obj (mupper obj mv) lower then Some lower else None
    | Amodemeet (a, mvs) ->
      let upper =
        List.fold_left (fun x mv -> C.meet obj x (mupper obj mv)) a mvs
      in
      let lower =
        List.fold_left
          (fun x mv ->
            C.meet obj x (constrain_lower_morphvar ~commit:false obj mv))
          a mvs
      in
      if C.le obj upper lower then Some upper else None
    | Amodejoin (a, mvs) ->
      let upper =
        List.fold_left (fun x mv -> C.join obj x (mupper obj mv)) a mvs
      in
      let lower =
        List.fold_left
          (fun x mv ->
            C.join obj x (constrain_lower_morphvar ~commit:false obj mv))
          a mvs
      in
      if C.le obj upper lower then Some lower else None

  let print :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = true) obj ppf m ->
    print_raw obj ~verbose ppf
      (match check_const obj m with None -> m | Some a -> Amode a)

  let newvar_above (type a) (obj : a C.obj) = function
    | Amode a when C.le obj (C.max obj) a -> Amode a, false
    | m ->
      let m' = newvar obj in
      let r = submode_try ~logging:false obj m m' in
      (match r with Ok None -> () | _ -> assert false);
      allow_right m', true

  let newvar_below (type a) (obj : a C.obj) = function
    | Amode a when C.le obj a (C.min obj) -> Amode a, false
    | m ->
      let m' = newvar obj in
      let r = submode_try ~logging:false obj m' m in
      (match r with
      | Ok None -> ()
      | Ok (Some _) -> assert false
      | Error _ -> assert false);
      allow_left m', true
end

module Solver_polarized (C : Lattices_mono) = struct
  module S = Solver_mono (C)

  type changes = S.changes

  let undo_changes = S.undo_changes

  let append_changes = S.append_changes

  type positive = private Positive

  type negative = private Negative

  type 'a pos = 'b * 'c constraint 'a = 'b * 'c

  type 'a neg = 'c * 'b constraint 'a = 'b * 'c

  type 'a obj =
    | Positive : 'a C.obj -> ('a * positive) obj
    | Negative : 'a C.obj -> ('a * negative) obj

  type ('a, 'd, 'b, 'e) morph =
    | Pos_Pos :
        ('a, 'b, 'd) C.morph
        -> ('a * positive, 'd pos, 'b * positive, 'd pos) morph
    | Pos_Neg :
        ('a, 'b, 'd) C.morph
        -> ('a * positive, 'd pos, 'b * negative, 'd neg) morph
    | Neg_Pos :
        ('a, 'b, 'd) C.morph
        -> ('a * negative, 'd neg, 'b * positive, 'd pos) morph
    | Neg_Neg :
        ('a, 'b, 'd) C.morph
        -> ('a * negative, 'd neg, 'b * negative, 'd neg) morph

  type ('a, 'd) mode =
    | Positive : ('a, 'd) S.mode -> ('a * positive, 'd pos) mode
    | Negative : ('a, 'd) S.mode -> ('a * negative, 'd neg) mode
  (* We just run Mono_solver on the original category C, and translate
     submoding on the new category to submoding on the original category.
     Hopefully everything here will be inlined and optimized away. *)

  let id : type a l r. a obj -> (a, l * r, a, l * r) morph = function
    | Positive _ -> Pos_Pos C.id
    | Negative _ -> Neg_Neg C.id

  let compose :
      type a b c al ar bl br cl cr.
      c obj ->
      (b, bl * br, c, cl * cr) morph ->
      (a, al * ar, b, bl * br) morph ->
      (a, al * ar, c, cl * cr) morph =
   fun dst f g ->
    match dst, f, g with
    | Positive dst, Pos_Pos f, Pos_Pos g -> Pos_Pos (C.compose dst f g)
    | Positive dst, Pos_Pos f, Neg_Pos g -> Neg_Pos (C.compose dst f g)
    | Negative dst, Pos_Neg f, Neg_Pos g -> Neg_Neg (C.compose dst f g)
    | Negative dst, Pos_Neg f, Pos_Pos g -> Pos_Neg (C.compose dst f g)
    | Positive dst, Neg_Pos f, Pos_Neg g -> Pos_Pos (C.compose dst f g)
    | Positive dst, Neg_Pos f, Neg_Neg g -> Neg_Pos (C.compose dst f g)
    | Negative dst, Neg_Neg f, Neg_Neg g -> Neg_Neg (C.compose dst f g)
    | Negative dst, Neg_Neg f, Pos_Neg g -> Pos_Neg (C.compose dst f g)

  let apply :
      type a b d0 d1 e0 e1.
      b obj ->
      (a, d0 * d1, b, e0 * e1) morph ->
      (a, d0 * d1) mode ->
      (b, e0 * e1) mode =
   fun dst f ->
    match dst, f with
    | Positive dst, Pos_Pos f -> fun (Positive m) -> Positive (S.apply dst f m)
    | Negative dst, Pos_Neg f -> fun (Positive m) -> Negative (S.apply dst f m)
    | Positive dst, Neg_Pos f -> fun (Negative m) -> Positive (S.apply dst f m)
    | Negative dst, Neg_Neg f -> fun (Negative m) -> Negative (S.apply dst f m)

  let newvar : type a l r. a obj -> (a, l * r) mode = function
    | Positive obj ->
      let m = S.newvar obj in
      Positive m
    | Negative obj ->
      let m = S.newvar obj in
      Negative m

  let submode :
      type a p l r.
      (a * p) obj ->
      (a * p, allowed * r) mode ->
      (a * p, l * allowed) mode ->
      (unit, a error) result = function
    | Positive obj -> (
      fun m0 m1 ->
        match m0, m1 with Positive m0, Positive m1 -> S.submode obj m0 m1)
    | Negative obj -> (
      fun m0 m1 ->
        match m0, m1 with Negative m0, Negative m1 -> S.submode obj m1 m0)

  let join :
      type a r. a obj -> (a, allowed * r) mode list -> (a, left_only) mode =
    function
    | Positive obj ->
      fun l ->
        let l = List.map (fun (Positive m : _ mode) -> m) l in
        Positive (S.join obj l)
    | Negative obj ->
      fun l ->
        let l = List.map (fun (Negative m : _ mode) -> m) l in
        Negative (S.meet obj l)

  let meet :
      type a l. a obj -> (a, l * allowed) mode list -> (a, right_only) mode =
    function
    | Positive obj ->
      fun l ->
        let l = List.map (fun (Positive m : _ mode) -> m) l in
        Positive (S.meet obj l)
    | Negative obj ->
      fun l ->
        let l = List.map (fun (Negative m : _ mode) -> m) l in
        Negative (S.join obj l)

  let of_const : type a p l r. (a * p) obj -> a -> (a * p, l * r) mode =
    function
    | Positive _ -> fun a -> Positive (S.of_const a)
    | Negative _ -> fun a -> Negative (S.of_const a)

  let min : type a l r. a obj -> (a, l * r) mode = function
    | Positive obj -> Positive (S.min obj)
    | Negative obj -> Negative (S.max obj)

  let max : type a l r. a obj -> (a, l * r) mode = function
    | Positive obj -> Positive (S.max obj)
    | Negative obj -> Negative (S.min obj)

  let constrain_lower :
      type a p r. (a * p) obj -> (a * p, allowed * r) mode -> a = function
    | Positive obj -> fun (Positive m) -> S.constrain_lower obj m
    | Negative obj -> fun (Negative m) -> S.constrain_upper obj m

  let constrain_upper :
      type a p l. (a * p) obj -> (a * p, l * allowed) mode -> a = function
    | Positive obj -> fun (Positive m) -> S.constrain_upper obj m
    | Negative obj -> fun (Negative m) -> S.constrain_lower obj m

  let newvar_above :
      type a r_ l r. a obj -> (a, allowed * r_) mode -> (a, l * r) mode * bool =
    function
    | Positive obj ->
      fun (Positive m) ->
        let m, b = S.newvar_above obj m in
        Positive m, b
    | Negative obj ->
      fun (Negative m) ->
        let m, b = S.newvar_below obj m in
        Negative m, b

  let newvar_below :
      type a l_ l r. a obj -> (a, l_ * allowed) mode -> (a, l * r) mode * bool =
    function
    | Positive obj ->
      fun (Positive m) ->
        let m, b = S.newvar_below obj m in
        Positive m, b
    | Negative obj ->
      fun (Negative m) ->
        let m, b = S.newvar_above obj m in
        Negative m, b

  let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode =
    function
    | Positive m -> Positive (S.disallow_left m)
    | Negative m -> Negative (S.disallow_right m)

  let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode =
    function
    | Positive m -> Positive (S.disallow_right m)
    | Negative m -> Negative (S.disallow_left m)

  let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
    function
    | Positive m -> Positive (S.allow_left m)
    | Negative m -> Negative (S.allow_right m)

  let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
    function
    | Positive m -> Positive (S.allow_right m)
    | Negative m -> Negative (S.allow_left m)

  let check_const : type a p l r. (a * p) obj -> (a * p, l * r) mode -> a option
      = function
    | Positive obj -> fun (Positive m) -> S.check_const obj m
    | Negative obj -> fun (Negative m) -> S.check_const obj m

  let print : type a. ?verbose:bool -> a obj -> _ -> (a, _) mode -> unit =
   fun ?(verbose = false) obj ppf m ->
    match obj, m with
    | Positive obj, Positive m -> S.print ~verbose obj ppf m
    | Negative obj, Negative m -> S.print ~verbose obj ppf m

  let print_raw : type a. ?verbose:bool -> a obj -> _ -> (a, _) mode -> unit =
   fun ?(verbose = false) obj ppf m ->
    match obj, m with
    | Positive obj, Positive m -> S.print_raw ~verbose obj ppf m
    | Negative obj, Negative m -> S.print_raw ~verbose obj ppf m
end
