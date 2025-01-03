(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let ( land ) = Nativeint.logand

let ( lor ) = Nativeint.logor

let ( lxor ) = Nativeint.logxor

let lnot = Nativeint.lognot

let num_bits = 8 * Arch.size_int

let () = assert (0 < num_bits && num_bits <= Nativeint.size)

let max_shift = num_bits - 1

let trailing_mask ~bits =
  assert (0 <= bits && bits <= num_bits);
  if bits = Nativeint.size
  then -1n
  else Nativeint.pred (Nativeint.shift_left 1n bits)

let leading_mask ~bits =
  if bits = 0
  then 0n
  else Nativeint.shift_left (trailing_mask ~bits) (num_bits - bits)

let count_leading_zeros =
  let rec go n ~acc ~num_bits =
    if num_bits = 1
    then acc + (1 - Nativeint.to_int n)
    else
      let right = num_bits / 2 in
      let left = num_bits - right in
      let left_mask = Nativeint.shift_left (-1n) right in
      if n land left_mask = 0n
      then go n ~acc:(acc + left) ~num_bits:right
      else
        let n = Nativeint.shift_right_logical n right in
        go n ~acc ~num_bits:left
  in
  fun n -> go (n land trailing_mask ~bits:num_bits) ~acc:0 ~num_bits

let count_trailing_zeros =
  let rec go n ~acc ~num_bits =
    if n = 0n
    then acc + num_bits
    else if num_bits = 1
    then acc
    else
      let right = num_bits / 2 in
      let left = num_bits - right in
      let right_mask = Nativeint.pred (Nativeint.shift_left 1n right) in
      if n land right_mask <> 0n
      then go n ~acc ~num_bits:right
      else
        let n = Nativeint.shift_right_logical n right in
        go n ~acc:(acc + right) ~num_bits:left
  in
  fun n -> go n ~acc:0 ~num_bits

let count_leading_ones x = count_leading_zeros (lnot x)

module Lattice = struct
  let sign_bit bits ~known =
    let sign_bit = Nativeint.shift_left 1n num_bits in
    if known land sign_bit = 0n then None else Some (bits land sign_bit <> 0n)

  type t =
    { bits : Nativeint.t;  (** known bits of the number (see [mask]) *)
      known : Nativeint.t;  (** a mask of which bits are known *)
      sign_bits : int
          (** How many leading bits are known to be the same, even if it's not known what
              they are. *)
    }

  let invariant { bits; known; sign_bits } =
    let unknown_bits_unset = bits land known in
    let unknown_bits_set = bits lor lnot known in
    let min_possible_sign_bits =
      match sign_bit bits ~known with
      | None -> 1
      | Some true -> count_leading_ones unknown_bits_unset
      | Some false -> count_leading_zeros unknown_bits_set
    in
    let max_possible_sign_bits =
      let max_sign_bits_if_sign_is_unset () =
        count_leading_zeros unknown_bits_unset
      in
      let max_sign_bits_if_sign_is_set () =
        count_leading_ones unknown_bits_set
      in
      match sign_bit bits ~known with
      | Some true -> max_sign_bits_if_sign_is_set ()
      | Some false -> max_sign_bits_if_sign_is_unset ()
      | None ->
        Int.max
          (max_sign_bits_if_sign_is_set ())
          (max_sign_bits_if_sign_is_unset ())
    in
    assert (bits = unknown_bits_unset);
    if min_possible_sign_bits > sign_bits
    then
      Misc.fatal_errorf
        "sign_bits = %d, min_possible_sign_bits = %d, bits = %nx, known = %nx"
        sign_bits min_possible_sign_bits bits known;
    if max_possible_sign_bits < sign_bits
    then
      Misc.fatal_errorf
        "sign_bits = %d, max_possible_sign_bits = %d, bits = %nx, known = %nx"
        sign_bits max_possible_sign_bits bits known;
    assert (
      min_possible_sign_bits <= sign_bits && sign_bits <= max_possible_sign_bits)

  let create ?(sign_bits = 1) bits ~known =
    let min_sign_bits =
      match sign_bit bits ~known with
      | None -> 1
      | Some false -> count_leading_zeros (bits lor lnot known)
      | Some true -> count_leading_ones (bits land known)
    in
    let sign_bits = Int.max sign_bits min_sign_bits in
    let t = { sign_bits; bits; known } in
    invariant t;
    t

  let sign_bits t = t.sign_bits

  let unknown_bits_set t = t.bits lor lnot t.known

  let is_power_of_2 const = const land Nativeint.pred const = 0n

  let leading_zeros t = count_leading_zeros (unknown_bits_set t)

  let leading_ones t = count_leading_zeros (lnot (unknown_bits_set t))

  let join t1 t2 =
    let known = t1.known land t2.known land lnot (t1.bits lxor t2.bits) in
    let t =
      { bits = t1.bits lor t2.bits land known;
        known;
        sign_bits = Int.min t1.sign_bits t2.sign_bits
      }
    in
    invariant t;
    t

  let top = { sign_bits = 1; bits = 0n; known = 0n }

  let is_top t = t.known = 0n

  let constant n = create n ~known:(-1n)

  let range ~min ~max =
    assert (min <= max);
    join (constant (Nativeint.of_int min)) (constant (Nativeint.of_int max))

  let to_constant t = if t.known = -1n then Some t.bits else None

  let to_small_int ~min ~max n =
    if Nativeint.of_int min <= n && n <= Nativeint.of_int max
    then Some (Nativeint.to_int n)
    else None

  let to_small_int t ~min ~max =
    Option.bind (to_constant t) (to_small_int ~min ~max)

  let can_weaken_add_to_or t1 t2 =
    (* we can weaken an [add] to an [or] if we know there's no overflow from
       unknown bits *)
    Nativeint.add (unknown_bits_set t1) (unknown_bits_set t2)
    = unknown_bits_set t1 lor unknown_bits_set t2

  let can_weaken_mul t1 t2 =
    match to_constant t1, to_constant t2 with
    | Some t1, Some t2 -> `Constant (Nativeint.mul t1 t2)
    | None, None -> `Unknown
    | Some 0n, None | None, Some 0n -> `Constant 0n
    | None, Some t2 ->
      if is_power_of_2 t2
      then `Shift_LHS_left_by (count_trailing_zeros t2)
      else `Unknown
    | Some t1, None ->
      if is_power_of_2 t1
      then `Shift_RHS_left_by (count_trailing_zeros t1)
      else `Unknown

  let logor t1 t2 =
    let bits = t1.bits lor t2.bits in
    let sign_bits = Int.min t1.sign_bits t2.sign_bits in
    create ~sign_bits bits ~known:(bits lor (t1.known land t2.known))

  let logand t1 t2 =
    let bits = t1.bits land t2.bits in
    let known_to_be_zero =
      lnot t1.bits land t1.known lor (lnot t2.bits land t2.known)
    in
    let sign_bits = Int.min t1.sign_bits t2.sign_bits in
    create ~sign_bits bits ~known:(known_to_be_zero lor (t1.known land t2.known))

  let logxor t1 t2 =
    let bits = t1.bits lxor t2.bits in
    let sign_bits = Int.min t1.sign_bits t2.sign_bits in
    create ~sign_bits bits ~known:(t1.known land t2.known)

  let shift_right_logical t1 t2 =
    match to_small_int t2 ~min:0 ~max:max_shift with
    | None -> top
    | Some 0 -> t1
    | Some shift ->
      create
        (Nativeint.shift_right_logical t1.bits shift)
        ~known:
          (leading_mask ~bits:shift
          lor Nativeint.shift_right_logical t1.known shift)

  let shift_left t1 t2 =
    match to_small_int t2 ~min:0 ~max:max_shift with
    | None -> top
    | Some 0 -> t1
    | Some shift ->
      create
        (Nativeint.shift_left t1.bits shift)
        ~known:
          (Nativeint.shift_left t1.known shift lor trailing_mask ~bits:shift)

  let shift_right t1 t2 =
    match to_small_int t2 ~min:0 ~max:max_shift with
    | None -> top
    | Some 0 -> t1
    | Some shift ->
      let ( asr ) x y =
        let unused_bits = Sys.word_size - num_bits in
        let x = Nativeint.shift_left x unused_bits in
        let x = Nativeint.shift_right x y in
        let x = Nativeint.shift_right_logical x unused_bits in
        x
      in
      create (t1.bits asr shift) ~known:(t1.known asr shift)

  let add t1 t2 =
    match to_constant t1, to_constant t2 with
    | Some t1, Some t2 -> constant (Nativeint.add t1 t2)
    | Some 0n, None -> t2
    | None, Some 0n -> t1
    | _, _ ->
      if can_weaken_add_to_or t1 t2
      then logor t1 t2
      else
        { top with
          sign_bits = Int.max 1 (Int.min t1.sign_bits t2.sign_bits - 1)
        }

  let sign_bit t = sign_bit t.bits ~known:t.known
end

module Rope = struct
  type 'a t =
    | Singleton of 'a
    | Concat of 'a t * 'a t

  let rec iter t ~f =
    match t with
    | Singleton x -> f x
    | Concat (x, y) ->
      iter x ~f;
      iter y ~f
end

module Var = Backend_var.With_provenance

type shift =
  | Shl
  | Ashr
  | Lshr

type binop =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | Xor

type operation =
  | Const of Nativeint.t
  | Shift of shift * expr * expr
  | Binop of binop * expr * expr
  | Icmp of Cmm.integer_comparison * expr * expr
  | Ite of
      expr
      * Debuginfo.t
      * t
      * Debuginfo.t
      * t
      * Debuginfo.t
      * Cmm.kind_for_unboxing
  | Alloca of
      { machtype : Cmm.machtype;
        initial_value : expr
      }
  | Uninterpreted of
      { cmm : Cmm.expression;
        free : Backend_var.Set.t
      }

and expr =
  { name : Var.t;
    op : operation;
    value : Lattice.t;
    dbg : Debuginfo.t;
    phantom_defining_expr : Cmm.phantom_defining_expr option
  }

and t =
  | Let of
      { bindings : t;
        result : expr
      }
  | Expr of expr

let to_bindings t =
  let rec go ~acc = function
    | Expr expr -> expr :: acc
    | Let { bindings; result } -> go bindings ~acc:(result :: acc)
  in
  go t ~acc:[]

let print_shift fmt binop =
  match binop with
  | Shl -> Format.pp_print_string fmt "shl"
  | Lshr -> Format.pp_print_string fmt "lshr"
  | Ashr -> Format.pp_print_string fmt "ashr"

let print_binop fmt binop =
  match binop with
  | Add -> Format.pp_print_string fmt "add"
  | Sub -> Format.pp_print_string fmt "sub"
  | Mul -> Format.pp_print_string fmt "mul"
  | And -> Format.pp_print_string fmt "and"
  | Or -> Format.pp_print_string fmt "or"
  | Xor -> Format.pp_print_string fmt "xor"

let rec print_operation fmt operation =
  match operation with
  | Const i -> Format.fprintf fmt "%nd" i
  | Shift (shift, x, y) ->
    Format.fprintf fmt "@[%a@ %a@ %a@]" print_shift shift Var.print x.name
      Var.print y.name
  | Binop (binop, x, y) ->
    Format.fprintf fmt "@[%a@ %a@ %a@]" print_binop binop Var.print x.name
      Var.print y.name
  | Alloca { machtype; initial_value = x } ->
    Format.fprintf fmt "@[alloca %a@ %a@]" Printcmm.machtype machtype Var.print
      x.name
  | Icmp (cmp, x, y) ->
    Format.fprintf fmt "@[icmp%s@ %a@ %a@]"
      (Printcmm.integer_comparison cmp)
      Var.print x.name Var.print y.name
  | Ite (x, _, y, _, z, _, _) ->
    Format.fprintf fmt "@[if@ %a@ then@ %a@ else@ %a@]" Var.print x.name print y
      print z
  | Uninterpreted { cmm; free = _ } -> Printcmm.expression fmt cmm

and print_expr fmt { name; op; value = _; dbg = _ } =
  Format.fprintf fmt "@[<hv>%a@ =@ @[%a@]@]" Var.print name print_operation op

and print fmt t =
  let bindings, expr =
    match t with
    | Expr e -> [], e
    | Let { bindings; result } -> to_bindings bindings, result
  in
  Format.pp_open_vbox fmt 0;
  ListLabels.iter bindings
    ~f:(Format.fprintf fmt "@[<hv>let@ %a@ in@]@;" print_expr);
  print_expr fmt expr;
  Format.pp_close_box fmt ()

let gensym () = Var.create (Ident.create_local "%tmp")

let expr (Expr expr | Let { bindings = _; result = expr }) = expr

let get_name t = Var.var (expr t).name

let bind t f = Let { bindings = t; result = f (expr t) }

let[@inline] make_expr ?phantom_defining_expr ?(dbg = Debuginfo.none)
    ?(name = gensym ()) op =
  let value =
    match op with
    | Const i -> Lattice.constant i
    | _ ->
      (* CR jvanburen: do more things here *)
      Lattice.top
  in
  { name; op; value; dbg; phantom_defining_expr }

let trivial = lazy (Expr (make_expr (Const 0n)))

module Env = struct
  type t = expr Backend_var.Map.t

  let empty = Backend_var.Map.empty

  let get (t : t) ?name (x : Backend_var.t) =
    match Backend_var.Map.find_opt x t with
    | None ->
      make_expr ?name
        (Uninterpreted { cmm = Cvar x; free = Backend_var.Set.singleton x })
    | Some { name = stack_name; op = Alloca _; _ } ->
      let stack_name = Var.var stack_name in
      make_expr ?name
        (Uninterpreted
           { cmm = Cvar stack_name;
             free = Backend_var.Set.singleton stack_name
           })
    | Some expr -> expr

  let add (t : t) (x : Var.t) y : t = Backend_var.Map.add (Var.var x) y t
end

let cvar x = Cmm.Cvar x

let rec of_cmm ?name ~bindings ~env (cmm : Cmm.expression) : t =
  let bind_multi ~bindings exprs ~f =
    let rec go ~bindings ~exprs ~rev_vars =
      match exprs with
      | [] -> f (List.rev rev_vars) ~bindings
      | e :: exprs ->
        let bindings = of_cmm e ~bindings ~env in
        go ~bindings ~exprs ~rev_vars:(expr bindings :: rev_vars)
    in
    go ~bindings ~exprs ~rev_vars:[]
  in
  match bindings, (cmm : Cmm.expression) with
  | bindings, Cconst_int (i, dbg) ->
    let i = Nativeint.of_int i in
    Let { bindings; result = make_expr ?name (Const i) }
  | bindings, Cconst_natint (i, dbg) ->
    Let { bindings; result = make_expr ?name (Const i) }
  | bindings, Cvar var -> Let { bindings; result = Env.get env var }
  | bindings, Clet (inner_name, x, y) ->
    let bindings = of_cmm ~name:inner_name ~bindings ~env x in
    of_cmm y ?name ~bindings ~env:(Env.add env inner_name (expr bindings))
  | bindings, Cphantom_let (_, _, y) -> of_cmm y ?name ~bindings ~env
  | bindings, Clet_mut (stack_var, machtype, x, y) ->
    let bindings = of_cmm ~bindings ~env x in
    let alloca =
      make_expr ~name:stack_var
        (Alloca { machtype; initial_value = expr bindings })
    in
    let bindings = Let { bindings; result = alloca } in
    of_cmm y ?name ~bindings ~env:(Env.add env stack_var alloca)
  | bindings, Cassign (var, expr) ->
    let bindings = of_cmm expr ~bindings ~env in
    Let
      { bindings;
        result =
          (let rhs = get_name bindings in
           make_expr ?name
             (Uninterpreted
                { cmm = Cassign (var, Cvar rhs);
                  free = Backend_var.Set.of_list [var; rhs]
                }))
      }
  | bindings, Ctuple exprs ->
    bind_multi exprs ~bindings ~f:(fun exprs ~bindings ->
        Let
          { bindings;
            result =
              (let vars = List.map (fun x -> Var.var x.name) exprs in
               make_expr ?name
                 (Uninterpreted
                    { cmm = Ctuple (List.map cvar vars);
                      free = Backend_var.Set.of_list vars
                    }))
          })
  | bindings, Csequence (x, y) ->
    let bindings = of_cmm x ~bindings ~env in
    of_cmm ?name y ~bindings ~env
  | bindings, Cifthenelse (x, x_dbg, y, y_dbg, z, z_dbg, kind) ->
    bind (of_cmm x ~bindings ~env) (fun x ->
        let y = of_cmm y ~bindings:(Lazy.force trivial) ~env
        and z = of_cmm z ~bindings:(Lazy.force trivial) ~env in
        make_expr ?name (Ite (x, x_dbg, y, y_dbg, z, z_dbg, kind)))
  | bindings, Cop (op, exprs, dbg) ->
    bind_multi ~bindings exprs ~f:(fun exprs ~bindings ->
        let operation =
          match op, exprs with
          | Caddi, [x; y] -> Binop (Add, x, y)
          | Csub, [x; y] -> Binop (Sub, x, y)
          | Cmuli, [x; y] -> Binop (Mul, x, y)
          | Cand, [x; y] -> Binop (And, x, y)
          | Cor, [x; y] -> Binop (Or, x, y)
          | Cxor, [x; y] -> Binop (Xor, x, y)
          | Ccmpi cmp, [x; y] -> Icmp (cmp, x, y)
          | _, _ ->
            let vars = List.map (fun x -> Var.var x.name) exprs in
            Uninterpreted
              { cmm = Cop (op, List.map cvar vars, dbg);
                free = Backend_var.Set.of_list vars
              }
        in
        Let { bindings; result = make_expr ~dbg ?name operation })
  | ( bindings,
      ((Cconst_float32 _ | Cconst_float _ | Cconst_vec128 _ | Cconst_symbol _)
      as cmm) ) ->
    Let
      { bindings;
        result =
          make_expr ?name (Uninterpreted { cmm; free = Backend_var.Set.empty })
      }
  | bindings, (Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _) ->
    (* CR jvanburen: translate these *)
    Misc.fatal_error "unimplemented"
