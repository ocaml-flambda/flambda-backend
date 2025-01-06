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

module VP = Backend_var.With_provenance

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
  | Shift of shift * bound * bound
  | Binop of binop * bound * bound
  | Icmp of Cmm.integer_comparison * bound * bound
  | Ifthenelse of
      bound
      * Debuginfo.t
      * body
      * Debuginfo.t
      * body
      * Debuginfo.t
      * Cmm.kind_for_unboxing
  | Alloca of
      { machtype : Cmm.machtype;
        initial_value : bound
      }
  | Uninterpreted of
      { cmm : Cmm.expression;
        free : Backend_var.Set.t
      }
  | Switch of
      bound
      * int array
      * (body * Debuginfo.t) array
      * Debuginfo.t
      * Cmm.kind_for_unboxing
  | Catch of
      { recursive : Cmm.rec_flag;
        body : body;
        handlers : continuation list
      }
  | Phi of phi

and phi = { mutable predecessors : expr list }

and continuation =
  | Continuation of
      { label : Lambda.static_label;
        vars : (VP.t * Cmm.machtype * phi) list;
        body : body;
        dbg : Debuginfo.t;
        is_cold : bool
      }

and expr =
  { name : VP.t;
    op : operation;
    mutable value : Lattice.t;
    dbg : Debuginfo.t
  }

and bound = expr

and terminator =
  | Return of expr
  | Exit of Cmm.exit_label * expr list * Cmm.trap_action list
  | Unreachable

and bindings = { rev_bindings : expr list } [@@unboxed]

and body =
  { bindings : bindings;
    terminator : terminator
  }

and procedure =
  | Procedure of
      { body : body;
        ret : phi
      }

module Print = struct
  open Format
  open Printcmm

  let shift ppf binop =
    match binop with
    | Shl -> Format.pp_print_string ppf "shl"
    | Lshr -> Format.pp_print_string ppf "lshr"
    | Ashr -> Format.pp_print_string ppf "ashr"

  let binop ppf binop =
    match binop with
    | Add -> Format.pp_print_string ppf "add"
    | Sub -> Format.pp_print_string ppf "sub"
    | Mul -> Format.pp_print_string ppf "mul"
    | And -> Format.pp_print_string ppf "and"
    | Or -> Format.pp_print_string ppf "or"
    | Xor -> Format.pp_print_string ppf "xor"

  let rec operation ppf op =
    match op with
    | Const i -> fprintf ppf "%nd" i
    | Shift (op, x, y) -> fprintf ppf "@[%a@ %a@ %a@]" shift op expr x expr y
    | Binop (op, x, y) -> fprintf ppf "@[%a@ %a@ %a@]" binop op expr x expr y
    | Alloca { machtype = ty; initial_value = x } ->
      fprintf ppf "@[alloca %a@ %a@]" machtype ty expr x
    | Icmp (cmp, x, y) ->
      fprintf ppf "@[icmp%s@ %a@ %a@]" (integer_comparison cmp) expr x expr y
    | Ifthenelse (x, _, y, _, z, _, _) ->
      fprintf ppf "@[(@[if@ %a@]@ @[then@ %a@]@ @[else@ %a@])@]" expr x body y
        body z
    | Phi p -> phi ppf p
    | Switch (e1, indices, cases, _, _) ->
      let indices = Array.to_list indices in
      let indices = List.mapi (fun i x -> i, x) indices in
      let print_cases ppf =
        ArrayLabels.iteri cases ~f:(fun i (case, _dbg) ->
            let lhs ppf =
              match List.filter (fun (_, x) -> x = i) indices with
              | [] -> fprintf ppf "unreachable:"
              | _ :: _ as cases ->
                fprintf ppf "case (%t):" (fun ppf ->
                    pp_print_list
                      ~pp_sep:(fun ppf () -> pp_print_char ppf '|')
                      (fun ppf (j, _) -> fprintf ppf "%d" j)
                      ppf cases)
            in
            fprintf ppf "@ @[<2>%t@ %a@]" lhs body case)
      in
      fprintf ppf "@[<v 0>@[<2>(switch@ %a@ @]%t)@]" expr e1 print_cases
    | Uninterpreted { cmm; free = _ } -> Printcmm.expression ppf cmm
    | Catch { recursive; body = b; handlers } ->
      let print_handlers ppf l = List.iter (handler ppf) l in
      fprintf ppf "@[<2>(catch%a@ %a@;<1 -2>with@[<v>%a@])@]" Printcmm.rec_flag
        recursive body b print_handlers handlers

  and handler ppf (Continuation { label; vars; body = b; dbg = _; is_cold }) =
    fprintf ppf "(%d:%a)%s@ %a" label
      (fun ppf ids ->
        ListLabels.iter ids ~f:(fun (id, machtype, phi_) ->
            fprintf ppf "@ @[%a@ : %a@ =@ %a@]" VP.print id Printcmm.machtype
              machtype phi phi_))
      vars
      (if is_cold then "(cold)" else "")
      body b

  and phi ppf phi =
    fprintf ppf "@[<2>phi@ %t@]" (fun ppf ->
        ListLabels.iter phi.predecessors ~f:(fun pred ->
            fprintf ppf ",@ %a" expr pred))

  and expr ppf { name; op; value = _; dbg = _ } =
    match op with Const i -> fprintf ppf "%nd" i | _ -> VP.print ppf name

  and binding ppf { name; op; value = _; dbg = _ } =
    fprintf ppf "@[<hv>%a@ =@ @[%a@]@]" VP.print name operation op

  and bindings ppf { rev_bindings } =
    pp_open_vbox ppf 0;
    ListLabels.iter (List.rev rev_bindings)
      ~f:(fprintf ppf "@[<hv>let@ %a@ in@]@;" binding);
    pp_close_box ppf ()

  and terminator ppf term =
    match term with
    | Unreachable -> pp_print_string ppf "unreachable"
    | Return x -> fprintf ppf "@[ret@ %a@]" expr x
    | Exit (label, exprs, _) ->
      pp_open_box ppf 2;
      (match label with
      | Return_lbl -> fprintf ppf "ret"
      | Lbl label -> fprintf ppf "br %d" label);
      ListLabels.iter exprs ~f:(fun e -> fprintf ppf ",@ %a" expr e);
      pp_close_box ppf ()

  and body ppf { bindings = b; terminator = t } =
    fprintf ppf "@[<v>%a@;%a@]" bindings b terminator t

  and procedure ppf (Procedure { body = b; ret }) =
    pp_open_vbox ppf 0;
    body ppf b;
    pp_print_string ppf "ret ";
    phi ppf ret;
    pp_close_box ppf ()
end

let gensym () = VP.create (Ident.create_local "%tmp")

let expr = function Unreachable | Exit _ -> None | Return x -> Some x

let expr_name expr = VP.var expr.name

let get_name t = Option.map expr_name (expr t)

(** cons an expression to the *end* of a list of bindings *)
let[@inline] snoc { rev_bindings } expr =
  { rev_bindings = expr :: rev_bindings }

let[@inline] bind ({ bindings; terminator } as t) ~f =
  match terminator with
  | Unreachable | Exit _ -> t
  | Return expr -> f expr ~bindings

let[@inline] return expr ~bindings =
  { bindings = snoc bindings expr; terminator = Return expr }

let[@inline] sequence t expr =
  bind t ~f:(fun _ ~bindings -> return expr ~bindings)

let[@inline] make_expr ?(dbg = Debuginfo.none) ?(name = gensym ()) op =
  let value =
    match op with
    | Const i -> Lattice.constant i
    | _ ->
      (* CR jvanburen: do more things here *)
      Lattice.top
  in
  { name; op; value; dbg }

module Env = struct
  type t =
    { vars : expr Backend_var.Map.t;
      handlers : phi list Numbers.Int.Map.t;
      ret : phi
    }

  let empty =
    { vars = Backend_var.Map.empty;
      handlers = Numbers.Int.Map.empty;
      ret = { predecessors = [] }
    }

  let get (t : t) ?name (x : Backend_var.t) =
    match Backend_var.Map.find_opt x t.vars with
    | None ->
      make_expr ?name
        (Uninterpreted { cmm = Cvar x; free = Backend_var.Set.singleton x })
    | Some { name = stack_name; op = Alloca _; _ } ->
      let stack_name = VP.var stack_name in
      make_expr ?name
        (Uninterpreted
           { cmm = Cvar stack_name;
             free = Backend_var.Set.singleton stack_name
           })
    | Some expr -> expr

  let add_var (t : t) (x : VP.t) y : t =
    assert (not (Backend_var.Map.mem (VP.var x) t.vars));
    { t with vars = Backend_var.Map.add (VP.var x) y t.vars }

  let add_handler (t : t) label vars : t =
    assert (not (Numbers.Int.Map.mem label t.handlers));
    { t with handlers = Numbers.Int.Map.add label vars t.handlers }

  let add_exit t exit_label vars =
    let phi_vars =
      match (exit_label : Cmm.exit_label) with
      | Return_lbl -> [t.ret]
      | Lbl label -> Numbers.Int.Map.find label t.handlers
    in
    ListLabels.iter2 phi_vars vars ~f:(fun phi var ->
        phi.predecessors <- var :: phi.predecessors)
end

let cvar x = Cmm.Cvar x

let rec of_cmm ?name ~bindings ~env (cmm : Cmm.expression) : body =
  let bind_multi ~bindings exprs ~f =
    let rec go ~bindings ~exprs ~vars =
      match exprs with
      | [] -> f vars ~bindings
      | e :: exprs ->
        bind (of_cmm e ~bindings ~env) ~f:(fun e ~bindings ->
            go ~bindings ~exprs ~vars:(e :: vars))
    in
    (* the evaluation order is right-to-left *)
    go ~bindings ~exprs:(List.rev exprs) ~vars:[]
  in
  match bindings, (cmm : Cmm.expression) with
  | bindings, Cconst_int (i, dbg) ->
    let i = Nativeint.of_int i in
    { bindings; terminator = Return (make_expr ?name (Const i)) }
  | bindings, Cconst_natint (i, dbg) ->
    { bindings; terminator = Return (make_expr ?name (Const i)) }
  | bindings, Cvar var -> return (Env.get env var) ~bindings
  | bindings, Clet (inner_name, x, y) ->
    bind (of_cmm ~name:inner_name ~bindings ~env x) ~f:(fun x ~bindings ->
        of_cmm y ?name ~bindings ~env:(Env.add_var env inner_name x))
  | bindings, Cphantom_let (_, _, y) ->
    Misc.fatal_error "Cphantom_let is not supported by cmm_optimizer"
  | bindings, Clet_mut (stack_var, machtype, x, y) ->
    bind (of_cmm ~bindings ~env x) ~f:(fun initial_value ~bindings ->
        let alloca =
          make_expr ~name:stack_var (Alloca { machtype; initial_value })
        in
        of_cmm y ?name ~bindings:(snoc bindings alloca)
          ~env:(Env.add_var env stack_var alloca))
  | bindings, Cassign (var, expr) ->
    bind (of_cmm expr ~bindings ~env) ~f:(fun x ~bindings ->
        let assignment =
          let rhs = expr_name x in
          make_expr ?name
            (Uninterpreted
               { cmm = Cassign (var, Cvar rhs);
                 free = Backend_var.Set.of_list [var; rhs]
               })
        in
        return assignment ~bindings)
  | bindings, Ctuple exprs ->
    bind_multi exprs ~bindings ~f:(fun exprs ~bindings ->
        let tuple =
          let vars = List.map expr_name exprs in
          make_expr ?name
            (Uninterpreted
               { cmm = Ctuple (List.map cvar vars);
                 free = Backend_var.Set.of_list vars
               })
        in
        return tuple ~bindings)
  | bindings, Csequence (x, y) ->
    bind (of_cmm x ~bindings ~env) ~f:(fun x ~bindings ->
        of_cmm ?name y ~bindings ~env)
  | bindings, Cifthenelse (x, x_dbg, y, y_dbg, z, z_dbg, kind) ->
    bind (of_cmm x ~bindings ~env) ~f:(fun x ~bindings ->
        let y = of_cmm y ~bindings:{ rev_bindings = [] } ~env
        and z = of_cmm z ~bindings:{ rev_bindings = [] } ~env in
        return ~bindings
          (make_expr ?name (Ifthenelse (x, x_dbg, y, y_dbg, z, z_dbg, kind))))
  | bindings, Cop (op, exprs, dbg) ->
    bind_multi ~bindings exprs ~f:(fun exprs ~bindings ->
        let operation =
          match op, exprs with
          | Caddi, [x; y] -> Binop (Add, x, y)
          | Csubi, [x; y] -> Binop (Sub, x, y)
          | Cmuli, [x; y] -> Binop (Mul, x, y)
          | Cand, [x; y] -> Binop (And, x, y)
          | Cor, [x; y] -> Binop (Or, x, y)
          | Cxor, [x; y] -> Binop (Xor, x, y)
          | Ccmpi cmp, [x; y] -> Icmp (cmp, x, y)
          | _, _ ->
            let vars = List.map expr_name exprs in
            Uninterpreted
              { cmm = Cop (op, List.map cvar vars, dbg);
                free = Backend_var.Set.of_list vars
              }
        in
        return (make_expr ?name operation) ~bindings)
  | ( bindings,
      ((Cconst_float32 _ | Cconst_float _ | Cconst_vec128 _ | Cconst_symbol _)
      as cmm) ) ->
    return ~bindings
      (make_expr ?name (Uninterpreted { cmm; free = Backend_var.Set.empty }))
  | bindings, Cswitch (e, cases, arms, dbg, kind) ->
    bind (of_cmm e ~bindings ~env) ~f:(fun e ~bindings ->
        let arms =
          ArrayLabels.map arms ~f:(fun (e, dbg) -> of_cmm ~bindings ~env e, dbg)
        in
        return (make_expr (Switch (e, cases, arms, dbg, kind))) ~bindings)
  | bindings, Cexit (exit_label, exprs, trap_actions) ->
    bind_multi ~bindings exprs ~f:(fun exprs ~bindings ->
        Env.add_exit env exit_label exprs;
        { bindings; terminator = Exit (exit_label, exprs, trap_actions) })
  | bindings, Ccatch (recursive, handlers, body, kind) ->
    return ~bindings
      (let bindings = { rev_bindings = [] } in
       let handlers =
         ListLabels.map handlers ~f:(fun (label, vars, body, dbg, is_cold) ->
             let vars =
               ListLabels.map vars ~f:(fun (vp, mach_kind) ->
                   vp, mach_kind, { predecessors = [] })
             in
             label, vars, body, dbg, is_cold)
       in
       let body_env =
         ListLabels.fold_left handlers ~init:env
           ~f:(fun env (label, vars, _, _, _) ->
             let vars = ListLabels.map vars ~f:(fun (_, _, phi) -> phi) in
             Env.add_handler env label vars)
       in
       let handlers =
         let env =
           match recursive with Recursive -> body_env | Nonrecursive -> env
         in
         ListLabels.map handlers ~f:(fun (label, vars, handler, dbg, is_cold) ->
             let body = of_cmm handler ~bindings ~env in
             Continuation { label; vars; body; dbg; is_cold })
       in
       make_expr ?name
         (Catch
            { recursive; body = of_cmm body ~bindings ~env:body_env; handlers }))
  | bindings, Ctrywith _ ->
    (* CR jvanburen: translate these *)
    Misc.fatal_error "trywith unimplemented"
