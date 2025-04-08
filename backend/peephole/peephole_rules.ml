(* CR-someday: see whether the `-4` can be dropped. *)
[@@@ocaml.warning "+a-29-40-41-42-4"]

open! Int_replace_polymorphic_compare
module DLL = Flambda_backend_utils.Doubly_linked_list
module U = Peephole_utils

let delete_snd_if_redundant ~fst ~(fst_val : Cfg.basic Cfg.instruction)
    ~(snd_val : Cfg.basic Cfg.instruction) =
  let fst_dst = fst_val.res.(0) in
  let snd_dst = snd_val.res.(0) in
  if U.are_equal_regs fst_dst snd_dst
  then (
    DLL.delete_curr fst;
    Some (U.prev_at_most U.go_back_const fst))
  else None

(** Logical condition for simplifying the following case:
    {|
    mov ..., x
    mov ..., x
    |}

    In this case, the first instruction should be removed *)

let remove_overwritten_mov (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match U.get_cells cell 2 with
  | [fst; snd] -> (
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    match fst_val.desc, snd_val.desc with
    | ( Op (Const_int _ | Const_float _ | Const_float32 _ | Const_vec128 _),
        Op (Const_int _ | Const_float _ | Const_float32 _ | Const_vec128 _) ) ->
      (* Removing the second instruction is okay here since it doesn't change
         the set of addresses we touch. *)
      delete_snd_if_redundant ~fst ~fst_val ~snd_val
    | Op (Spill | Reload), Op (Move | Spill | Reload) ->
      (* We only consider the removal of spill and reload instructions because a
         move from/to an arbitrary memory location could fail because of memory
         protection. *)
      delete_snd_if_redundant ~fst ~fst_val ~snd_val
    | _, _ -> None)
  | _ -> None

(** Logical condition for simplifying the following case:
    {|
    mov x, y
    mov y, x
    |}

    In this case, the second instruction should be removed *)

let remove_useless_mov (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match U.get_cells cell 2 with
  | [fst; snd] -> (
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    match fst_val.desc with
    | Op (Move | Spill | Reload) -> (
      let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
      match snd_val.desc with
      | Op (Move | Spill | Reload) ->
        let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
        if U.are_equal_regs fst_src snd_dst && U.are_equal_regs fst_dst snd_src
        then (
          DLL.delete_curr snd;
          Some (U.prev_at_most U.go_back_const fst))
        else None
      | _ -> None)
    | _ -> None)
  | _ -> None

(** Logical condition for simplifying the following case:
  {|
    <op1> const1, r
    <op2> const2, r
  |}

  to:
  {|
    <op1> (const1 <op2> const2), r
  |}

    where
    const1 and const2 are immediate values, and
    <op1> and <op2> are associative binary operators such
    that either <op1> is the same as <op2>, or <op1> is the inverse of <op2>,
    or there exists const3 such that <op1 const1> can be expressed as <op2 const3>
    or <op2 const2> can be expressed as <op1 const3> *)

let are_compatible op1 op2 imm1 imm2 :
    (Operation.integer_operation * int) option =
  match
    (op1 : Operation.integer_operation), (op2 : Operation.integer_operation)
  with
  | Iand, Iand -> U.bitwise_immediates op1 imm1 imm2 ( land )
  | Ior, Ior -> U.bitwise_immediates op1 imm1 imm2 ( lor )
  | Ixor, Ixor -> U.bitwise_immediates op1 imm1 imm2 ( lxor )
  (* For the following three cases we have the issue that in some situations,
     one or both immediate values could be out of bounds, but the result might
     be within bounds (e.g. imm1 = -4 and imm2 = 65, their sum being 61). This
     should not happen at all since the immediate values should always be within
     the bounds [0, Sys.int_size]. *)
  | Ilsl, Ilsl | Ilsr, Ilsr | Iasr, Iasr | Iadd, Iadd ->
    U.add_immediates op1 imm1 imm2
  | Iadd, Isub ->
    (* The following transformation changes the order of operations on [r] and
       therefore might change the overflow behavior: if [r+c1] overflows, but
       r-[c2-c1] does not overflow. This is fine, other compiler transformations
       may also do it. The code below only ensures that immediates that the
       compiler emits do not overflow. *)
    if imm1 >= imm2
    then U.sub_immediates Iadd imm1 imm2
    else U.sub_immediates Isub imm2 imm1
  | Isub, Isub (* r - (imm1 + imm2 *) -> U.add_immediates Isub imm1 imm2
  | Isub, Iadd ->
    if imm1 >= imm2
    then U.sub_immediates Isub imm1 imm2
    else U.sub_immediates Iadd imm2 imm1
  | Ilsl, Imul ->
    (* [imm1] is guaranteed to be within bounds for [Ilsl], but [1 lsl imm1] may
       not be within bounds for [Imul]. *)
    U.assert_within_range Ilsl imm1;
    let imm1 = 1 lsl imm1 in
    if Arch.is_immediate_for_intop Imul imm1
    then U.mul_immediates Imul imm1 imm2
    else None
  | Imul, Ilsl ->
    (* [imm2] is guaranteed to be within bounds for [Ilsl], but [1 lsl imm2] may
       not be within bounds for [Imul]. *)
    U.assert_within_range Ilsl imm2;
    let imm2 = 1 lsl imm2 in
    if Arch.is_immediate_for_intop Imul imm2
    then U.mul_immediates Imul imm1 imm2
    else None
  | Imul, Imul -> U.mul_immediates op1 imm1 imm2
  (* CR-soon gtulba-lecu: check this last case | Imod, Imod -> if imm1 mod imm2
     = 0 then Some (Imod, imm2) else None

     The integer modulo imm2 group is a subgroup of the integer modulo imm1 iff
     imm2 divides imm1

     This is because the operations in the groups are addition modulo n and m
     respectively. If n divides m, then every result of the operation (addition)
     in the n group will also be a legal result in the m group, which is
     essentially the definition of a subgroup. If n does not divide m, there
     will be some results in the n group that are not acceptable in the m
     group. *)
  | _ -> None

let fold_intop_imm (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match U.get_cells cell 2 with
  | [fst; snd] ->
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    (* The following check does the following: 1. Ensures that both instructions
       use the same source register; 2. Ensures that both instructions output
       the result to the source register. This is currently redundant for amd64
       since there are no instructions that invalidate this condition. *)
    if Array.length fst_val.arg = 1
       && Array.length snd_val.arg = 1
       && Array.length fst_val.res = 1
       && Array.length snd_val.res = 1
       && U.are_equal_regs
            (Array.unsafe_get fst_val.arg 0)
            (Array.unsafe_get snd_val.arg 0)
       && U.are_equal_regs
            (Array.unsafe_get fst_val.arg 0)
            (Array.unsafe_get fst_val.res 0)
       && U.are_equal_regs
            (Array.unsafe_get snd_val.arg 0)
            (Array.unsafe_get snd_val.res 0)
    then
      match fst_val.desc, snd_val.desc with
      | Op (Intop_imm (op1, imm1)), Op (Intop_imm (op2, imm2)) -> (
        match are_compatible op1 op2 imm1 imm2 with
        | Some (op, imm) ->
          let new_cell =
            DLL.insert_and_return_before fst
              { fst_val with desc = Cfg.Op (Intop_imm (op, imm)) }
          in
          DLL.delete_curr fst;
          DLL.delete_curr snd;
          Some ((U.prev_at_most U.go_back_const) new_cell)
        | _ -> None)
      | _ -> None
    else None
  | _ -> None

let apply cell =
  let[@inline always] if_none_do f o =
    match o with Some _ -> o | None -> f cell
  in
  None
  |> if_none_do remove_overwritten_mov
  |> if_none_do remove_useless_mov
  |> if_none_do fold_intop_imm
