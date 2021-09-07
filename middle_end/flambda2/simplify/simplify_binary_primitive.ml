(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

module A = Number_adjuncts
module Float_by_bit_pattern = Numeric_types.Float_by_bit_pattern

type 'a binary_arith_outcome_for_one_side_only =
  | Exactly of 'a
  | The_other_side
  | Negation_of_the_other_side
  | Float_negation_of_the_other_side
  | Cannot_simplify
  | Invalid

module type Binary_arith_like_sig = sig
  module Lhs : Container_types.S
  module Rhs : Container_types.S
  module Pair : Container_types.S with type t = Lhs.t * Rhs.t
  module Result : Container_types.S

  val ok_to_evaluate : DE.t -> bool

  val cross_product : Lhs.Set.t -> Rhs.Set.t -> Pair.Set.t

  (* CR mshinwell: Rename to [arg_kind] *)
  val kind : K.Standard_int_or_float.t
  val result_kind : K.t

  val term : Result.t -> Named.t

  val prover_lhs : (T.t -> Lhs.Set.t T.proof) T.type_accessor
  val prover_rhs : (T.t -> Rhs.Set.t T.proof) T.type_accessor

  type op

  val unknown : op -> T.t
  val these : Result.Set.t -> T.t

  val op : op -> Lhs.t -> Rhs.t -> Result.t option

  val op_lhs_unknown
     : op
    -> rhs:Rhs.t
    -> Result.t binary_arith_outcome_for_one_side_only

  val op_rhs_unknown
     : op
    -> lhs:Lhs.t
    -> Result.t binary_arith_outcome_for_one_side_only
end

module Binary_arith_like (N : Binary_arith_like_sig) : sig
  val simplify
     : N.op
    -> DA.t
    -> original_term:Named.t
    -> Debuginfo.t
    -> arg1:Simple.t
    -> arg1_ty:Flambda_type.t
    -> arg2:Simple.t
    -> arg2_ty:Flambda_type.t
    -> result_var:Var_in_binding_pos.t
    -> Simplified_named.t * TEE.t * DA.t
end = struct
  module Possible_result = struct
    type t =
      | Simple of Simple.t
      | Prim of P.t
      | Exactly of N.Result.t

    include Container_types.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | Simple simple1, Simple simple2 -> Simple.compare simple1 simple2
        | Prim prim1, Prim prim2 -> P.compare prim1 prim2
        | Exactly i1, Exactly i2 -> N.Result.compare i1 i2
        | Simple _, (Prim _ | Exactly _) -> -1
        | Prim _, Simple _ -> 1
        | Prim _, Exactly _ -> -1
        | Exactly _, (Simple _ | Prim _) -> 1

      let equal t1 t2 =
        compare t1 t2 = 0

      let hash _ = Misc.fatal_error "Not yet implemented"
      let [@ocamlformat "disable"] print _ppf _t = Misc.fatal_error "Not yet implemented"
      let output _ _ = Misc.fatal_error "Not yet implemented"
    end)
  end

  (* CR-someday mshinwell: Add command-line flag to control this *)
  let max_num_possible_results = 10

  let simplify op dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty
        ~result_var =
    let module PR = Possible_result in
    let result = Name.var (Var_in_binding_pos.var result_var) in
    let denv = DA.denv dacc in
    let typing_env = DE.typing_env denv in
    let proof1 = N.prover_lhs typing_env arg1_ty in
    let proof2 = N.prover_rhs typing_env arg2_ty in
    let kind = N.result_kind in
    let result_unknown () =
      let env_extension = TEE.one_equation result (N.unknown op) in
      Simplified_named.reachable original_term, env_extension, dacc
    in
    let result_invalid () =
      let env_extension = TEE.one_equation result (T.bottom kind) in
      Simplified_named.invalid (), env_extension, dacc
    in
    let check_possible_results ~possible_results =
      if PR.Set.is_empty possible_results then
        result_invalid ()
      else
        let named =
          match PR.Set.get_singleton possible_results with
          | Some (Exactly i) -> N.term i
          | Some (Prim prim) -> Named.create_prim prim dbg
          | Some (Simple simple) -> Named.create_simple simple
          | None -> original_term
        in
        let ty =
          let is =
            List.filter_map (fun (possible_result : PR.t) ->
                match possible_result with
                | Exactly i -> Some i
                | Prim _ | Simple _ -> None)
              (PR.Set.elements possible_results)
          in
          if List.length is = PR.Set.cardinal possible_results
          then
            N.these (N.Result.Set.of_list is)
          else
            match PR.Set.get_singleton possible_results with
            | Some (Simple simple) -> T.alias_type_of kind simple
            | Some (Exactly _)
            | Some (Prim _)
            | None -> N.unknown op
        in
        let env_extension = TEE.one_equation result ty in
        Simplified_named.reachable named, env_extension, dacc
    in
    let only_one_side_known op nums ~folder ~other_side =
      let possible_results =
        folder (fun i possible_results ->
            match possible_results with
            | None -> None
            | Some possible_results ->
              match op i with
              | Exactly result ->
                Some (PR.Set.add (Exactly result) possible_results)
              | The_other_side ->
                Some (PR.Set.add (Simple other_side) possible_results)
              | Negation_of_the_other_side ->
                let standard_int_kind : K.Standard_int.t =
                  match N.kind with
                  | Tagged_immediate -> Tagged_immediate
                  | Naked_immediate -> Naked_immediate
                  | Naked_int32 -> Naked_int32
                  | Naked_int64 -> Naked_int64
                  | Naked_nativeint -> Naked_nativeint
                  | Naked_float ->
                    Misc.fatal_error "Cannot use [Negation_of_the_other_side] \
                        with floats; use the float version instead"
                in
                let prim : P.t =
                  Unary (Int_arith (standard_int_kind, Neg), other_side)
                in
                Some (PR.Set.add (Prim prim) possible_results)
              | Float_negation_of_the_other_side ->
                let prim : P.t =
                  Unary (Float_arith Neg, other_side)
                in
                Some (PR.Set.add (Prim prim) possible_results)
              | Cannot_simplify -> None
              | Invalid -> Some possible_results)
          nums
          (Some PR.Set.empty)
      in
      match possible_results with
      | Some results -> check_possible_results ~possible_results:results
      | None -> result_unknown ()
    in
    match proof1, proof2 with
    | (Proved nums1, Proved nums2)
        when N.ok_to_evaluate denv ->
      assert (not (N.Lhs.Set.is_empty nums1));
      assert (not (N.Rhs.Set.is_empty nums2));
      if N.Lhs.Set.cardinal nums1 > max_num_possible_results
        || N.Rhs.Set.cardinal nums2 > max_num_possible_results
      then
        result_unknown ()
      else
        let all_pairs = N.cross_product nums1 nums2 in
        let possible_results =
          N.Pair.Set.fold (fun (i1, i2) possible_results ->
              match N.op op i1 i2 with
              | None -> possible_results
              | Some result ->
                PR.Set.add (Exactly result) possible_results)
            all_pairs
            PR.Set.empty
        in
        check_possible_results ~possible_results
    | (Proved nums1, Unknown)
        when N.ok_to_evaluate denv ->
      assert (not (N.Lhs.Set.is_empty nums1));
      only_one_side_known (fun i -> N.op_rhs_unknown op ~lhs:i) nums1
        ~folder:N.Lhs.Set.fold
        ~other_side:arg2
    | (Unknown, Proved nums2)
        when N.ok_to_evaluate denv ->
      assert (not (N.Rhs.Set.is_empty nums2));
      only_one_side_known (fun i -> N.op_lhs_unknown op ~rhs:i) nums2
        ~folder:N.Rhs.Set.fold
        ~other_side:arg1
    | (Proved _ | Unknown), (Proved _ | Unknown) ->
      result_unknown ()
    | Invalid, _ | _, Invalid ->
      result_invalid ()
end

module Int_ops_for_binary_arith (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig
    with type op = P.binary_int_arith_op
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = I.Num

  type op = P.binary_int_arith_op

  (* There are never any restrictions on the constant propagation of
     integers, unlike for floats. *)
  let ok_to_evaluate _env = true

  let kind = I.kind
  let result_kind = K.Standard_int_or_float.to_kind kind

  let prover_lhs = I.unboxed_prover
  let prover_rhs = I.unboxed_prover

  let unknown _ =
    match kind with
    | Tagged_immediate -> T.any_tagged_immediate ()
    | Naked_immediate -> T.any_naked_immediate ()
    | Naked_float -> T.any_naked_float ()
    | Naked_int32 -> T.any_naked_int32 ()
    | Naked_int64 -> T.any_naked_int64 ()
    | Naked_nativeint -> T.any_naked_nativeint ()

  let these = I.these_unboxed

  let term = I.term_unboxed

  module Pair = I.Num.Pair
  let cross_product = I.Num.cross_product

  let op (op : P.binary_int_arith_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some I.Num.add
    | Sub -> always_some I.Num.sub
    | Mul -> always_some I.Num.mul
    | Div -> I.Num.div n1 n2
    | Mod -> I.Num.mod_ n1 n2
    | And -> always_some I.Num.and_
    | Or -> always_some I.Num.or_
    | Xor -> always_some I.Num.xor

  type symmetric_op =
    | Add
    | Mul
    | And
    | Or
    | Xor

  module Num = I.Num

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side
        : Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add ->
      if Num.equal this_side Num.zero then The_other_side
      else Cannot_simplify
    | Mul ->
      if Num.equal this_side Num.zero then Exactly Num.zero
      else if Num.equal this_side Num.one then The_other_side
      else if Num.equal this_side Num.minus_one then Negation_of_the_other_side
      else Cannot_simplify
    | And ->
      if Num.equal this_side Num.minus_one then The_other_side
      else if Num.equal this_side Num.zero then Exactly Num.zero
      else Cannot_simplify
    | Or ->
      if Num.equal this_side Num.minus_one then Exactly Num.minus_one
      else if Num.equal this_side Num.zero then The_other_side
      else Cannot_simplify
    | Xor ->
      if Num.equal this_side Num.zero then The_other_side
      else Cannot_simplify

  let op_lhs_unknown (op : P.binary_int_arith_op) ~rhs
        : Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | And -> symmetric_op_one_side_unknown And ~this_side:rhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:rhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:rhs
    | Sub ->
      if Num.equal rhs Num.zero then The_other_side
      else Cannot_simplify
    | Div ->
      (* CR mshinwell: We should think very carefully to make sure our
         handling of division is correct.  Also see whether unsafe division
         can be exposed to the user.  The current assumption that division
         by zero reaching here is dead code. *)
      if Num.equal rhs Num.zero then Invalid
      else if Num.equal rhs Num.one then The_other_side
      else if Num.equal rhs Num.minus_one then Negation_of_the_other_side
      (* CR mshinwell: Add 0 / x = 0 when x <> 0 *)
      else Cannot_simplify
    | Mod ->
      (* CR mshinwell: We could be more clever for Mod and And *)
      if Num.equal rhs Num.zero then Invalid
      else if Num.equal rhs Num.one then Exactly Num.zero
      else if Num.equal rhs Num.minus_one then Exactly Num.zero
      else Cannot_simplify

  let op_rhs_unknown (op : P.binary_int_arith_op) ~lhs
        : Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | And -> symmetric_op_one_side_unknown And ~this_side:lhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:lhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:lhs
    | Sub ->
      if Num.equal lhs Num.zero then Negation_of_the_other_side
      else Cannot_simplify
    | Div | Mod -> Cannot_simplify
end

module Int_ops_for_binary_arith_tagged_immediate =
  Int_ops_for_binary_arith (A.For_tagged_immediates)
module Int_ops_for_binary_arith_naked_immediate =
  Int_ops_for_binary_arith (A.For_naked_immediates)
module Int_ops_for_binary_arith_int32 =
  Int_ops_for_binary_arith (A.For_int32s)
module Int_ops_for_binary_arith_int64 =
  Int_ops_for_binary_arith (A.For_int64s)
module Int_ops_for_binary_arith_nativeint =
  Int_ops_for_binary_arith (A.For_nativeints)

module Binary_int_arith_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_arith_tagged_immediate)
module Binary_int_arith_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_arith_naked_immediate)
module Binary_int_arith_int32 =
  Binary_arith_like (Int_ops_for_binary_arith_int32)
module Binary_int_arith_int64 =
  Binary_arith_like (Int_ops_for_binary_arith_int64)
module Binary_int_arith_nativeint =
  Binary_arith_like (Int_ops_for_binary_arith_nativeint)

module Int_ops_for_binary_shift (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig
    with type op = P.int_shift_op
end = struct
  module Lhs = I.Num
  module Rhs = Targetint_31_63
  module Result = I.Num

  type op = P.int_shift_op

  let kind = I.kind
  let result_kind = K.Standard_int_or_float.to_kind kind

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover
  let prover_rhs = T.prove_naked_immediates

  let unknown _ =
    match kind with
    | Tagged_immediate -> T.any_tagged_immediate ()
    | Naked_immediate -> T.any_naked_immediate ()
    | Naked_float -> T.any_naked_float ()
    | Naked_int32 -> T.any_naked_int32 ()
    | Naked_int64 -> T.any_naked_int64 ()
    | Naked_nativeint -> T.any_naked_nativeint ()

  let these = I.these_unboxed

  let term = I.term_unboxed

  (* CR mshinwell: Try to factor out this cross product code directly into
     the stdlib *)
  module Pair = struct
    type nonrec t = Lhs.t * Rhs.t
    include Container_types.Make_pair (Lhs) (Rhs)
  end

  let cross_product set1 set2 =
    Lhs.Set.fold (fun elt1 result ->
        Rhs.Set.fold (fun elt2 result ->
            Pair.Set.add (elt1, elt2) result)
          set2
          result)
      set1
      Pair.Set.empty

  module Num = I.Num

  let op (op : P.int_shift_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Lsl -> always_some Num.shift_left
    | Lsr -> always_some Num.shift_right_logical
    | Asr -> always_some Num.shift_right

  let op_lhs_unknown (op : P.int_shift_op) ~rhs
        : Num.t binary_arith_outcome_for_one_side_only =
    let module O = Targetint_31_63.Imm in
    let rhs = Targetint_31_63.to_targetint rhs in
    match op with
    | Lsl | Lsr | Asr ->
      (* Shifting either way by [Targetint_32_64.size] or above, or by a negative
         amount, is undefined.

         However note that we cannot produce [Invalid] unless the code is
         type unsafe, which it is not here.  (Otherwise a GADT match might
         be reduced to only one possible case which it would be wrong to
         take.) *)
      if O.equal rhs O.zero then The_other_side
      else Cannot_simplify

  let op_rhs_unknown (op : P.int_shift_op) ~lhs
        : Num.t binary_arith_outcome_for_one_side_only =
    (* In these cases we are giving a semantics for some cases where the
       right-hand side may be less than zero or greater than or equal to
       [Targetint_32_64.size].  These cases have undefined semantics, as above;
       however, it seems fine to give them a semantics since there is benefit
       to doing so in this particular case.  (This is not the case for
       the situation in [op_lhs_unknown], above, where there would be no
       such benefit.) *)
    match op with
    | Lsl | Lsr ->
      if Num.equal lhs Num.zero then Exactly Num.zero
      else Cannot_simplify
    | Asr ->
      if Num.equal lhs Num.zero then Exactly Num.zero
      else if Num.equal lhs Num.minus_one then Exactly Num.minus_one
      else Cannot_simplify
end

module Int_ops_for_binary_shift_tagged_immediate =
  Int_ops_for_binary_shift (A.For_tagged_immediates)
module Int_ops_for_binary_shift_naked_immediate =
  Int_ops_for_binary_shift (A.For_naked_immediates)
module Int_ops_for_binary_shift_int32 =
  Int_ops_for_binary_shift (A.For_int32s)
module Int_ops_for_binary_shift_int64 =
  Int_ops_for_binary_shift (A.For_int64s)
module Int_ops_for_binary_shift_nativeint =
  Int_ops_for_binary_shift (A.For_nativeints)

module Binary_int_shift_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_shift_tagged_immediate)
module Binary_int_shift_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_shift_naked_immediate)
module Binary_int_shift_int32 =
  Binary_arith_like (Int_ops_for_binary_shift_int32)
module Binary_int_shift_int64 =
  Binary_arith_like (Int_ops_for_binary_shift_int64)
module Binary_int_shift_nativeint =
  Binary_arith_like (Int_ops_for_binary_shift_nativeint)

module Int_ops_for_binary_comp (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig
    with type op = P.ordered_comparison P.comparison_behaviour
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = Targetint_31_63

  type op = P.ordered_comparison P.comparison_behaviour

  let kind = I.kind
  let result_kind = K.naked_immediate

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover
  let prover_rhs = I.unboxed_prover

  let unknown (op : op) =
    match op with
    | Yielding_bool _ -> T.these_naked_immediates Targetint_31_63.all_bools
    | Yielding_int_like_compare_functions ->
      T.these_naked_immediates Targetint_31_63.zero_one_and_minus_one

  let these = T.these_naked_immediates

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = I.Num.Pair
  let cross_product = I.Num.cross_product

  module Num = I.Num

  let op (op : P.ordered_comparison P.comparison_behaviour) n1 n2 =
    match op with
    | Yielding_bool op ->
      let bool b = Targetint_31_63.bool b in
      begin match op with
      | Lt -> Some (bool (Num.compare n1 n2 < 0))
      | Gt -> Some (bool (Num.compare n1 n2 > 0))
      | Le -> Some (bool (Num.compare n1 n2 <= 0))
      | Ge -> Some (bool (Num.compare n1 n2 >= 0))
      end
    | Yielding_int_like_compare_functions ->
      let int i = Targetint_31_63.int (Targetint_31_63.Imm.of_int i) in
      let c = Num.compare n1 n2 in
      if c < 0 then Some (int (-1))
      else if c = 0 then Some (int 0)
      else Some (int 1)

  let op_lhs_unknown _op ~rhs:_ = Cannot_simplify
  let op_rhs_unknown _op ~lhs:_ = Cannot_simplify
end

module Int_ops_for_binary_comp_tagged_immediate =
  Int_ops_for_binary_comp (A.For_tagged_immediates)
module Int_ops_for_binary_comp_naked_immediate =
  Int_ops_for_binary_comp (A.For_naked_immediates)
module Int_ops_for_binary_comp_int32 =
  Int_ops_for_binary_comp (A.For_int32s)
module Int_ops_for_binary_comp_int64 =
  Int_ops_for_binary_comp (A.For_int64s)
module Int_ops_for_binary_comp_nativeint =
  Int_ops_for_binary_comp (A.For_nativeints)

module Binary_int_comp_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_tagged_immediate)
module Binary_int_comp_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_naked_immediate)
module Binary_int_comp_int32 =
  Binary_arith_like (Int_ops_for_binary_comp_int32)
module Binary_int_comp_int64 =
  Binary_arith_like (Int_ops_for_binary_comp_int64)
module Binary_int_comp_nativeint =
  Binary_arith_like (Int_ops_for_binary_comp_nativeint)

module Int_ops_for_binary_comp_unsigned (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig
    with type op = P.ordered_comparison P.comparison_behaviour
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = Targetint_31_63

  type op = P.ordered_comparison P.comparison_behaviour

  let kind = I.kind
  let result_kind = K.naked_immediate

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover
  let prover_rhs = I.unboxed_prover

  let unknown (op : op) =
    match op with
    | Yielding_bool _ -> T.these_naked_immediates Targetint_31_63.all_bools
    | Yielding_int_like_compare_functions ->
      T.these_naked_immediates Targetint_31_63.zero_one_and_minus_one

  let these = T.these_naked_immediates

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = I.Num.Pair
  let cross_product = I.Num.cross_product

  module Num = I.Num

  let op (op : P.ordered_comparison P.comparison_behaviour) n1 n2 =
    match op with
    | Yielding_bool op ->
      let bool b = Targetint_31_63.bool b in
      begin match op with
      | Lt -> Some (bool (Num.compare_unsigned n1 n2 < 0))
      | Gt -> Some (bool (Num.compare_unsigned n1 n2 > 0))
      | Le -> Some (bool (Num.compare_unsigned n1 n2 <= 0))
      | Ge -> Some (bool (Num.compare_unsigned n1 n2 >= 0))
      end
    | Yielding_int_like_compare_functions ->
      let int i = Targetint_31_63.int (Targetint_31_63.Imm.of_int i) in
      let c = Num.compare_unsigned n1 n2 in
      if c < 0 then Some (int (-1))
      else if c = 0 then Some (int 0)
      else Some (int 1)

  let op_lhs_unknown _op ~rhs:_ = Cannot_simplify
  let op_rhs_unknown _op ~lhs:_ = Cannot_simplify
end

module Int_ops_for_binary_comp_unsigned_tagged_immediate =
  Int_ops_for_binary_comp_unsigned (A.For_tagged_immediates)
module Int_ops_for_binary_comp_unsigned_naked_immediate =
  Int_ops_for_binary_comp_unsigned (A.For_naked_immediates)
module Int_ops_for_binary_comp_unsigned_int32 =
  Int_ops_for_binary_comp_unsigned (A.For_int32s)
module Int_ops_for_binary_comp_unsigned_int64 =
  Int_ops_for_binary_comp_unsigned (A.For_int64s)
module Int_ops_for_binary_comp_unsigned_nativeint =
  Int_ops_for_binary_comp_unsigned (A.For_nativeints)

module Binary_int_comp_unsigned_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned_tagged_immediate)
module Binary_int_comp_unsigned_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned_naked_immediate)
module Binary_int_comp_unsigned_int32 =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned_int32)
module Binary_int_comp_unsigned_int64 =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned_int64)
module Binary_int_comp_unsigned_nativeint =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned_nativeint)

module Float_ops_for_binary_arith : sig
  include Binary_arith_like_sig
    with type op = P.binary_float_arith_op
end = struct
  module F = Float_by_bit_pattern

  module Lhs = F
  module Rhs = F
  module Result = F

  type op = P.binary_float_arith_op

  let kind = K.Standard_int_or_float.Naked_float
  let result_kind = K.naked_float

  let ok_to_evaluate denv = DE.float_const_prop denv

  let prover_lhs = T.prove_naked_floats
  let prover_rhs = T.prove_naked_floats

  let unknown _ = T.any_naked_float ()
  let these = T.these_naked_floats

  let term f =
    Named.create_simple (Simple.const (Reg_width_const.naked_float f))

  module Pair = F.Pair
  let cross_product = F.cross_product

  let op (op : op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some F.IEEE_semantics.add
    | Sub -> always_some F.IEEE_semantics.sub
    | Mul -> always_some F.IEEE_semantics.mul
    | Div -> always_some F.IEEE_semantics.div

  type symmetric_op =
    | Add
    | Mul

  (* To be certain of correctness we restrict identities on floating-point
     numbers to those that preserve the _bit pattern_. *)

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side
        : F.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add ->
      (* You might think that "x + 0" has the same representation as "x".
         However it doesn't in the case where that constant zero is +0 and
         x is equal to -0.
      *)
      Cannot_simplify
    | Mul ->
      if F.equal this_side F.one then
        The_other_side
        [@z3 check_float_binary_neutral `Mul (+1.0) `Right]
        [@z3 check_float_binary_neutral `Mul (+1.0) `Left]
      else if F.equal this_side F.minus_one then
        Float_negation_of_the_other_side
        [@z3 check_float_binary_opposite `Mul (-1.0) `Left]
        [@z3 check_float_binary_opposite `Mul (-1.0) `Right]
      else
        Cannot_simplify

  let op_lhs_unknown (op : op) ~rhs
        : F.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | Sub -> Cannot_simplify
    | Div ->
      if F.equal rhs F.one then
        The_other_side
        [@z3 check_float_binary_neutral `Div (+1.0) `Right]
      else if F.equal rhs F.minus_one then
        Float_negation_of_the_other_side
        [@z3 check_float_binary_opposite `Div (-1.0) `Right]
      else
        Cannot_simplify

  let op_rhs_unknown (op : op)
        ~lhs : F.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | Sub -> Cannot_simplify
    | Div -> Cannot_simplify
end

module Binary_float_arith = Binary_arith_like (Float_ops_for_binary_arith)

module Float_ops_for_binary_comp : sig
  include Binary_arith_like_sig
    with type op = P.comparison P.comparison_behaviour
end = struct
  module F = Float_by_bit_pattern

  module Lhs = F
  module Rhs = F
  module Result = Targetint_31_63

  type op = P.comparison P.comparison_behaviour

  let kind = K.Standard_int_or_float.Naked_float
  let result_kind = K.naked_immediate

  let ok_to_evaluate denv = DE.float_const_prop denv

  let prover_lhs = T.prove_naked_floats
  let prover_rhs = T.prove_naked_floats

  let unknown (op : op) =
    match op with
    | Yielding_bool _ -> T.these_naked_immediates Targetint_31_63.all_bools
    | Yielding_int_like_compare_functions ->
      T.these_naked_immediates Targetint_31_63.zero_one_and_minus_one

  let these = T.these_naked_immediates

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = F.Pair
  let cross_product = F.cross_product

  let op (op : op) n1 n2 =
    match op with
    | Yielding_bool op ->
      let has_nan = F.is_any_nan n1 || F.is_any_nan n2 in
      let bool b = Targetint_31_63.bool b in
      begin match op with
      | Eq -> Some (bool (F.IEEE_semantics.equal n1 n2))
      | Neq -> Some (bool (not (F.IEEE_semantics.equal n1 n2)))
      | Lt ->
        if has_nan then
          Some (bool false)
        else
          Some (bool (F.IEEE_semantics.compare n1 n2 < 0))
      | Gt ->
        if has_nan then
          Some (bool false)
        else
          Some (bool (F.IEEE_semantics.compare n1 n2 > 0))
      | Le ->
        if has_nan then
          Some (bool false)
        else
          Some (bool (F.IEEE_semantics.compare n1 n2 <= 0))
      | Ge ->
        if has_nan then
          Some (bool false)
        else
          Some (bool (F.IEEE_semantics.compare n1 n2 >= 0))
      end
    | Yielding_int_like_compare_functions ->
      let int i = Targetint_31_63.int (Targetint_31_63.Imm.of_int i) in
      let c = F.IEEE_semantics.compare n1 n2 in
      if c < 0 then Some (int (-1))
      else if c = 0 then Some (int 0)
      else Some (int 1)

  let result_of_comparison_with_nan (op : P.comparison) =
    match op with
    | Neq -> Exactly Targetint_31_63.bool_true
    | Eq | Lt | Gt | Le | Ge -> Exactly Targetint_31_63.bool_false

  let op_lhs_unknown (op : op) ~rhs : _ binary_arith_outcome_for_one_side_only =
    match op with
    | Yielding_bool op ->
      if F.is_any_nan rhs then result_of_comparison_with_nan op
      else Cannot_simplify
    | Yielding_int_like_compare_functions -> Cannot_simplify

  let op_rhs_unknown (op : op) ~lhs : _ binary_arith_outcome_for_one_side_only =
    match op with
    | Yielding_bool op ->
      if F.is_any_nan lhs then result_of_comparison_with_nan op
      else Cannot_simplify
    | Yielding_int_like_compare_functions -> Cannot_simplify
end

module Binary_float_comp = Binary_arith_like (Float_ops_for_binary_comp)

module Int_ops_for_binary_eq_comp (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig
    with type op = P.equality_comparison
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = Targetint_31_63

  type op = P.equality_comparison

  let kind = I.kind
  let result_kind = K.naked_immediate

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover
  let prover_rhs = I.unboxed_prover

  let unknown _ = T.any_naked_immediate ()
  let these = T.these_naked_immediates

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = I.Num.Pair
  let cross_product = I.Num.cross_product

  module Num = I.Num

  let op (op : P.equality_comparison) n1 n2 =
    let bool b = Targetint_31_63.bool b in
    match op with
    | Eq -> Some (bool (Num.compare n1 n2 = 0))
    | Neq -> Some (bool (Num.compare n1 n2 <> 0))

  let op_lhs_unknown _op ~rhs:_ = Cannot_simplify
  let op_rhs_unknown _op ~lhs:_ = Cannot_simplify
end

module Int_ops_for_binary_eq_comp_tagged_immediate =
  Int_ops_for_binary_eq_comp (A.For_tagged_immediates)
module Int_ops_for_binary_eq_comp_naked_immediate =
  Int_ops_for_binary_eq_comp (A.For_naked_immediates)
module Int_ops_for_binary_eq_comp_int32 =
  Int_ops_for_binary_eq_comp (A.For_int32s)
module Int_ops_for_binary_eq_comp_int64 =
  Int_ops_for_binary_eq_comp (A.For_int64s)
module Int_ops_for_binary_eq_comp_nativeint =
  Int_ops_for_binary_eq_comp (A.For_nativeints)

module Binary_int_eq_comp_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_eq_comp_tagged_immediate)
module Binary_int_eq_comp_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_eq_comp_naked_immediate)
module Binary_int_eq_comp_int32 =
  Binary_arith_like (Int_ops_for_binary_eq_comp_int32)
module Binary_int_eq_comp_int64 =
  Binary_arith_like (Int_ops_for_binary_eq_comp_int64)
module Binary_int_eq_comp_nativeint =
  Binary_arith_like (Int_ops_for_binary_eq_comp_nativeint)

let simplify_immutable_block_load (access_kind : P.Block_access_kind.t)
      ~min_name_mode dacc ~original_term _dbg
      ~arg1:_ ~arg1_ty:block_ty ~arg2:_ ~arg2_ty:index_ty ~result_var =
  let result_kind =
    match access_kind with
    | Values _ -> K.value
    | Naked_floats _ -> K.naked_float
  in
  let result_var' = Var_in_binding_pos.var result_var in
  let unchanged () =
    let ty = T.unknown result_kind in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.reachable original_term, env_extension, dacc
  in
  let invalid () =
    let ty = T.bottom result_kind in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Simplified_named.invalid (), env_extension, dacc
  in
  let exactly simple =
    let env_extension =
      TEE.one_equation (Name.var result_var')
        (T.alias_type_of result_kind simple)
    in
    Simplified_named.reachable original_term, env_extension, dacc
  in
  let typing_env = DA.typing_env dacc in
  match T.prove_equals_single_tagged_immediate typing_env index_ty with
  | Invalid -> invalid ()
  | Unknown -> unchanged ()
  | Proved index ->
    let skip_simplification =
      let size =
        match access_kind with
        | Values { size; _ } | Naked_floats { size; } -> size
      in
      match size with
      | Unknown -> false
      | Known size ->
        match Flambda_features.Expert.max_block_size_for_projections () with
        | None -> false
        | Some max_size ->
          let max_size = Targetint_31_63.Imm.of_int max_size in
          not (Targetint_31_63.Imm.(<=) size max_size)
    in
    match
      T.prove_block_field_simple typing_env ~min_name_mode block_ty index
    with
    | Invalid -> invalid ()
    | Proved simple -> exactly simple
    | Unknown when skip_simplification -> unchanged ()
    | Unknown ->
      let n =
        Targetint_31_63.Imm.add (Targetint_31_63.to_targetint index) Targetint_31_63.Imm.one
      in
      (* CR mshinwell: We should be able to use the size in the [access_kind]
         to constrain the type of the block *)
      let tag : _ Or_unknown.t =
        match access_kind with
        | Values { tag; _ } -> Or_unknown.map tag ~f:Tag.Scannable.to_tag
        | Naked_floats { size; } ->
          match size with
          | Known size ->
            (* We don't expect blocks of naked floats of size zero (it doesn't
               seem that the frontend currently emits code to create such
               blocks) and so it isn't clear whether such blocks should have
               tag zero (like zero-sized naked float arrays) or another tag. *)
            if Targetint_31_63.Imm.equal size Targetint_31_63.Imm.zero then
              Unknown
            else
              Known Tag.double_array_tag
          | Unknown -> Unknown
      in
      Simplify_common.simplify_projection
        dacc ~original_term ~deconstructing:block_ty
        ~shape:(T.immutable_block_with_size_at_least ~tag ~n
          ~field_kind:result_kind ~field_n_minus_one:result_var')
        ~result_var ~result_kind

let simplify_phys_equal (op : P.equality_comparison)
      (kind : K.t) dacc ~original_term dbg
      ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var =
  let result = Name.var (Var_in_binding_pos.var result_var) in
  let const bool =
    let env_extension =
      TEE.one_equation result
        (T.this_naked_immediate (Targetint_31_63.bool bool))
    in
    Simplified_named.reachable (Named.create_simple (Simple.const_bool bool)),
      env_extension, dacc
  in
  if Simple.equal arg1 arg2 then
    match op with
    | Eq -> const true
    | Neq -> const false
  else
    begin match kind with
    | Value ->
      let typing_env = DA.typing_env dacc in
      let proof1 = T.prove_equals_tagged_immediates typing_env arg1_ty in
      let proof2 = T.prove_equals_tagged_immediates typing_env arg2_ty in
      begin match proof1, proof2 with
      | Proved _, Proved _ ->
        Binary_int_eq_comp_tagged_immediate.simplify op dacc ~original_term dbg
          ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
      | _, _ ->
        let physically_equal =
          false
          (* CR mshinwell: Resurrect this -- see cps_types branch.
          T.values_physically_equal arg1_ty arg2_ty
          *)
        in
        let physically_distinct =
          false
          (* CR mshinwell: Resurrect this -- see cps_types branch.
          (* Structural inequality implies physical inequality. *)
          let env = E.get_typing_environment env in
          T.values_structurally_distinct (env, arg1_ty) (env, arg2_ty)
          *)
        in
        begin match op, physically_equal, physically_distinct with
  (*
        | Eq, true, _ -> const true
        | Neq, true, _ -> const false
        | Eq, _, true -> const false
        | Neq, _, true -> const true
  *)
        | _, _, _ ->
          let env_extension =
            TEE.one_equation result
              (T.these_naked_immediates Targetint_31_63.all_bools)
          in
          Simplified_named.reachable original_term, env_extension, dacc
        end
      end
    | Naked_number Naked_immediate ->
      let typing_env = DA.typing_env dacc in
      let proof1 = T.prove_naked_immediates typing_env arg1_ty in
      let proof2 = T.prove_naked_immediates typing_env arg2_ty in
      begin match proof1, proof2 with
      | Proved _, Proved _ ->
        Binary_int_eq_comp_naked_immediate.simplify op dacc ~original_term dbg
          ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
      | _, _ ->
        let env_extension =
          TEE.one_equation result
            (T.these_naked_immediates Targetint_31_63.all_bools)
        in
        Simplified_named.reachable original_term, env_extension, dacc
      end
    | Naked_number Naked_float ->
      (* CR mshinwell: Should this case be statically disallowed in the type,
         to force people to use [Float_comp]? *)
      let op : P.comparison =
        match op with
        | Eq -> Eq
        | Neq -> Neq
      in
      Binary_float_comp.simplify (Yielding_bool op) dacc ~original_term dbg
        ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
    | Naked_number Naked_int32 ->
      Binary_int_eq_comp_int32.simplify op dacc ~original_term dbg
        ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
    | Naked_number Naked_int64 ->
      Binary_int_eq_comp_int64.simplify op dacc ~original_term dbg
        ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
    | Naked_number Naked_nativeint ->
      Binary_int_eq_comp_nativeint.simplify op dacc ~original_term dbg
        ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var
    | Fabricated -> Misc.fatal_error "Fabricated kind not expected here"
    | Rec_info -> Misc.fatal_error "Rec_info kind not expected here"
    end

let simplify_binary_primitive dacc (prim : P.binary_primitive)
      ~arg1 ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var =
  let result_var' = Var_in_binding_pos.var result_var in
  let min_name_mode = Var_in_binding_pos.name_mode result_var in
  let original_prim : P.t = Binary (prim, arg1, arg2) in
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Block_load (access_kind, Immutable) ->
      simplify_immutable_block_load access_kind ~min_name_mode
    | Int_arith (kind, op) ->
      begin match kind with
      | Tagged_immediate -> Binary_int_arith_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_arith_naked_immediate.simplify op
      | Naked_int32 -> Binary_int_arith_int32.simplify op
      | Naked_int64 -> Binary_int_arith_int64.simplify op
      | Naked_nativeint -> Binary_int_arith_nativeint.simplify op
      end
    | Int_shift (kind, op) ->
      begin match kind with
      | Tagged_immediate -> Binary_int_shift_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_shift_naked_immediate.simplify op
      | Naked_int32 -> Binary_int_shift_int32.simplify op
      | Naked_int64 -> Binary_int_shift_int64.simplify op
      | Naked_nativeint -> Binary_int_shift_nativeint.simplify op
      end
    | Int_comp (kind, Signed, op) ->
      begin match kind with
      | Tagged_immediate -> Binary_int_comp_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_comp_naked_immediate.simplify op
      | Naked_int32 -> Binary_int_comp_int32.simplify op
      | Naked_int64 -> Binary_int_comp_int64.simplify op
      | Naked_nativeint -> Binary_int_comp_nativeint.simplify op
      end
    | Int_comp (kind, Unsigned, op) ->
      begin match kind with
      | Tagged_immediate ->
        Binary_int_comp_unsigned_tagged_immediate.simplify op
      | Naked_immediate ->
        Binary_int_comp_unsigned_naked_immediate.simplify op
      | Naked_int32 -> Binary_int_comp_unsigned_int32.simplify op
      | Naked_int64 -> Binary_int_comp_unsigned_int64.simplify op
      | Naked_nativeint -> Binary_int_comp_unsigned_nativeint.simplify op
      end
    | Float_arith op -> Binary_float_arith.simplify op
    | Float_comp op -> Binary_float_comp.simplify op
    | Phys_equal (kind, op) -> simplify_phys_equal op kind
    | Block_load _
    | Array_load _
    | String_or_bigstring_load _
    | Bigarray_load _ ->
      fun dacc ~original_term:_ dbg ~arg1 ~arg1_ty:_ ~arg2 ~arg2_ty:_
          ~result_var:_ ->
        let prim : P.t = Binary (prim, arg1, arg2) in
        let named = Named.create_prim prim dbg in
        let ty = T.unknown (P.result_kind' prim) in
        let env_extension = TEE.one_equation (Name.var result_var') ty in
        Simplified_named.reachable named, env_extension, dacc
  in
  let reachable, env_extension, dacc =
    simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty
      ~result_var
  in
  reachable, env_extension, [arg1; arg2], dacc
