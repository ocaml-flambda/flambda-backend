(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

type number_decider =
  { param_name : string;
    kind : K.Naked_number_kind.t;
    prove_is_a_boxed_number : TE.t -> T.t -> unit T.proof_of_property
  }

type unboxer =
  { var_name : string;
    invalid_const : Const.t;
    unboxing_prim : Simple.t -> P.t;
    prove_simple :
      TE.t -> min_name_mode:Name_mode.t -> T.t -> Simple.t T.meet_shortcut
  }

module type Number_S = sig
  val decider : number_decider

  val unboxing_prim : Simple.t -> P.t

  val unboxer : unboxer
end

module Immediate = struct
  let decider =
    { param_name = "naked_immediate";
      kind = K.Naked_number_kind.Naked_immediate;
      prove_is_a_boxed_number = T.prove_is_a_tagged_immediate
    }

  let unboxing_prim simple = P.(Unary (Untag_immediate, simple))

  let unboxer =
    { var_name = "naked_immediate";
      invalid_const = Const.naked_immediate (Targetint_31_63.of_int 0xabcd);
      unboxing_prim;
      prove_simple = T.meet_tagging_of_simple
    }
end

module Float = struct
  let decider =
    { param_name = "unboxed_float";
      kind = K.Naked_number_kind.Naked_float;
      prove_is_a_boxed_number = T.prove_is_a_boxed_float
    }

  let unboxing_prim simple = P.(Unary (Unbox_number Naked_float, simple))

  let unboxer =
    { var_name = "unboxed_float";
      invalid_const = Const.naked_float Numeric_types.Float_by_bit_pattern.zero;
      unboxing_prim;
      prove_simple = T.meet_boxed_float_containing_simple
    }
end

module Int32 = struct
  let decider =
    { param_name = "unboxed_int32";
      kind = K.Naked_number_kind.Naked_int32;
      prove_is_a_boxed_number = T.prove_is_a_boxed_int32
    }

  let unboxing_prim simple = P.(Unary (Unbox_number Naked_int32, simple))

  let unboxer =
    { var_name = "unboxed_int32";
      invalid_const = Const.naked_int32 Int32.(div 0xabcd0l 2l);
      unboxing_prim;
      prove_simple = T.meet_boxed_int32_containing_simple
    }
end

module Int64 = struct
  let decider =
    { param_name = "unboxed_int64";
      kind = K.Naked_number_kind.Naked_int64;
      prove_is_a_boxed_number = T.prove_is_a_boxed_int64
    }

  let unboxing_prim simple = P.(Unary (Unbox_number Naked_int64, simple))

  let unboxer =
    { var_name = "unboxed_int64";
      invalid_const = Const.naked_int64 Int64.(div 0xdcba0L 2L);
      unboxing_prim;
      prove_simple = T.meet_boxed_int64_containing_simple
    }
end

module Nativeint = struct
  let decider =
    { param_name = "unboxed_nativeint";
      kind = K.Naked_number_kind.Naked_nativeint;
      prove_is_a_boxed_number = T.prove_is_a_boxed_nativeint
    }

  let unboxing_prim simple = P.(Unary (Unbox_number Naked_nativeint, simple))

  let unboxer =
    { var_name = "unboxed_nativeint";
      invalid_const = Const.naked_nativeint Targetint_32_64.zero;
      unboxing_prim;
      prove_simple = T.meet_boxed_nativeint_containing_simple
    }
end

module type Vector_type = sig
  val t : Vector_types.t
end

module Vector_unboxer (Vector_type : Vector_type) = struct
  let decider =
    { param_name = "unboxed_" ^ Vector_types.name Vector_type.t;
      kind = K.Naked_number_kind.Naked_vector Vector_type.t;
      prove_is_a_boxed_number = prove_is_a_boxed_vector Vector_type.t
    }

  let unboxing_prim simple =
    P.(Unary (Unbox_number (Naked_vector Vector_type.t), simple))

  let zero =
    match Vector_type.t with
    | Vec128 ty -> Const.naked_vec128 ty Vector_types.Vec128.Bit_pattern.zero

  let unboxer =
    { var_name = "unboxed_" ^ Vector_types.name Vector_type.t;
      invalid_const = zero;
      unboxing_prim;
      prove_simple = T.meet_boxed_vector_containing_simple Vector_type.t
    }
end

module Int8x16 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Int8x16)
end)

module Int16x8 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Int16x8)
end)

module Int32x4 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Int32x4)
end)

module Int64x2 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Int64x2)
end)

module Float32x4 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Float32x4)
end)

module Float64x2 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Float64x2)
end)

module Unknown128 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Unknown128)
end)

module Any128 = Vector_unboxer (struct
  let t = Vector_types.(Vec128 Any128)
end)

let vector_unboxer : Vector_types.t -> unboxer = function
  | Vec128 Unknown128 -> Unknown128.unboxer
  | Vec128 Int8x16 -> Int8x16.unboxer
  | Vec128 Int16x8 -> Int16x8.unboxer
  | Vec128 Int32x4 -> Int32x4.unboxer
  | Vec128 Int64x2 -> Int64x2.unboxer
  | Vec128 Float32x4 -> Float32x4.unboxer
  | Vec128 Float64x2 -> Float64x2.unboxer
  | Vec128 Any128 -> Any128.unboxer

module Field = struct
  let unboxing_prim bak ~block ~index =
    let field_const = Simple.const (Const.tagged_immediate index) in
    P.Binary (Block_load (bak, Immutable), block, field_const)

  let unboxer ~invalid_const bak ~index =
    { var_name = "field_at_use";
      invalid_const;
      unboxing_prim = (fun block -> unboxing_prim bak ~block ~index);
      prove_simple =
        (fun tenv ~min_name_mode t ->
          T.meet_block_field_simple tenv ~min_name_mode
            ~field_kind:(P.Block_access_kind.element_kind_for_load bak)
            t index)
    }
end

module Closure_field = struct
  let unboxing_prim function_slot ~closure value_slot kind =
    P.Unary
      ( Project_value_slot { project_from = function_slot; value_slot; kind },
        closure )

  let unboxer function_slot value_slot kind =
    { var_name = "closure_field_at_use";
      invalid_const = Const.const_zero;
      unboxing_prim =
        (fun closure -> unboxing_prim function_slot ~closure value_slot kind);
      prove_simple =
        (fun tenv ~min_name_mode t ->
          T.meet_project_value_slot_simple tenv ~min_name_mode t value_slot)
    }
end
