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

(** Constants that fit in registers on the target machine. *)

include module type of struct
  include Int_ids.Const
end

val of_descr : Descr.t -> t

val is_naked_immediate : t -> Targetint_31_63.t option

val is_tagged_immediate : t -> Targetint_31_63.t option

val is_naked_float32 : t -> Numeric_types.Float32_by_bit_pattern.t option

val is_naked_float : t -> Numeric_types.Float_by_bit_pattern.t option

val is_naked_int32 : t -> int32 option

val is_naked_int64 : t -> int64 option

val is_naked_nativeint : t -> Targetint_32_64.t option

val is_naked_vec128 : t -> Vector_types.Vec128.Bit_pattern.t option

(** Create a numeric constant of the given kind ([Region] and [Rec_info] are
    forbidden). *)

(** Create a numeric constant of the given kind ([Region] and [Rec_info] are
    forbidden). *)
val of_int_of_kind : Flambda_kind.t -> int -> t
