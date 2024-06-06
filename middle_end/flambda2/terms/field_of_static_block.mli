(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
type t =
  | Symbol of Symbol.t  (** The address of the given symbol. *)
  | Tagged_immediate of Targetint_31_63.t  (** The given tagged immediate. *)
  | Dynamically_computed of Variable.t * Debuginfo.t
      (** The value of the given variable. This has [Debuginfo.t] for the same
          reason as the [Or_variable.Var] constructor does. *)

include Container_types.S with type t := t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val tagged_immediate : Targetint_31_63.t -> t

(** Inhabitants of fields of statically-allocated mixed blocks, only some
    of which are [Value].
*)
module Mixed_field : sig
  module Unboxed_number : sig
    type t =
      | Unboxed_float of Numeric_types.Float_by_bit_pattern.t
      | Unboxed_float32 of Numeric_types.Float32_by_bit_pattern.t
      | Unboxed_int32 of Numeric_types.Int32.t
      | Unboxed_int64 of Numeric_types.Int64.t
      | Unboxed_nativeint of Targetint_32_64.t

    include Container_types.S with type t := t
  end

  type nonrec t =
    | Value of t
    | Unboxed_number of Unboxed_number.t

  include Container_types.S with type t := t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end
