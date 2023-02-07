(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let typ_region = Cmm.typ_int

(* The following two functions are here instead of [To_cmm_shared] to avoid a
   circularity. They can't go in [Flambda_kind] as that module does not depend
   on the backend. *)

let exttype_of_kind (k : Flambda_kind.t) : Cmm.exttype =
  match k with
  | Value -> XInt
  | Naked_number Naked_float -> XFloat
  | Naked_number Naked_int64 -> XInt64
  | Naked_number Naked_int32 -> XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) -> (
    match Targetint_32_64.num_bits with
    | Thirty_two -> XInt32
    | Sixty_four -> XInt64)
  | Region -> Misc.fatal_error "[Region] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

let machtype_of_kind (k : Flambda_kind.t) =
  match k with
  | Value -> Cmm.typ_val
  | Naked_number Naked_float -> Cmm.typ_float
  | Naked_number Naked_int64 -> Cmm_helpers.typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
    Cmm.typ_int
  | Region -> (* See comments elsewhere about these bindings. *) typ_region
  | Rec_info -> assert false
