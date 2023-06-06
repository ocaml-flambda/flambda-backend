(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

type raw

module View : sig
  type t = private
    | Code_present of Code.t
    | Metadata_only of Code_metadata.t
end

val view : t -> View.t

(** Will return the code or cause a fatal error. *)
val get_code : t -> Code.t

val print : Format.formatter -> t -> unit

val merge : Code_id.t -> t -> t -> t option

val create : Code.t -> t

val create_metadata_only : Code_metadata.t -> t

val from_raw :
  sections:Flambda_backend_utils.File_sections.t ->
  in_current_dir:In_current_dir.t ->
  raw ->
  t

val to_raw : add_section:(Obj.t -> int) -> t -> raw

val remember_only_metadata : t -> t

val iter_code : t -> f:(Code.t -> unit) -> unit

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t

val code_metadata : t -> Code_metadata.t

val code_present : t -> bool

val map_raw_index : (int -> int) -> raw -> raw

(** As for [Code_metadata], the free names of a value of type [t] do not include
    the code ID, which is only kept for convenience. *)
include Contains_names.S with type t := t

include Contains_ids.S with type t := t
