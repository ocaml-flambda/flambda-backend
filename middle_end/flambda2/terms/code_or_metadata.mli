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

[@@@ocaml.warning "+a-30-40-41-42"]

type t = private
  | Code_present of Code.t
  | Metadata_only of Code_metadata.t

val print : Format.formatter -> t -> unit

val merge : Code_id.t -> t -> t -> t option

val create : Code.t -> t

val remember_only_metadata : t -> t

val iter_code : t -> f:(Code.t -> unit) -> unit

val code_metadata : t -> Code_metadata.t

(** As for [Code_metadata], the free names of a value of type [t] do not include
    the code ID, which is only kept for convenience. *)
include Contains_names.S with type t := t

include Contains_ids.S with type t := t
