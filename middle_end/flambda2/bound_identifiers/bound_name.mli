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

(** A [Name] equipped with extra information that arises from the name occurring
    in binding position. *)

type t

val create : Name.t -> Name_mode.t -> t

val name : t -> Name.t

val name_mode : t -> Name_mode.t

val var : Bound_var.t -> t

val symbol : Symbol.t -> t

val must_be_symbol : t -> Symbol.t

(* CR mshinwell: Ensure naming consistent with Bound_var. Make
   constructors and destructors clear. *)
val to_var : t -> Bound_var.t option

val to_name : t -> Name.t

val to_simple : t -> Simple.t

val rename : t -> t

val is_symbol : t -> bool

include Container_types.S with type t := t
