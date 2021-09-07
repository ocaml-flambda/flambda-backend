(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type string_contents =
  | Contents of string
  | Unknown_or_mutable

type t

(* CR mshinwell: [size] shouldn't be needed when passing [Contents] *)
val create : contents:string_contents -> size:Targetint_31_63.Imm.t -> t

val contents : t -> string_contents

val size : t -> Targetint_31_63.Imm.t

include Container_types.S with type t := t
