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

(** Variables with name modes, as occur on the left-hand sides of
    [Let]-expressions (see [Bound_pattern]). *)

type t

val create : Variable.t -> Flambda_debug_uid.t -> Name_mode.t -> t

val var : t -> Variable.t

val debug_uid : t -> Flambda_debug_uid.t

val name_mode : t -> Name_mode.t

val with_name_mode : t -> Name_mode.t -> t

include Bindable.S with type t := t
