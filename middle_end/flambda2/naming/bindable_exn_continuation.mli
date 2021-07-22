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

(** An [Exn_continuation] equipped with operations that mean it can be used in
    binding position within a [Name_abstraction] value.

    When this happens, only the [exn_handler] is bound, not the [extra_args].
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Exn_continuation.t

include Bindable.S with type t := t
