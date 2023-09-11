(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Guillaume Bury and Nathanaëlle Courant, OCamlPro               *)
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

(* These are use primarily by the continuation lifting process. During that
   process, lifted continuations need to have new parameters. While the lifting
   can rewrite some of the uses of lifted continuations, rewriting all of them
   (particularly inside the bodies of lifted continuations) woudl exhibit bad
   complexity. *)
type extra_args =
  { added_args : Simple.t list;
    after_nth_arg : int
  }

type t = extra_args Continuation.Map.t
