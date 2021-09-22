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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Not_yet_decided
  | Never_inline_attribute
  | Function_body_too_large of Code_size.t
  | Stub
  | Attribute_inline
  | Small_function of
      { size : Code_size.t;
        small_function_size : Code_size.t
      }
  | Speculatively_inlinable of
      { size : Code_size.t;
        small_function_size : Code_size.t;
        large_function_size : Code_size.t
      }
  | Functor of { size : Code_size.t }

val print : Format.formatter -> t -> unit

val report : Format.formatter -> t -> unit

type inlining_behaviour = private
  | Cannot_be_inlined
  | Must_be_inlined
  | Could_possibly_be_inlined

val behaviour : t -> inlining_behaviour
