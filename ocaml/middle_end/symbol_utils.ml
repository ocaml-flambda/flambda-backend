(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-30-40-41-42"]

module Flambda = struct
  let for_variable var =
    Symbol.for_name (Variable.get_compilation_unit var) (Variable.unique_name var)

  let for_closure closure_id =
    Symbol.for_name (Closure_id.get_compilation_unit closure_id)
      (Closure_id.unique_name closure_id ^ "_closure")

  let for_code_of_closure closure_id =
    Symbol.for_name (Closure_id.get_compilation_unit closure_id)
      (Closure_id.unique_name closure_id)
end
