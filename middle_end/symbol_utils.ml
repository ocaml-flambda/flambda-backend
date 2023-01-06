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

module CU = Compilation_unit

let for_fun_ident ~compilation_unit loc id =
  let compilation_unit =
    match compilation_unit with
    | None -> CU.get_current_exn ()
    | Some cu -> cu
  in
  if Config.with_cpp_mangling then
    (* CR lmaurer: Properly integrate [Compilation_unit.t] into [Mangling] *)
    let unitname = CU.full_path_as_string compilation_unit in
    let linkage_name =
      Mangling.fun_symbol ~unitname ~loc ~id:(Ident.unique_name id)
      |> Linkage_name.of_string
    in
    Symbol.unsafe_create compilation_unit linkage_name
  else
    Symbol.for_local_ident id

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
