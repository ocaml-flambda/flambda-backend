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

type t = unit Code0.t

let code_metadata = Code0.code_metadata

module Metadata_view = struct
  type nonrec 'a t = t

  let metadata = code_metadata
end

include Code_metadata.Code_metadata_accessors [@inlined hint] (Metadata_view)

let create_with_metadata =
  Code0.create_with_metadata ~print_function_params_and_body:Unit.print
    ~params_and_body:()

let create =
  Code0.create ~print_function_params_and_body:Unit.print ~params_and_body:()

let print = Code0.print ~print_function_params_and_body:Unit.print

let free_names = Code0.free_names

let apply_renaming =
  Code0.apply_renaming ~apply_renaming_function_params_and_body:(fun () _ -> ())
