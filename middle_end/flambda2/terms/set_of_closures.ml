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

type t =
  { function_decls : Function_declarations.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
    alloc_mode : Alloc_mode.t
  }

let [@ocamlformat "disable"] print ppf
      { function_decls;
        closure_elements;alloc_mode;
      } =
  Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_elements@ %a)@]@ \
      @[<hov 1>(alloc_mode@ %a)@]\
      )@]"
    (Flambda_colours.prim_constructive ())
    (Flambda_colours.normal ())
    (Function_declarations.print) function_decls
    (Var_within_closure.Map.print Simple.print) closure_elements
    Alloc_mode.print alloc_mode

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
      { function_decls = function_decls1;
        closure_elements = closure_elements1;
        alloc_mode = alloc_mode1
      }
      { function_decls = function_decls2;
        closure_elements = closure_elements2;
        alloc_mode = alloc_mode2
      } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0
    then c
    else
      let c =
        Var_within_closure.Map.compare Simple.compare closure_elements1
          closure_elements2
      in
      if c <> 0 then c else Alloc_mode.compare alloc_mode1 alloc_mode2

  let equal t1 t2 = compare t1 t2 = 0
end)

let is_empty { function_decls; closure_elements; alloc_mode = _ } =
  Function_declarations.is_empty function_decls
  && Var_within_closure.Map.is_empty closure_elements

let create ~closure_elements alloc_mode function_decls =
  (* CR mshinwell: Make sure invariant checks are applied here, e.g. that the
     set of closures is indeed closed. *)
  { function_decls; closure_elements; alloc_mode }

let function_decls t = t.function_decls

let closure_elements t = t.closure_elements

let alloc_mode t = t.alloc_mode

let has_empty_environment t = Var_within_closure.Map.is_empty t.closure_elements

let environment_doesn't_mention_variables t =
  Var_within_closure.Map.for_all
    (fun _vwc simple -> Simple.is_symbol simple)
    t.closure_elements

let [@ocamlformat "disable"] print ppf
      { function_decls;
        closure_elements;
        alloc_mode;
      } =
  if Var_within_closure.Map.is_empty closure_elements then
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ %a@ \
        @[<hov 1>%a@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      Alloc_mode.print alloc_mode
      (Function_declarations.print) function_decls
  else
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ %a@ \
        @[<hov 1>%a@]@ \
        @[<hov 1>(env@ %a)@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      Alloc_mode.print alloc_mode
      (Function_declarations.print) function_decls
      (Var_within_closure.Map.print Simple.print) closure_elements

let free_names { function_decls; closure_elements; alloc_mode = _ } =
  (* We are here interested in the the uses of closure_id and
     var_within_closure, so we do not count the closure_ids and
     var_within_closures that are bound by a set of closures. Indeed, the
     free_names will be used later to filter out unused env_vars from sets of
     closures (in the offset computation and in to_cmm), so if they are added to
     the free_names here, they can never be simplified away. *)
  Name_occurrences.union_list
    [ Function_declarations.free_names function_decls;
      Simple.List.free_names (Var_within_closure.Map.data closure_elements) ]

let apply_renaming ({ function_decls; closure_elements; alloc_mode } as t)
    renaming =
  let function_decls' =
    Function_declarations.apply_renaming function_decls renaming
  in
  let closure_elements' =
    Var_within_closure.Map.filter_map
      (fun var simple ->
        if Renaming.closure_var_is_used renaming var
        then Some (Simple.apply_renaming simple renaming)
        else None)
      closure_elements
  in
  if function_decls == function_decls' && closure_elements == closure_elements'
  then t
  else
    { function_decls = function_decls';
      closure_elements = closure_elements';
      alloc_mode
    }

let all_ids_for_export { function_decls; closure_elements; alloc_mode = _ } =
  let function_decls_ids =
    Function_declarations.all_ids_for_export function_decls
  in
  Var_within_closure.Map.fold
    (fun _closure_var simple ids -> Ids_for_export.add_simple ids simple)
    closure_elements function_decls_ids

let filter_function_declarations t ~f =
  let function_decls = Function_declarations.filter t.function_decls ~f in
  { t with function_decls }
