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

type t =
  { exn_handler : Continuation.t;
    extra_args : (Simple.t * Flambda_kind.With_subkind.t) list
  }

include Container_types.Make (struct
  type nonrec t = t

  let print_simple_and_kind ppf (simple, kind) =
    Format.fprintf ppf "@[<h>(%a@ \u{2237}@ %a)@]" Simple.print simple
      Flambda_kind.With_subkind.print kind

  let [@ocamlformat "disable"] print ppf { exn_handler; extra_args; } =
    match extra_args with
    | [] -> Continuation.print ppf exn_handler
    | _ ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(exn_handler@ %a)@]@ \
          @[<hov 1>%t(extra_args@ %t(%a)%t@<0>)%t@]\
          )@]"
        Continuation.print exn_handler
        (match extra_args with
         | [] -> Flambda_colours.elide
         | _ -> Flambda_colours.none)
        Flambda_colours.pop
        (Format.pp_print_list
          ~pp_sep:Format.pp_print_space print_simple_and_kind)
        extra_args
        (match extra_args with
         | [] -> Flambda_colours.elide
         | _ -> Flambda_colours.none)
        Flambda_colours.pop

  let compare_simple_and_kind (simple1, kind1) (simple2, kind2) =
    let c = Simple.compare simple1 simple2 in
    if c <> 0 then c else Flambda_kind.With_subkind.compare kind1 kind2

  let compare { exn_handler = exn_handler1; extra_args = extra_args1 }
      { exn_handler = exn_handler2; extra_args = extra_args2 } =
    let c = Continuation.compare exn_handler1 exn_handler2 in
    if c <> 0
    then c
    else
      Misc.Stdlib.List.compare compare_simple_and_kind extra_args1 extra_args2

  let equal t1 t2 = compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Exn_continuation.hash not yet implemented"
end)

let create ~exn_handler ~extra_args =
  (match Continuation.sort exn_handler with
  | Normal_or_exn -> ()
  | Return | Define_root_symbol | Toplevel_return ->
    Misc.fatal_errorf "Continuation %a has wrong sort (must be [Normal_or_exn])"
      Continuation.print exn_handler);
  { exn_handler; extra_args }

let exn_handler t = t.exn_handler

let extra_args t = t.extra_args

let free_names { exn_handler; extra_args } =
  let extra_args = List.map (fun (simple, _kind) -> simple) extra_args in
  Name_occurrences.union
    (Name_occurrences.singleton_continuation exn_handler)
    (Simple.List.free_names extra_args)

let apply_renaming ({ exn_handler; extra_args } as t) renaming =
  let exn_handler' = Renaming.apply_continuation renaming exn_handler in
  let extra_args_changed = ref false in
  let new_extra_args =
    List.map
      (fun ((simple, kind) as extra_arg) ->
        let simple' = Simple.apply_renaming simple renaming in
        if simple == simple'
        then extra_arg
        else (
          extra_args_changed := true;
          simple', kind))
      extra_args
  in
  let extra_args' =
    if !extra_args_changed then new_extra_args else extra_args
  in
  if exn_handler == exn_handler' && extra_args == extra_args'
  then t
  else { exn_handler = exn_handler'; extra_args = extra_args' }

let arity t =
  let extra_args = List.map (fun (_simple, kind) -> kind) t.extra_args in
  let exn_bucket_kind =
    Flambda_kind.With_subkind.create Flambda_kind.value Anything
  in
  Flambda_arity.create_singletons (exn_bucket_kind :: extra_args)

let with_exn_handler t exn_handler = { t with exn_handler }

let without_extra_args t = { t with extra_args = [] }

let ids_for_export { exn_handler; extra_args } =
  List.fold_left
    (fun ids (arg, _kind) -> Ids_for_export.add_simple ids arg)
    (Ids_for_export.add_continuation Ids_for_export.empty exn_handler)
    extra_args
