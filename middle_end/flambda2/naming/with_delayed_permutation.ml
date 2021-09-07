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

[@@@ocaml.warning "+a-30-40-41-42"]

(* CR mshinwell: Use this module in [Expr] *)

module Make (Descr : sig
  include Contains_names.S

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end) =
struct
  type t =
    { mutable descr : Descr.t;
      mutable delayed_permutation : Renaming.t;
      mutable free_names : Name_occurrences.t option
    }

  let create descr =
    { descr; delayed_permutation = Renaming.empty; free_names = None }

  let peek_descr t = t.descr

  let descr t =
    if Renaming.is_empty t.delayed_permutation
    then t.descr
    else
      let descr = Descr.apply_renaming t.descr t.delayed_permutation in
      t.descr <- descr;
      let free_names =
        match t.free_names with
        | None -> Descr.free_names descr
        | Some free_names ->
          Name_occurrences.apply_renaming free_names t.delayed_permutation
      in
      t.delayed_permutation <- Renaming.empty;
      t.free_names <- Some free_names;
      descr

  let apply_renaming t perm =
    let delayed_permutation =
      Renaming.compose ~second:perm ~first:t.delayed_permutation
    in
    { t with delayed_permutation }

  let free_names t =
    let descr = descr t in
    match t.free_names with
    | Some free_names -> free_names
    | None ->
      let free_names = Descr.free_names descr in
      t.free_names <- Some free_names;
      free_names

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    Descr.print_with_cache ~cache ppf (descr t)

  let [@ocamlformat "disable"] print ppf t = Descr.print ppf (descr t)
end
