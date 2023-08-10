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

open Or_bottom.Let_syntax

(* This module conceals the implementation of type ['head t]. Functions such as
   [T.descr] can be inlined with return values unboxed by Flambda 2. *)
module T : sig
  module Descr : sig
    type 'head t = private
      | No_alias of 'head
      | Equals of Simple.t

    val print :
      print_head:(Format.formatter -> 'head -> unit) ->
      Format.formatter ->
      'head t ->
      unit

    val apply_renaming :
      apply_renaming_head:('head -> Renaming.t -> 'head) ->
      'head t ->
      Renaming.t ->
      'head t

    val free_names :
      free_names_head:('head -> Name_occurrences.t) ->
      'head t ->
      Name_occurrences.t
  end

  type 'head t

  val create : 'head -> 'head t

  val create_equals : Simple.t -> _ t

  val bottom : _ t

  val unknown : _ t

  val descr : 'head t -> 'head Descr.t Or_unknown_or_bottom.t

  val is_obviously_bottom : _ t -> bool

  val is_obviously_unknown : _ t -> bool

  val get_alias_exn : 'head t -> Simple.t

  val apply_renaming :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Renaming.t ->
    'head t

  val free_names :
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t

  val free_names_no_cache :
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t

  val remove_unused_value_slots_and_shortcut_aliases :
    remove_unused_value_slots_and_shortcut_aliases_head:
      ('head ->
      used_value_slots:Value_slot.Set.t ->
      canonicalise:(Simple.t -> Simple.t) ->
      'head) ->
    'head t ->
    used_value_slots:Value_slot.Set.t ->
    canonicalise:(Simple.t -> Simple.t) ->
    'head t

  val project_variables_out :
    free_names_head:('head -> Name_occurrences.t) ->
    to_project:Variable.Set.t ->
    expand:(Variable.t -> coercion:Coercion.t -> 'head t) ->
    project_head:('head -> 'head) ->
    'head t ->
    'head t
end = struct
  module Descr = struct
    type 'head t =
      | No_alias of 'head
      | Equals of Simple.t

    let print ~print_head ppf t =
      match t with
      | No_alias head -> print_head ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(%t=%t %a)@]" Flambda_colours.error
          Flambda_colours.pop Simple.print simple

    let[@inline always] apply_renaming ~apply_renaming_head t renaming =
      if Renaming.is_identity renaming
      then t
      else
        match t with
        | No_alias head ->
          let head' = apply_renaming_head head renaming in
          if head == head' then t else No_alias head'
        | Equals simple ->
          let simple' = Simple.apply_renaming simple renaming in
          if simple == simple' then t else Equals simple'

    let[@inline always] free_names ~free_names_head t =
      match t with
      | No_alias head -> free_names_head head
      | Equals simple ->
        Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
          (Simple.free_names simple) Name_mode.in_types

    let remove_unused_value_slots_and_shortcut_aliases
        ~remove_unused_value_slots_and_shortcut_aliases_head t ~used_value_slots
        ~canonicalise =
      match t with
      | No_alias head ->
        let head' =
          remove_unused_value_slots_and_shortcut_aliases_head head
            ~used_value_slots ~canonicalise
        in
        if head == head' then t else No_alias head'
      | Equals alias ->
        let canonical = canonicalise alias in
        if alias == canonical then t else Equals canonical

    type ('head, 'descr) project_result =
      | Not_expanded of 'head t
      | Expanded of 'descr

    let project_variables_out ~to_project ~expand ~project_head t =
      match t with
      | No_alias head ->
        let head' = project_head head in
        if head == head' then Not_expanded t else Not_expanded (No_alias head')
      | Equals simple ->
        Simple.pattern_match' simple
          ~const:(fun _ -> Not_expanded t)
          ~symbol:(fun symbol ~coercion ->
            if Coercion.is_id coercion
            then Not_expanded t
            else
              (* Coercions might contain variables. Removing any coercion
                 happens to fix all potential problems. *)
              Not_expanded (Equals (Simple.symbol symbol)))
          ~var:(fun var ~coercion ->
            if Variable.Set.mem var to_project
            then Expanded (expand var ~coercion)
            else Not_expanded t)
  end

  module WCFN = With_cached_free_names

  type 'head t = 'head WCFN.t Descr.t Or_unknown_or_bottom.t

  let[@inline always] descr (t : 'head t) : 'head Descr.t Or_unknown_or_bottom.t
      =
    match t with
    | Unknown -> Unknown
    | Bottom -> Bottom
    | Ok (Equals simple) -> Ok (Equals simple)
    | Ok (No_alias wcfn) -> Ok (No_alias (WCFN.descr wcfn))

  let create head : _ t = Ok (Descr.No_alias (WCFN.create head))

  let create_equals simple : _ t = Ok (Descr.Equals simple)

  let bottom : _ t = Bottom

  let unknown : _ t = Unknown

  let is_obviously_bottom (t : _ t) =
    match t with Bottom -> true | Unknown | Ok _ -> false

  let is_obviously_unknown (t : _ t) =
    match t with Unknown -> true | Bottom | Ok _ -> false

  let[@inline always] get_alias_exn (t : _ t) =
    match t with
    | Unknown | Bottom | Ok (No_alias _) -> raise Not_found
    | Ok (Equals alias) -> alias

  let apply_renaming ~apply_renaming_head ~free_names_head (t : _ t) renaming :
      _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok descr ->
      let descr' =
        Descr.apply_renaming
          ~apply_renaming_head:
            (WCFN.apply_renaming ~apply_renaming_descr:apply_renaming_head
               ~free_names_descr:free_names_head)
          descr renaming
      in
      if descr == descr' then t else Ok descr'

  let free_names ~free_names_head (t : _ t) =
    match t with
    | Unknown | Bottom -> Name_occurrences.empty
    | Ok descr ->
      Descr.free_names
        ~free_names_head:(WCFN.free_names ~free_names_descr:free_names_head)
        descr

  let free_names_no_cache ~free_names_head (t : _ t) =
    match t with
    | Unknown | Bottom -> Name_occurrences.empty
    | Ok descr ->
      Descr.free_names
        ~free_names_head:
          (WCFN.free_names_no_cache ~free_names_descr:free_names_head)
        descr

  let remove_unused_value_slots_and_shortcut_aliases
      ~remove_unused_value_slots_and_shortcut_aliases_head (t : _ t)
      ~used_value_slots ~canonicalise : _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok descr ->
      let descr' =
        Descr.remove_unused_value_slots_and_shortcut_aliases
          ~remove_unused_value_slots_and_shortcut_aliases_head:
            (WCFN.remove_unused_value_slots_and_shortcut_aliases
               ~remove_unused_value_slots_and_shortcut_aliases_descr:
                 remove_unused_value_slots_and_shortcut_aliases_head)
          descr ~used_value_slots ~canonicalise
      in
      if descr == descr' then t else Ok descr'

  let project_variables_out ~free_names_head ~to_project ~expand ~project_head
      (t : _ t) : _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok descr -> (
      let project_head wdr =
        WCFN.project_variables_out ~free_names_descr:free_names_head ~to_project
          ~project_descr:project_head wdr
      in
      match
        Descr.project_variables_out ~to_project ~expand ~project_head descr
      with
      | Not_expanded descr' -> if descr == descr' then t else Ok descr'
      | Expanded t' -> t')
end

include T

let print ~print_head ppf t =
  let colour = Flambda_colours.top_or_bottom_type in
  match descr t with
  | Unknown ->
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a4}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%tT%t" colour Flambda_colours.pop
  | Bottom ->
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a5}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%t_|_%t" colour Flambda_colours.pop
  | Ok descr -> Descr.print ~print_head ppf descr

let[@inline always] apply_coercion ~apply_coercion_head coercion t :
    _ t Or_bottom.t =
  match descr t with
  | Unknown | Bottom -> Ok t
  | Ok (Equals simple) -> (
    match Simple.apply_coercion simple coercion with
    | None -> Bottom
    | Some simple -> Ok (create_equals simple))
  | Ok (No_alias head) ->
    let<+ head = apply_coercion_head head coercion in
    create head

let ids_for_export ~ids_for_export_head (t : _ t) =
  match descr t with
  | Unknown | Bottom -> Ids_for_export.empty
  | Ok (No_alias head) -> ids_for_export_head head
  | Ok (Equals simple) -> Ids_for_export.from_simple simple
