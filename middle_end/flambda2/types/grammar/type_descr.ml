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

  val descr :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    'head Descr.t Or_unknown_or_bottom.t

  val is_obviously_bottom : _ t -> bool

  val is_obviously_unknown : _ t -> bool

  val get_alias_exn :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Simple.t

  val apply_renaming : 'head t -> Renaming.t -> 'head t

  val free_names :
    apply_renaming_head:('head -> Renaming.t -> 'head) ->
    free_names_head:('head -> Name_occurrences.t) ->
    'head t ->
    Name_occurrences.t
end = struct
  module Descr = struct
    type 'head t =
      | No_alias of 'head
      | Equals of Simple.t

    let print ~print_head ppf t =
      match t with
      | No_alias head -> print_head ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(@<0>%s=@<0>%s %a)@]" (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Simple.print simple

    let[@inline always] apply_renaming ~apply_renaming_head t renaming =
      if Renaming.is_empty renaming
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
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          (Simple.free_names simple) Name_mode.in_types
  end

  module WDP = With_delayed_permutation

  type 'head t = 'head Descr.t WDP.t Or_unknown_or_bottom.t

  let[@inline always] descr ~apply_renaming_head ~free_names_head (t : _ t) :
      _ Descr.t Or_unknown_or_bottom.t =
    match t with
    | Unknown -> Unknown
    | Bottom -> Bottom
    | Ok wdp ->
      Ok
        (WDP.descr
           ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
           ~free_names_descr:(Descr.free_names ~free_names_head)
           wdp)

  let create head : _ t = Ok (WDP.create (Descr.No_alias head))

  let create_equals simple : _ t = Ok (WDP.create (Descr.Equals simple))

  let bottom : _ t = Bottom

  let unknown : _ t = Unknown

  let is_obviously_bottom (t : _ t) =
    match t with Bottom -> true | Unknown | Ok _ -> false

  let is_obviously_unknown (t : _ t) =
    match t with Unknown -> true | Bottom | Ok _ -> false

  let[@inline always] get_alias_exn ~apply_renaming_head ~free_names_head
      (t : _ t) =
    match t with
    | Unknown | Bottom -> raise Not_found
    | Ok wdp -> (
      (* This uses [peek_descr] first to avoid unnecessary application of
         permutations. *)
      match WDP.peek_descr wdp with
      | No_alias _ -> raise Not_found
      | Equals _ -> (
        match
          WDP.descr
            ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
            ~free_names_descr:(Descr.free_names ~free_names_head)
            wdp
        with
        | Equals alias -> alias
        | No_alias _ -> assert false))

  let apply_renaming (t : _ t) renaming : _ t =
    match t with
    | Unknown | Bottom -> t
    | Ok wdp ->
      let wdp' = WDP.apply_renaming wdp renaming in
      if wdp == wdp' then t else Ok wdp'

  let free_names ~apply_renaming_head ~free_names_head (t : _ t) =
    match t with
    | Unknown | Bottom -> Name_occurrences.empty
    | Ok wdp ->
      WDP.free_names
        ~apply_renaming_descr:(Descr.apply_renaming ~apply_renaming_head)
        ~free_names_descr:(Descr.free_names ~free_names_head)
        wdp
end

include T

let print ~print_head ~apply_renaming_head ~free_names_head ppf t =
  let colour = Flambda_colours.top_or_bottom_type () in
  match descr ~apply_renaming_head ~free_names_head t with
  | Unknown ->
    if Flambda_features.unicode ()
    then
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s" colour
        (Flambda_colours.normal ())
    else Format.fprintf ppf "@<0>%sT@<0>%s" colour (Flambda_colours.normal ())
  | Bottom ->
    if Flambda_features.unicode ()
    then
      Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s" colour
        (Flambda_colours.normal ())
    else Format.fprintf ppf "@<0>%s_|_@<0>%s" colour (Flambda_colours.normal ())
  | Ok descr -> Descr.print ~print_head ppf descr

let[@inline always] apply_coercion ~apply_coercion_head ~apply_renaming_head
    ~free_names_head coercion t : _ t Or_bottom.t =
  match descr ~apply_renaming_head ~free_names_head t with
  | Unknown | Bottom -> Ok t
  | Ok (Equals simple) -> (
    match Simple.apply_coercion simple coercion with
    | None -> Bottom
    | Some simple -> Ok (create_equals simple))
  | Ok (No_alias head) ->
    let<+ head = apply_coercion_head head coercion in
    create head

let all_ids_for_export ~apply_renaming_head ~free_names_head
    ~all_ids_for_export_head (t : _ t) =
  match descr ~apply_renaming_head ~free_names_head t with
  | Unknown | Bottom -> Ids_for_export.empty
  | Ok (No_alias head) -> all_ids_for_export_head head
  | Ok (Equals simple) -> Ids_for_export.from_simple simple
