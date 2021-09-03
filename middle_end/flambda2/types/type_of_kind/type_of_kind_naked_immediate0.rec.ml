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

module I = Targetint_31_63
module T = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension

type t =
  | Naked_immediates of I.Set.t
  | Is_int of T.t
  | Get_tag of T.t

let print_with_cache ~cache ppf t =
  match t with
  | Naked_immediates is ->
    Format.fprintf ppf "@[<hov 1>(%a)@]" I.Set.print is
  | Is_int ty ->
    Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]"
      (T.print_with_cache ~cache) ty
  | Get_tag ty ->
    Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]"
      (T.print_with_cache ~cache) ty

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_renaming t renaming =
  match t with
  | Naked_immediates _ -> t
  | Is_int ty ->
    let ty' = T.apply_renaming ty renaming in
    if ty == ty' then t
    else Is_int ty'
  | Get_tag ty ->
    let ty' = T.apply_renaming ty renaming in
    if ty == ty' then t
    else Get_tag ty'

let free_names t =
  match t with
  | Naked_immediates _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> T.free_names ty

let all_ids_for_export t =
  match t with
  | Naked_immediates _ -> Ids_for_export.empty
  | Is_int ty | Get_tag ty -> T.all_ids_for_export ty

let apply_coercion t coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok t
  else Bottom

let eviscerate _ : _ Or_unknown.t = Unknown

let meet env t1 t2 : _ Or_bottom.t =
    match t1, t2 with
    | Naked_immediates is1, Naked_immediates is2 ->
      let is = I.Set.inter is1 is2 in
      if I.Set.is_empty is then Bottom
      else Ok (Naked_immediates is, TEE.empty ())
    | Is_int ty1, Is_int ty2 ->
      Or_bottom.map (T.meet env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
    | Get_tag ty1, Get_tag ty2 ->
      Or_bottom.map (T.meet env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
    | Is_int ty, Naked_immediates is_int
    | Naked_immediates is_int, Is_int ty ->
      begin match I.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if I.equal is_int I.zero then Some (T.any_block ())
          else if I.equal is_int I.one then Some (T.any_tagged_immediate ())
          else None
        in
        begin match shape with
        | Some shape ->
          Or_bottom.map
            (T.meet env ty shape)
            ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
        | None -> Bottom
        end
      | _ :: _ :: _ ->
        (* Note: we're potentially losing precision because the set could end up
           not containing either 0 or 1 or both, but this should be uncommon. *)
        Ok (Is_int ty, TEE.empty ())
      end
    | Get_tag ty, Naked_immediates tags
    | Naked_immediates tags, Get_tag ty ->
      let tags =
        I.Set.fold (fun tag tags ->
            match Tag.create_from_targetint tag with
            | Some tag -> Tag.Set.add tag tags
            | None -> tags (* No blocks exist with this tag *))
          tags
          Tag.Set.empty
      in
      begin match T.blocks_with_these_tags tags with
      | Known shape ->
        Or_bottom.map
          (T.meet env ty shape)
          ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
      | Unknown ->
        Ok (Get_tag ty, TEE.empty ())
      end
    | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
      (* We can't return Bottom, as it would be unsound, so we need to either
         do the actual meet with Naked_immediates, or just give up and return
         one of the arguments. *)
      Ok (t1, TEE.empty ())

let join env t1 t2 : _ Or_unknown.t =
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    let is = I.Set.union is1 is2 in
    Known (Naked_immediates is)
  | Is_int ty1, Is_int ty2 ->
    Or_unknown.map (T.join env ty1 ty2)
      ~f:(fun ty -> Is_int ty)
  | Get_tag ty1, Get_tag ty2 ->
    Or_unknown.map (T.join env ty1 ty2)
      ~f:(fun ty -> Get_tag ty)
  (* From now on: Irregular cases *)
  (* CR vlaviron: There could be improvements based on reduction
     (trying to reduce the is_int and get_tag cases to naked_immediate sets,
     then joining those) but this looks unlikely to be useful and could end up
     begin quite expensive. *)
  | Is_int ty, Naked_immediates is_int
  | Naked_immediates is_int, Is_int ty ->
    if I.Set.is_empty is_int then Known (Is_int ty)
    else
      (* Slightly better than Unknown *)
      Known (Naked_immediates (I.Set.add I.zero (I.Set.add I.one is_int)))
  | Get_tag ty, Naked_immediates tags
  | Naked_immediates tags, Get_tag ty ->
    if I.Set.is_empty tags then Known (Get_tag ty)
    else Unknown
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
    Unknown
