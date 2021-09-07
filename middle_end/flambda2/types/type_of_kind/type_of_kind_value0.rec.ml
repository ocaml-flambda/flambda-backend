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

module T = Type_grammar
module TEE = Typing_env_extension

module Blocks = Row_like.For_blocks

type t =
  | Variant of Variant.t
  (* CR mshinwell: Add constructors for the following too so we can check
     they aren't bottom? *)
  | Boxed_float of T.t
  | Boxed_int32 of T.t
  | Boxed_int64 of T.t
  | Boxed_nativeint of T.t
  | Closures of {
      by_closure_id : Row_like.For_closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : T.t; }

let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
  match t with
  | Variant { blocks; immediates; is_unique } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Variant%s@ \
        @[<hov 1>(blocks@ %a)@]@ \
        @[<hov 1>(tagged_imms@ %a)@]\
        )@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print (Blocks.print_with_cache ~cache)) blocks
      (Or_unknown.print (T.print_with_cache ~cache)) immediates
  | Boxed_float naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int32 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int64 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_nativeint naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (T.print_with_cache ~cache) length

let [@ocamlformat "disable"] print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_renaming_variant blocks immediates perm =
  let immediates' =
    Or_unknown.map immediates ~f:(fun immediates ->
      T.apply_renaming immediates perm)
  in
  let blocks' =
    Or_unknown.map blocks ~f:(fun blocks ->
      Blocks.apply_renaming blocks perm)
  in
  if immediates == immediates' && blocks == blocks' then
    None
  else
    Some (blocks', immediates')

let apply_renaming t perm =
  match t with
  | Variant { blocks; immediates; is_unique; } ->
    begin match
      apply_renaming_variant blocks immediates perm
    with
    | None -> t
    | Some (blocks, immediates) ->
      Variant (Variant.create ~is_unique ~blocks ~immediates)
    end
  | Boxed_float ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_nativeint ty'
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Row_like.For_closures_entry_by_set_of_closures_contents.apply_renaming
        by_closure_id perm
    in
    if by_closure_id == by_closure_id' then t
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> t
  | Array { length; } ->
    let length' = T.apply_renaming length perm in
    if length == length' then t
    else Array { length = length'; }

let free_names t =
  match t with
  | Variant { blocks; immediates; is_unique = _; } ->
    Name_occurrences.union
      (Or_unknown.free_names Blocks.free_names blocks)
      (Or_unknown.free_names T.free_names immediates)
  | Boxed_float ty -> T.free_names ty
  | Boxed_int32 ty -> T.free_names ty
  | Boxed_int64 ty -> T.free_names ty
  | Boxed_nativeint ty -> T.free_names ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } -> T.free_names length

let all_ids_for_export t =
  match t with
  | Variant { blocks; immediates; is_unique = _; } ->
    Ids_for_export.union
      (Or_unknown.all_ids_for_export Blocks.all_ids_for_export blocks)
      (Or_unknown.all_ids_for_export T.all_ids_for_export immediates)
  | Boxed_float ty -> T.all_ids_for_export ty
  | Boxed_int32 ty -> T.all_ids_for_export ty
  | Boxed_int64 ty -> T.all_ids_for_export ty
  | Boxed_nativeint ty -> T.all_ids_for_export ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.all_ids_for_export
      by_closure_id
  | String _ -> Ids_for_export.empty
  | Array { length; } -> T.all_ids_for_export length

let apply_coercion t coercion : _ Or_bottom.t =
  match t with
  | Closures { by_closure_id; } ->
    begin match
      Row_like.For_closures_entry_by_set_of_closures_contents.
        map_function_decl_types
         by_closure_id
         ~f:(fun (decl : Function_declaration_type.t) : _ Or_bottom.t ->
           Function_declaration_type.apply_coercion decl coercion)
    with
    | Bottom -> Bottom
    | Ok by_closure_id ->
      match
        Row_like.For_closures_entry_by_set_of_closures_contents.
          map_closure_types
          by_closure_id
          ~f:(fun ty -> Type_grammar.apply_coercion ty coercion)
      with
      | Bottom -> Bottom
      | Ok by_closure_id ->
        Ok (Closures { by_closure_id; })
    end
  | Variant _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | String _
  | Array _ ->
    if Coercion.is_id coercion then Ok t
    else Bottom

let eviscerate t : _ Or_unknown.t =
  match t with
  | Boxed_float _ -> Known (Boxed_float (T.any_naked_float ()))
  | Boxed_int32 _ -> Known (Boxed_int32 (T.any_naked_int32 ()))
  | Boxed_int64 _ -> Known (Boxed_int64 (T.any_naked_int64 ()))
  | Boxed_nativeint _ -> Known (Boxed_nativeint (T.any_naked_nativeint ()))
  | Closures _
  | Variant _
  | String _
  | Array _ -> Unknown

let meet_unknown meet_contents ~contents_is_bottom env
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
    : ((_ Or_unknown.t) * TEE.t) Or_bottom.t =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
  (* CR mshinwell: Think about the next two cases more *)
  | Known contents, _ when contents_is_bottom contents -> Bottom
  | _, Known contents when contents_is_bottom contents -> Bottom
  | _, Unknown -> Ok (or_unknown1, TEE.empty ())
  | Unknown, _ -> Ok (or_unknown2, TEE.empty ())
  | Known contents1, Known contents2 ->
    let result =
      Or_bottom.map (meet_contents env contents1 contents2)
        ~f:(fun (contents, env_extension) ->
          Or_unknown.Known contents, env_extension)
    in
    match result with
    | Bottom | Ok (Unknown, _) -> result
    | Ok (Known contents, _env_extension) ->
      (* XXX Why isn't [meet_contents] returning bottom? *)
      if contents_is_bottom contents then Bottom
      else result

let join_unknown join_contents (env : Join_env.t)
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
    : _ Or_unknown.t =
  match or_unknown1, or_unknown2 with
  | _, Unknown
  | Unknown, _ -> Unknown
  | Known contents1, Known contents2 ->
    join_contents env contents1 contents2

let meet_variant env
      ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_bottom.t =
  let blocks =
    meet_unknown Blocks.meet ~contents_is_bottom:Blocks.is_bottom
      env blocks1 blocks2
  in
  let blocks : _ Or_bottom.t =
    (* XXX Clean this up *)
    match blocks with
    | Bottom | Ok (Or_unknown.Unknown, _) -> blocks
    | Ok (Or_unknown.Known blocks', _) ->
      if Blocks.is_bottom blocks' then Bottom else blocks
  in
  let imms =
    meet_unknown T.meet ~contents_is_bottom:T.is_obviously_bottom
      env imms1 imms2
  in
  let imms : _ Or_bottom.t =
    match imms with
    | Bottom | Ok (Or_unknown.Unknown, _) -> imms
    | Ok (Or_unknown.Known imms', _) ->
      if T.is_obviously_bottom imms' then Bottom else imms
  in
  match blocks, imms with
  | Bottom, Bottom -> Bottom
  | Ok (blocks, env_extension), Bottom ->
    let immediates : _ Or_unknown.t = Known (T.bottom K.naked_immediate) in
    Ok (blocks, immediates, env_extension)
  | Bottom, Ok (immediates, env_extension) ->
    let blocks : _ Or_unknown.t = Known (Blocks.create_bottom ()) in
    Ok (blocks, immediates, env_extension)
  | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
    begin match (blocks : _ Or_unknown.t) with
    | Unknown -> ()
    | Known blocks -> assert (not (Blocks.is_bottom blocks));
    end;
    begin match (immediates : _ Or_unknown.t) with
    | Unknown -> ()
    | Known imms -> assert (not (T.is_obviously_bottom imms));
    end;
    let env_extension =
      let env = Meet_env.env env in
      let join_env =
        Join_env.create env ~left_env:env ~right_env:env
      in
      TEE.join join_env env_extension1 env_extension2
    in
    Ok (blocks, immediates, env_extension)

let meet env t1 t2 : _ Or_bottom.t =
  match t1, t2 with
  | Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1; },
    Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2; } ->
    Or_bottom.map
      (meet_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates, env_extension) ->
        (* Uniqueness tracks whether duplication/lifting is allowed.
           It must always be propagated, both for meet and join. *)
        let is_unique = is_unique1 || is_unique2 in
        Variant (Variant.create ~is_unique ~blocks ~immediates), env_extension)
  | Boxed_float n1, Boxed_float n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_float n, env_extension)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_int32 n, env_extension)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_int64 n, env_extension)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_nativeint n, env_extension)
  | Closures { by_closure_id = by_closure_id1; },
    Closures { by_closure_id = by_closure_id2; } ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    Or_bottom.map
      (C.meet env by_closure_id1 by_closure_id2)
      ~f:(fun (by_closure_id, env_extension) ->
        Closures { by_closure_id; }, env_extension)
  | String strs1, String strs2 ->
    let strs = String_info.Set.inter strs1 strs2 in
    if String_info.Set.is_empty strs then Bottom
    else Or_bottom.Ok (String strs, TEE.empty ())
  | Array { length = length1; }, Array { length = length2; } ->
    Or_bottom.map
      (T.meet env length1 length2)
      ~f:(fun (length, env_extension) -> Array { length; }, env_extension)
  | (Variant _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Closures _
    | String _
    | Array _), _ ->
    (* CR vlaviron: This assumes that all the different constructors are
       incompatible. This could break very hard for users of Obj. *)
    Bottom

let join_variant env
      ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_unknown.t =
  let blocks_join env b1 b2 : _ Or_unknown.t =
    Known (Blocks.join env b1 b2)
  in
  let blocks =
    join_unknown blocks_join env blocks1 blocks2
  in
  let imms =
    join_unknown (T.join ?bound_name:None) env imms1 imms2
  in
  match blocks, imms with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown | Unknown, Known _ | Known _, Known _ ->
    Known (blocks, imms)

let join env t1 t2 : _ Or_unknown.t =
  match t1, t2 with
  | Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1; },
    Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2; } ->
    Or_unknown.map
      (join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates) ->
        (* Uniqueness tracks whether duplication/lifting is allowed.
           It must always be propagated, both for meet and join. *)
        let is_unique = is_unique1 || is_unique2 in
        Variant (Variant.create ~is_unique ~blocks ~immediates))
  | Boxed_float n1, Boxed_float n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_float n)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_int32 n)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_int64 n)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_nativeint n)
  | Closures { by_closure_id = by_closure_id1; },
    Closures { by_closure_id = by_closure_id2; } ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    let by_closure_id = C.join env by_closure_id1 by_closure_id2 in
    Known (Closures { by_closure_id; })
  | String strs1, String strs2 ->
    let strs = String_info.Set.union strs1 strs2 in
    Known (String strs)
  | Array { length = length1; }, Array { length = length2; } ->
    Or_unknown.map
      (T.join env length1 length2)
      ~f:(fun length -> Array { length; })
  | (Variant _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Closures _
    | String _
    | Array _), _ -> Unknown
