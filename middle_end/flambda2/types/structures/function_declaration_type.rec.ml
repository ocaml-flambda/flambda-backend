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

module TE = Typing_env
module TEE = Typing_env_extension

module Inlinable = struct
  type t =
    { code_id : Code_id.t;
      rec_info : Type_grammar.t;
      must_be_inlined : bool
    }

  let [@ocamlformat "disable"] print ppf { code_id; rec_info; must_be_inlined } =
    Format.fprintf ppf
      "@[<hov 1>(Inlinable@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]@ \
        @[<hov 1><must_be_inlined@ %b)@]\
        )@]"
      Code_id.print code_id
      Type_grammar.print rec_info
      must_be_inlined

  let code_id t = t.code_id

  let rec_info t = t.rec_info

  let must_be_inlined t = t.must_be_inlined

  let free_names { code_id; rec_info; must_be_inlined = _ } =
    Name_occurrences.add_code_id
      (Type_grammar.free_names rec_info)
      code_id Name_mode.normal

  let all_ids_for_export { code_id; rec_info; must_be_inlined = _ } =
    Ids_for_export.add_code_id
      (Type_grammar.all_ids_for_export rec_info)
      code_id

  let apply_renaming ({ code_id; rec_info; must_be_inlined = _ } as t) renaming
      =
    let code_id' = Renaming.apply_code_id renaming code_id in
    let rec_info' = Type_grammar.apply_renaming rec_info renaming in
    if code_id == code_id' && rec_info == rec_info'
    then t
    else { t with code_id = code_id'; rec_info = rec_info' }
end

module Non_inlinable = struct
  type t = { code_id : Code_id.t }

  let [@ocamlformat "disable"] print ppf { code_id; } =
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
        @[<hov 1>(code_id@ %a)@]\
        )@]"
      Code_id.print code_id

  let code_id t = t.code_id

  let free_names { code_id } =
    Name_occurrences.singleton_code_id code_id Name_mode.normal

  let all_ids_for_export { code_id } = Ids_for_export.singleton_code_id code_id

  let apply_renaming ({ code_id } as t) renaming =
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id' then t else { code_id = code_id' }
end

type t0 =
  | Inlinable of Inlinable.t
  | Non_inlinable of Non_inlinable.t

type t = t0 Or_unknown_or_bottom.t

let create ~code ~rec_info =
  let code_id = Flambda.Code.code_id code in
  let inlining_decision = Function_decl_inlining_decision.make_decision code in
  let t : t =
    match Function_decl_inlining_decision.behaviour inlining_decision with
    | Cannot_be_inlined -> Ok (Non_inlinable { code_id })
    | Must_be_inlined ->
      Ok (Inlinable { code_id; rec_info; must_be_inlined = true })
    | Could_possibly_be_inlined ->
      Ok (Inlinable { code_id; rec_info; must_be_inlined = false })
  in
  t, inlining_decision

let create_non_inlinable ~code_id : t = Ok (Non_inlinable { code_id })

let print_t0 ppf t0 =
  match t0 with
  | Inlinable inlinable -> Inlinable.print ppf inlinable
  | Non_inlinable non_inlinable -> Non_inlinable.print ppf non_inlinable

let print ppf t = Or_unknown_or_bottom.print print_t0 ppf t

let free_names (t : t) =
  match t with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok (Inlinable i) -> Inlinable.free_names i
  | Ok (Non_inlinable ni) -> Non_inlinable.free_names ni

let all_ids_for_export (t : t) =
  match t with
  | Bottom | Unknown -> Ids_for_export.empty
  | Ok (Inlinable i) -> Inlinable.all_ids_for_export i
  | Ok (Non_inlinable ni) -> Non_inlinable.all_ids_for_export ni

let apply_renaming (t : t) renaming : t =
  match t with
  | Bottom | Unknown -> t
  | Ok (Inlinable inlinable) ->
    Ok (Inlinable (Inlinable.apply_renaming inlinable renaming))
  | Ok (Non_inlinable non_inlinable) ->
    Ok (Non_inlinable (Non_inlinable.apply_renaming non_inlinable renaming))

let meet (env : Meet_env.t) (t1 : t) (t2 : t) : (t * TEE.t) Or_bottom.t =
  match t1, t2 with
  (* CR mshinwell: Try to factor out "Or_unknown_or_bottom" handling from here
     and elsewhere *)
  | Bottom, _ | _, Bottom -> Ok (Bottom, TEE.empty ())
  | Unknown, t | t, Unknown -> Ok (t, TEE.empty ())
  | ( Ok (Non_inlinable { code_id = code_id1 }),
      Ok (Non_inlinable { code_id = code_id2 }) ) -> (
    let typing_env = Meet_env.env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : (t * TEE.t) Or_bottom.t =
      Ok (Ok (Non_inlinable { code_id }), TEE.empty ())
    in
    match
      Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
    with
    | Ok code_id -> check_other_things_and_return code_id
    | Bottom -> Bottom)
  | Ok (Non_inlinable _), Ok (Inlinable _)
  | Ok (Inlinable _), Ok (Non_inlinable _) ->
    (* CR mshinwell: This should presumably return [Non_inlinable] if the
       arities match. *)
    (* CR vlaviron: The above comment was from before meet and join were split.
       Now that we know we're in meet, we can actually keep either of them (the
       inlinable one seems better) *)
    Ok (Unknown, TEE.empty ())
  | ( Ok
        (Inlinable
          { code_id = code_id1; rec_info = rec_info1; must_be_inlined = _ }),
      Ok
        (Inlinable
          { code_id = code_id2; rec_info = rec_info2; must_be_inlined = _ }) )
    -> (
    let typing_env = Meet_env.env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id rec_info extension :
        (t * TEE.t) Or_bottom.t =
      match TE.find_code typing_env code_id with
      | None -> Ok (Ok (Non_inlinable { code_id }), extension)
      | Some code ->
        let t, _inlining_decision = create ~code ~rec_info in
        Ok (t, extension)
    in
    match
      Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
    with
    | Ok code_id -> begin
      match Type_grammar.meet env rec_info1 rec_info2 with
      | Ok (rec_info, extension) ->
        check_other_things_and_return code_id rec_info extension
      | Bottom -> Bottom
    end
    | Bottom -> Bottom)

let join (env : Join_env.t) (t1 : t) (t2 : t) : t =
  match t1, t2 with
  (* CR mshinwell: Try to factor out "Or_unknown_or_bottom" handling from here
     and elsewhere *)
  | Bottom, t | t, Bottom -> t
  | Unknown, _ | _, Unknown -> Unknown
  | ( Ok (Non_inlinable { code_id = code_id1 }),
      Ok (Non_inlinable { code_id = code_id2 }) ) -> (
    let typing_env = Join_env.target_join_env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : t =
      Ok (Non_inlinable { code_id })
    in
    let code_age_rel1 = TE.code_age_relation (Join_env.left_join_env env) in
    let code_age_rel2 = TE.code_age_relation (Join_env.right_join_env env) in
    match
      Code_age_relation.join ~target_t:target_code_age_rel ~resolver
        code_age_rel1 code_age_rel2 code_id1 code_id2
    with
    | Known code_id -> check_other_things_and_return code_id
    | Unknown -> Unknown)
  | Ok (Non_inlinable _), Ok (Inlinable _)
  | Ok (Inlinable _), Ok (Non_inlinable _) ->
    (* CR mshinwell: This should presumably return [Non_inlinable] if the
       arities match. *)
    Unknown
  | ( Ok
        (Inlinable
          { code_id = code_id1; rec_info = rec_info1; must_be_inlined = _ }),
      Ok
        (Inlinable
          { code_id = code_id2; rec_info = rec_info2; must_be_inlined = _ }) )
    -> (
    let typing_env = Join_env.target_join_env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id rec_info : t =
      match TE.find_code typing_env code_id with
      | None -> Ok (Non_inlinable { code_id })
      | Some code ->
        let t, _inlining_decision = create ~code ~rec_info in
        t
    in
    let code_age_rel1 = TE.code_age_relation (Join_env.left_join_env env) in
    let code_age_rel2 = TE.code_age_relation (Join_env.right_join_env env) in
    match
      Code_age_relation.join ~target_t:target_code_age_rel ~resolver
        code_age_rel1 code_age_rel2 code_id1 code_id2
    with
    | Known code_id -> begin
      match Type_grammar.join env rec_info1 rec_info2 with
      | Known rec_info -> check_other_things_and_return code_id rec_info
      | Unknown -> Unknown
    end
    | Unknown -> Unknown)

let apply_coercion (t : t) (coercion : Coercion.t) : t Or_bottom.t =
  match coercion with
  | Id -> Ok t
  | Change_depth { from; to_ } -> begin
    match t with
    | Unknown | Bottom | Ok (Non_inlinable _) -> Ok t
    | Ok (Inlinable ({ rec_info; _ } as inlinable)) ->
      (* CR lmaurer: We should really be checking that [from] matches the
         current [rec_info], but that requires either passing in a typing
         environment or making absolutely sure that rec_infos get
         canonicalized. *)
      ignore (from, rec_info);
      let rec_info = Type_grammar.this_rec_info to_ in
      Ok (Ok (Inlinable { inlinable with rec_info }))
  end
