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

[@@@ocaml.warning "+a-30-40-41-42"]

module TE = Typing_env
module TEE = Typing_env_extension

module T0 = struct
  type t =
    { code_id : Code_id.t;
      rec_info : Type_grammar.t
    }

  let [@ocamlformat "disable"] print ppf { code_id; rec_info } =
    Format.fprintf ppf
      "@[<hov 1>(function_decl@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]\
        )@]"
      Code_id.print code_id
      Type_grammar.print rec_info

  let code_id t = t.code_id

  let rec_info t = t.rec_info

  let free_names { code_id; rec_info } =
    Name_occurrences.add_code_id
      (Type_grammar.free_names rec_info)
      code_id Name_mode.normal

  let all_ids_for_export { code_id; rec_info } =
    Ids_for_export.add_code_id
      (Type_grammar.all_ids_for_export rec_info)
      code_id

  let apply_renaming ({ code_id; rec_info } as t) renaming =
    let code_id' = Renaming.apply_code_id renaming code_id in
    let rec_info' = Type_grammar.apply_renaming rec_info renaming in
    if code_id == code_id' && rec_info == rec_info'
    then t
    else { code_id = code_id'; rec_info = rec_info' }
end

type t = T0.t Or_unknown_or_bottom.t

let create code_id ~rec_info : t = Ok { code_id; rec_info }

let print ppf t = Or_unknown_or_bottom.print T0.print ppf t

let free_names (t : t) =
  match t with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok t0 -> T0.free_names t0

let all_ids_for_export (t : t) =
  match t with
  | Bottom | Unknown -> Ids_for_export.empty
  | Ok t0 -> T0.all_ids_for_export t0

let apply_renaming (t : t) renaming : t =
  match t with
  | Bottom | Unknown -> t
  | Ok t0 -> Ok (T0.apply_renaming t0 renaming)

let meet (env : Meet_env.t) (t1 : t) (t2 : t) : (t * TEE.t) Or_bottom.t =
  match t1, t2 with
  | Bottom, _ | _, Bottom -> Ok (Bottom, TEE.empty ())
  | Unknown, t | t, Unknown -> Ok (t, TEE.empty ())
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) -> (
    let typing_env = Meet_env.env env in
    match
      Code_age_relation.meet
        (TE.code_age_relation typing_env)
        ~resolver:(TE.code_age_relation_resolver typing_env)
        code_id1 code_id2
    with
    | Bottom -> Bottom
    | Ok code_id -> (
      (* It's possible that [code_id] corresponds to [Deleted] code. In that
         case, any attempt to inline will fail, as the code will not be found in
         the simplifier's environment -- see
         [Simplify_apply_expr.simplify_direct_function_call]. *)
      match Type_grammar.meet env rec_info1 rec_info2 with
      | Ok (rec_info, extension) ->
        let t = create code_id ~rec_info in
        Ok (t, extension)
      | Bottom -> Bottom))

let join (env : Join_env.t) (t1 : t) (t2 : t) : t =
  match t1, t2 with
  | Bottom, t | t, Bottom -> t
  | Unknown, _ | _, Unknown -> Unknown
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) -> (
    let target_typing_env = Join_env.target_join_env env in
    match
      Code_age_relation.join
        ~target_t:(TE.code_age_relation target_typing_env)
        ~resolver:(TE.code_age_relation_resolver target_typing_env)
        (TE.code_age_relation (Join_env.left_join_env env))
        (TE.code_age_relation (Join_env.right_join_env env))
        code_id1 code_id2
    with
    | Unknown -> Unknown
    | Known code_id -> (
      match Type_grammar.join env rec_info1 rec_info2 with
      | Known rec_info -> create code_id ~rec_info
      | Unknown -> Unknown))

let apply_coercion (t : t) (coercion : Coercion.t) : t Or_bottom.t =
  match coercion with
  | Id -> Ok t
  | Change_depth { from; to_ } -> (
    match t with
    | Unknown | Bottom -> Ok t
    | Ok ({ rec_info; _ } as t0) ->
      (* CR lmaurer: We should really be checking that [from] matches the
         current [rec_info], but that requires either passing in a typing
         environment or making absolutely sure that rec_infos get
         canonicalized. *)
      ignore (from, rec_info);
      let rec_info = Type_grammar.this_rec_info to_ in
      Ok (Ok { t0 with rec_info }))
