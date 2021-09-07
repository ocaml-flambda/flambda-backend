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
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level

module Make
    (Head : Type_head_intf.S
              with type meet_env := Meet_env.t
              with type join_env := Join_env.t
              with type typing_env := Typing_env.t
              with type typing_env_extension := Typing_env_extension.t
              with type type_grammar := Type_grammar.t) =
struct
  module Descr = struct
    type t = No_alias of Head.t Or_unknown_or_bottom.t | Equals of Simple.t

    let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
      let colour = Flambda_colours.top_or_bottom_type () in
      match t with
      | No_alias Unknown ->
        if Flambda_features.unicode () then
          Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
            colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "@<0>%sT@<0>%s" colour (Flambda_colours.normal ())
      | No_alias Bottom ->
        if Flambda_features.unicode () then
          Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s"
            colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "@<0>%s_|_@<0>%s" colour (Flambda_colours.normal ())
      | No_alias (Ok head) -> Head.print_with_cache ~cache ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(@<0>%s=@<0>%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Simple.print simple

    let [@ocamlformat "disable"] print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let apply_renaming t renaming =
      if Renaming.is_empty renaming
      then t
      else
        match t with
        | No_alias Bottom | No_alias Unknown -> t
        | No_alias (Ok head) ->
          let head' = Head.apply_renaming head renaming in
          if head == head' then t else No_alias (Ok head')
        | Equals simple ->
          let simple' = Simple.apply_renaming simple renaming in
          if simple == simple' then t else Equals simple'

    let free_names t =
      match t with
      | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
      | No_alias (Ok head) -> Head.free_names head
      | Equals simple ->
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          (Simple.free_names simple) Name_mode.in_types
  end

  (* CR mshinwell: Flambda 2 compilation is causing calls to e.g. [descr] to be
     indirect. *)
  include With_delayed_permutation.Make (Descr)

  let all_ids_for_export t =
    match descr t with
    | No_alias Bottom | No_alias Unknown -> Ids_for_export.empty
    | No_alias (Ok head) -> Head.all_ids_for_export head
    | Equals simple -> Ids_for_export.from_simple simple

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    Descr.print_with_cache ~cache ppf (descr t)

  let [@ocamlformat "disable"] print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create_no_alias head = create (No_alias head)

  let create_equals simple = create (Equals simple)

  let bottom = lazy (create (No_alias Bottom))

  let unknown = lazy (create (No_alias Unknown))

  let bottom () = Lazy.force bottom

  let unknown () = Lazy.force unknown

  let create head = create_no_alias (Ok head)

  let is_obviously_bottom t =
    match peek_descr t with
    | No_alias Bottom -> true
    | No_alias (Ok _ | Unknown) | Equals _ -> false

  let is_obviously_unknown t =
    match peek_descr t with
    | No_alias Unknown -> true
    | No_alias (Ok _ | Bottom) | Equals _ -> false

  let get_alias_exn t =
    match peek_descr t with
    | No_alias _ -> raise Not_found
    | Equals _ -> (
      match descr t with Equals alias -> alias | No_alias _ -> assert false)

  let apply_coercion t coercion : _ Or_bottom.t =
    match descr t with
    | Equals simple -> begin
      match Simple.apply_coercion simple coercion with
      | None -> Bottom
      | Some simple -> Ok (create_equals simple)
    end
    | No_alias Unknown -> Ok t
    | No_alias Bottom -> Bottom
    | No_alias (Ok head) ->
      Or_bottom.map (Head.apply_coercion head coercion) ~f:(fun head ->
          create head)

  let force_to_head ~force_to_kind t =
    match descr (force_to_kind t) with
    | No_alias head -> head
    | Equals _ -> Misc.fatal_errorf "Expected [No_alias]:@ %a" T.print t

  let expand_head ~force_to_kind t env kind : _ Or_unknown_or_bottom.t =
    match descr t with
    | No_alias head -> head
    | Equals simple -> (
      let min_name_mode = Name_mode.min_in_types in
      match TE.get_canonical_simple_exn env simple ~min_name_mode with
      | exception Not_found ->
        (* This can happen when [simple] is of [Phantom] name mode. We're not
           interested in propagating types for phantom variables, so [Unknown]
           is fine here. *)
        Unknown
      | simple ->
        let[@inline always] const const =
          let typ =
            match Reg_width_const.descr const with
            | Naked_immediate i -> T.this_naked_immediate_without_alias i
            | Tagged_immediate i -> T.this_tagged_immediate_without_alias i
            | Naked_float f -> T.this_naked_float_without_alias f
            | Naked_int32 i -> T.this_naked_int32_without_alias i
            | Naked_int64 i -> T.this_naked_int64_without_alias i
            | Naked_nativeint i -> T.this_naked_nativeint_without_alias i
          in
          force_to_head ~force_to_kind typ
        in
        let[@inline always] name name ~coercion : _ Or_unknown_or_bottom.t =
          let t = force_to_kind (TE.find env name (Some kind)) in
          match descr t with
          | No_alias Bottom -> Bottom
          | No_alias Unknown -> Unknown
          | No_alias (Ok head) -> (
            if Coercion.is_id coercion
            then Ok head
            else
              (* [simple] already has [coercion] applied to it (see
                 [get_canonical_simple], above). However we also need to apply
                 it to the expanded head of the type. *)
              match Head.apply_coercion head coercion with
              | Bottom -> Bottom
              | Ok head -> Ok head)
          | Equals _ ->
            Misc.fatal_errorf
              "Canonical alias %a should never have [Equals] type %a:@ %a"
              Simple.print simple print t TE.print env
        in
        Simple.pattern_match simple ~const ~name)

  let expand_head' ~force_to_kind t env kind =
    match expand_head ~force_to_kind t env kind with
    | Unknown -> unknown ()
    | Ok head -> create_no_alias (Ok head)
    | Bottom -> bottom ()

  let eviscerate ~force_to_kind t env kind =
    match descr t with
    | No_alias (Bottom | Unknown) -> t
    | No_alias (Ok head) -> begin
      match Head.eviscerate head with
      | Known head -> create_no_alias (Ok head)
      | Unknown -> unknown ()
    end
    | Equals simple -> (
      if Simple.is_symbol simple || Simple.is_const simple
      then t
      else
        let t = expand_head' ~force_to_kind t env kind in
        match descr t with
        | No_alias (Bottom | Unknown) -> t
        | No_alias (Ok head) -> begin
          match Head.eviscerate head with
          | Known head -> create_no_alias (Ok head)
          | Unknown -> unknown ()
        end
        | Equals _ -> assert false)

  let add_equation _env (simple : Simple.t) ty_of_simple env_extension ~to_type
      =
    match Simple.must_be_name simple with
    (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
    | Some (name, coercion_from_name_to_simple) ->
      let coercion_from_simple_to_name =
        Coercion.inverse coercion_from_name_to_simple
      in
      let ty_of_name =
        match apply_coercion ty_of_simple coercion_from_simple_to_name with
        | Ok ty -> ty
        | Bottom -> bottom ()
      in
      TEE.add_or_replace_equation env_extension name (to_type ty_of_name)
    | None -> env_extension

  let all_aliases_of env simple_opt ~in_env =
    match simple_opt with
    | None -> Aliases.Alias_set.empty
    | Some simple ->
      let simples = TE.aliases_of_simple_allowable_in_types env simple in
      (* Format.eprintf "Aliases of %a are: %a\n%!" Simple.print simple
         Simple.Set.print simples; *)
      Aliases.Alias_set.filter
        ~f:(fun simple -> Typing_env.mem_simple in_env simple)
        simples

  let[@inline always] get_canonical_simples_and_expand_heads ~force_to_kind
      ~to_type kind ~left_env ~left_ty ~right_env ~right_ty =
    let canonical_simple1 =
      match
        TE.get_alias_then_canonical_simple_exn left_env (to_type left_ty)
          ~min_name_mode:Name_mode.in_types
      with
      | exception Not_found -> None
      | canonical_simple -> Some canonical_simple
    in
    let head1 = expand_head ~force_to_kind left_ty left_env kind in
    let canonical_simple2 =
      match
        TE.get_alias_then_canonical_simple_exn right_env (to_type right_ty)
          ~min_name_mode:Name_mode.in_types
      with
      | exception Not_found -> None
      | canonical_simple -> Some canonical_simple
    in
    let head2 = expand_head ~force_to_kind right_ty right_env kind in
    canonical_simple1, head1, canonical_simple2, head2

  type meet_or_join_head_or_unknown_or_bottom_result =
    | Left_head_unchanged
    | Right_head_unchanged
    | New_head of Head.t Or_unknown_or_bottom.t * TEE.t

  let meet_head_or_unknown_or_bottom (env : Meet_env.t)
      (head1 : _ Or_unknown_or_bottom.t) (head2 : _ Or_unknown_or_bottom.t) :
      meet_or_join_head_or_unknown_or_bottom_result =
    match head1, head2 with
    | _, Unknown -> Left_head_unchanged
    | Unknown, _ -> Right_head_unchanged
    | Bottom, _ -> Left_head_unchanged
    | _, Bottom -> Right_head_unchanged
    | Ok head1, Ok head2 -> (
      match Head.meet env head1 head2 with
      | Ok (head, env_extension) -> New_head (Ok head, env_extension)
      | Bottom -> New_head (Bottom, TEE.empty ()))

  let join_head_or_unknown_or_bottom (env : Join_env.t)
      (head1 : _ Or_unknown_or_bottom.t) (head2 : _ Or_unknown_or_bottom.t) :
      _ Or_unknown_or_bottom.t =
    match head1, head2 with
    | Bottom, Bottom -> Bottom
    (* The target environment defines all the names from the left and right
       environments, so we can safely return any input as the result *)
    | Ok head, Bottom | Bottom, Ok head -> Ok head
    | Unknown, _ -> Unknown
    | _, Unknown -> Unknown
    | Ok head1, Ok head2 -> (
      match Head.join env head1 head2 with
      | Known head -> Ok head
      | Unknown -> Unknown)

  (* CR mshinwell: I've seen one case (on tests12.ml) where it appears that an
     env extension for a join point contains an equation for a symbol which is
     just the same as that already in the environment. This shouldn't have been
     emitted from [meet]. *)

  let meet ~force_to_kind ~to_type env kind ty1 ty2 t1 t2 : _ Or_bottom.t =
    let typing_env = Meet_env.env env in
    let head1 = expand_head ~force_to_kind t1 typing_env kind in
    let head2 = expand_head ~force_to_kind t2 typing_env kind in
    match
      TE.get_alias_then_canonical_simple_exn typing_env (to_type t1)
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> begin
      match
        TE.get_alias_then_canonical_simple_exn typing_env (to_type t2)
          ~min_name_mode:Name_mode.in_types
      with
      | exception Not_found -> begin
        match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged -> Ok (ty1, TEE.empty ())
        | Right_head_unchanged -> Ok (ty2, TEE.empty ())
        | New_head (head, env_extension) -> (
          match head with
          | Bottom -> Bottom
          | Unknown -> Ok (to_type (unknown ()), env_extension)
          | Ok head -> Ok (to_type (create head), env_extension))
      end
      | simple2 -> begin
        match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple2 (create_no_alias head1) ~to_type
          in
          Ok (to_type (create_equals simple2), env_extension)
        | Right_head_unchanged ->
          Ok (to_type (create_equals simple2), TEE.empty ())
        | New_head (head, env_extension) -> (
          let env_extension =
            env_extension
            |> add_equation env simple2 (create_no_alias head) ~to_type
          in
          match head with
          | Bottom -> Bottom
          | Unknown | Ok _ -> Ok (to_type (create_equals simple2), env_extension)
          )
      end
    end
    | simple1 -> (
      match
        TE.get_alias_then_canonical_simple_exn typing_env (to_type t2)
          ~min_name_mode:Name_mode.in_types
      with
      | exception Not_found -> begin
        match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged ->
          Ok (to_type (create_equals simple1), TEE.empty ())
        | Right_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple1 ~to_type (create_no_alias head2)
          in
          Ok (to_type (create_equals simple1), env_extension)
        | New_head (head, env_extension) -> (
          let env_extension =
            env_extension
            |> add_equation env simple1 ~to_type (create_no_alias head)
          in
          match head with
          | Bottom -> Bottom
          | Unknown | Ok _ -> Ok (to_type (create_equals simple1), env_extension)
          )
      end
      | simple2 ->
        if Simple.equal simple1 simple2
           || Meet_env.already_meeting env simple1 simple2
        then
          (* This produces "=simple" for the output rather than a type that
             might need transformation back from an expanded head (as would
             happen if we used the next case). *)
          Ok (to_type (create_equals simple1), TEE.empty ())
        else begin
          assert (not (Simple.equal simple1 simple2));
          let env = Meet_env.now_meeting env simple1 simple2 in
          (* In the following cases we may generate equations "pointing the
             wrong way", for example "y : =x" when [y] is the canonical element.
             This doesn't matter, however, because [Typing_env] sorts this out
             when adding equations into an environment. *)
          (* CR mshinwell: May be able to improve efficiency by not doing [meet]
             again (via [TE.add_env_extension]) if we tried here to emit the
             equations the correct way around *)
          match meet_head_or_unknown_or_bottom env head1 head2 with
          | Left_head_unchanged ->
            let env_extension =
              TEE.empty ()
              |> add_equation env simple2 ~to_type (create_equals simple1)
            in
            Ok (to_type (create_equals simple1), env_extension)
          | Right_head_unchanged ->
            let env_extension =
              TEE.empty ()
              |> add_equation env simple1 ~to_type (create_equals simple2)
            in
            Ok (to_type (create_equals simple2), env_extension)
          | New_head (head, env_extension) -> (
            let env_extension =
              env_extension
              |> add_equation env simple1 ~to_type (create_no_alias head)
              |> add_equation env simple2 ~to_type (create_equals simple1)
            in
            (* It makes things easier (to check if the result of [meet] was
               bottom) to not return "=simple" in the bottom case. This is ok
               because no constraint is being dropped; the type cannot be
               refined any further. *)
            match head with
            | Bottom -> Bottom
            | Unknown | Ok _ ->
              Ok (to_type (create_equals simple1), env_extension))
        end)

  let join ?bound_name ~force_to_kind ~to_type join_env kind _ty1 _ty2 t1 t2 :
      _ Or_unknown.t =
    (* Format.eprintf "DESCR: Joining %a and %a\n%!" print t1 print t2;
       Format.eprintf "Left:@ %a@ Right:@ %a\n%!" Code_age_relation.print
       (Join_env.left_join_env join_env |> TE.code_age_relation)
       Code_age_relation.print (Join_env.right_join_env join_env |>
       TE.code_age_relation); *)
    (* Typing_env.print (Join_env.left_join_env join_env) Typing_env.print
       (Join_env.right_join_env join_env); *)
    (* CR mshinwell: Rewrite this to avoid the [option] allocations from
       [get_canonical_simples_and_expand_heads] *)
    let canonical_simple1, head1, canonical_simple2, head2 =
      get_canonical_simples_and_expand_heads ~force_to_kind ~to_type kind
        ~left_env:(Join_env.left_join_env join_env)
        ~left_ty:t1
        ~right_env:(Join_env.right_join_env join_env)
        ~right_ty:t2
    in

    (* CR mshinwell: Add shortcut when the canonical simples are equal *)
    let shared_aliases =
      let shared_aliases =
        match canonical_simple1, head1, canonical_simple2, head2 with
        | None, _, None, _
        | None, (Ok _ | Unknown), _, _
        | _, _, None, (Ok _ | Unknown) ->
          Aliases.Alias_set.empty
        | Some simple1, _, _, Bottom -> Aliases.Alias_set.singleton simple1
        | _, Bottom, Some simple2, _ -> Aliases.Alias_set.singleton simple2
        | Some simple1, _, Some simple2, _ ->
          if Simple.same simple1 simple2
          then Aliases.Alias_set.singleton simple1
          else
            Aliases.Alias_set.inter
              (all_aliases_of
                 (Join_env.left_join_env join_env)
                 canonical_simple1
                 ~in_env:(Join_env.target_join_env join_env))
              (all_aliases_of
                 (Join_env.right_join_env join_env)
                 canonical_simple2
                 ~in_env:(Join_env.target_join_env join_env))
      in
      match bound_name with
      | None -> shared_aliases
      | Some bound_name ->
        (* CR vlaviron: this ensures that we're not creating an alias to a
           different simple that is just bound_name with different coercion.
           Such an alias is forbidden. *)
        Aliases.Alias_set.filter
          ~f:(fun simple -> not (Simple.same simple (Simple.name bound_name)))
          shared_aliases
    in
    (* Format.eprintf "Shared aliases:@ %a\n%!" Simple.Set.print
       shared_aliases; *)
    match Aliases.Alias_set.find_best shared_aliases with
    | Some alias -> Known (to_type (create_equals alias))
    | None -> (
      match canonical_simple1, canonical_simple2 with
      | Some simple1, Some simple2
        when Join_env.already_joining join_env simple1 simple2 ->
        (* CR vlaviron: Fix this to Unknown when Product can handle it *)
        Known (to_type (unknown ()))
      | Some _, Some _ | Some _, None | None, Some _ | None, None -> (
        let join_env =
          match canonical_simple1, canonical_simple2 with
          | Some simple1, Some simple2 ->
            Join_env.now_joining join_env simple1 simple2
          | Some _, None | None, Some _ | None, None -> join_env
        in
        match join_head_or_unknown_or_bottom join_env head1 head2 with
        | Bottom -> Known (to_type (bottom ()))
        | Unknown -> Known (to_type (unknown ()))
        | Ok head -> Known (to_type (create head))))
end
