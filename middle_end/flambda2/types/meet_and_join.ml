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

module Meet_env = Typing_env.Meet_env
module Join_env = Typing_env.Join_env
module ET = Expand_head.Expanded_type
module K = Flambda_kind
module MTC = More_type_creators
module TG = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension
open Or_unknown.Let_syntax

let all_aliases_of env simple_opt ~in_env =
  match simple_opt with
  | None -> Aliases.Alias_set.empty
  | Some simple ->
    let simples = TE.aliases_of_simple_allowable_in_types env simple in
    Aliases.Alias_set.filter
      ~f:(fun simple -> TE.mem_simple in_env simple)
      simples

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type extension_with_memory =
  { pairs_of_names : Name.Pair.Set.t;
    extension : TEE.t
  }

let empty_extension =
  { pairs_of_names = Name.Pair.Set.empty; extension = TEE.empty }

let no_memory extension = { pairs_of_names = Name.Pair.Set.empty; extension }

let add_equation (simple : Simple.t) ty_of_simple
    ({ pairs_of_names; extension } as env_extension) =
  match Simple.must_be_name simple with
  | Some (name, coercion_from_name_to_simple) ->
    let coercion_from_simple_to_name =
      Coercion.inverse coercion_from_name_to_simple
    in
    let ty_of_name =
      TG.apply_coercion ty_of_simple coercion_from_simple_to_name
    in
    let pairs_of_names =
      match TG.get_alias_opt ty_of_name with
      | None -> pairs_of_names
      | Some simple' -> (
        match Simple.must_be_name simple' with
        | None -> pairs_of_names
        | Some (name', _coercion) ->
          Name.Pair.Set.add (name, name') pairs_of_names)
    in
    let extension = TEE.add_or_replace_equation extension name ty_of_name in
    { pairs_of_names; extension }
  | None -> env_extension

type 'a meet_result =
  | Bottom
  | Ok of 'a meet_return_value * extension_with_memory

let map_result ~f = function
  | Bottom -> Bottom
  | Ok (Left_input, ext) -> Ok (Left_input, ext)
  | Ok (Right_input, ext) -> Ok (Right_input, ext)
  | Ok (Both_inputs, ext) -> Ok (Both_inputs, ext)
  | Ok (New_result x, ext) -> Ok (New_result (f x), ext)

let extract_value res left right =
  match res with
  | Left_input -> left
  | Right_input -> right
  | Both_inputs -> left
  | New_result value -> value

let set_meet (type a b) (module S : Container_types_intf.Set with type t = a)
    (s1 : a) (s2 : a) ~(of_set : a -> b) : b meet_result =
  match S.subset s1 s2, S.subset s2 s1 with
  | true, true -> Ok (Both_inputs, empty_extension)
  | true, false -> Ok (Left_input, empty_extension)
  | false, true -> Ok (Right_input, empty_extension)
  | false, false ->
    let s = S.inter s1 s2 in
    if S.is_empty s then Bottom else Ok (New_result (of_set s), empty_extension)

module Map_meet (M : Container_types_intf.Map) = struct
  let meet ~(meet_data : Meet_env.t -> 'a -> 'a -> 'a meet_result)
      ~(meet_env_extension :
         Meet_env.t ->
         extension_with_memory ->
         extension_with_memory ->
         extension_with_memory Or_bottom.t) meet_env (left : 'a M.t)
      (right : 'a M.t) : 'a M.t meet_result =
    let any_bottom = ref false in
    let all_left = ref true in
    let all_right = ref true in
    let env_extensions = ref empty_extension in
    let meet_env = ref meet_env in
    let result_map =
      M.merge
        (fun _key left_data right_data ->
          match left_data, right_data with
          | None, None -> None
          | Some data, None | None, Some data -> Some data
          | Some left_data, Some right_data -> (
            match meet_data !meet_env left_data right_data with
            | Bottom ->
              any_bottom := true;
              None
            | Ok (data_result, env_extension) -> (
              meet_env
                := Meet_env.assume_already_meeting !meet_env
                     env_extension.pairs_of_names;
              (match
                 meet_env_extension !meet_env !env_extensions env_extension
               with
              | Bottom -> any_bottom := true
              | Ok env_extension ->
                meet_env
                  := Meet_env.assume_already_meeting !meet_env
                       env_extension.pairs_of_names;
                env_extensions := env_extension);
              match data_result with
              | Left_input ->
                all_right := false;
                Some left_data
              | Right_input ->
                all_left := false;
                Some right_data
              | Both_inputs ->
                (* Arbitrarily pick one *)
                Some left_data
              | New_result data ->
                all_left := false;
                all_right := false;
                Some data)))
        left right
    in
    if !any_bottom
    then Bottom
    else
      let result =
        match !all_left, !all_right with
        | true, true -> Both_inputs
        | true, false -> Left_input
        | false, true -> Right_input
        | false, false -> New_result result_map
      in
      Ok (result, !env_extensions)
end

module Function_slot_map_meet = Map_meet (Function_slot.Map)
module Value_slot_map_meet = Map_meet (Value_slot.Map)

type 'a pairwise_disjunction_meet_arg =
  { is_bottom : 'a -> bool;
    mk_bottom : unit -> 'a;
    meet : Meet_env.t -> 'a -> 'a -> 'a meet_result
  }

let pairwise_disjunction_meet ~join_env_extension meetx meety env x1 x2 y1 y2 =
  match meetx.meet env x1 x2, meety.meet env y1 y2 with
  | Bottom, Bottom -> Bottom
  | Ok (xr, env_extension), Bottom ->
    let x = extract_value xr x1 x2 in
    if meetx.is_bottom x
    then Bottom
    else
      let y = meety.mk_bottom () in
      Ok (New_result (x, y), env_extension)
  | Bottom, Ok (yr, env_extension) ->
    let y = extract_value yr y1 y2 in
    if meety.is_bottom y
    then Bottom
    else
      let x = meetx.mk_bottom () in
      Ok (New_result (x, y), env_extension)
  | Ok (xr, xext), Ok (yr, yext) -> (
    let x = extract_value xr x1 x2 in
    let y = extract_value yr y1 y2 in
    let bottom_x = meetx.is_bottom x in
    let bottom_y = meety.is_bottom y in
    let env_extension =
      if bottom_x
      then yext
      else if bottom_y
      then xext
      else
        let env = Meet_env.env env in
        let join_env = Join_env.create env ~left_env:env ~right_env:env in
        let pairs_of_names =
          Name.Pair.Set.inter xext.pairs_of_names yext.pairs_of_names
        in
        let extension =
          join_env_extension join_env xext.extension yext.extension
        in
        { pairs_of_names; extension }
    in
    match xr, yr with
    | Both_inputs, Both_inputs -> Ok (Both_inputs, env_extension)
    | (Left_input | Both_inputs), (Left_input | Both_inputs) ->
      Ok (Left_input, env_extension)
    | (Right_input | Both_inputs), (Right_input | Both_inputs) ->
      Ok (Right_input, env_extension)
    | Left_input, Right_input
    | Right_input, Left_input
    | New_result _, _
    | _, New_result _ ->
      if bottom_x && bottom_y
      then Bottom
      else Ok (New_result (x, y), env_extension))

type _ combine_results_meet_ops =
  | Meet :
      { meet : Meet_env.t -> 'a -> 'a -> 'a meet_result;
        next : 'b combine_results_meet_ops
      }
      -> ('a * 'b) combine_results_meet_ops
  | End : unit combine_results_meet_ops

type _ combine_results_inputs =
  | Input :
      { input : 'a;
        next : 'b combine_results_inputs
      }
      -> ('a * 'b) combine_results_inputs
  | End : unit combine_results_inputs

let rec build_values : type a. a combine_results_inputs -> a = function
  | Input { input; next } -> input, build_values next
  | End -> ()

let extract_values res left right =
  match res with
  | Left_input -> build_values left
  | Right_input -> build_values right
  | Both_inputs -> build_values left
  | New_result value -> value

let combine_results env
    ~(meet_env_extension :
       Meet_env.t ->
       extension_with_memory ->
       extension_with_memory ->
       extension_with_memory Or_bottom.t)
    ~(meet_ops : 'a combine_results_meet_ops)
    ~(left_inputs : 'a combine_results_inputs)
    ~(right_inputs : 'a combine_results_inputs) ~(rebuild : 'a -> 'b) :
    'b meet_result =
  let rec do_meets :
      type a.
      Meet_env.t ->
      a combine_results_meet_ops ->
      a combine_results_inputs ->
      a combine_results_inputs ->
      a meet_result =
   fun meet_env meet_ops left right : a meet_result ->
    match meet_ops, left, right with
    | End, End, End -> Ok (Both_inputs, empty_extension)
    | ( Meet { meet; next = next_meet },
        Input { input = left_input; next = next_left },
        Input { input = right_input; next = next_right } ) -> (
      match meet meet_env left_input right_input with
      | Bottom -> Bottom
      | Ok (result_hd, env_extension_hd) -> (
        let meet_env =
          Meet_env.assume_already_meeting env env_extension_hd.pairs_of_names
        in
        match do_meets meet_env next_meet next_left next_right with
        | Bottom -> Bottom
        | Ok (result_tl, env_extension_tl) -> (
          let meet_env =
            Meet_env.assume_already_meeting env env_extension_tl.pairs_of_names
          in
          match
            meet_env_extension meet_env env_extension_hd env_extension_tl
          with
          | Bottom -> Bottom
          | Ok env_extension -> (
            match result_hd, result_tl with
            | Both_inputs, Both_inputs -> Ok (Both_inputs, env_extension)
            | (Left_input | Both_inputs), (Left_input | Both_inputs) ->
              Ok (Left_input, env_extension)
            | (Right_input | Both_inputs), (Right_input | Both_inputs) ->
              Ok (Right_input, env_extension)
            | New_result _, _
            | _, New_result _
            | Left_input, Right_input
            | Right_input, Left_input ->
              let result_hd = extract_value result_hd left_input right_input in
              let result_tl = extract_values result_tl next_left next_right in
              Ok (New_result (result_hd, result_tl), env_extension)))))
  in
  map_result ~f:rebuild (do_meets env meet_ops left_inputs right_inputs)

let combine_results2 env ~meet_env_extension ~meet_a ~meet_b ~left_a ~right_a
    ~left_b ~right_b ~rebuild =
  combine_results env ~meet_env_extension
    ~meet_ops:
      (Meet { meet = meet_a; next = Meet { meet = meet_b; next = End } })
    ~left_inputs:
      (Input { input = left_a; next = Input { input = left_b; next = End } })
    ~right_inputs:
      (Input { input = right_a; next = Input { input = right_b; next = End } })
    ~rebuild:(fun (a, (b, ())) -> rebuild a b)

let meet_code_id (env : Meet_env.t) (code_id1 : Code_id.t)
    (code_id2 : Code_id.t) : Code_id.t meet_result =
  if Code_id.equal code_id1 code_id2
  then Ok (Both_inputs, empty_extension)
  else
    let typing_env = Meet_env.env env in
    match
      Code_age_relation.meet
        (TE.code_age_relation typing_env)
        ~resolver:(TE.code_age_relation_resolver typing_env)
        code_id1 code_id2
    with
    | Bottom -> Bottom
    | Ok code_id ->
      if Code_id.equal code_id code_id1
      then Ok (Left_input, empty_extension)
      else if Code_id.equal code_id code_id2
      then Ok (Right_input, empty_extension)
      else Ok (New_result code_id, empty_extension)

type meet_keep_side =
  | Left
  | Right

(* type meet_expanded_head_result =
 *   | Left_head_unchanged
 *   | Right_head_unchanged
 *   | New_head of ET.t * TEE.t *)

exception Bottom_meet

let meet_alloc_mode _meet_env (alloc_mode1 : Alloc_mode.For_types.t)
    (alloc_mode2 : Alloc_mode.For_types.t) : Alloc_mode.For_types.t meet_result
    =
  match alloc_mode1, alloc_mode2 with
  | Heap_or_local, Heap_or_local | Heap, Heap | Local, Local ->
    Ok (Both_inputs, empty_extension)
  | Heap_or_local, _ -> Ok (Right_input, empty_extension)
  | _, Heap_or_local -> Ok (Left_input, empty_extension)
  | Heap, Local | Local, Heap ->
    (* It is not safe to pick either [Heap] or [Local] and moreover we should
       never be in this situation by virtue of the OCaml type checker; it is
       bottom. *)
    Bottom

let join_alloc_mode (alloc_mode1 : Alloc_mode.For_types.t)
    (alloc_mode2 : Alloc_mode.For_types.t) : Alloc_mode.For_types.t =
  match alloc_mode1, alloc_mode2 with
  | Heap_or_local, _ | _, Heap_or_local -> Alloc_mode.For_types.unknown ()
  | Heap, Heap -> Alloc_mode.For_types.heap
  | Local, Local -> Alloc_mode.For_types.local ()
  | Heap, Local | Local, Heap -> Alloc_mode.For_types.unknown ()

let[@inline always] meet_unknown meet_contents ~contents_is_bottom env
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t) :
    _ Or_unknown.t meet_result =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> Ok (Both_inputs, empty_extension)
  | Known contents, _ when contents_is_bottom contents -> Bottom
  | _, Known contents when contents_is_bottom contents -> Bottom
  | _, Unknown -> Ok (Left_input, empty_extension)
  | Unknown, _ -> Ok (Right_input, empty_extension)
  | Known contents1, Known contents2 ->
    map_result
      ~f:(fun contents -> Or_unknown.Known contents)
      (meet_contents env contents1 contents2)

let[@inline always] join_unknown join_contents (env : Join_env.t)
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t) :
    _ Or_unknown.t =
  match or_unknown1, or_unknown2 with
  | _, Unknown | Unknown, _ -> Unknown
  | Known contents1, Known contents2 -> join_contents env contents1 contents2

(* Note: Bottom is a valid element kind for empty arrays, so this function never
   leads to a general Bottom result *)
let meet_array_element_kinds (element_kind1 : _ Or_unknown_or_bottom.t)
    (element_kind2 : _ Or_unknown_or_bottom.t) : _ Or_unknown_or_bottom.t =
  match element_kind1, element_kind2 with
  | Unknown, Unknown -> Unknown
  | Bottom, _ | _, Bottom -> Bottom
  | Unknown, Ok kind | Ok kind, Unknown -> Ok kind
  | Ok element_kind1, Ok element_kind2 ->
    if Flambda_kind.With_subkind.compatible element_kind1
         ~when_used_at:element_kind2
    then Ok element_kind1
    else if Flambda_kind.With_subkind.compatible element_kind2
              ~when_used_at:element_kind1
    then Ok element_kind2
    else Bottom

let join_array_element_kinds (element_kind1 : _ Or_unknown_or_bottom.t)
    (element_kind2 : _ Or_unknown_or_bottom.t) : _ Or_unknown_or_bottom.t =
  match element_kind1, element_kind2 with
  | Unknown, _ | _, Unknown -> Unknown
  | Bottom, element_kind | element_kind, Bottom -> element_kind
  | Ok element_kind1, Ok element_kind2 ->
    if Flambda_kind.With_subkind.compatible element_kind1
         ~when_used_at:element_kind2
    then Ok element_kind2
    else if Flambda_kind.With_subkind.compatible element_kind2
              ~when_used_at:element_kind1
    then Ok element_kind1
    else Unknown

let rec meet env (t1 : TG.t) (t2 : TG.t) : TG.t meet_result =
  if not (K.equal (TG.kind t1) (TG.kind t2))
  then
    Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a" TG.print t1
      TG.print t2;
  let kind = TG.kind t1 in
  let typing_env = Meet_env.env env in
  let simple1 =
    match
      TE.get_alias_then_canonical_simple_exn typing_env t1
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let simple2 =
    match
      TE.get_alias_then_canonical_simple_exn typing_env t2
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match simple1 with
  | None -> (
    let expanded1 =
      Expand_head.expand_head0 typing_env t1
        ~known_canonical_simple_at_in_types_mode:simple1
    in
    let expanded2 =
      Expand_head.expand_head0 typing_env t2
        ~known_canonical_simple_at_in_types_mode:simple2
    in
    match simple2 with
    | None ->
      map_result ~f:ET.to_type (meet_expanded_head env expanded1 expanded2)
    | Some simple2 -> (
      (* Here we are meeting a non-alias type on the left with an alias on the
         right. In all cases, the return type is the alias, so we will always
         return [Right_input]; the interesting part will be the extension. *)
      let env_extension : extension_with_memory Or_bottom.t =
        match meet_expanded_head env expanded1 expanded2 with
        | Ok (Left_input, env_extension) ->
          Ok (add_equation simple2 (ET.to_type expanded1) env_extension)
        | Ok ((Right_input | Both_inputs), env_extension) -> Ok env_extension
        | Ok (New_result expanded, env_extension) ->
          Ok (add_equation simple2 (ET.to_type expanded) env_extension)
        | Bottom -> Bottom
      in
      match env_extension with
      | Ok env_extension -> Ok (Right_input, env_extension)
      | Bottom -> Bottom))
  | Some simple1 as simple1_opt -> (
    match simple2 with
    | None -> (
      let expanded1 =
        Expand_head.expand_head0 typing_env t1
          ~known_canonical_simple_at_in_types_mode:simple1_opt
      in
      let expanded2 =
        Expand_head.expand_head0 typing_env t2
          ~known_canonical_simple_at_in_types_mode:simple2
      in
      (* We always return [Left_input] (see comment above) *)
      let env_extension : extension_with_memory Or_bottom.t =
        match meet_expanded_head env expanded1 expanded2 with
        | Ok (Right_input, env_extension) ->
          Ok (add_equation simple1 (ET.to_type expanded2) env_extension)
        | Ok ((Left_input | Both_inputs), env_extension) -> Ok env_extension
        | Ok (New_result expanded, env_extension) ->
          Ok (add_equation simple1 (ET.to_type expanded) env_extension)
        | Bottom -> Bottom
      in
      match env_extension with
      | Ok env_extension -> Ok (Left_input, env_extension)
      | Bottom -> Bottom)
    | Some simple2 as simple2_opt ->
      (* We are doing a meet between two alias types. Whatever happens, the
         resulting extension of the global meet will contain an alias equation
         between the two inputs, so both the left-hand alias and the right-hand
         alias are correct results for the meet, allowing us to return
         [Both_inputs] in all cases. *)
      if Simple.equal simple1 simple2
         || Meet_env.already_meeting env simple1 simple2
      then
        (* The alias is either already present or being added in another path
           that will be merged with this one; no need to add any extension
           here *)
        Ok (Both_inputs, empty_extension)
      else (
        assert (not (Simple.equal simple1 simple2));
        let env = Meet_env.now_meeting env simple1 simple2 in
        let add_alias_equation env_extension =
          (* [add_equation] does nothing if the first argument is not a name
             (e.g. a constant). We indeed don't care about equations of the form
             [constant = constant], but we need to make sure we don't drop
             equations of the form [constant = name] just because they're in the
             wrong order. *)
          match Simple.must_be_name simple2 with
          | Some _ ->
            add_equation simple2 (TG.alias_type_of kind simple1) env_extension
          | None ->
            add_equation simple1 (TG.alias_type_of kind simple2) env_extension
        in
        let expanded1 =
          Expand_head.expand_head0 typing_env t1
            ~known_canonical_simple_at_in_types_mode:simple1_opt
        in
        let expanded2 =
          Expand_head.expand_head0 typing_env t2
            ~known_canonical_simple_at_in_types_mode:simple2_opt
        in
        (* In the following cases we may generate equations "pointing the wrong
           way", for example "y : =x" when [y] is the canonical element. This
           doesn't matter, however, because [TE] sorts this out when adding
           equations into an environment. *)
        (* Note that since we're going to add an alias between simple1 and
           simple2, either input can be used to express the result; so we will
           return Both_inputs as the result in all cases. The only thing
           remaining to compute is the extension. *)
        let env_extension : extension_with_memory Or_bottom.t =
          match meet_expanded_head env expanded1 expanded2 with
          | Ok ((Both_inputs | Left_input | Right_input), env_extension) ->
            (* No additional extension except for the new alias *)
            Ok (add_alias_equation env_extension)
          | Ok (New_result expanded, env_extension) ->
            (* The new result is tracked in the extension *)
            let env_extension =
              env_extension
              |> add_equation simple1 (ET.to_type expanded)
              |> add_alias_equation
            in
            Ok env_extension
          | Bottom -> Bottom
        in
        match env_extension with
        | Ok env_extension -> Ok (Both_inputs, env_extension)
        | Bottom -> Bottom))

and meet_expanded_head env (expanded1 : ET.t) (expanded2 : ET.t) :
    ET.t meet_result =
  match ET.descr expanded1, ET.descr expanded2 with
  | Unknown, Unknown -> Ok (Both_inputs, empty_extension)
  | _, Unknown -> Ok (Left_input, empty_extension)
  | Unknown, _ -> Ok (Right_input, empty_extension)
  | Bottom, _ -> Bottom
  | _, Bottom -> Bottom
  | Ok descr1, Ok descr2 -> meet_expanded_head0 env descr1 descr2

and meet_expanded_head0 env (descr1 : ET.descr) (descr2 : ET.descr) :
    ET.t meet_result =
  match descr1, descr2 with
  | Value head1, Value head2 ->
    map_result ~f:ET.create_value (meet_head_of_kind_value env head1 head2)
  | Naked_immediate head1, Naked_immediate head2 ->
    map_result ~f:ET.create_naked_immediate
      (meet_head_of_kind_naked_immediate env head1 head2)
  | Naked_float head1, Naked_float head2 ->
    map_result ~f:ET.create_naked_float
      (meet_head_of_kind_naked_float env head1 head2)
  | Naked_int32 head1, Naked_int32 head2 ->
    map_result ~f:ET.create_naked_int32
      (meet_head_of_kind_naked_int32 env head1 head2)
  | Naked_int64 head1, Naked_int64 head2 ->
    map_result ~f:ET.create_naked_int64
      (meet_head_of_kind_naked_int64 env head1 head2)
  | Naked_nativeint head1, Naked_nativeint head2 ->
    map_result ~f:ET.create_naked_nativeint
      (meet_head_of_kind_naked_nativeint env head1 head2)
  | Naked_vec128 head1, Naked_vec128 head2 ->
    map_result ~f:ET.create_naked_vec128
      (meet_head_of_kind_naked_vec128 env head1 head2)
  | Rec_info head1, Rec_info head2 ->
    map_result ~f:ET.create_rec_info
      (meet_head_of_kind_rec_info env head1 head2)
  | Region head1, Region head2 ->
    map_result ~f:ET.create_region (meet_head_of_kind_region env head1 head2)
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _ | Rec_info _
      | Region _ ),
      _ ) ->
    assert false

and meet_head_of_kind_value env (head1 : TG.head_of_kind_value)
    (head2 : TG.head_of_kind_value) : TG.head_of_kind_value meet_result =
  match head1, head2 with
  | ( Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1 },
      Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2 } )
    ->
    (* Uniqueness tracks whether duplication/lifting is allowed. It must always
       be propagated, both for meet and join. *)
    let is_unique = is_unique1 || is_unique2 in
    map_result
      ~f:(fun (blocks, immediates) ->
        TG.Head_of_kind_value.create_variant ~is_unique ~blocks ~immediates)
      (meet_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
  | ( Mutable_block { alloc_mode = alloc_mode1 },
      Mutable_block { alloc_mode = alloc_mode2 } ) ->
    map_result
      ~f:(fun alloc_mode ->
        TG.Head_of_kind_value.create_mutable_block alloc_mode)
      (meet_alloc_mode env alloc_mode1 alloc_mode2)
  | Variant { blocks; _ }, Mutable_block { alloc_mode = alloc_mode_right } -> (
    match blocks with
    | Unknown -> Ok (Right_input, empty_extension)
    | Known { alloc_mode = alloc_mode_left; _ } -> (
      (* CR vlaviron: This is not nice. We're more or less treating
         [Mutable_block] as more precise than [Variant], while losing precision
         on the way (the row_like equations on known immutable fields). Here is
         an example where it matters: *)
      (* type r = { a : int; mutable b : int }
       * let f r =
       *   let a1 = r.a in
       *   let a2 = r.a in
       *   (a1, a2)
       *
       * let g a =
       *   let r = { a; b = 0 } in
       *   let a1 = r.a in
       *   let a2 = r.a in
       *   (a1, a2) *)
      (* In [f], the two accesses will be shared because the type for those
         loads is [Variant]. But in [g], we get a more precise type
         ([Mutable_block]), and this prevents us from sharing the immutable
         loads.

         I have several ideas to fix this, but none of them are simple so we
         will need to have a proper discussion before I commit to implementing
         one. *)
      match meet_alloc_mode env alloc_mode_left alloc_mode_right with
      | Bottom -> Bottom
      | Ok ((Both_inputs | Right_input), ext) -> Ok (Right_input, ext)
      | Ok (Left_input, ext) ->
        Ok
          ( New_result
              (TG.Head_of_kind_value.create_mutable_block alloc_mode_left),
            ext )
      | Ok (New_result alloc_mode, ext) ->
        Ok
          ( New_result (TG.Head_of_kind_value.create_mutable_block alloc_mode),
            ext )))
  | Mutable_block { alloc_mode = alloc_mode_left }, Variant { blocks; _ } -> (
    match blocks with
    | Unknown -> Ok (Left_input, empty_extension)
    | Known { alloc_mode = alloc_mode_right; _ } -> (
      (* CR vlaviron: see symmetric case above *)
      match meet_alloc_mode env alloc_mode_left alloc_mode_right with
      | Bottom -> Bottom
      | Ok ((Both_inputs | Left_input), ext) -> Ok (Left_input, ext)
      | Ok (Right_input, ext) ->
        Ok
          ( New_result
              (TG.Head_of_kind_value.create_mutable_block alloc_mode_right),
            ext )
      | Ok (New_result alloc_mode, ext) ->
        Ok
          ( New_result (TG.Head_of_kind_value.create_mutable_block alloc_mode),
            ext )))
  | Boxed_float (n1, alloc_mode1), Boxed_float (n2, alloc_mode2) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_boxed_float ~meet_a:meet
      ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | Boxed_int32 (n1, alloc_mode1), Boxed_int32 (n2, alloc_mode2) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_boxed_int32 ~meet_a:meet
      ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | Boxed_int64 (n1, alloc_mode1), Boxed_int64 (n2, alloc_mode2) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_boxed_int64 ~meet_a:meet
      ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | Boxed_nativeint (n1, alloc_mode1), Boxed_nativeint (n2, alloc_mode2) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_boxed_nativeint ~meet_a:meet
      ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | Boxed_vec128 (n1, alloc_mode1), Boxed_vec128 (n2, alloc_mode2) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_boxed_vec128 ~meet_a:meet
      ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | ( Closures { by_function_slot = by_function_slot1; alloc_mode = alloc_mode1 },
      Closures
        { by_function_slot = by_function_slot2; alloc_mode = alloc_mode2 } ) ->
    combine_results2 env ~meet_env_extension
      ~rebuild:TG.Head_of_kind_value.create_closures
      ~meet_a:meet_row_like_for_closures ~meet_b:meet_alloc_mode
      ~left_a:by_function_slot1 ~right_a:by_function_slot2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | String strs1, String strs2 ->
    map_result ~f:TG.Head_of_kind_value.create_string
      (set_meet (module String_info.Set) strs1 strs2 ~of_set:Fun.id)
  | ( Array
        { element_kind = element_kind1;
          length = length1;
          contents = contents1;
          alloc_mode = alloc_mode1
        },
      Array
        { element_kind = element_kind2;
          length = length2;
          contents = contents2;
          alloc_mode = alloc_mode2
        } ) ->
    meet_array_type env
      (element_kind1, length1, contents1, alloc_mode1)
      (element_kind2, length2, contents2, alloc_mode2)
  | ( ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
      | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ),
      _ ) ->
    (* This assumes that all the different constructors are incompatible. This
       could break very hard for dubious uses of Obj. *)
    Bottom

and meet_array_type env (element_kind1, length1, contents1, alloc_mode1)
    (element_kind2, length2, contents2, alloc_mode2) =
  let element_kind = meet_array_element_kinds element_kind1 element_kind2 in
  combine_results env ~meet_env_extension
    ~rebuild:(fun (length, (contents, (alloc_mode, ()))) ->
      TG.Head_of_kind_value.create_array_with_contents ~element_kind ~length
        contents alloc_mode)
    ~meet_ops:
      (Meet
         { meet;
           next =
             Meet
               { meet = meet_array_contents ~meet_element_kind:element_kind;
                 next = Meet { meet = meet_alloc_mode; next = End }
               }
         })
    ~left_inputs:
      (Input
         { input = length1;
           next =
             Input
               { input = contents1;
                 next = Input { input = alloc_mode1; next = End }
               }
         })
    ~right_inputs:
      (Input
         { input = length2;
           next =
             Input
               { input = contents2;
                 next = Input { input = alloc_mode2; next = End }
               }
         })

and meet_array_contents env (array_contents1 : TG.array_contents Or_unknown.t)
    (array_contents2 : TG.array_contents Or_unknown.t)
    ~(meet_element_kind : _ Or_unknown_or_bottom.t) =
  meet_unknown
    (fun env (array_contents1 : TG.array_contents)
         (array_contents2 : TG.array_contents) : TG.array_contents meet_result ->
      match array_contents1, array_contents2 with
      | Mutable, Mutable -> Ok (Both_inputs, empty_extension)
      | Mutable, Immutable _ | Immutable _, Mutable -> Bottom
      | Immutable { fields = fields1 }, Immutable { fields = fields2 } ->
        if Array.length fields1 <> Array.length fields2
        then Bottom
        else
          match meet_element_kind with
          | Bottom -> Bottom
          | Unknown ->
            (* vlaviron: If the meet of the kinds is Unknown, then both
                       inputs had Unknown kinds. I don't see how we could end up
                       with an array type where the contents are known but we
                       don't know the kind, but in that case we wouldn't be able
                       to call meet because the two sides may have different
                       kinds. So we'll just return the first input, which is
                       guaranteed to be a correct approximation of the meet. *)
            Ok (Left_input, TEE.empty)
          | Ok _ ->
            map_result
              ~f:(fun fields : TG.array_contents -> Immutable { fields })
              (meet_array_of_types env fields1 fields2
                 ~length:(Array.length fields1)))
    ~contents_is_bottom:(fun (array_contents : TG.array_contents) ->
      match array_contents with
      | Mutable -> false
      | Immutable { fields } -> Array.exists TG.is_obviously_bottom fields)
    env array_contents1 array_contents2

and meet_variant env ~(blocks1 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms1 : TG.t Or_unknown.t)
    ~(blocks2 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms2 : TG.t Or_unknown.t) :
    (TG.Row_like_for_blocks.t Or_unknown.t * TG.t Or_unknown.t) meet_result =
  let blocks_meet =
    { is_bottom =
        (fun blocks ->
          match (blocks : _ Or_unknown.t) with
          | Unknown -> false
          | Known blocks -> TG.Row_like_for_blocks.is_bottom blocks);
      mk_bottom = (fun () -> Or_unknown.Known TG.Row_like_for_blocks.bottom);
      meet =
        meet_unknown meet_row_like_for_blocks
          ~contents_is_bottom:TG.Row_like_for_blocks.is_bottom
    }
  in
  let immediates_meet =
    { is_bottom =
        (fun immediates ->
          match (immediates : _ Or_unknown.t) with
          | Unknown -> false
          | Known imms -> TG.is_obviously_bottom imms);
      mk_bottom = (fun () -> Or_unknown.Known TG.bottom_naked_immediate);
      meet = meet_unknown meet ~contents_is_bottom:TG.is_obviously_bottom
    }
  in
  pairwise_disjunction_meet ~join_env_extension blocks_meet immediates_meet env
    blocks1 blocks2 imms1 imms2

and meet_head_of_kind_naked_immediate env (t1 : TG.head_of_kind_naked_immediate)
    (t2 : TG.head_of_kind_naked_immediate) :
    TG.head_of_kind_naked_immediate meet_result =
  let module I = Targetint_31_63 in
  let keep_side side : _ meet_result =
    match side with
    | Left -> Ok (Left_input, empty_extension)
    | Right -> Ok (Right_input, empty_extension)
  in
  let keep_other_side side : _ meet_result =
    match side with
    | Left -> Ok (Right_input, empty_extension)
    | Right -> Ok (Left_input, empty_extension)
  in
  let meet_with_shape ~rebuild ty shape side =
    map_result ~f:rebuild
      (match side with Left -> meet env ty shape | Right -> meet env shape ty)
  in
  let is_int_immediate ~is_int_ty ~immediates ~is_int_side =
    let rebuild = TG.Head_of_kind_naked_immediate.create_is_int in
    match I.Set.mem I.zero immediates, I.Set.mem I.one immediates with
    | false, false -> Bottom
    | true, true -> keep_side is_int_side
    | true, false ->
      meet_with_shape ~rebuild is_int_ty MTC.any_block is_int_side
    | false, true ->
      meet_with_shape ~rebuild is_int_ty MTC.any_tagged_immediate is_int_side
  in
  let get_tag_immediate ~get_tag_ty ~immediates ~get_tag_side =
    if I.Set.is_empty immediates
    then keep_other_side get_tag_side
    else
      let tags =
        I.Set.fold
          (fun tag tags ->
            match Tag.create_from_targetint tag with
            | Some tag -> Tag.Set.add tag tags
            | None -> tags (* No blocks exist with this tag *))
          immediates Tag.Set.empty
      in
      if Tag.Set.is_empty tags
      then Bottom
      else
        match
          MTC.blocks_with_these_tags tags (Alloc_mode.For_types.unknown ())
        with
        | Known shape ->
          meet_with_shape
            ~rebuild:TG.Head_of_kind_naked_immediate.create_get_tag get_tag_ty
            shape get_tag_side
        | Unknown -> keep_side get_tag_side
  in
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    map_result
      ~f:TG.Head_of_kind_naked_immediate.create_naked_immediates_non_empty
      (set_meet (module I.Set) is1 is2 ~of_set:Fun.id)
  | Is_int ty1, Is_int ty2 ->
    map_result ~f:TG.Head_of_kind_naked_immediate.create_is_int
      (meet env ty1 ty2)
  | Get_tag ty1, Get_tag ty2 ->
    map_result ~f:TG.Head_of_kind_naked_immediate.create_get_tag
      (meet env ty1 ty2)
  | Is_int is_int_ty, Naked_immediates immediates ->
    is_int_immediate ~is_int_ty ~immediates ~is_int_side:Left
  | Naked_immediates immediates, Is_int is_int_ty ->
    is_int_immediate ~is_int_ty ~immediates ~is_int_side:Right
  | Get_tag get_tag_ty, Naked_immediates immediates ->
    get_tag_immediate ~get_tag_ty ~immediates ~get_tag_side:Left
  | Naked_immediates immediates, Get_tag get_tag_ty ->
    get_tag_immediate ~get_tag_ty ~immediates ~get_tag_side:Right
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
    (* We can't return Bottom, as it would be unsound, so we need to either do
       the actual meet with Naked_immediates, or just give up and return one of
       the arguments. *)
    Ok (Left_input, empty_extension)

and meet_head_of_kind_naked_float _env t1 t2 =
  set_meet
    (module Numeric_types.Float_by_bit_pattern.Set)
    (t1
      : TG.head_of_kind_naked_float
      :> Numeric_types.Float_by_bit_pattern.Set.t)
    (t2
      : TG.head_of_kind_naked_float
      :> Numeric_types.Float_by_bit_pattern.Set.t)
    ~of_set:TG.Head_of_kind_naked_float.create_non_empty_set

and meet_head_of_kind_naked_int32 _env t1 t2 =
  set_meet
    (module Numeric_types.Int32.Set)
    (t1 : TG.head_of_kind_naked_int32 :> Numeric_types.Int32.Set.t)
    (t2 : TG.head_of_kind_naked_int32 :> Numeric_types.Int32.Set.t)
    ~of_set:TG.Head_of_kind_naked_int32.create_non_empty_set

and meet_head_of_kind_naked_int64 _env t1 t2 =
  set_meet
    (module Numeric_types.Int64.Set)
    (t1 : TG.head_of_kind_naked_int64 :> Numeric_types.Int64.Set.t)
    (t2 : TG.head_of_kind_naked_int64 :> Numeric_types.Int64.Set.t)
    ~of_set:TG.Head_of_kind_naked_int64.create_non_empty_set

and meet_head_of_kind_naked_nativeint _env t1 t2 =
  set_meet
    (module Targetint_32_64.Set)
    (t1 : TG.head_of_kind_naked_nativeint :> Targetint_32_64.Set.t)
    (t2 : TG.head_of_kind_naked_nativeint :> Targetint_32_64.Set.t)
    ~of_set:TG.Head_of_kind_naked_nativeint.create_non_empty_set

and meet_head_of_kind_naked_vec128 _env t1 t2 : _ Or_bottom.t =
  set_meet
    (module Vec128.Set)
    (t1 : TG.head_of_kind_naked_vec128 :> Vec128.Set.t)
    (t2 : TG.head_of_kind_naked_vec128 :> Vec128.Set.t)
    ~of_set:TG.Head_of_kind_naked_vec128.create_non_empty_set

and meet_head_of_kind_rec_info _env _t1 _t2 =
  (* CR-someday lmaurer: This could be doing things like discovering two depth
     variables are equal *)
  Ok (Both_inputs, empty_extension)

and meet_head_of_kind_region _env () () : _ meet_result =
  Ok (Both_inputs, empty_extension)

and meet_row_like :
      'index 'maps_to 'row_tag 'known.
      meet_maps_to:(Meet_env.t -> 'maps_to -> 'maps_to -> 'maps_to meet_result) ->
      equal_index:('index -> 'index -> bool) ->
      subset_index:('index -> 'index -> bool) ->
      union_index:('index -> 'index -> 'index) ->
      is_empty_map_known:('known -> bool) ->
      get_singleton_map_known:
        ('known -> ('row_tag * ('index, 'maps_to) TG.Row_like_case.t) option) ->
      merge_map_known:
        (('row_tag ->
         ('index, 'maps_to) TG.Row_like_case.t option ->
         ('index, 'maps_to) TG.Row_like_case.t option ->
         ('index, 'maps_to) TG.Row_like_case.t option) ->
        'known ->
        'known ->
        'known) ->
      Meet_env.t ->
      known1:'known ->
      known2:'known ->
      other1:('index, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      other2:('index, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      ('known * ('index, 'maps_to) TG.Row_like_case.t Or_bottom.t) meet_result =
 fun ~meet_maps_to ~equal_index ~subset_index ~union_index ~is_empty_map_known
     ~get_singleton_map_known ~merge_map_known meet_env ~known1 ~known2 ~other1
     ~other2 ->
  let env_extension = ref None in
  let need_join =
    (* The returned env_extension is the join of the env_extension produced by
       each non bottom cases. Therefore there is some loss of precision in that
       case and we need to store the one produced for each tag. But when only
       one tag is kept it would be wasteful (but correct) to store it.

       We consider that the result of the meet between t1 and t2 will have only
       one tag when t1 (or t2) has exactly one tag (one that and no 'other'
       cases).

       This is an overapproximation because the result could have only one tag
       for instance if

       t1 = [Tag 1 | Tag 2] and t2 = [Tag 2 | Tag 3], or if

       t1 = [Tag 1 | Tag 2] and t2 = [Tag 1 | Tag 2]

       but the meet between some combinations result in a bottom. *)
    match
      ( other1,
        get_singleton_map_known known1,
        other2,
        get_singleton_map_known known2 )
    with
    | Bottom, Some _, _, _ | _, _, Bottom, Some _ -> false
    | (Ok _ | Bottom), _, (Ok _ | Bottom), _ ->
      if is_empty_map_known known1 && is_empty_map_known known2
      then false
      else true
  in
  let env = Meet_env.env meet_env in
  let join_env = Join_env.create env ~left_env:env ~right_env:env in
  let result_is_t1 = ref true in
  let result_is_t2 = ref true in
  let join_env_extension ext =
    match !env_extension with
    | None -> env_extension := Some ext
    | Some ext2 ->
      assert need_join;
      let extension =
        join_env_extension join_env ext2.extension ext.extension
      in
      let pairs_of_names =
        Name.Pair.Set.inter ext2.pairs_of_names ext.pairs_of_names
      in
      env_extension := Some { pairs_of_names; extension }
  in
  let meet_index (i1 : 'index TG.row_like_index) (i2 : 'index TG.row_like_index)
      : 'index TG.row_like_index meet_result =
    match i1, i2 with
    | Known i1', Known i2' ->
      if equal_index i1' i2' then Ok (Both_inputs, empty_extension) else Bottom
    | Known known, At_least at_least ->
      if subset_index at_least known
      then
        (* [at_least] is included in [known] hence [Known known] is included in
           [At_least at_least], hence [Known known] \inter [At_least at_least] =
           [Known known] *)
        Ok (Left_input, empty_extension)
      else Bottom
    | At_least at_least, Known known ->
      if subset_index at_least known
      then Ok (Right_input, empty_extension)
      else Bottom
    | At_least i1', At_least i2' ->
      if subset_index i1' i2'
      then
        if subset_index i2' i1'
        then Ok (Both_inputs, empty_extension)
        else Ok (Right_input, empty_extension)
      else if subset_index i2' i1'
      then Ok (Left_input, empty_extension)
      else
        Ok
          ( New_result (TG.Row_like_index.at_least (union_index i1' i2')),
            empty_extension )
  in
  let bottom_case () =
    result_is_t1 := false;
    result_is_t2 := false;
    None
  in
  let meet_case (case1 : ('index, 'maps_to) TG.Row_like_case.t)
      (case2 : ('index, 'maps_to) TG.Row_like_case.t) =
    match meet_index case1.index case2.index with
    | Bottom -> bottom_case ()
    | Ok (index_result, _ext (* empty *)) -> (
      match meet_maps_to meet_env case1.maps_to case2.maps_to with
      | Bottom -> bottom_case ()
      | Ok (maps_to_result, env_extension') -> (
        let meet_env =
          Meet_env.assume_already_meeting meet_env env_extension'.pairs_of_names
        in
        match
          meet_env_extension meet_env
            (no_memory case1.env_extension)
            (no_memory case2.env_extension)
        with
        | Bottom -> bottom_case ()
        | Ok env_extension'' -> (
          let meet_env =
            Meet_env.assume_already_meeting meet_env
              env_extension''.pairs_of_names
          in
          match meet_env_extension meet_env env_extension' env_extension'' with
          | Bottom -> bottom_case ()
          | Ok env_extension ->
            join_env_extension env_extension;
            let update_refs = function
              | Both_inputs -> ()
              | Left_input -> result_is_t2 := false
              | Right_input -> result_is_t1 := false
              | New_result _ ->
                result_is_t1 := false;
                result_is_t2 := false
            in
            update_refs index_result;
            update_refs maps_to_result;
            let index = extract_value index_result case1.index case2.index in
            let maps_to =
              extract_value maps_to_result case1.maps_to case2.maps_to
            in
            let env_extension =
              if need_join then env_extension else empty_extension
            in
            Some
              (TG.Row_like_case.create ~maps_to ~index
                 ~env_extension:env_extension.extension))))
  in
  let meet_knowns case1 case2 : ('index, 'maps_to) TG.Row_like_case.t option =
    match case1, case2 with
    | None, None -> None
    | Some case1, None -> (
      match other2 with
      | Bottom ->
        result_is_t1 := false;
        None
      | Ok other_case -> meet_case case1 other_case)
    | None, Some case2 -> (
      match other1 with
      | Bottom ->
        result_is_t2 := false;
        None
      | Ok other_case -> meet_case other_case case2)
    | Some case1, Some case2 -> meet_case case1 case2
  in
  let known =
    merge_map_known
      (fun _tag case1 case2 -> meet_knowns case1 case2)
      known1 known2
  in
  let other : ('index, 'maps_to) TG.Row_like_case.t Or_bottom.t =
    match other1, other2 with
    | Bottom, Bottom -> Bottom
    | Bottom, _ ->
      result_is_t2 := false;
      Bottom
    | _, Bottom ->
      result_is_t1 := false;
      Bottom
    | Ok other1, Ok other2 -> (
      match meet_case other1 other2 with None -> Bottom | Some r -> Ok r)
  in
  if is_empty_map_known known
     && match other with Bottom -> true | Ok _ -> false
  then Bottom
  else
    let env_extension =
      match !env_extension with
      | None -> assert false (* This should be bottom *)
      | Some ext -> ext
    in
    match !result_is_t1, !result_is_t2 with
    | true, true -> Ok (Both_inputs, env_extension)
    | true, false -> Ok (Left_input, env_extension)
    | false, true -> Ok (Right_input, env_extension)
    | false, false -> Ok (New_result (known, other), env_extension)

and meet_row_like_for_blocks env
    ({ known_tags = known1; other_tags = other1; alloc_mode = alloc_mode1 } :
      TG.Row_like_for_blocks.t)
    ({ known_tags = known2; other_tags = other2; alloc_mode = alloc_mode2 } :
      TG.Row_like_for_blocks.t) : TG.Row_like_for_blocks.t meet_result =
  combine_results2 env ~meet_env_extension
    ~rebuild:(fun (known_tags, other_tags) alloc_mode ->
      TG.Row_like_for_blocks.create_raw ~known_tags ~other_tags ~alloc_mode)
    ~meet_a:(fun env (known1, other1) (known2, other2) ->
      meet_row_like ~meet_maps_to:meet_int_indexed_product
        ~equal_index:TG.Block_size.equal ~subset_index:TG.Block_size.subset
        ~union_index:TG.Block_size.union ~is_empty_map_known:Tag.Map.is_empty
        ~get_singleton_map_known:Tag.Map.get_singleton
        ~merge_map_known:Tag.Map.merge env ~known1 ~known2 ~other1 ~other2)
    ~meet_b:meet_alloc_mode ~left_a:(known1, other1) ~right_a:(known2, other2)
    ~left_b:alloc_mode1 ~right_b:alloc_mode2

and meet_row_like_for_closures env
    ({ known_closures = known1; other_closures = other1 } :
      TG.Row_like_for_closures.t)
    ({ known_closures = known2; other_closures = other2 } :
      TG.Row_like_for_closures.t) : TG.Row_like_for_closures.t meet_result =
  map_result
    ~f:(fun (known_closures, other_closures) ->
      TG.Row_like_for_closures.create_raw ~known_closures ~other_closures)
    (meet_row_like ~meet_maps_to:meet_closures_entry
       ~equal_index:Set_of_closures_contents.equal
       ~subset_index:Set_of_closures_contents.subset
       ~union_index:Set_of_closures_contents.union
       ~is_empty_map_known:Function_slot.Map.is_empty
       ~get_singleton_map_known:Function_slot.Map.get_singleton
       ~merge_map_known:Function_slot.Map.merge env ~known1 ~known2 ~other1
       ~other2)

and meet_closures_entry (env : Meet_env.t)
    ({ function_types = function_types1;
       closure_types = closure_types1;
       value_slot_types = value_slot_types1
     } :
      TG.Closures_entry.t)
    ({ function_types = function_types2;
       closure_types = closure_types2;
       value_slot_types = value_slot_types2
     } :
      TG.Closures_entry.t) : TG.Closures_entry.t meet_result =
  combine_results env ~meet_env_extension
    ~meet_ops:
      (Meet
         { meet =
             Function_slot_map_meet.meet ~meet_env_extension
               ~meet_data:meet_function_type;
           next =
             Meet
               { meet = meet_product_function_slot_indexed;
                 next =
                   Meet { meet = meet_product_value_slot_indexed; next = End }
               }
         })
    ~left_inputs:
      (Input
         { input = function_types1;
           next =
             Input
               { input = closure_types1;
                 next = Input { input = value_slot_types1; next = End }
               }
         })
    ~right_inputs:
      (Input
         { input = function_types2;
           next =
             Input
               { input = closure_types2;
                 next = Input { input = value_slot_types2; next = End }
               }
         })
    ~rebuild:(fun (function_types, (closure_types, (value_slot_types, ()))) ->
      TG.Closures_entry.create ~function_types ~closure_types ~value_slot_types)

and meet_product_function_slot_indexed env
    ({ function_slot_components_by_index = components_by_index1 } :
      TG.Product.Function_slot_indexed.t)
    ({ function_slot_components_by_index = components_by_index2 } :
      TG.Product.Function_slot_indexed.t) :
    TG.Product.Function_slot_indexed.t meet_result =
  map_result ~f:TG.Product.Function_slot_indexed.create
    (Function_slot_map_meet.meet ~meet_data:meet ~meet_env_extension env
       components_by_index1 components_by_index2)

and meet_product_value_slot_indexed env
    ({ value_slot_components_by_index = components_by_index1 } :
      TG.Product.Value_slot_indexed.t)
    ({ value_slot_components_by_index = components_by_index2 } :
      TG.Product.Value_slot_indexed.t) :
    TG.Product.Value_slot_indexed.t meet_result =
  map_result ~f:TG.Product.Value_slot_indexed.create
    (Value_slot_map_meet.meet ~meet_data:meet ~meet_env_extension env
       components_by_index1 components_by_index2)

and meet_int_indexed_product env (prod1 : TG.Product.Int_indexed.t)
    (prod2 : TG.Product.Int_indexed.t) : _ meet_result =
  if not (K.equal prod1.kind prod2.kind)
  then Bottom
  else
    let fields1 = prod1.fields in
    let fields2 = prod2.fields in
    let length = max (Array.length fields1) (Array.length fields2) in
    map_result
      ~f:(TG.Product.Int_indexed.create_from_array prod1.kind)
      (meet_array_of_types env fields1 fields2 ~length)

and meet_array_of_types env fields1 fields2 ~length =
  let any_bottom = ref false in
  let all_left = ref true in
  let all_right = ref true in
  let env_extension = ref empty_extension in
  let env = ref env in
  let fields =
    Array.init length (fun index ->
        let get_opt fields =
          if index >= Array.length fields then None else Some fields.(index)
        in
        match get_opt fields1, get_opt fields2 with
        | None, None -> assert false
        | Some t, None ->
          all_right := false;
          t
        | None, Some t ->
          all_left := false;
          t
        | Some ty1, Some ty2 -> (
          match meet !env ty1 ty2 with
          | Ok (meet_result, env_extension') -> (
            env
              := Meet_env.assume_already_meeting !env
                   env_extension'.pairs_of_names;
            match meet_env_extension !env !env_extension env_extension' with
            | Bottom ->
              any_bottom := true;
              MTC.bottom_like ty1
            | Ok extension -> (
              env_extension := extension;
              env
                := Meet_env.assume_already_meeting !env extension.pairs_of_names;
              match meet_result with
              | Left_input ->
                all_right := false;
                ty1
              | Right_input ->
                all_left := false;
                ty2
              | Both_inputs -> ty1
              | New_result ty ->
                all_left := false;
                all_right := false;
                ty))
          | Bottom ->
            any_bottom := true;
            MTC.bottom_like ty1))
  in
  if !any_bottom
  then Bottom
  else
    let result =
      match !all_left, !all_right with
      | true, true -> Both_inputs
      | true, false -> Left_input
      | false, true -> Right_input
      | false, false -> New_result fields
    in
    Ok (result, !env_extension)

and meet_function_type (env : Meet_env.t)
    (func_type1 : TG.Function_type.t Or_unknown_or_bottom.t)
    (func_type2 : TG.Function_type.t Or_unknown_or_bottom.t) :
    TG.Function_type.t Or_unknown_or_bottom.t meet_result =
  match func_type1, func_type2 with
  | Bottom, Bottom | Unknown, Unknown -> Ok (Both_inputs, empty_extension)
  | Bottom, _ | _, Unknown -> Ok (Left_input, empty_extension)
  | _, Bottom | Unknown, _ -> Ok (Right_input, empty_extension)
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) ->
    let rebuild code_id rec_info =
      (* It's possible that [code_id] corresponds to deleted code. In that case,
         any attempt to inline will fail, as the code will not be found in the
         simplifier's environment -- see
         [Simplify_apply_expr.simplify_direct_function_call]. *)
      Or_unknown_or_bottom.Ok (TG.Function_type.create code_id ~rec_info)
    in
    combine_results2 env ~meet_env_extension ~rebuild ~meet_a:meet_code_id
      ~left_a:code_id1 ~right_a:code_id2 ~meet_b:meet ~left_b:rec_info1
      ~right_b:rec_info2

and meet_env_extension0 env (ext1 : extension_with_memory)
    (ext2 : extension_with_memory) extra_extensions : extension_with_memory =
  (* A symmetrical meet would be hard to implement, as the inner meets can
     produce extra extensions that need to be merged with the result.

     To get around this, we'll suppose that [t2] is smaller than [t1] and add
     equations from [t2] to [t1], along with all extra equations *)
  let equations, extra_extensions, env =
    Name.Map.fold
      (fun name ty (eqs, extra_extensions, env) ->
        match Name.Map.find_opt name eqs with
        | None ->
          MTC.check_equation name ty;
          Name.Map.add name ty eqs, extra_extensions, env
        | Some ty0 -> (
          match meet env ty0 ty with
          | Bottom -> raise Bottom_meet
          | Ok (res, new_ext) -> (
            let extra_extensions =
              if TEE.is_empty new_ext.extension
              then extra_extensions
              else new_ext :: extra_extensions
            in
            let env =
              Meet_env.assume_already_meeting env new_ext.pairs_of_names
            in
            match res with
            | Left_input | Both_inputs ->
              (* The equation is already there *)
              eqs, extra_extensions, env
            | Right_input ->
              Name.Map.add (*replace*) name ty eqs, extra_extensions, env
            | New_result ty ->
              MTC.check_equation name ty;
              Name.Map.add (*replace*) name ty eqs, extra_extensions, env)))
      (TEE.to_map ext2.extension)
      (TEE.to_map ext1.extension, extra_extensions, env)
  in
  let extension = TEE.from_map equations in
  let pairs_of_names =
    Name.Pair.Set.union ext1.pairs_of_names ext2.pairs_of_names
  in
  let ext = { pairs_of_names; extension } in
  match extra_extensions with
  | [] -> ext
  | new_ext :: extra_extensions ->
    (* CR vlaviron: think about this comment and update it (might be fixed ?) *)
    (* CR-someday vlaviron: It's a bad idea to drop the extensions in the
       general case, but since we lack the property that the new extensions are
       stricter than the existing ones we can get into an infinite loop here
       (see flambdatest/unit_test/extension_meet.ml, function
       test_double_recursion for an example).

       This is very uncommon though (it needs recursive types involving at least
       three different names), so for now we still do the meet
       systematically. *)
    meet_env_extension0 env ext new_ext extra_extensions

and meet_env_extension (env : Meet_env.t) t1 t2 :
    extension_with_memory Or_bottom.t =
  try Ok (meet_env_extension0 env t1 t2 []) with Bottom_meet -> Bottom

and join ?bound_name env (t1 : TG.t) (t2 : TG.t) : TG.t Or_unknown.t =
  if not (K.equal (TG.kind t1) (TG.kind t2))
  then
    Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus@ %a" TG.print t1
      TG.print t2;
  let kind = TG.kind t1 in
  let canonical_simple1 =
    match
      TE.get_alias_then_canonical_simple_exn
        (Join_env.left_join_env env)
        t1 ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let canonical_simple2 =
    match
      TE.get_alias_then_canonical_simple_exn
        (Join_env.right_join_env env)
        t2 ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let expanded1 =
    Expand_head.expand_head0
      (Join_env.left_join_env env)
      t1 ~known_canonical_simple_at_in_types_mode:canonical_simple1
  in
  let expanded2 =
    Expand_head.expand_head0
      (Join_env.right_join_env env)
      t2 ~known_canonical_simple_at_in_types_mode:canonical_simple2
  in
  let shared_aliases =
    let shared_aliases =
      match
        ( canonical_simple1,
          ET.descr expanded1,
          canonical_simple2,
          ET.descr expanded2 )
      with
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
               (Join_env.left_join_env env)
               canonical_simple1
               ~in_env:(Join_env.target_join_env env))
            (all_aliases_of
               (Join_env.right_join_env env)
               canonical_simple2
               ~in_env:(Join_env.target_join_env env))
    in
    match bound_name with
    | None -> shared_aliases
    | Some bound_name ->
      (* This ensures that we're not creating an alias to a different simple
         that is just bound_name with different coercion. Such an alias is
         forbidden. *)
      Aliases.Alias_set.filter
        ~f:(fun simple -> not (Simple.same simple (Simple.name bound_name)))
        shared_aliases
  in
  let unknown () : _ Or_unknown.t =
    (* CR vlaviron: Fix this to Unknown when Product can handle it *)
    Known (MTC.unknown kind)
  in
  match Aliases.Alias_set.find_best shared_aliases with
  | Some alias -> Known (TG.alias_type_of kind alias)
  | None -> (
    match canonical_simple1, canonical_simple2 with
    | Some simple1, Some simple2
      when Join_env.already_joining env simple1 simple2 ->
      unknown ()
    | Some _, Some _ | Some _, None | None, Some _ | None, None -> (
      let join_heads env : _ Or_unknown.t =
        Known (ET.to_type (join_expanded_head env kind expanded1 expanded2))
      in
      match canonical_simple1, canonical_simple2 with
      | Some simple1, Some simple2 -> (
        match Join_env.now_joining env simple1 simple2 with
        | Continue env -> join_heads env
        | Stop -> unknown ())
      | Some _, None | None, Some _ | None, None -> join_heads env))

and join_expanded_head env kind (expanded1 : ET.t) (expanded2 : ET.t) : ET.t =
  match ET.descr expanded1, ET.descr expanded2 with
  | Bottom, Bottom -> ET.create_bottom kind
  (* The target environment defines all the names from the left and right
     environments, so we can safely return any input as the result *)
  | Ok _, Bottom -> expanded1
  | Bottom, Ok _ -> expanded2
  | Unknown, _ | _, Unknown -> ET.create_unknown kind
  | Ok descr1, Ok descr2 -> (
    let expanded_or_unknown =
      match descr1, descr2 with
      | Value head1, Value head2 ->
        let>+ head = join_head_of_kind_value env head1 head2 in
        ET.create_value head
      | Naked_immediate head1, Naked_immediate head2 ->
        let>+ head = join_head_of_kind_naked_immediate env head1 head2 in
        ET.create_naked_immediate head
      | Naked_float head1, Naked_float head2 ->
        let>+ head = join_head_of_kind_naked_float env head1 head2 in
        ET.create_naked_float head
      | Naked_int32 head1, Naked_int32 head2 ->
        let>+ head = join_head_of_kind_naked_int32 env head1 head2 in
        ET.create_naked_int32 head
      | Naked_int64 head1, Naked_int64 head2 ->
        let>+ head = join_head_of_kind_naked_int64 env head1 head2 in
        ET.create_naked_int64 head
      | Naked_nativeint head1, Naked_nativeint head2 ->
        let>+ head = join_head_of_kind_naked_nativeint env head1 head2 in
        ET.create_naked_nativeint head
      | Naked_vec128 head1, Naked_vec128 head2 ->
        let>+ head = join_head_of_kind_naked_vec128 env head1 head2 in
        ET.create_naked_vec128 head
      | Rec_info head1, Rec_info head2 ->
        let>+ head = join_head_of_kind_rec_info env head1 head2 in
        ET.create_rec_info head
      | Region head1, Region head2 ->
        let>+ head = join_head_of_kind_region env head1 head2 in
        ET.create_region head
      | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _ | Rec_info _
          | Region _ ),
          _ ) ->
        assert false
    in
    match expanded_or_unknown with
    | Known expanded -> expanded
    | Unknown -> ET.unknown_like expanded1)

and join_head_of_kind_value env (head1 : TG.head_of_kind_value)
    (head2 : TG.head_of_kind_value) : TG.head_of_kind_value Or_unknown.t =
  match head1, head2 with
  | ( Variant { blocks = blocks1; immediates = imms1; is_unique = is_unique1 },
      Variant { blocks = blocks2; immediates = imms2; is_unique = is_unique2 } )
    ->
    let>+ blocks, immediates =
      join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2
    in
    (* Uniqueness tracks whether duplication/lifting is allowed. It must always
       be propagated, both for meet and join. *)
    let is_unique = is_unique1 || is_unique2 in
    TG.Head_of_kind_value.create_variant ~is_unique ~blocks ~immediates
  | ( Mutable_block { alloc_mode = alloc_mode1 },
      Mutable_block { alloc_mode = alloc_mode2 } ) ->
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    Known (TG.Head_of_kind_value.create_mutable_block alloc_mode)
  | Boxed_float (n1, alloc_mode1), Boxed_float (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_float n alloc_mode
  | Boxed_int32 (n1, alloc_mode1), Boxed_int32 (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_int32 n alloc_mode
  | Boxed_int64 (n1, alloc_mode1), Boxed_int64 (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_int64 n alloc_mode
  | Boxed_nativeint (n1, alloc_mode1), Boxed_nativeint (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_nativeint n alloc_mode
  | Boxed_vec128 (n1, alloc_mode1), Boxed_vec128 (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_vec128 n alloc_mode
  | ( Closures { by_function_slot = by_function_slot1; alloc_mode = alloc_mode1 },
      Closures
        { by_function_slot = by_function_slot2; alloc_mode = alloc_mode2 } ) ->
    let by_function_slot =
      join_row_like_for_closures env by_function_slot1 by_function_slot2
    in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    Known (TG.Head_of_kind_value.create_closures by_function_slot alloc_mode)
  | String strs1, String strs2 ->
    let strs = String_info.Set.union strs1 strs2 in
    Known (TG.Head_of_kind_value.create_string strs)
  | ( Array
        { element_kind = element_kind1;
          length = length1;
          contents = array_contents1;
          alloc_mode = alloc_mode1
        },
      Array
        { element_kind = element_kind2;
          length = length2;
          contents = array_contents2;
          alloc_mode = alloc_mode2
        } ) ->
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    let element_kind = join_array_element_kinds element_kind1 element_kind2 in
    let contents =
      join_array_contents env array_contents1 array_contents2
        ~joined_element_kind:element_kind
    in
    let>+ length = join env length1 length2 in
    TG.Head_of_kind_value.create_array_with_contents ~element_kind ~length
      contents alloc_mode
  | ( ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
      | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ),
      _ ) ->
    Unknown

and join_array_contents env (array_contents1 : TG.array_contents Or_unknown.t)
    (array_contents2 : TG.array_contents Or_unknown.t)
    ~(joined_element_kind : _ Or_unknown_or_bottom.t) =
  join_unknown
    (fun env (array_contents1 : TG.array_contents)
         (array_contents2 : TG.array_contents) : TG.array_contents Or_unknown.t ->
      match array_contents1, array_contents2 with
      | Mutable, Mutable -> Known TG.Mutable
      | Mutable, Immutable _ | Immutable _, Mutable -> Unknown
      | Immutable { fields = fields1 }, Immutable { fields = fields2 } -> (
        if Array.length fields1 <> Array.length fields2
        then Unknown
        else
          match joined_element_kind with
          | Bottom | Unknown -> Unknown
          | Ok _ ->
            let exception Unknown_result in
            try
              let fields =
                Array.init (Array.length fields1) (fun idx ->
                    match join env fields1.(idx) fields2.(idx) with
                    | Unknown -> raise Unknown_result
                    | Known ty -> ty)
              in
              Known (TG.Immutable { fields })
            with Unknown_result -> Unknown))
    env array_contents1 array_contents2

and join_variant env ~(blocks1 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms1 : TG.t Or_unknown.t)
    ~(blocks2 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms2 : TG.t Or_unknown.t) :
    (TG.Row_like_for_blocks.t Or_unknown.t * TG.t Or_unknown.t) Or_unknown.t =
  let blocks_join env b1 b2 : _ Or_unknown.t =
    Known (join_row_like_for_blocks env b1 b2)
  in
  let blocks = join_unknown blocks_join env blocks1 blocks2 in
  let imms = join_unknown (join ?bound_name:None) env imms1 imms2 in
  match blocks, imms with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown | Unknown, Known _ | Known _, Known _ ->
    Known (blocks, imms)

and join_head_of_kind_naked_immediate env
    (head1 : TG.Head_of_kind_naked_immediate.t)
    (head2 : TG.Head_of_kind_naked_immediate.t) :
    TG.Head_of_kind_naked_immediate.t Or_unknown.t =
  let module I = Targetint_31_63 in
  match head1, head2 with
  | Naked_immediates is1, Naked_immediates is2 -> (
    assert (not (Targetint_31_63.Set.is_empty is1));
    assert (not (Targetint_31_63.Set.is_empty is2));
    let is = I.Set.union is1 is2 in
    let head = TG.Head_of_kind_naked_immediate.create_naked_immediates is in
    match head with
    | Ok head -> Known head
    | Bottom ->
      Misc.fatal_error "Did not expect [Bottom] from [create_naked_immediates]")
  | Is_int ty1, Is_int ty2 ->
    let>+ ty = join env ty1 ty2 in
    TG.Head_of_kind_naked_immediate.create_is_int ty
  | Get_tag ty1, Get_tag ty2 ->
    let>+ ty = join env ty1 ty2 in
    TG.Head_of_kind_naked_immediate.create_get_tag ty
  (* From now on: Irregular cases *)
  (* CR vlaviron: There could be improvements based on reduction (trying to
     reduce the is_int and get_tag cases to naked_immediate sets, then joining
     those) but this looks unlikely to be useful and could end up begin quite
     expensive. *)
  | Is_int ty, Naked_immediates is_int | Naked_immediates is_int, Is_int ty -> (
    if I.Set.is_empty is_int
    then Known (TG.Head_of_kind_naked_immediate.create_is_int ty)
    else
      (* Slightly better than Unknown *)
      let head =
        TG.Head_of_kind_naked_immediate.create_naked_immediates
          (I.Set.add I.zero (I.Set.add I.one is_int))
      in
      match head with
      | Ok head -> Known head
      | Bottom ->
        Misc.fatal_error
          "Did not expect [Bottom] from [create_naked_immediates]")
  | Get_tag ty, Naked_immediates tags | Naked_immediates tags, Get_tag ty ->
    if I.Set.is_empty tags
    then Known (TG.Head_of_kind_naked_immediate.create_get_tag ty)
    else Unknown
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) -> Unknown

and join_head_of_kind_naked_float _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_float.union t1 t2)

and join_head_of_kind_naked_int32 _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_int32.union t1 t2)

and join_head_of_kind_naked_int64 _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_int64.union t1 t2)

and join_head_of_kind_naked_nativeint _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_nativeint.union t1 t2)

and join_head_of_kind_naked_vec128 _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_vec128.union t1 t2)

and join_head_of_kind_rec_info _env t1 t2 : _ Or_unknown.t =
  if Rec_info_expr.equal t1 t2 then Known t1 else Unknown

and join_head_of_kind_region _env () () : _ Or_unknown.t = Known ()

(* Note that unlike the [join] function on types, for structures (closures
   entry, row-like, etc.) the return type is [t] (and not [t Or_unknown.t]).
   This simplifies some parts of the code a bit that cannot handle the Unknown
   case gracefully. All join functions for structures can handle [Unknown]
   results from generic [join]s without needing to propagate them. *)

and join_row_like :
      'index 'maps_to 'row_tag 'known.
      join_maps_to:(Join_env.t -> 'maps_to -> 'maps_to -> 'maps_to) ->
      maps_to_field_kind:('maps_to -> K.t) option ->
      equal_index:('index -> 'index -> bool) ->
      inter_index:('index -> 'index -> 'index) ->
      merge_map_known:
        (('row_tag ->
         ('index, 'maps_to) TG.Row_like_case.t option ->
         ('index, 'maps_to) TG.Row_like_case.t option ->
         ('index, 'maps_to) TG.Row_like_case.t option) ->
        'known ->
        'known ->
        'known) ->
      Join_env.t ->
      known1:'known ->
      known2:'known ->
      other1:('index, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      other2:('index, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      'known * ('index, 'maps_to) TG.Row_like_case.t Or_bottom.t =
 fun ~join_maps_to ~maps_to_field_kind ~equal_index ~inter_index
     ~merge_map_known join_env ~known1 ~known2 ~other1 ~other2 ->
  let join_index (i1 : 'index TG.row_like_index) (i2 : 'index TG.row_like_index)
      : 'index TG.row_like_index =
    match i1, i2 with
    | Known i1', Known i2' ->
      if equal_index i1' i2'
      then i1
      else
        (* We can't represent exactly the union, This is the best
           approximation *)
        TG.Row_like_index.at_least (inter_index i1' i2')
    | Known i1', At_least i2'
    | At_least i1', Known i2'
    | At_least i1', At_least i2' ->
      TG.Row_like_index.at_least (inter_index i1' i2')
  in
  let matching_kinds (case1 : ('index, 'maps_to) TG.Row_like_case.t)
      (case2 : ('index, 'maps_to) TG.Row_like_case.t) =
    match maps_to_field_kind with
    | None -> true
    | Some maps_to_field_kind ->
      K.equal
        (maps_to_field_kind case1.maps_to)
        (maps_to_field_kind case2.maps_to)
  in
  let join_case join_env (case1 : ('index, 'maps_to) TG.Row_like_case.t)
      (case2 : ('index, 'maps_to) TG.Row_like_case.t) =
    let index = join_index case1.index case2.index in
    let maps_to = join_maps_to join_env case1.maps_to case2.maps_to in
    let env_extension =
      join_env_extension join_env case1.env_extension case2.env_extension
    in
    TG.Row_like_case.create ~maps_to ~index ~env_extension
  in
  let join_knowns case1 case2 : ('index, 'maps_to) TG.Row_like_case.t option =
    (* We assume that if tags are equals, the products will contains values of
       the same kinds. *)
    match case1, case2 with
    | None, None -> None
    | Some case1, None -> (
      let only_case1 () =
        (* cf. Type_descr.join_head_or_unknown_or_bottom, we need to join these
           to ensure that free variables not present in the target env are
           cleaned out of the types. Same below *)
        (* CR pchambart: This seams terribly inefficient. *)
        let join_env =
          Join_env.create
            (Join_env.target_join_env join_env)
            ~left_env:(Join_env.left_join_env join_env)
            ~right_env:(Join_env.left_join_env join_env)
        in
        let case1 = join_case join_env case1 case1 in
        Some case1
      in
      match other2 with
      | Bottom -> only_case1 ()
      | Ok other_case ->
        if matching_kinds case1 other_case
        then Some (join_case join_env case1 other_case)
        else (* If kinds don't match, the tags can't match *)
          only_case1 ())
    | None, Some case2 -> (
      let only_case2 () =
        (* See at the other bottom case *)
        let join_env =
          Join_env.create
            (Join_env.target_join_env join_env)
            ~left_env:(Join_env.right_join_env join_env)
            ~right_env:(Join_env.right_join_env join_env)
        in
        let case2 = join_case join_env case2 case2 in
        Some case2
      in
      match other1 with
      | Bottom -> only_case2 ()
      | Ok other_case ->
        if matching_kinds other_case case2
        then Some (join_case join_env other_case case2)
        else only_case2 ())
    | Some case1, Some case2 -> Some (join_case join_env case1 case2)
  in
  let known =
    merge_map_known
      (fun _tag case1 case2 -> join_knowns case1 case2)
      known1 known2
  in
  let other : ('index, 'maps_to) TG.Row_like_case.t Or_bottom.t =
    match other1, other2 with
    | Bottom, Bottom -> Bottom
    | Ok other1, Bottom ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env join_env)
          ~left_env:(Join_env.left_join_env join_env)
          ~right_env:(Join_env.left_join_env join_env)
      in
      let other1 = join_case env other1 other1 in
      Ok other1
    | Bottom, Ok other2 ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env join_env)
          ~left_env:(Join_env.right_join_env join_env)
          ~right_env:(Join_env.right_join_env join_env)
      in
      let other2 = join_case env other2 other2 in
      Ok other2
    | Ok other1, Ok other2 -> Ok (join_case join_env other1 other2)
  in
  known, other

and join_row_like_for_blocks env
    ({ known_tags = known1; other_tags = other1; alloc_mode = alloc_mode1 } :
      TG.Row_like_for_blocks.t)
    ({ known_tags = known2; other_tags = other2; alloc_mode = alloc_mode2 } :
      TG.Row_like_for_blocks.t) =
  let known_tags, other_tags =
    join_row_like ~join_maps_to:join_int_indexed_product
      ~maps_to_field_kind:(Some TG.Product.Int_indexed.field_kind)
      ~equal_index:TG.Block_size.equal ~inter_index:TG.Block_size.inter
      ~merge_map_known:Tag.Map.merge env ~known1 ~known2 ~other1 ~other2
  in
  let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
  TG.Row_like_for_blocks.create_raw ~known_tags ~other_tags ~alloc_mode

and join_row_like_for_closures env
    ({ known_closures = known1; other_closures = other1 } :
      TG.Row_like_for_closures.t)
    ({ known_closures = known2; other_closures = other2 } :
      TG.Row_like_for_closures.t) : TG.Row_like_for_closures.t =
  let known_closures, other_closures =
    join_row_like ~join_maps_to:join_closures_entry ~maps_to_field_kind:None
      ~equal_index:Set_of_closures_contents.equal
      ~inter_index:Set_of_closures_contents.inter
      ~merge_map_known:Function_slot.Map.merge env ~known1 ~known2 ~other1
      ~other2
  in
  TG.Row_like_for_closures.create_raw ~known_closures ~other_closures

and join_closures_entry env
    ({ function_types = function_types1;
       closure_types = closure_types1;
       value_slot_types = value_slot_types1
     } :
      TG.Closures_entry.t)
    ({ function_types = function_types2;
       closure_types = closure_types2;
       value_slot_types = value_slot_types2
     } :
      TG.Closures_entry.t) : TG.Closures_entry.t =
  let function_types =
    Function_slot.Map.merge
      (fun _function_slot func_type1 func_type2 ->
        match func_type1, func_type2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some func_type1, Some func_type2 ->
          Some (join_function_type env func_type1 func_type2))
      function_types1 function_types2
  in
  let closure_types =
    join_function_slot_indexed_product env closure_types1 closure_types2
  in
  let value_slot_types =
    join_value_slot_indexed_product env value_slot_types1 value_slot_types2
  in
  TG.Closures_entry.create ~function_types ~closure_types ~value_slot_types

and join_generic_product :
      'key 'key_map.
      Join_env.t ->
      components_by_index1:'key_map ->
      components_by_index2:'key_map ->
      merge:
        (('key -> TG.t option -> TG.t option -> TG.t option) ->
        'key_map ->
        'key_map ->
        'key_map) ->
      'key_map =
 fun env ~components_by_index1 ~components_by_index2 ~merge ->
  merge
    (fun _index ty1_opt ty2_opt ->
      match ty1_opt, ty2_opt with
      | None, _ | _, None -> None
      | Some ty1, Some ty2 -> (
        match join env ty1 ty2 with Known ty -> Some ty | Unknown -> None))
    components_by_index1 components_by_index2

and join_function_slot_indexed_product env
    ({ function_slot_components_by_index = components_by_index1 } :
      TG.Product.Function_slot_indexed.t)
    ({ function_slot_components_by_index = components_by_index2 } :
      TG.Product.Function_slot_indexed.t) : TG.Product.Function_slot_indexed.t =
  let function_slot_components_by_index =
    join_generic_product env ~components_by_index1 ~components_by_index2
      ~merge:Function_slot.Map.merge
  in
  TG.Product.Function_slot_indexed.create function_slot_components_by_index

and join_value_slot_indexed_product env
    ({ value_slot_components_by_index = components_by_index1 } :
      TG.Product.Value_slot_indexed.t)
    ({ value_slot_components_by_index = components_by_index2 } :
      TG.Product.Value_slot_indexed.t) : TG.Product.Value_slot_indexed.t =
  let value_slot_components_by_index =
    join_generic_product env ~components_by_index1 ~components_by_index2
      ~merge:Value_slot.Map.merge
  in
  TG.Product.Value_slot_indexed.create value_slot_components_by_index

and join_int_indexed_product env
    ({ fields = fields1; kind = kind1 } : TG.Product.Int_indexed.t)
    ({ fields = fields2; kind = kind2 } : TG.Product.Int_indexed.t) :
    TG.Product.Int_indexed.t =
  if not (K.equal kind1 kind2)
  then
    Misc.fatal_errorf
      "join_int_indexed_product between mismatching kinds %a and %a@." K.print
      kind1 K.print kind2;
  let length1 = Array.length fields1 in
  let length2 = Array.length fields2 in
  let length = min length1 length2 in
  let exception Exit in
  let all_phys_equal =
    try
      for index = 0 to length - 1 do
        if fields1.(index) != fields2.(index) then raise Exit
      done;
      true
    with Exit -> false
  in
  let fields =
    if all_phys_equal
    then
      if Int.equal length1 length
      then fields1
      else (
        assert (Int.equal length2 length);
        fields2)
    else
      Array.init length (fun index ->
          if fields1.(index) == fields2.(index)
          then fields1.(index)
          else
            match join env fields1.(index) fields2.(index) with
            | Unknown -> MTC.unknown kind1
            | Known ty -> ty)
  in
  TG.Product.Int_indexed.create_from_array kind1 fields

and join_function_type (env : Join_env.t)
    (func_type1 : TG.Function_type.t Or_unknown_or_bottom.t)
    (func_type2 : TG.Function_type.t Or_unknown_or_bottom.t) :
    TG.Function_type.t Or_unknown_or_bottom.t =
  match func_type1, func_type2 with
  | Bottom, func_type | func_type, Bottom -> func_type
  | Unknown, _ | _, Unknown -> Unknown
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) -> (
    let target_typing_env = Join_env.target_join_env env in
    (* As a note, sometimes it might be preferable not to do the code age
       relation join, and take the hit of an indirect call in exchange for
       calling specialised versions of the code. Maybe an annotation would be
       needed. Dolan thinks there isn't a single good answer here and we should
       maybe just not do the join. (The code age relation meet would remain
       though as it's useful elsewhere.) *)
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
      match join env rec_info1 rec_info2 with
      | Known rec_info -> Ok (TG.Function_type.create code_id ~rec_info)
      | Unknown -> Unknown))

and join_env_extension env (ext1 : TEE.t) (ext2 : TEE.t) : TEE.t =
  let equations =
    Name.Map.merge
      (fun name ty1_opt ty2_opt ->
        match ty1_opt, ty2_opt with
        | None, _ | _, None -> None
        | Some ty1, Some ty2 -> (
          match join env ty1 ty2 with
          | Known ty ->
            if MTC.is_alias_of_name ty name
            then
              (* This is rare but not anomalous. It may mean that [ty1] and
                 [ty2] are both alias types which canonicalize to [name], for
                 instance. In any event, if the best type available for [name]
                 is [= name], we effectively know nothing, so we drop [name].
                 ([name = name] would be rejected by [TE.add_equation]
                 anyway.) *)
              None
            else (
              (* This should always pass due to the [is_alias_of_name] check. *)
              MTC.check_equation name ty;
              Some ty)
          | Unknown -> None))
      (TEE.to_map ext1) (TEE.to_map ext2)
  in
  TEE.from_map equations

(* Exposed to the outside world with Or_bottom type *)
let meet env ty1 ty2 : _ Or_bottom.t =
  match meet env ty1 ty2 with
  | Bottom -> Bottom
  | Ok (r, env_extension) ->
    Ok (extract_value r ty1 ty2, env_extension.extension)

let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
  let result = Bound_name.create_var result_var in
  let env = TE.add_definition env result result_kind in
  match meet (Meet_env.create env) t shape with
  | Bottom -> Bottom
  | Ok (_, env_extension) -> Ok env_extension

let meet_env_extension env ext1 ext2 : _ Or_bottom.t =
  match meet_env_extension env (no_memory ext1) (no_memory ext2) with
  | Bottom -> Bottom
  | Ok ext -> Ok ext.extension
