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

module Join_env = Typing_env.Join_env
module ET = Expand_head.Expanded_type
module K = Flambda_kind
module MTC = More_type_creators
module TG = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension
module Vec128 = Vector_types.Vec128.Bit_pattern
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

let add_equation (simple : Simple.t) ty_of_simple env ~meet_type : _ Or_bottom.t
    =
  match Simple.must_be_name simple with
  | Some (name, coercion_from_name_to_simple) ->
    let coercion_from_simple_to_name =
      Coercion.inverse coercion_from_name_to_simple
    in
    let ty_of_name =
      TG.apply_coercion ty_of_simple coercion_from_simple_to_name
    in
    TE.add_equation_strict env name ty_of_name ~meet_type:(TE.New meet_type)
  | None -> Ok env

type 'a meet_result =
  | Bottom
  | Ok of 'a meet_return_value * TE.t

let map_result ~f = function
  | Bottom -> Bottom
  | Ok (Left_input, env) -> Ok (Left_input, env)
  | Ok (Right_input, env) -> Ok (Right_input, env)
  | Ok (Both_inputs, env) -> Ok (Both_inputs, env)
  | Ok (New_result x, env) -> Ok (New_result (f x), env)

let map_env ~f = function
  | Bottom -> Bottom
  | Ok (r, env) -> (
    match (f env : _ Or_bottom.t) with Bottom -> Bottom | Ok env -> Ok (r, env))

let extract_value res left right =
  match res with
  | Left_input -> left
  | Right_input -> right
  | Both_inputs -> left
  | New_result value -> value

let set_meet (type a b) (module S : Container_types_intf.Set with type t = a)
    ~(of_set : a -> b) env (s1 : a) (s2 : a) : b meet_result =
  match S.subset s1 s2, S.subset s2 s1 with
  | true, true -> Ok (Both_inputs, env)
  | true, false -> Ok (Left_input, env)
  | false, true -> Ok (Right_input, env)
  | false, false ->
    let s = S.inter s1 s2 in
    if S.is_empty s then Bottom else Ok (New_result (of_set s), env)

type ('key, 'data, 'mapping) fold2 =
  { fold2 :
      'acc.
      ('key -> 'data option -> 'data option -> 'acc -> 'acc) ->
      'mapping ->
      'mapping ->
      'acc ->
      'acc
  }

let meet_mapping (type key data mapping)
    ~(meet_data : TE.t -> data -> data -> data meet_result)
    ~(fold2 : (key, data, mapping) fold2) ~env ~(left : mapping)
    ~(right : mapping) ~(rebuild : (key * data) list -> mapping) :
    mapping meet_result =
  let { fold2 } = fold2 in
  let open struct
    type t =
      { all_left : bool;
        all_right : bool;
        mapping : (key * data) list;
        env : TE.t
      }

    exception Bottom_result
  end in
  try
    let res =
      fold2
        (fun key data_left_opt data_right_opt
             { all_left; all_right; mapping; env } ->
          match data_left_opt, data_right_opt with
          | None, None -> assert false
          | Some data_left, None ->
            { all_left;
              all_right = false;
              mapping = (key, data_left) :: mapping;
              env
            }
          | None, Some data_right ->
            { all_left = false;
              all_right;
              mapping = (key, data_right) :: mapping;
              env
            }
          | Some data_left, Some data_right -> (
            match meet_data env data_left data_right with
            | Bottom -> raise Bottom_result
            | Ok (res, env) -> (
              let[@local] result ~all_left ~all_right data =
                { all_left; all_right; mapping = (key, data) :: mapping; env }
              in
              match res with
              | Both_inputs -> result ~all_left ~all_right data_left
              | Left_input -> result ~all_left ~all_right:false data_left
              | Right_input -> result ~all_left:false ~all_right data_right
              | New_result data -> result ~all_left:false ~all_right:false data)
            ))
        left right
        { all_left = true; all_right = true; mapping = []; env }
    in
    let result =
      match res.all_left, res.all_right with
      | true, true -> Both_inputs
      | true, false -> Left_input
      | false, true -> Right_input
      | false, false -> New_result (rebuild res.mapping)
    in
    Ok (result, res.env)
  with Bottom_result -> Bottom

module Map_meet (M : Container_types_intf.Map) = struct
  let meet ~(meet_data : TE.t -> 'a -> 'a -> 'a meet_result) env (left : 'a M.t)
      (right : 'a M.t) : 'a M.t meet_result =
    let fold2 f m1 m2 init =
      let r = ref init in
      let _m =
        M.merge
          (fun key left right ->
            r := f key left right !r;
            None)
          m1 m2
      in
      !r
    in
    let rebuild l =
      List.fold_left (fun m (key, data) -> M.add key data m) M.empty l
    in
    let fold2 = { fold2 } in
    meet_mapping ~meet_data ~fold2 ~env ~left ~right ~rebuild
end

module Function_slot_map_meet = Map_meet (Function_slot.Map)
module Value_slot_map_meet = Map_meet (Value_slot.Map)

type 'a pairwise_disjunction_meet_arg =
  { is_bottom : 'a -> bool;
    mk_bottom : unit -> 'a;
    meet : TE.t -> 'a -> 'a -> 'a meet_result
  }

(* Note: We want to avoid allocating the arguments to this function. Since we
   currently can't lift them (some limitations around coercions), we force
   inlining of this function, which should allow the compiler to track the
   individual fields and remove the allocation. Fortunately there is only one
   call site at the moment, so this shouldn't produce any code duplication. *)
let[@inline] pairwise_disjunction_meet ~join_env_extension ~meet_type meetx
    meety initial_env x1 x2 y1 y2 =
  let join_scope = TE.current_scope initial_env in
  let env = TE.increment_scope initial_env in
  let to_extension scoped_env =
    TE.cut scoped_env ~cut_after:join_scope
    |> Typing_env_level.as_extension_without_bindings
  in
  let direct_return r =
    map_env r ~f:(fun scoped_env ->
        TE.add_env_extension_strict initial_env (to_extension scoped_env)
          ~meet_type:(New meet_type))
  in
  match meetx.meet env x1 x2, meety.meet env y1 y2 with
  | Bottom, Bottom -> Bottom
  | Ok (xr, env), Bottom ->
    let x = extract_value xr x1 x2 in
    if meetx.is_bottom x
    then Bottom
    else
      let y = meety.mk_bottom () in
      direct_return (Ok (New_result (x, y), env))
  | Bottom, Ok (yr, env) ->
    let y = extract_value yr y1 y2 in
    if meety.is_bottom y
    then Bottom
    else
      let x = meetx.mk_bottom () in
      direct_return (Ok (New_result (x, y), env))
  | Ok (xr, xenv), Ok (yr, yenv) ->
    let x = extract_value xr x1 x2 in
    let y = extract_value yr y1 y2 in
    let bottom_x = meetx.is_bottom x in
    let bottom_y = meety.is_bottom y in
    let result_env _ : _ Or_bottom.t =
      let[@local] add_extension ext =
        TE.add_env_extension_strict initial_env ext ~meet_type:(New meet_type)
      in
      if bottom_x
      then if bottom_y then Bottom else add_extension (to_extension yenv)
      else if bottom_y
      then add_extension (to_extension xenv)
      else
        let x_extension = to_extension xenv in
        let y_extension = to_extension yenv in
        let join_env = Join_env.create env ~left_env:xenv ~right_env:yenv in
        let extension = join_env_extension join_env x_extension y_extension in
        add_extension extension
    in
    let result =
      match xr, yr with
      | Both_inputs, Both_inputs -> Both_inputs
      | (Left_input | Both_inputs), (Left_input | Both_inputs) -> Left_input
      | (Right_input | Both_inputs), (Right_input | Both_inputs) -> Right_input
      | Left_input, Right_input
      | Right_input, Left_input
      | New_result _, _
      | _, New_result _ ->
        New_result (x, y)
    in
    map_env ~f:result_env (Ok (result, initial_env (* ignored *)))

module Combine_results_meet_ops = struct
  type _ t =
    | [] : unit t
    | ( :: ) : ((TE.t -> 'a -> 'a -> 'a meet_result) * 'b t) -> ('a * 'b) t
end

module Combine_results_inputs = struct
  type _ t =
    | [] : unit t
    | ( :: ) : ('a * 'b t) -> ('a * 'b) t
end

let rec build_values : type a. a Combine_results_inputs.t -> a = function
  | input :: next -> input, build_values next
  | [] -> ()

let extract_values res left right =
  match res with
  | Left_input -> build_values left
  | Right_input -> build_values right
  | Both_inputs -> build_values left
  | New_result value -> value

let combine_results env ~(meet_ops : 'a Combine_results_meet_ops.t)
    ~(left_inputs : 'a Combine_results_inputs.t)
    ~(right_inputs : 'a Combine_results_inputs.t) ~(rebuild : 'a -> 'b) :
    'b meet_result =
  let rec do_meets :
      type a.
      TE.t ->
      a Combine_results_meet_ops.t ->
      a Combine_results_inputs.t ->
      a Combine_results_inputs.t ->
      a meet_result =
   fun env meet_ops left right : a meet_result ->
    match meet_ops, left, right with
    | [], [], [] -> Ok (Both_inputs, env)
    | meet :: next_meet, left_input :: next_left, right_input :: next_right -> (
      match meet env left_input right_input with
      | Bottom -> Bottom
      | Ok (result_hd, env) -> (
        match do_meets env next_meet next_left next_right with
        | Bottom -> Bottom
        | Ok (result_tl, env) -> (
          match result_hd, result_tl with
          | Both_inputs, Both_inputs -> Ok (Both_inputs, env)
          | (Left_input | Both_inputs), (Left_input | Both_inputs) ->
            Ok (Left_input, env)
          | (Right_input | Both_inputs), (Right_input | Both_inputs) ->
            Ok (Right_input, env)
          | New_result _, _
          | _, New_result _
          | Left_input, Right_input
          | Right_input, Left_input ->
            let result_hd = extract_value result_hd left_input right_input in
            let result_tl = extract_values result_tl next_left next_right in
            Ok (New_result (result_hd, result_tl), env))))
  in
  map_result ~f:rebuild (do_meets env meet_ops left_inputs right_inputs)

let combine_results2 env ~meet_a ~meet_b ~left_a ~right_a ~left_b ~right_b
    ~rebuild =
  combine_results env ~meet_ops:[meet_a; meet_b] ~left_inputs:[left_a; left_b]
    ~right_inputs:[right_a; right_b] ~rebuild:(fun (a, (b, ())) -> rebuild a b)

let meet_code_id (env : TE.t) (code_id1 : Code_id.t) (code_id2 : Code_id.t) :
    Code_id.t meet_result =
  if Code_id.equal code_id1 code_id2
  then Ok (Both_inputs, env)
  else
    match
      Code_age_relation.meet (TE.code_age_relation env)
        ~resolver:(TE.code_age_relation_resolver env)
        code_id1 code_id2
    with
    | Bottom -> Bottom
    | Ok code_id ->
      if Code_id.equal code_id code_id1
      then Ok (Left_input, env)
      else if Code_id.equal code_id code_id2
      then Ok (Right_input, env)
      else Ok (New_result code_id, env)

type meet_keep_side =
  | Left
  | Right

(* type meet_expanded_head_result =
 *   | Left_head_unchanged
 *   | Right_head_unchanged
 *   | New_head of ET.t * TEE.t *)

let meet_alloc_mode env (alloc_mode1 : Alloc_mode.For_types.t)
    (alloc_mode2 : Alloc_mode.For_types.t) : Alloc_mode.For_types.t meet_result
    =
  match alloc_mode1, alloc_mode2 with
  | (Heap_or_local | Local), (Heap_or_local | Local) | Heap, Heap ->
    Ok (Both_inputs, env)
  | (Heap_or_local | Local), _ -> Ok (Right_input, env)
  | _, (Heap_or_local | Local) -> Ok (Left_input, env)

let join_alloc_mode (alloc_mode1 : Alloc_mode.For_types.t)
    (alloc_mode2 : Alloc_mode.For_types.t) : Alloc_mode.For_types.t =
  match alloc_mode1, alloc_mode2 with
  | (Heap_or_local | Local), _ | _, (Heap_or_local | Local) ->
    Alloc_mode.For_types.unknown ()
  | Heap, Heap -> Alloc_mode.For_types.heap

let[@inline always] meet_unknown meet_contents ~contents_is_bottom env
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t) :
    _ Or_unknown.t meet_result =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> Ok (Both_inputs, env)
  | Known contents, _ when contents_is_bottom contents -> Bottom
  | _, Known contents when contents_is_bottom contents -> Bottom
  | _, Unknown -> Ok (Left_input, env)
  | Unknown, _ -> Ok (Right_input, env)
  | Known contents1, Known contents2 ->
    map_result ~f:Or_unknown.known (meet_contents env contents1 contents2)

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
  let simple1 =
    match
      TE.get_alias_then_canonical_simple_exn env t1
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let simple2 =
    match
      TE.get_alias_then_canonical_simple_exn env t2
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match simple1 with
  | None -> (
    let expanded1 =
      Expand_head.expand_head0 env t1
        ~known_canonical_simple_at_in_types_mode:simple1
    in
    let expanded2 =
      Expand_head.expand_head0 env t2
        ~known_canonical_simple_at_in_types_mode:simple2
    in
    match simple2 with
    | None ->
      map_result ~f:ET.to_type (meet_expanded_head env expanded1 expanded2)
    | Some simple2 -> (
      (* Here we are meeting a non-alias type on the left with an alias on the
         right. In all cases, the return type is the alias, so we will always
         return [Right_input]; the interesting part will be the environment. *)
      let env : TE.t Or_bottom.t =
        match meet_expanded_head env expanded1 expanded2 with
        | Ok (Left_input, env) ->
          add_equation simple2 (ET.to_type expanded1) env ~meet_type
        | Ok ((Right_input | Both_inputs), env) -> Ok env
        | Ok (New_result expanded, env) ->
          add_equation simple2 (ET.to_type expanded) env ~meet_type
        | Bottom -> Bottom
      in
      match env with Ok env -> Ok (Right_input, env) | Bottom -> Bottom))
  | Some simple1 as simple1_opt -> (
    match simple2 with
    | None -> (
      let expanded1 =
        Expand_head.expand_head0 env t1
          ~known_canonical_simple_at_in_types_mode:simple1_opt
      in
      let expanded2 =
        Expand_head.expand_head0 env t2
          ~known_canonical_simple_at_in_types_mode:simple2
      in
      (* We always return [Left_input] (see comment above) *)
      let env : TE.t Or_bottom.t =
        match meet_expanded_head env expanded1 expanded2 with
        | Ok (Right_input, env) ->
          add_equation simple1 (ET.to_type expanded2) env ~meet_type
        | Ok ((Left_input | Both_inputs), env) -> Ok env
        | Ok (New_result expanded, env) ->
          add_equation simple1 (ET.to_type expanded) env ~meet_type
        | Bottom -> Bottom
      in
      match env with Ok env -> Ok (Left_input, env) | Bottom -> Bottom)
    | Some simple2 -> (
      if (* We are doing a meet between two alias types. Whatever happens, the
            resulting environment will contain an alias equation between the two
            inputs, so both the left-hand alias and the right-hand alias are
            correct results for the meet, allowing us to return [Both_inputs] in
            all cases. *)
         Simple.equal simple1 simple2
      then
        (* The alias is already present; no need to add any equation here *)
        Ok (Both_inputs, env)
      else
        let env =
          Simple.pattern_match simple2
            ~name:(fun _ ~coercion:_ ->
              add_equation simple2
                (TG.alias_type_of kind simple1)
                env ~meet_type)
            ~const:(fun const2 ->
              Simple.pattern_match simple1
                ~name:(fun _ ~coercion:_ ->
                  add_equation simple1
                    (TG.alias_type_of kind simple2)
                    env ~meet_type)
                ~const:(fun const1 : _ Or_bottom.t ->
                  if Reg_width_const.equal const1 const2 then Ok env else Bottom))
        in
        (* [add_equation] will have called [meet] on the underlying types, so
           [env] now contains all extra equations arising from meeting the
           expanded heads. *)
        match env with Ok env -> Ok (Both_inputs, env) | Bottom -> Bottom))

and meet_expanded_head env (expanded1 : ET.t) (expanded2 : ET.t) :
    ET.t meet_result =
  match ET.descr expanded1, ET.descr expanded2 with
  | Unknown, Unknown -> Ok (Both_inputs, env)
  | _, Unknown -> Ok (Left_input, env)
  | Unknown, _ -> Ok (Right_input, env)
  (* CR vlaviron: in the next two cases, returning [Bottom] is correct but
     inconsistent with the rest of the file: since the bottom type was already
     present, we should return [Left_input] or [Right_input]. *)
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
  | Naked_float32 head1, Naked_float32 head2 ->
    map_result ~f:ET.create_naked_float32
      (meet_head_of_kind_naked_float32 env head1 head2)
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
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
      | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _
      | Rec_info _ | Region _ ),
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
    | Unknown -> Ok (Right_input, env)
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
      | Ok ((Both_inputs | Right_input), env) -> Ok (Right_input, env)
      | Ok (Left_input, env) ->
        Ok
          ( New_result
              (TG.Head_of_kind_value.create_mutable_block alloc_mode_left),
            env )
      | Ok (New_result alloc_mode, env) ->
        Ok
          ( New_result (TG.Head_of_kind_value.create_mutable_block alloc_mode),
            env )))
  | Mutable_block { alloc_mode = alloc_mode_left }, Variant { blocks; _ } -> (
    match blocks with
    | Unknown -> Ok (Left_input, env)
    | Known { alloc_mode = alloc_mode_right; _ } -> (
      (* CR vlaviron: see symmetric case above *)
      match meet_alloc_mode env alloc_mode_left alloc_mode_right with
      | Bottom -> Bottom
      | Ok ((Both_inputs | Left_input), env) -> Ok (Left_input, env)
      | Ok (Right_input, env) ->
        Ok
          ( New_result
              (TG.Head_of_kind_value.create_mutable_block alloc_mode_right),
            env )
      | Ok (New_result alloc_mode, env) ->
        Ok
          ( New_result (TG.Head_of_kind_value.create_mutable_block alloc_mode),
            env )))
  | Boxed_float32 (n1, alloc_mode1), Boxed_float32 (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_float32
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | Boxed_float (n1, alloc_mode1), Boxed_float (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_float
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | Boxed_int32 (n1, alloc_mode1), Boxed_int32 (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_int32
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | Boxed_int64 (n1, alloc_mode1), Boxed_int64 (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_int64
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | Boxed_nativeint (n1, alloc_mode1), Boxed_nativeint (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_nativeint
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | Boxed_vec128 (n1, alloc_mode1), Boxed_vec128 (n2, alloc_mode2) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_boxed_vec128
      ~meet_a:meet ~meet_b:meet_alloc_mode ~left_a:n1 ~right_a:n2
      ~left_b:alloc_mode1 ~right_b:alloc_mode2
  | ( Closures { by_function_slot = by_function_slot1; alloc_mode = alloc_mode1 },
      Closures
        { by_function_slot = by_function_slot2; alloc_mode = alloc_mode2 } ) ->
    combine_results2 env ~rebuild:TG.Head_of_kind_value.create_closures
      ~meet_a:meet_row_like_for_closures ~meet_b:meet_alloc_mode
      ~left_a:by_function_slot1 ~right_a:by_function_slot2 ~left_b:alloc_mode1
      ~right_b:alloc_mode2
  | String strs1, String strs2 ->
    map_result ~f:TG.Head_of_kind_value.create_string
      (set_meet (module String_info.Set) env strs1 strs2 ~of_set:Fun.id)
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
  | ( ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ),
      _ ) ->
    (* This assumes that all the different constructors are incompatible. This
       could break very hard for dubious uses of Obj. *)
    Bottom

and meet_array_type env (element_kind1, length1, contents1, alloc_mode1)
    (element_kind2, length2, contents2, alloc_mode2) =
  let element_kind = meet_array_element_kinds element_kind1 element_kind2 in
  combine_results env
    ~rebuild:(fun (length, (contents, (alloc_mode, ()))) ->
      TG.Head_of_kind_value.create_array_with_contents ~element_kind ~length
        contents alloc_mode)
    ~meet_ops:
      [ meet;
        meet_array_contents ~meet_element_kind:element_kind;
        meet_alloc_mode ]
    ~left_inputs:[length1; contents1; alloc_mode1]
    ~right_inputs:[length2; contents2; alloc_mode2]

and meet_array_contents env (array_contents1 : TG.array_contents Or_unknown.t)
    (array_contents2 : TG.array_contents Or_unknown.t)
    ~(meet_element_kind : _ Or_unknown_or_bottom.t) =
  meet_unknown
    (fun env (array_contents1 : TG.array_contents)
         (array_contents2 : TG.array_contents) : TG.array_contents meet_result ->
      match array_contents1, array_contents2 with
      | Mutable, Mutable -> Ok (Both_inputs, env)
      | Mutable, Immutable _ | Immutable _, Mutable -> Bottom
      | Immutable { fields = fields1 }, Immutable { fields = fields2 } -> (
        if Array.length fields1 <> Array.length fields2
        then Bottom
        else
          match meet_element_kind with
          | Bottom -> Bottom
          | Unknown ->
            (* vlaviron: If the meet of the kinds is Unknown, then both inputs
               had Unknown kinds. I don't see how we could end up with an array
               type where the contents are known but we don't know the kind, but
               in that case we wouldn't be able to call meet because the two
               sides may have different kinds. So we'll just return the first
               input, which is guaranteed to be a correct approximation of the
               meet. *)
            Ok (Left_input, env)
          | Ok _ ->
            map_result
              ~f:(fun fields : TG.array_contents -> Immutable { fields })
              (meet_array_of_types env fields1 fields2
                 ~length:(Array.length fields1))))
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
  pairwise_disjunction_meet ~join_env_extension ~meet_type blocks_meet
    immediates_meet env blocks1 blocks2 imms1 imms2

and meet_head_of_kind_naked_immediate env (t1 : TG.head_of_kind_naked_immediate)
    (t2 : TG.head_of_kind_naked_immediate) :
    TG.head_of_kind_naked_immediate meet_result =
  let module I = Targetint_31_63 in
  let keep_side side : _ meet_result =
    match side with
    | Left -> Ok (Left_input, env)
    | Right -> Ok (Right_input, env)
  in
  let keep_other_side side : _ meet_result =
    match side with
    | Left -> Ok (Right_input, env)
    | Right -> Ok (Left_input, env)
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
      (set_meet (module I.Set) env is1 is2 ~of_set:Fun.id)
  | Is_int is_int_ty, Naked_immediates immediates ->
    is_int_immediate ~is_int_ty ~immediates ~is_int_side:Left
  | Naked_immediates immediates, Is_int is_int_ty ->
    is_int_immediate ~is_int_ty ~immediates ~is_int_side:Right
  | Get_tag get_tag_ty, Naked_immediates immediates ->
    get_tag_immediate ~get_tag_ty ~immediates ~get_tag_side:Left
  | Naked_immediates immediates, Get_tag get_tag_ty ->
    get_tag_immediate ~get_tag_ty ~immediates ~get_tag_side:Right
  | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
    (* CR mshinwell: introduce improved handling for
     *   Is_int meet Is_int
     *   Get_tag meet Get_tag
     * i.e. a better fix for PR1515, at which point we might also be able
     * to consider improving:
     *   Is_int meet Get_tag
     * and vice-versa. *)
    (* We can't return Bottom, as it would be unsound, so we need to either do
       the actual meet with Naked_immediates, or just give up and return one of
       the arguments. *)
    Ok (Left_input, env)

and meet_head_of_kind_naked_float32 env t1 t2 =
  set_meet
    (module Numeric_types.Float32_by_bit_pattern.Set)
    env
    (t1
      : TG.head_of_kind_naked_float32
      :> Numeric_types.Float32_by_bit_pattern.Set.t)
    (t2
      : TG.head_of_kind_naked_float32
      :> Numeric_types.Float32_by_bit_pattern.Set.t)
    ~of_set:TG.Head_of_kind_naked_float32.create_non_empty_set

and meet_head_of_kind_naked_float env t1 t2 =
  set_meet
    (module Numeric_types.Float_by_bit_pattern.Set)
    env
    (t1
      : TG.head_of_kind_naked_float
      :> Numeric_types.Float_by_bit_pattern.Set.t)
    (t2
      : TG.head_of_kind_naked_float
      :> Numeric_types.Float_by_bit_pattern.Set.t)
    ~of_set:TG.Head_of_kind_naked_float.create_non_empty_set

and meet_head_of_kind_naked_int32 env t1 t2 =
  set_meet
    (module Numeric_types.Int32.Set)
    env
    (t1 : TG.head_of_kind_naked_int32 :> Numeric_types.Int32.Set.t)
    (t2 : TG.head_of_kind_naked_int32 :> Numeric_types.Int32.Set.t)
    ~of_set:TG.Head_of_kind_naked_int32.create_non_empty_set

and meet_head_of_kind_naked_int64 env t1 t2 =
  set_meet
    (module Numeric_types.Int64.Set)
    env
    (t1 : TG.head_of_kind_naked_int64 :> Numeric_types.Int64.Set.t)
    (t2 : TG.head_of_kind_naked_int64 :> Numeric_types.Int64.Set.t)
    ~of_set:TG.Head_of_kind_naked_int64.create_non_empty_set

and meet_head_of_kind_naked_nativeint env t1 t2 =
  set_meet
    (module Targetint_32_64.Set)
    env
    (t1 : TG.head_of_kind_naked_nativeint :> Targetint_32_64.Set.t)
    (t2 : TG.head_of_kind_naked_nativeint :> Targetint_32_64.Set.t)
    ~of_set:TG.Head_of_kind_naked_nativeint.create_non_empty_set

and meet_head_of_kind_naked_vec128 env t1 t2 =
  set_meet
    (module Vec128.Set)
    env
    (t1 : TG.head_of_kind_naked_vec128 :> Vec128.Set.t)
    (t2 : TG.head_of_kind_naked_vec128 :> Vec128.Set.t)
    ~of_set:TG.Head_of_kind_naked_vec128.create_non_empty_set

and meet_head_of_kind_rec_info env _t1 _t2 =
  (* CR-someday lmaurer: This could be doing things like discovering two depth
     variables are equal *)
  (* CR vlaviron: This looks awfully wrong. I think we'll need to remove it at
     some point; it is only reachable from function types, and we should already
     have any relevant coercion from closure types. *)
  Ok (Both_inputs, env)

and meet_head_of_kind_region env () () : _ meet_result = Ok (Both_inputs, env)

and meet_row_like :
      'lattice 'shape 'maps_to 'row_tag 'known.
      meet_maps_to:(TE.t -> 'maps_to -> 'maps_to -> 'maps_to meet_result) ->
      equal_index:('lattice -> 'lattice -> bool) ->
      subset_index:('lattice -> 'lattice -> bool) ->
      union_index:('lattice -> 'lattice -> 'lattice) ->
      meet_shape:('shape -> 'shape -> 'shape Or_bottom.t) ->
      is_empty_map_known:('known -> bool) ->
      get_singleton_map_known:
        ('known ->
        ('row_tag * ('lattice, 'shape, 'maps_to) TG.Row_like_case.t) option) ->
      merge_map_known:
        (('row_tag ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option) ->
        'known ->
        'known ->
        'known) ->
      TE.t ->
      known1:'known ->
      known2:'known ->
      other1:('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      other2:('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      ('known * ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t)
      meet_result =
 fun ~meet_maps_to ~equal_index ~subset_index ~union_index ~meet_shape
     ~is_empty_map_known ~get_singleton_map_known ~merge_map_known initial_env
     ~known1 ~known2 ~other1 ~other2 ->
  let common_scope = TE.current_scope initial_env in
  let base_env = TE.increment_scope initial_env in
  let extract_extension scoped_env =
    TE.cut_as_extension scoped_env ~cut_after:common_scope
  in
  let open struct
    type result_env =
      | No_result
      | Extension of TEE.t
  end in
  let result_env = ref No_result in
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
  let result_is_t1 = ref true in
  let result_is_t2 = ref true in
  let join_result_env scoped_env =
    let new_result_env =
      match !result_env with
      | No_result -> Extension (extract_extension scoped_env)
      | Extension ext1 ->
        assert need_join;
        let ext2 = extract_extension scoped_env in
        let join_env =
          Join_env.create base_env ~left_env:base_env ~right_env:scoped_env
        in
        let extension = join_env_extension join_env ext1 ext2 in
        Extension extension
    in
    result_env := new_result_env
  in
  let meet_index env (i1 : ('lattice, 'shape) TG.row_like_index)
      (i2 : ('lattice, 'shape) TG.row_like_index) :
      ('lattice, 'shape) TG.row_like_index meet_result =
    match meet_shape i1.shape i2.shape with
    | Bottom -> Bottom
    | Ok shape -> (
      match i1.domain, i2.domain with
      | Known i1', Known i2' ->
        if equal_index i1' i2' then Ok (Both_inputs, env) else Bottom
      | Known known, At_least at_least ->
        if subset_index at_least known
        then
          (* [at_least] is included in [known] hence [Known known] is included
             in [At_least at_least], hence [Known known] \inter [At_least
             at_least] = [Known known] *)
          Ok (Left_input, env)
        else Bottom
      | At_least at_least, Known known ->
        if subset_index at_least known then Ok (Right_input, env) else Bottom
      | At_least i1', At_least i2' ->
        if subset_index i1' i2'
        then
          if subset_index i2' i1'
          then Ok (Both_inputs, env)
          else Ok (Right_input, env)
        else if subset_index i2' i1'
        then Ok (Left_input, env)
        else
          let domain =
            TG.Row_like_index_domain.at_least (union_index i1' i2')
          in
          Ok (New_result (TG.Row_like_index.create ~domain ~shape), env))
  in
  let bottom_case () =
    result_is_t1 := false;
    result_is_t2 := false;
    None
  in
  let meet_case env (case1 : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t)
      (case2 : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t) =
    match meet_index env case1.index case2.index with
    | Bottom -> bottom_case ()
    | Ok (index_result, env) -> (
      match meet_maps_to env case1.maps_to case2.maps_to with
      | Bottom -> bottom_case ()
      | Ok (maps_to_result, env) -> (
        let env : _ Or_bottom.t =
          match
            TE.add_env_extension_strict env case1.env_extension
              ~meet_type:(New meet_type)
          with
          | Bottom -> Bottom
          | Ok env ->
            TE.add_env_extension_strict env case2.env_extension
              ~meet_type:(New meet_type)
        in
        match env with
        | Bottom -> bottom_case ()
        | Ok env ->
          join_result_env env;
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
            if need_join then extract_extension env else TEE.empty
          in
          if TEE.is_empty env_extension
          then ()
          else (
            result_is_t1 := false;
            result_is_t2 := false);
          Some
            (Or_unknown.Known
               (TG.Row_like_case.create ~maps_to ~index ~env_extension))))
  in
  let meet_knowns case1 case2 :
      ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option =
    match
      ( (case1
          : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option),
        (case2
          : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option)
      )
    with
    | None, None -> None
    | Some case1, None -> (
      match other2 with
      | Bottom ->
        result_is_t1 := false;
        None
      | Ok other_case -> (
        match case1 with
        | Unknown -> (
          match
            TE.add_env_extension_strict base_env other_case.env_extension
              ~meet_type:(New meet_type)
          with
          | Bottom -> None
          | Ok env ->
            join_result_env env;
            result_is_t1 := false;
            result_is_t2 := false;
            Some (Known other_case))
        | Known case1 -> meet_case base_env case1 other_case))
    | None, Some case2 -> (
      match other1 with
      | Bottom ->
        result_is_t2 := false;
        None
      | Ok other_case -> (
        match case2 with
        | Unknown -> (
          match
            TE.add_env_extension_strict base_env other_case.env_extension
              ~meet_type:(New meet_type)
          with
          | Bottom -> None
          | Ok env ->
            join_result_env env;
            result_is_t1 := false;
            result_is_t2 := false;
            Some (Known other_case))
        | Known case2 -> meet_case base_env other_case case2))
    | Some case1, Some case2 -> (
      match case1, case2 with
      | Unknown, Unknown ->
        join_result_env base_env;
        Some Unknown
      | Known case, Unknown -> (
        match
          TE.add_env_extension_strict base_env case.env_extension
            ~meet_type:(New meet_type)
        with
        | Bottom -> None
        | Ok env ->
          join_result_env env;
          result_is_t2 := false;
          Some (Known case))
      | Unknown, Known case -> (
        match
          TE.add_env_extension_strict base_env case.env_extension
            ~meet_type:(New meet_type)
        with
        | Bottom -> None
        | Ok env ->
          join_result_env env;
          result_is_t1 := false;
          Some (Known case))
      | Known case1, Known case2 -> meet_case base_env case1 case2)
  in
  let known =
    merge_map_known
      (fun _tag case1 case2 -> meet_knowns case1 case2)
      known1 known2
  in
  let other : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t =
    match other1, other2 with
    | Bottom, Bottom -> Bottom
    | Bottom, _ ->
      result_is_t2 := false;
      Bottom
    | _, Bottom ->
      result_is_t1 := false;
      Bottom
    | Ok other1, Ok other2 -> (
      match meet_case base_env other1 other2 with
      | None -> Bottom
      | Some Unknown -> Misc.fatal_error "meet_case should not produce Unknown"
      | Some (Known r) -> Ok r)
  in
  if is_empty_map_known known
     && match other with Bottom -> true | Ok _ -> false
  then Bottom
  else
    let env : _ Or_bottom.t =
      match !result_env with
      | No_result -> Bottom
      | Extension ext ->
        TE.add_env_extension_strict initial_env ext ~meet_type:(New meet_type)
    in
    match env, !result_is_t1, !result_is_t2 with
    | Bottom, _, _ -> Bottom
    | Ok env, true, true -> Ok (Both_inputs, env)
    | Ok env, true, false -> Ok (Left_input, env)
    | Ok env, false, true -> Ok (Right_input, env)
    | Ok env, false, false -> Ok (New_result (known, other), env)

and meet_row_like_for_blocks env
    ({ known_tags = known1; other_tags = other1; alloc_mode = alloc_mode1 } :
      TG.Row_like_for_blocks.t)
    ({ known_tags = known2; other_tags = other2; alloc_mode = alloc_mode2 } :
      TG.Row_like_for_blocks.t) : TG.Row_like_for_blocks.t meet_result =
  let meet_shape shape1 shape2 : _ Or_bottom.t =
    if K.Block_shape.equal shape1 shape2 then Ok shape1 else Bottom
  in
  let get_singleton_map_known known =
    match (Tag.Map.get_singleton known : (_ * _ Or_unknown.t) option) with
    | Some (tag, Known case) -> Some (tag, case)
    | Some (_, Unknown) | None -> None
  in
  combine_results2 env
    ~rebuild:(fun (known_tags, other_tags) alloc_mode ->
      TG.Row_like_for_blocks.create_raw ~known_tags ~other_tags ~alloc_mode)
    ~meet_a:(fun env (known1, other1) (known2, other2) ->
      meet_row_like ~meet_maps_to:meet_int_indexed_product
        ~equal_index:TG.Block_size.equal ~subset_index:TG.Block_size.subset
        ~union_index:TG.Block_size.union ~meet_shape
        ~is_empty_map_known:Tag.Map.is_empty ~get_singleton_map_known
        ~merge_map_known:Tag.Map.merge env ~known1 ~known2 ~other1 ~other2)
    ~meet_b:meet_alloc_mode ~left_a:(known1, other1) ~right_a:(known2, other2)
    ~left_b:alloc_mode1 ~right_b:alloc_mode2

and meet_row_like_for_closures env
    ({ known_closures = known1; other_closures = other1 } :
      TG.Row_like_for_closures.t)
    ({ known_closures = known2; other_closures = other2 } :
      TG.Row_like_for_closures.t) : TG.Row_like_for_closures.t meet_result =
  let meet_shape () () : _ Or_bottom.t = Ok () in
  let merge_map_known merge_case known1 known2 =
    Function_slot.Map.merge
      (fun fslot case1 case2 ->
        let case1 = Option.map Or_unknown.known case1 in
        let case2 = Option.map Or_unknown.known case2 in
        match merge_case fslot case1 case2 with
        | None -> None
        | Some (Or_unknown.Known case) -> Some case
        | Some Or_unknown.Unknown ->
          Misc.fatal_error "Unknown case in closure meet")
      known1 known2
  in
  map_result
    ~f:(fun (known_closures, other_closures) ->
      TG.Row_like_for_closures.create_raw ~known_closures ~other_closures)
    (meet_row_like ~meet_maps_to:meet_closures_entry
       ~equal_index:Set_of_closures_contents.equal
       ~subset_index:Set_of_closures_contents.subset
       ~union_index:Set_of_closures_contents.union ~meet_shape
       ~is_empty_map_known:Function_slot.Map.is_empty
       ~get_singleton_map_known:Function_slot.Map.get_singleton ~merge_map_known
       env ~known1 ~known2 ~other1 ~other2)

and meet_closures_entry (env : TE.t)
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
  combine_results env
    ~meet_ops:
      [ Function_slot_map_meet.meet ~meet_data:meet_function_type;
        meet_product_function_slot_indexed;
        meet_product_value_slot_indexed ]
    ~left_inputs:[function_types1; closure_types1; value_slot_types1]
    ~right_inputs:[function_types2; closure_types2; value_slot_types2]
    ~rebuild:(fun (function_types, (closure_types, (value_slot_types, ()))) ->
      TG.Closures_entry.create ~function_types ~closure_types ~value_slot_types)

and meet_product_function_slot_indexed env
    ({ function_slot_components_by_index = components_by_index1 } :
      TG.Product.Function_slot_indexed.t)
    ({ function_slot_components_by_index = components_by_index2 } :
      TG.Product.Function_slot_indexed.t) :
    TG.Product.Function_slot_indexed.t meet_result =
  map_result ~f:TG.Product.Function_slot_indexed.create
    (Function_slot_map_meet.meet ~meet_data:meet env components_by_index1
       components_by_index2)

and meet_product_value_slot_indexed env
    ({ value_slot_components_by_index = components_by_index1 } :
      TG.Product.Value_slot_indexed.t)
    ({ value_slot_components_by_index = components_by_index2 } :
      TG.Product.Value_slot_indexed.t) :
    TG.Product.Value_slot_indexed.t meet_result =
  map_result ~f:TG.Product.Value_slot_indexed.create
    (Value_slot_map_meet.meet ~meet_data:meet env components_by_index1
       components_by_index2)

and meet_int_indexed_product env (fields1 : TG.Product.Int_indexed.t)
    (fields2 : TG.Product.Int_indexed.t) : _ meet_result =
  let length = max (Array.length fields1) (Array.length fields2) in
  map_result ~f:TG.Product.Int_indexed.create_from_array
    (meet_array_of_types env fields1 fields2 ~length)

and meet_array_of_types env fields1 fields2 ~length =
  let fold2 f left right init =
    let r = ref init in
    for i = 0 to length - 1 do
      let left_data = if i >= Array.length left then None else Some left.(i) in
      let right_data =
        if i >= Array.length right then None else Some right.(i)
      in
      r := f i left_data right_data !r
    done;
    !r
  in
  let rebuild l =
    match l with
    | [] -> [||]
    | (_key, data) :: _ ->
      let result = Array.make length data in
      List.iter (fun (key, data) -> result.(key) <- data) l;
      result
  in
  let fold2 = { fold2 } in
  meet_mapping ~meet_data:meet ~fold2 ~env ~left:fields1 ~right:fields2 ~rebuild

and meet_function_type (env : TE.t)
    (func_type1 : TG.Function_type.t Or_unknown_or_bottom.t)
    (func_type2 : TG.Function_type.t Or_unknown_or_bottom.t) :
    TG.Function_type.t Or_unknown_or_bottom.t meet_result =
  match func_type1, func_type2 with
  | Bottom, Bottom | Unknown, Unknown -> Ok (Both_inputs, env)
  | Bottom, _ | _, Unknown -> Ok (Left_input, env)
  | _, Bottom | Unknown, _ -> Ok (Right_input, env)
  | ( Ok { code_id = code_id1; rec_info = rec_info1 },
      Ok { code_id = code_id2; rec_info = rec_info2 } ) ->
    let rebuild code_id rec_info =
      (* It's possible that [code_id] corresponds to deleted code. In that case,
         any attempt to inline will fail, as the code will not be found in the
         simplifier's environment -- see
         [Simplify_apply_expr.simplify_direct_function_call]. *)
      Or_unknown_or_bottom.Ok (TG.Function_type.create code_id ~rec_info)
    in
    combine_results2 env ~rebuild ~meet_a:meet_code_id ~left_a:code_id1
      ~right_a:code_id2 ~meet_b:meet ~left_b:rec_info1 ~right_b:rec_info2

and meet_type env t1 t2 : _ Or_bottom.t =
  if TE.is_bottom env
  then Bottom
  else
    match meet env t1 t2 with
    | Ok (res, env) -> Ok (extract_value res t1 t2, env)
    | Bottom -> Bottom

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
      | Naked_float32 head1, Naked_float32 head2 ->
        let>+ head = join_head_of_kind_naked_float32 env head1 head2 in
        ET.create_naked_float32 head
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
      | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
          | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _
          | Rec_info _ | Region _ ),
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
  | Boxed_float32 (n1, alloc_mode1), Boxed_float32 (n2, alloc_mode2) ->
    let>+ n = join env n1 n2 in
    let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
    TG.Head_of_kind_value.create_boxed_float32 n alloc_mode
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
  | ( ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ),
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
          | Ok _ -> (
            let exception Unknown_result in
            try
              let fields =
                Array.init (Array.length fields1) (fun idx ->
                    match join env fields1.(idx) fields2.(idx) with
                    | Unknown -> raise Unknown_result
                    | Known ty -> ty)
              in
              Known (TG.Immutable { fields })
            with Unknown_result -> Unknown)))
    env array_contents1 array_contents2

and join_variant env ~(blocks1 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms1 : TG.t Or_unknown.t)
    ~(blocks2 : TG.Row_like_for_blocks.t Or_unknown.t)
    ~(imms2 : TG.t Or_unknown.t) :
    (TG.Row_like_for_blocks.t Or_unknown.t * TG.t Or_unknown.t) Or_unknown.t =
  let blocks = join_unknown join_row_like_for_blocks env blocks1 blocks2 in
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

and join_head_of_kind_naked_float32 _env t1 t2 : _ Or_unknown.t =
  Known (TG.Head_of_kind_naked_float32.union t1 t2)

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
      'lattice 'shape 'maps_to 'row_tag 'known.
      join_maps_to:(Join_env.t -> 'shape -> 'maps_to -> 'maps_to -> 'maps_to) ->
      equal_index:('lattice -> 'lattice -> bool) ->
      inter_index:('lattice -> 'lattice -> 'lattice) ->
      join_shape:('shape -> 'shape -> 'shape Or_unknown.t) ->
      merge_map_known:
        (('row_tag ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option ->
         ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option) ->
        'known ->
        'known ->
        'known) ->
      Join_env.t ->
      known1:'known ->
      known2:'known ->
      other1:('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      other2:('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t ->
      ('known * ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t)
      Or_unknown.t =
 fun ~join_maps_to ~equal_index ~inter_index ~join_shape ~merge_map_known
     join_env ~known1 ~known2 ~other1 ~other2 ->
  let join_index (i1 : ('lattice, 'shape) TG.row_like_index)
      (i2 : ('lattice, 'shape) TG.row_like_index) :
      ('lattice, 'shape) TG.row_like_index Or_unknown.t =
    match join_shape i1.shape i2.shape with
    | Unknown -> Unknown
    | Known shape -> (
      let return_index domain =
        Or_unknown.Known (TG.Row_like_index.create ~domain ~shape)
      in
      match i1.domain, i2.domain with
      | Known i1', Known i2' ->
        if equal_index i1' i2'
        then return_index i1.domain
        else
          (* We can't represent exactly the union, This is the best
             approximation *)
          return_index (TG.Row_like_index_domain.at_least (inter_index i1' i2'))
      | Known i1', At_least i2'
      | At_least i1', Known i2'
      | At_least i1', At_least i2' ->
        return_index (TG.Row_like_index_domain.at_least (inter_index i1' i2')))
  in
  let join_case join_env
      (case1 : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t)
      (case2 : ('lattice, 'shape, 'maps_to) TG.Row_like_case.t) : _ Or_unknown.t
      =
    let index = join_index case1.index case2.index in
    Or_unknown.map index
      ~f:(fun (index : ('lattice, 'shape) TG.Row_like_index.t) ->
        let maps_to =
          join_maps_to join_env index.shape case1.maps_to case2.maps_to
        in
        let env_extension =
          join_env_extension join_env case1.env_extension case2.env_extension
        in
        TG.Row_like_case.create ~maps_to ~index ~env_extension)
  in
  let join_knowns
      (case1 :
        ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option)
      (case2 :
        ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option) :
      ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_unknown.t option =
    match case1, case2 with
    | None, None -> None
    | Some Unknown, _ | _, Some Unknown -> Some Unknown
    | Some (Known case1), None -> (
      let only_case1 () =
        (* cf. Type_descr.join_head_or_unknown_or_bottom, we need to join these
           to ensure that free variables not present in the target env are
           cleaned out of the types. Same below *)
        (* CR pchambart: This seems terribly inefficient. *)
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
      | Ok other_case -> Some (join_case join_env case1 other_case))
    | None, Some (Known case2) -> (
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
      | Ok other_case -> Some (join_case join_env other_case case2))
    | Some (Known case1), Some (Known case2) ->
      Some (join_case join_env case1 case2)
  in
  let known =
    merge_map_known
      (fun _tag case1 case2 -> join_knowns case1 case2)
      known1 known2
  in
  let other :
      ('lattice, 'shape, 'maps_to) TG.Row_like_case.t Or_bottom.t Or_unknown.t =
    match other1, other2 with
    | Bottom, Bottom -> Known Bottom
    | Ok other1, Bottom ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env join_env)
          ~left_env:(Join_env.left_join_env join_env)
          ~right_env:(Join_env.left_join_env join_env)
      in
      let other1 = join_case env other1 other1 in
      Or_unknown.map other1 ~f:(fun other1 -> Or_bottom.Ok other1)
    | Bottom, Ok other2 ->
      (* See the previous cases *)
      let env =
        Join_env.create
          (Join_env.target_join_env join_env)
          ~left_env:(Join_env.right_join_env join_env)
          ~right_env:(Join_env.right_join_env join_env)
      in
      let other2 = join_case env other2 other2 in
      Or_unknown.map other2 ~f:(fun other2 -> Or_bottom.Ok other2)
    | Ok other1, Ok other2 ->
      Or_unknown.map (join_case join_env other1 other2) ~f:(fun case ->
          Or_bottom.Ok case)
  in
  Or_unknown.map other ~f:(fun other -> known, other)

and join_row_like_for_blocks env
    ({ known_tags = known1; other_tags = other1; alloc_mode = alloc_mode1 } :
      TG.Row_like_for_blocks.t)
    ({ known_tags = known2; other_tags = other2; alloc_mode = alloc_mode2 } :
      TG.Row_like_for_blocks.t) =
  let join_shape shape1 shape2 : _ Or_unknown.t =
    if K.Block_shape.equal shape1 shape2 then Known shape1 else Unknown
  in
  Or_unknown.map
    (join_row_like ~join_maps_to:join_int_indexed_product
       ~equal_index:TG.Block_size.equal ~inter_index:TG.Block_size.inter
       ~join_shape ~merge_map_known:Tag.Map.merge env ~known1 ~known2 ~other1
       ~other2) ~f:(fun (known_tags, other_tags) ->
      let alloc_mode = join_alloc_mode alloc_mode1 alloc_mode2 in
      TG.Row_like_for_blocks.create_raw ~known_tags ~other_tags ~alloc_mode)

and join_row_like_for_closures env
    ({ known_closures = known1; other_closures = other1 } :
      TG.Row_like_for_closures.t)
    ({ known_closures = known2; other_closures = other2 } :
      TG.Row_like_for_closures.t) : TG.Row_like_for_closures.t =
  let merge_map_known join_case known1 known2 =
    Function_slot.Map.merge
      (fun function_slot case1 case2 ->
        let case1 = Option.map Or_unknown.known case1 in
        let case2 = Option.map Or_unknown.known case2 in
        match (join_case function_slot case1 case2 : _ Or_unknown.t option) with
        | None -> None
        | Some (Known case) -> Some case
        | Some Unknown ->
          Misc.fatal_error "Join row_like case for closures returned Unknown")
      known1 known2
  in
  match
    join_row_like
      ~join_maps_to:(fun env () x y -> join_closures_entry env x y)
      ~equal_index:Set_of_closures_contents.equal
      ~inter_index:Set_of_closures_contents.inter
      ~join_shape:(fun () () -> Or_unknown.Known ())
      ~merge_map_known env ~known1 ~known2 ~other1 ~other2
  with
  | Known (known_closures, other_closures) ->
    TG.Row_like_for_closures.create_raw ~known_closures ~other_closures
  | Unknown ->
    Misc.fatal_error "Join row_like case for closures returned Unknown"

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

and join_int_indexed_product env shape (fields1 : TG.Product.Int_indexed.t)
    (fields2 : TG.Product.Int_indexed.t) : TG.Product.Int_indexed.t =
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
            | Unknown -> MTC.unknown_from_shape shape index
            | Known ty -> ty)
  in
  TG.Product.Int_indexed.create_from_array fields

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
  if TE.is_bottom env
  then Bottom
  else
    let scope = TE.current_scope env in
    let scoped_env = TE.increment_scope env in
    match meet scoped_env ty1 ty2 with
    | Bottom -> Bottom
    | Ok (r, scoped_env) ->
      let res_ty = extract_value r ty1 ty2 in
      if TG.is_obviously_bottom res_ty
      then Bottom
      else
        let env_extension = TE.cut_as_extension scoped_env ~cut_after:scope in
        Ok (res_ty, env_extension)

let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
  if TE.is_bottom env
  then Bottom
  else
    let result = Bound_name.create_var result_var in
    let env = TE.add_definition env result result_kind in
    match meet env t shape with
    | Bottom -> Bottom
    | Ok (_, env_extension) -> Ok env_extension

let meet_env_extension env ext1 ext2 : _ Or_bottom.t =
  if TE.is_bottom env
  then Bottom
  else
    let scope = TE.current_scope env in
    let scoped_env = TE.increment_scope env in
    match
      TE.add_env_extension_strict scoped_env ext1 ~meet_type:(New meet_type)
    with
    | Bottom -> Bottom
    | Ok scoped_env -> (
      match
        TE.add_env_extension_strict scoped_env ext2 ~meet_type:(New meet_type)
      with
      | Bottom -> Bottom
      | Ok scoped_env ->
        let env_extension = TE.cut_as_extension scoped_env ~cut_after:scope in
        Ok env_extension)
