(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda
module ART = Are_rebuilding_terms
module SC = Static_const

type t =
  | Normal of
      { const : Static_const_or_code.t;
        free_names : Name_occurrences.t
      }
  | Block_not_rebuilt of { free_names : Name_occurrences.t }
  | Set_of_closures_not_rebuilt of { free_names : Name_occurrences.t }
  | Code_not_rebuilt of Non_constructed_code.t

type rebuilt_static_const = t

let is_block t =
  match t with
  | Normal { const; _ } -> Static_const_or_code.is_block const
  | Block_not_rebuilt _ -> true
  | Set_of_closures_not_rebuilt _ | Code_not_rebuilt _ -> false

let is_set_of_closures t =
  match t with
  | Normal { const; _ } -> Static_const_or_code.is_set_of_closures const
  | Set_of_closures_not_rebuilt _ -> true
  | Code_not_rebuilt _ | Block_not_rebuilt _ -> false

let is_code t =
  match t with
  | Normal { const; _ } -> Static_const_or_code.is_code const
  | Code_not_rebuilt _ -> true
  | Set_of_closures_not_rebuilt _ | Block_not_rebuilt _ -> false

let create_normal_non_code const =
  Normal
    { const = Static_const_or_code.create_static_const const;
      free_names = Static_const.free_names const
    }

let create_code are_rebuilding ~params_and_body ~free_names_of_params_and_body =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    Code_metadata.createk (fun code_metadata ->
        ( Code_not_rebuilt
            (Non_constructed_code.create_with_metadata
               ~free_names_of_params_and_body ~code_metadata),
          None ))
  else
    let params_and_body =
      Rebuilt_expr.Function_params_and_body.to_function_params_and_body
        params_and_body are_rebuilding
    in
    Code_metadata.createk (fun code_metadata ->
        let code =
          Code.create_with_metadata ~params_and_body
            ~free_names_of_params_and_body ~code_metadata
        in
        ( Normal
            { const = Static_const_or_code.create_code code;
              free_names = Code.free_names code
            },
          Some code ))

let create_code' code =
  Normal
    { const = Static_const_or_code.create_code code;
      free_names = Code.free_names code
    }

let create_set_of_closures are_rebuilding set =
  (* Even if the set of closures was locally allocated, this allocation is
     global. This will not cause leaks, as lifted constants are static and
     therefore only allocated once. *)
  let set =
    Set_of_closures.create
      ~value_slots:(Set_of_closures.value_slots set)
      Alloc_mode.For_allocations.heap
      (Set_of_closures.function_decls set)
  in
  let free_names = Set_of_closures.free_names set in
  if ART.do_not_rebuild_terms are_rebuilding
  then Set_of_closures_not_rebuilt { free_names }
  else
    Normal
      { const =
          Static_const_or_code.create_static_const (SC.set_of_closures set);
        free_names
      }

let free_names_of_fields fields free_names_of_field =
  ListLabels.fold_left fields ~init:Name_occurrences.empty
    ~f:(fun free_names field ->
      Name_occurrences.union free_names (free_names_of_field field))

let create_block are_rebuilding tag is_mutable shape ~fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      free_names_of_fields fields Simple.With_debuginfo.free_names
    in
    Block_not_rebuilt { free_names }
  else create_normal_non_code (SC.block tag is_mutable shape fields)

let create_boxed_float32 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_float32 or_var)

let create_boxed_float are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_float or_var)

let create_boxed_int32 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_int32 or_var)

let create_boxed_int64 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_int64 or_var)

let create_boxed_nativeint are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_nativeint or_var)

let create_boxed_vec128 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (SC.boxed_vec128 or_var)

let create_immutable_float_block are_rebuilding fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names (Or_variable.free_names field))
    in
    Block_not_rebuilt { free_names }
  else create_normal_non_code (SC.immutable_float_block fields)

let create_immutable_naked_number_array builder are_rebuilding fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names (Or_variable.free_names field))
    in
    Block_not_rebuilt { free_names }
  else create_normal_non_code (builder fields)

let create_immutable_float_array =
  create_immutable_naked_number_array SC.immutable_float_array

let create_immutable_float32_array =
  create_immutable_naked_number_array SC.immutable_float32_array

let create_immutable_int32_array =
  create_immutable_naked_number_array SC.immutable_int32_array

let create_immutable_int64_array =
  create_immutable_naked_number_array SC.immutable_int64_array

let create_immutable_nativeint_array =
  create_immutable_naked_number_array SC.immutable_nativeint_array

let create_immutable_value_array are_rebuilding fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names
            (Simple.With_debuginfo.free_names field))
    in
    Block_not_rebuilt { free_names }
  else create_normal_non_code (SC.immutable_value_array fields)

let create_empty_array are_rebuilding array_kind =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Name_occurrences.empty }
  else create_normal_non_code (SC.empty_array array_kind)

let create_mutable_string are_rebuilding ~initial_value =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Name_occurrences.empty }
  else create_normal_non_code (SC.mutable_string ~initial_value)

let create_immutable_string are_rebuilding str =
  if ART.do_not_rebuild_terms are_rebuilding
  then Block_not_rebuilt { free_names = Name_occurrences.empty }
  else create_normal_non_code (SC.immutable_string str)

let map_set_of_closures t ~f =
  match t with
  | Normal { const; _ } -> (
    match const with
    | Code _ | Deleted_code -> t
    | Static_const const -> (
      match const with
      | Set_of_closures set_of_closures ->
        let set_of_closures = f set_of_closures in
        Normal
          { const =
              Static_const_or_code.create_static_const
                (SC.set_of_closures set_of_closures);
            free_names = Set_of_closures.free_names set_of_closures
          }
      | Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _
      | Immutable_float_block _ | Immutable_float_array _
      | Immutable_float32_array _ | Immutable_int32_array _
      | Immutable_int64_array _ | Immutable_nativeint_array _
      | Immutable_value_array _ | Empty_array _ | Mutable_string _
      | Immutable_string _ ->
        t))
  | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _ | Code_not_rebuilt _ ->
    t

let free_names t =
  match t with
  | Normal { free_names; _ } -> free_names
  | Block_not_rebuilt { free_names }
  | Set_of_closures_not_rebuilt { free_names } ->
    free_names
  | Code_not_rebuilt code -> Non_constructed_code.free_names code

let is_fully_static t = Name_occurrences.no_variables (free_names t)

let to_const t =
  match t with
  | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _ | Code_not_rebuilt _ ->
    None
  | Normal { const; _ } -> Some const

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Normal { const; _ } -> Static_const_or_code.print ppf const
  | Block_not_rebuilt { free_names = _; } ->
    Format.fprintf ppf "Block_not_rebuilt"
  | Set_of_closures_not_rebuilt { free_names = _; } ->
    Format.fprintf ppf "Set_of_closures_not_rebuilt"
  | Code_not_rebuilt code ->
    Format.fprintf ppf "@[<hov 1>(Code_not_rebuilt@ %a)@]"
      Non_constructed_code.print code

let deleted_code =
  Normal
    { const = Static_const_or_code.deleted_code;
      free_names = Name_occurrences.empty
    }

let make_code_deleted t ~if_code_id_is_member_of =
  match t with
  | Normal { const; _ } -> (
    match Static_const_or_code.to_code const with
    | None -> t
    | Some code ->
      if Code_id.Set.mem (Code.code_id code) if_code_id_is_member_of
      then deleted_code
      else t)
  | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _ -> t
  | Code_not_rebuilt code ->
    if Code_id.Set.mem
         (Non_constructed_code.code_id code)
         if_code_id_is_member_of
    then deleted_code
    else t

module Group = struct
  type t =
    { consts : rebuilt_static_const list;
      mutable free_names : Name_occurrences.t Or_unknown.t
    }

  let empty = { consts = []; free_names = Known Name_occurrences.empty }

  let create consts = { consts; free_names = Unknown }

  let [@ocamlformat "disable"] print ppf { consts; free_names = _; } =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print) consts

  let free_names t =
    match t.free_names with
    | Known free_names -> free_names
    | Unknown ->
      let free_names =
        ListLabels.fold_left t.consts ~init:Name_occurrences.empty
          ~f:(fun free_names_acc (const : rebuilt_static_const) ->
            Name_occurrences.union free_names_acc (free_names const))
      in
      t.free_names <- Known free_names;
      free_names

  let to_named t =
    ListLabels.map t.consts ~f:(fun (const : rebuilt_static_const) ->
        match const with
        | Normal { const; _ } -> const
        | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _
        | Code_not_rebuilt _ ->
          Misc.fatal_error
            "Cannot extract static constants when not rebuilding terms")
    |> Static_const_group.create |> Named.create_static_consts

  let pieces_of_code_for_cmx t =
    ListLabels.filter_map t.consts ~f:(fun const ->
        match const with
        | Normal { const; _ } -> (
          match Static_const_or_code.to_code const with
          | None -> None
          | Some code -> Some (Code.code_id code, code))
        | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _
        | Code_not_rebuilt _ ->
          None)
    |> Code_id.Map.of_list

  let function_params_and_body_for_code_not_rebuilt =
    lazy
      (Function_params_and_body.create
         ~return_continuation:(Continuation.create ())
         ~exn_continuation:(Continuation.create ()) Bound_parameters.empty
         ~body:(Expr.create_invalid Code_not_rebuilt)
         ~free_names_of_body:Unknown
         ~my_closure:(Variable.create "my_closure")
         ~my_region:(Variable.create "my_region")
         ~my_depth:(Variable.create "my_depth"))

  let pieces_of_code_including_those_not_rebuilt t =
    ListLabels.filter_map t.consts ~f:(fun const ->
        match const with
        | Normal { const; _ } -> Static_const_or_code.to_code const
        | Block_not_rebuilt _ | Set_of_closures_not_rebuilt _ -> None
        | Code_not_rebuilt code ->
          let module NCC = Non_constructed_code in
          (* See comment in the .mli. *)
          let params_and_body =
            Lazy.force function_params_and_body_for_code_not_rebuilt
          in
          Some
            (Code.create_with_metadata ~params_and_body
               ~free_names_of_params_and_body:Name_occurrences.empty
               ~code_metadata:(NCC.code_metadata code)))
    |> List.map (fun code -> Code.code_id code, code)
    |> Code_id.Map.of_list

  let map t ~f =
    let changed = ref false in
    let consts =
      ListLabels.map t.consts ~f:(fun const ->
          let const' = f const in
          if const != const' then changed := true;
          const')
    in
    if not !changed then t else { consts; free_names = Unknown }

  let fold_left t ~init ~f = ListLabels.fold_left t.consts ~init ~f

  let add const t = { consts = const :: t.consts; free_names = Unknown }

  let concat t1 t2 =
    let free_names : _ Or_unknown.t =
      match t1.free_names, t2.free_names with
      | Known free_names1, Known free_names2 ->
        Known (Name_occurrences.union free_names1 free_names2)
      | Known _, Unknown | Unknown, Known _ | Unknown, Unknown -> Unknown
    in
    { consts = t1.consts @ t2.consts; free_names }

  let to_list t = t.consts
end
