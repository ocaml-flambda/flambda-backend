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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda
module ART = Are_rebuilding_terms

type t =
  | Normal of
      { const : Static_const.t;
        free_names : Name_occurrences.t
      }
  | Non_code_not_rebuilt of { free_names : Name_occurrences.t }
  | Code_not_rebuilt of Non_constructed_code.t

type rebuilt_static_const = t

let create_normal_non_code const =
  Normal { const; free_names = Static_const.free_names const }

let create_code are_rebuilding code_id ~(params_and_body : _ Or_deleted.t)
    ~newer_version_of ~params_arity ~result_arity ~stub ~inline ~is_a_functor
    ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~inlining_decision =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let params_and_body =
      match params_and_body with
      | Present (_, free_names_of_params_and_body) ->
        Or_deleted.Present ((), free_names_of_params_and_body)
      | Deleted -> Or_deleted.Deleted
    in
    let non_constructed_code =
      Non_constructed_code.create code_id ~params_and_body ~newer_version_of
        ~params_arity ~result_arity ~stub ~inline ~is_a_functor ~recursive
        ~cost_metrics ~inlining_arguments ~dbg ~is_tupled ~inlining_decision
    in
    Code_not_rebuilt non_constructed_code
  else
    let params_and_body =
      match params_and_body with
      | Present (params_and_body, free_names_of_params_and_body) ->
        Or_deleted.Present
          ( Rebuilt_expr.Function_params_and_body.to_function_params_and_body
              params_and_body are_rebuilding,
            free_names_of_params_and_body )
      | Deleted -> Or_deleted.Deleted
    in
    let code =
      Code.create code_id ~params_and_body ~newer_version_of ~params_arity
        ~result_arity ~stub ~inline ~is_a_functor ~recursive ~cost_metrics
        ~inlining_arguments ~dbg ~is_tupled ~inlining_decision
    in
    Normal { const = Code code; free_names = Code.free_names code }

let create_code' code =
  Normal { const = Code code; free_names = Code.free_names code }

let create_set_of_closures are_rebuilding set =
  let free_names = Set_of_closures.free_names set in
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names }
  else Normal { const = Set_of_closures set; free_names }

let create_block are_rebuilding tag is_mutable ~fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names
            (Static_const.Field_of_block.free_names field))
    in
    Non_code_not_rebuilt { free_names }
  else create_normal_non_code (Block (tag, is_mutable, fields))

let create_boxed_float are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (Boxed_float or_var)

let create_boxed_int32 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (Boxed_int32 or_var)

let create_boxed_int64 are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (Boxed_int64 or_var)

let create_boxed_nativeint are_rebuilding or_var =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Or_variable.free_names or_var }
  else create_normal_non_code (Boxed_nativeint or_var)

let create_immutable_float_block are_rebuilding fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names (Or_variable.free_names field))
    in
    Non_code_not_rebuilt { free_names }
  else create_normal_non_code (Immutable_float_block fields)

let create_immutable_float_array are_rebuilding fields =
  if ART.do_not_rebuild_terms are_rebuilding
  then
    let free_names =
      ListLabels.fold_left fields ~init:Name_occurrences.empty
        ~f:(fun free_names field ->
          Name_occurrences.union free_names (Or_variable.free_names field))
    in
    Non_code_not_rebuilt { free_names }
  else create_normal_non_code (Immutable_float_array fields)

let create_mutable_string are_rebuilding ~initial_value =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Name_occurrences.empty }
  else create_normal_non_code (Mutable_string { initial_value })

let create_immutable_string are_rebuilding str =
  if ART.do_not_rebuild_terms are_rebuilding
  then Non_code_not_rebuilt { free_names = Name_occurrences.empty }
  else create_normal_non_code (Immutable_string str)

let map_set_of_closures t ~f =
  match t with
  | Normal { const; _ } -> begin
    match const with
    | Set_of_closures set_of_closures ->
      let set_of_closures = f set_of_closures in
      Normal
        { const = Set_of_closures set_of_closures;
          free_names = Set_of_closures.free_names set_of_closures
        }
    | Code _ | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
    | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
    | Mutable_string _ | Immutable_string _ ->
      t
  end
  | Non_code_not_rebuilt _ | Code_not_rebuilt _ -> t

let free_names t =
  match t with
  | Normal { free_names; _ } -> free_names
  | Non_code_not_rebuilt { free_names } -> free_names
  | Code_not_rebuilt code -> Non_constructed_code.free_names code

let is_fully_static t = Name_occurrences.no_variables (free_names t)

let to_const t =
  match t with
  | Non_code_not_rebuilt _ | Code_not_rebuilt _ -> None
  | Normal { const; _ } -> Some const

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Normal { const; _ } -> Static_const.print ppf const
  | Non_code_not_rebuilt { free_names = _; } ->
    Format.fprintf ppf "Non_code_not_rebuilt"
  | Code_not_rebuilt code ->
    Format.fprintf ppf "@[<hov 1>(Code_not_rebuilt@ %a)@]"
      Non_constructed_code.print code

let make_all_code_deleted t =
  match t with
  | Normal { const; _ } -> begin
    match Static_const.to_code const with
    | None -> t
    | Some code ->
      let code = Code.make_deleted code in
      let const : Static_const.t = Code code in
      Normal { const; free_names = Code.free_names code }
  end
  | Non_code_not_rebuilt _ -> t
  | Code_not_rebuilt code ->
    Code_not_rebuilt (Non_constructed_code.make_deleted code)

let make_code_deleted t ~if_code_id_is_member_of =
  match t with
  | Normal { const; _ } -> begin
    match Static_const.to_code const with
    | None -> t
    | Some code ->
      if Code_id.Set.mem (Code.code_id code) if_code_id_is_member_of
      then
        let code = Code.make_deleted code in
        let const : Static_const.t = Code code in
        Normal { const; free_names = Code.free_names code }
      else t
  end
  | Non_code_not_rebuilt _ -> t
  | Code_not_rebuilt code ->
    if Code_id.Set.mem
         (Non_constructed_code.code_id code)
         if_code_id_is_member_of
    then Code_not_rebuilt (Non_constructed_code.make_deleted code)
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
        | Non_code_not_rebuilt _ | Code_not_rebuilt _ ->
          Misc.fatal_error
            "Cannot extract static constants when not rebuilding terms")
    |> Static_const.Group.create |> Named.create_static_consts

  let pieces_of_code_for_cmx t =
    let consts =
      ListLabels.filter_map t.consts ~f:(fun const ->
          match const with
          | Normal { const; _ } -> Static_const.to_code const
          | Non_code_not_rebuilt _ | Code_not_rebuilt _ -> None)
    in
    consts
    |> List.filter_map (fun code ->
           if Code.is_deleted code then None else Some (Code.code_id code, code))
    |> Code_id.Map.of_list

  let function_params_and_body_for_code_not_rebuilt =
    lazy
      (Function_params_and_body.create
         ~return_continuation:(Continuation.create ())
         (Exn_continuation.create ~exn_handler:(Continuation.create ())
            ~extra_args:[])
         [] ~dbg:Debuginfo.none ~body:(Expr.create_invalid ())
         ~free_names_of_body:Unknown
         ~my_closure:(Variable.create "my_closure")
         ~my_depth:(Variable.create "my_depth"))

  let pieces_of_code_including_those_not_rebuilt t =
    let consts =
      ListLabels.filter_map t.consts ~f:(fun const ->
          match const with
          | Normal { const; _ } -> Static_const.to_code const
          | Non_code_not_rebuilt _ -> None
          | Code_not_rebuilt code ->
            let module NCC = Non_constructed_code in
            (* See comment in the .mli. *)
            let params_and_body : _ Or_deleted.t =
              match NCC.params_and_body code with
              | Deleted -> Deleted
              | Present () ->
                Present
                  ( Lazy.force function_params_and_body_for_code_not_rebuilt,
                    Name_occurrences.empty )
            in
            Some
              (Code.create (NCC.code_id code) ~params_and_body
                 ~newer_version_of:(NCC.newer_version_of code)
                 ~params_arity:(NCC.params_arity code)
                 ~result_arity:(NCC.result_arity code) ~stub:(NCC.stub code)
                 ~inline:(NCC.inline code) ~is_a_functor:(NCC.is_a_functor code)
                 ~recursive:(NCC.recursive code)
                 ~cost_metrics:(NCC.cost_metrics code)
                 ~inlining_arguments:(NCC.inlining_arguments code)
                 ~dbg:(NCC.dbg code) ~is_tupled:(NCC.is_tupled code)
                 ~inlining_decision:(NCC.inlining_decision code)))
    in
    consts
    |> List.filter_map (fun code ->
           if Code.is_deleted code then None else Some (Code.code_id code, code))
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
