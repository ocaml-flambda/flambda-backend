(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module T = Type_grammar
module TEE = Typing_env_extension

module Make (Index : Product_intf.Index) = struct

  (* Product are a set of constraints: each new field reduces the
     concrete set. The empty product is Top. There is no bottom. All
     components must be of the same kind.

     { 1 => Unknown; 2 => V } is equal to { 2 => V } *)
  type t = {
    components_by_index : T.t Index.Map.t;
    (* Consider moving that field to row_like instead. It is not
       required for the closures_entry cases which are always values *)
    kind : Flambda_kind.t;
  }

  let [@ocamlformat "disable"] print ppf { components_by_index; kind = _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Type_grammar.print) components_by_index

  let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

  let fields_kind t = t.kind

  let create kind components_by_index =
    (* CR mshinwell: Check that the types are all of the same kind *)
    { components_by_index;
      kind;
    }

  let create_top kind = create kind Index.Map.empty

  let width t =
    Targetint_31_63.Imm.of_int (Index.Map.cardinal t.components_by_index)

  let components t = Index.Map.data t.components_by_index

  let project t index : _ Or_unknown.t =
    match Index.Map.find_opt index t.components_by_index with
    | None -> Unknown
    | Some ty -> Known ty

  let meet env
        { components_by_index = components_by_index1; kind = kind1; }
        { components_by_index = components_by_index2; kind = kind2; }
    : _ Or_bottom.t =
    if not (Flambda_kind.equal kind1 kind2) then begin
      Misc.fatal_errorf "Product.meet between mismatching kinds %a and %a@."
        Flambda_kind.print kind1 Flambda_kind.print kind2
    end;
    let any_bottom = ref false in
    let env_extension = ref (TEE.empty ()) in
    let components_by_index =
      Index.Map.union (fun _index ty1 ty2 ->
          match Type_grammar.meet env ty1 ty2 with
          | Ok (ty, env_extension') ->
            begin match TEE.meet env !env_extension env_extension' with
            | Bottom ->
              any_bottom := true;
              Some (Type_grammar.bottom_like ty1)
            | Ok extension ->
              env_extension := extension;
              Some ty
            end
          | Bottom ->
            any_bottom := true;
            Some (Type_grammar.bottom_like ty1))
        components_by_index1
        components_by_index2
    in
    if !any_bottom then Bottom
    else Ok ({ components_by_index; kind = kind1; }, !env_extension)

  let join env
        { components_by_index = components_by_index1; kind = kind1; }
        { components_by_index = components_by_index2; kind = kind2; } =
    if not (Flambda_kind.equal kind1 kind2) then begin
      Misc.fatal_errorf "Product.join between mismatching kinds %a and %a@."
        Flambda_kind.print kind1 Flambda_kind.print kind2
    end;
    let components_by_index =
      Index.Map.merge (fun _index ty1_opt ty2_opt ->
          match ty1_opt, ty2_opt with
          | None, _ | _, None -> None
          | Some ty1, Some ty2 ->
            begin match Type_grammar.join env ty1 ty2 with
            | Known ty -> Some ty
            | Unknown -> None
            end)
        components_by_index1
        components_by_index2
    in
    { components_by_index; kind = kind1; }

  let apply_renaming { components_by_index; kind; } renaming =
    let components_by_index =
      (* CR-someday mshinwell: some loss of sharing here, potentially *)
      Index.Map.filter_map (fun index ty ->
          if Index.remove_on_import index renaming then None
          else Some (Type_grammar.apply_renaming ty renaming))
        components_by_index
    in
    { kind; components_by_index; }

  let free_names { components_by_index; kind = _; } =
    Index.Map.fold (fun _index ty free_names ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      components_by_index
      Name_occurrences.empty

  let all_ids_for_export { components_by_index; kind = _; } =
    Index.Map.fold (fun _index ty ids ->
        Ids_for_export.union (Type_grammar.all_ids_for_export ty) ids)
      components_by_index
      Ids_for_export.empty

  let map_types ({ components_by_index; kind } as t)
        ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t)
        : _ Or_bottom.t =
    let found_bottom = ref false in
    let components_by_index' =
      Index.Map.map_sharing (fun ty ->
          match f ty with
          | Bottom ->
            found_bottom := true;
            ty
          | Ok ty -> ty)
        components_by_index
    in
    if !found_bottom then Bottom
    else if components_by_index == components_by_index' then Ok t
    else Ok { components_by_index = components_by_index'; kind; }

  let to_map t = t.components_by_index
end

module Closure_id_index = struct
  include Closure_id

  let remove_on_import _ _ = false
end

module Closure_id_indexed = Make (Closure_id_index)

module Var_within_closure_index = struct
  include Var_within_closure

  let remove_on_import var renaming =
    not (Renaming.closure_var_is_used renaming var)
end

module Var_within_closure_indexed = Make (Var_within_closure_index)

module Int_indexed = struct
  (* CR mshinwell: Add [Or_bottom].  However what should [width] return for
     [Bottom]?  Maybe we can circumvent that question if removing [Row_like].
  *)
  type t = {
    fields : T.t array;
    kind : Flambda_kind.t;
  }

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      (Array.to_list t.fields)

  let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

  let fields_kind t = t.kind

  let create_from_list kind tys = {
    kind;
    fields = Array.of_list tys;
  }

  let create_top kind = {
    kind;
    fields = [| |];
  }

  let width t = Targetint_31_63.Imm.of_int (Array.length t.fields)

  let components t = Array.to_list t.fields

  let project t index : _ Or_unknown.t =
    if Array.length t.fields <= index then Unknown
    else Known t.fields.(index)

  let meet env t1 t2 : _ Or_bottom.t =
    if not (Flambda_kind.equal t1.kind t2.kind) then begin
      Misc.fatal_errorf "Product.Int_indexed.meet between mismatching \
                         kinds %a and %a@."
        Flambda_kind.print t1.kind Flambda_kind.print t2.kind
    end;
    let fields1 = t1.fields in
    let fields2 = t2.fields in
    let any_bottom = ref false in
    let env_extension = ref (TEE.empty ()) in
    let length = max (Array.length fields1) (Array.length fields2) in
    let fields =
      Array.init length (fun index ->
        let get_opt fields =
          if index >= Array.length fields then
            None
          else
            Some fields.(index)
        in
        match get_opt fields1, get_opt fields2 with
        | None, None -> assert false
        | Some t, None | None, Some t -> t
        | Some ty1, Some ty2 ->
          begin match Type_grammar.meet env ty1 ty2 with
          | Ok (ty, env_extension') ->
            begin match TEE.meet env !env_extension env_extension' with
            | Bottom ->
              any_bottom := true;
              Type_grammar.bottom_like ty1
            | Ok extension ->
              env_extension := extension;
              ty
            end
          | Bottom ->
            any_bottom := true;
            Type_grammar.bottom_like ty1
          end)
    in
    if !any_bottom then Bottom
    else Ok ({ fields; kind = t1.kind; }, !env_extension)

  let join env t1 t2 =
    if not (Flambda_kind.equal t1.kind t2.kind) then begin
      Misc.fatal_errorf "Product.Int_indexed.join between mismatching \
                         kinds %a and %a@."
        Flambda_kind.print t1.kind Flambda_kind.print t2.kind
    end;
    let fields1 = t1.fields in
    let fields2 = t2.fields in
    let length1 = Array.length fields1 in
    let length2 = Array.length fields2 in
    let length = min length1 length2 in
    let exception Exit in
    let all_phys_equal =
      try
        for index = 0 to length - 1 do
          if fields1.(index) != fields2.(index) then begin
            raise Exit
          end
        done;
        true
      with Exit -> false
    in
    let fields =
      if all_phys_equal then
        if length1 = length then fields1
        else begin
          assert (length2 = length);
          fields2
        end
      else
        Array.init length (fun index ->
          if fields1.(index) == fields2.(index) then fields1.(index)
          else match Type_grammar.join env fields1.(index) fields2.(index) with
            | Unknown ->
              Type_grammar.unknown t1.kind
            | Known ty -> ty)
    in
    { kind = t1.kind; fields }

  let apply_renaming { kind; fields; } perm =
    let fields = Array.copy fields in
    for i = 0 to Array.length fields - 1 do
      fields.(i) <- Type_grammar.apply_renaming fields.(i) perm
    done;
    { kind; fields; }

  let free_names t =
    Array.fold_left (fun free_names ty ->
        Name_occurrences.union (Type_grammar.free_names ty) free_names)
      Name_occurrences.empty
      t.fields

  let all_ids_for_export t =
    Array.fold_left (fun ids ty ->
        Ids_for_export.union (Type_grammar.all_ids_for_export ty) ids)
      Ids_for_export.empty
      t.fields

  let map_types t ~(f : Type_grammar.t -> Type_grammar.t Or_bottom.t)
        : _ Or_bottom.t =
    let found_bottom = ref false in
    let fields = Array.copy t.fields in
    for i = 0 to Array.length fields - 1 do
      match f fields.(i) with
      | Bottom -> found_bottom := true
      | Ok typ -> fields.(i) <- typ
    done;
    if !found_bottom then Bottom
    else Ok { t with fields }

end
