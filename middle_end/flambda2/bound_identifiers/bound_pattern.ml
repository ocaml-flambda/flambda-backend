(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Singleton of Bound_var.t
  | Set_of_closures of Bound_var.t list
  | Static of Bound_static.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Singleton bound_var -> Bound_var.print ppf bound_var
  | Set_of_closures bound_vars ->
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Bound_var.print)
      bound_vars
  | Static bound_static ->
    Format.fprintf ppf "@[<hov 1>\
        @[(bound_static@ %a)@]\
        )@]"
      Bound_static.print bound_static

let free_names t =
  match t with
  | Singleton bound_var ->
    Name_occurrences.singleton_variable (Bound_var.var bound_var)
      Name_mode.normal
  | Set_of_closures bound_vars ->
    List.fold_left
      (fun free_names bound_var ->
        Name_occurrences.add_variable free_names (Bound_var.var bound_var)
          Name_mode.normal)
      Name_occurrences.empty bound_vars
  | Static bound_static -> Bound_static.free_names bound_static

let rec map_sharing f l0 =
  match l0 with
  | a :: l ->
    let a' = f a in
    let l' = map_sharing f l in
    if a' == a && l' == l then l0 else a' :: l'
  | [] -> []

let apply_renaming t renaming =
  match t with
  | Singleton bound_var ->
    let bound_var' = Bound_var.apply_renaming bound_var renaming in
    if bound_var == bound_var' then t else Singleton bound_var'
  | Set_of_closures bound_vars ->
    let bound_vars' =
      map_sharing
        (fun bound_var -> Bound_var.apply_renaming bound_var renaming)
        bound_vars
    in
    if bound_vars == bound_vars' then t else Set_of_closures bound_vars'
  | Static bound_static ->
    let bound_static' = Bound_static.apply_renaming bound_static renaming in
    if bound_static == bound_static' then t else Static bound_static'

let ids_for_export t =
  match t with
  | Singleton bound_var -> Bound_var.ids_for_export bound_var
  | Set_of_closures bound_vars ->
    List.fold_left
      (fun ids bound_var ->
        Ids_for_export.union ids (Bound_var.ids_for_export bound_var))
      Ids_for_export.empty bound_vars
  | Static bound_static -> Bound_static.ids_for_export bound_static

let rename t =
  match t with
  | Singleton bound_var -> Singleton (Bound_var.rename bound_var)
  | Set_of_closures bound_vars ->
    let bound_vars =
      List.map (fun bound_var -> Bound_var.rename bound_var) bound_vars
    in
    Set_of_closures bound_vars
  | Static _ -> t

let is_renamed_version_of t t' =
  match t, t' with
  | Singleton bound_var, Singleton bound_var' ->
    Bound_var.is_renamed_version_of bound_var bound_var'
  | Set_of_closures bound_vars, Set_of_closures bound_vars' ->
    Misc.Stdlib.List.equal Bound_var.is_renamed_version_of bound_vars
      bound_vars'
  | Static _bound_static, Static _bound_static' ->
    (* CR gbury/ncourant: We should try and compare the bound statics here *)
    true
  | Singleton _, (Set_of_closures _ | Static _)
  | Set_of_closures _, (Singleton _ | Static _)
  | Static _, (Singleton _ | Set_of_closures _) ->
    false

let renaming t1 ~guaranteed_fresh:t2 =
  match t1, t2 with
  | Singleton bound_var1, Singleton bound_var2 ->
    Renaming.add_fresh_variable Renaming.empty (Bound_var.var bound_var1)
      ~guaranteed_fresh:(Bound_var.var bound_var2)
  | Set_of_closures bound_vars1, Set_of_closures bound_vars2 ->
    if List.compare_lengths bound_vars1 bound_vars2 = 0
    then
      List.fold_left2
        (fun renaming var1 var2 ->
          Renaming.add_fresh_variable renaming (Bound_var.var var1)
            ~guaranteed_fresh:(Bound_var.var var2))
        Renaming.empty bound_vars1 bound_vars2
    else
      Misc.fatal_errorf
        "Mismatching bound vars for sets of closures:@ %a@ and@ %a" print t1
        print t2
  | Static _, Static _ ->
    (* We never permute symbols or code IDs. *)
    Renaming.empty
  | (Singleton _ | Set_of_closures _ | Static _), _ ->
    Misc.fatal_errorf "Pattern mismatch:@ %a@ and@ %a" print t1 print t2

let singleton var = Singleton var

let set_of_closures bound_vars =
  let name_mode =
    List.fold_left
      (fun name_mode bound_var ->
        let next_name_mode = Bound_var.name_mode bound_var in
        match name_mode with
        | None -> Some next_name_mode
        | Some name_mode ->
          if not (Name_mode.equal name_mode next_name_mode)
          then
            Misc.fatal_errorf "Mismatching name modes:@ %a"
              (Format.pp_print_list ~pp_sep:Format.pp_print_space
                 Bound_var.print)
              bound_vars
          else Some name_mode)
      None bound_vars
  in
  match name_mode with
  | None -> Misc.fatal_error "No bound variables provided for closures"
  | Some _name_mode -> Set_of_closures bound_vars

let static bound_static = Static bound_static

let name_mode t =
  match t with
  | Singleton bound_var | Set_of_closures (bound_var :: _) ->
    Bound_var.name_mode bound_var
  | Set_of_closures [] -> assert false (* see [set_of_closures] above *)
  | Static _ -> Name_mode.normal

let with_name_mode t name_mode =
  match t with
  | Singleton bound_var ->
    Singleton (Bound_var.with_name_mode bound_var name_mode)
  | Set_of_closures bound_vars ->
    Set_of_closures
      (List.map
         (fun bound_var -> Bound_var.with_name_mode bound_var name_mode)
         bound_vars)
  | Static _ -> t

let must_be_singleton t =
  match t with
  | Singleton bound_var -> bound_var
  | Set_of_closures _ | Static _ ->
    Misc.fatal_errorf "Bound pattern is not [Singleton]:@ %a" print t

let must_be_singleton_opt t =
  match t with
  | Singleton bound_var -> Some bound_var
  | Set_of_closures _ | Static _ -> None

let must_be_set_of_closures t =
  match t with
  | Set_of_closures bound_vars -> bound_vars
  | Singleton _ | Static _ ->
    Misc.fatal_errorf "Bound pattern is not [Set_of_closures]:@ %a" print t

let must_be_static t =
  match t with
  | Static bound_static -> bound_static
  | Singleton _ | Set_of_closures _ ->
    Misc.fatal_errorf "Bound pattern is not [Static]:@ %a" print t

let exists_all_bound_vars t ~f =
  match t with
  | Singleton var -> f var
  | Set_of_closures bound_vars -> ListLabels.exists bound_vars ~f
  | Static _ -> false

let fold_all_bound_vars t ~init ~f =
  match t with
  | Singleton bound_var -> f init bound_var
  | Set_of_closures bound_vars -> ListLabels.fold_left bound_vars ~init ~f
  | Static _ -> init

let fold_all_bound_names t ~init ~var ~symbol ~code_id =
  match t with
  | Singleton bound_var -> var init bound_var
  | Set_of_closures bound_vars -> ListLabels.fold_left bound_vars ~init ~f:var
  | Static bound_static ->
    Code_id.Set.fold
      (fun cid acc -> code_id acc cid)
      (Bound_static.code_being_defined bound_static)
      init
    |> Symbol.Set.fold
         (fun s acc -> symbol acc s)
         (Bound_static.symbols_being_defined bound_static)
