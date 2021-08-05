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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type symbols = {
  bound_symbols : Bound_symbols.t;
}

type t =
  | Singleton of Var_in_binding_pos.t
  | Set_of_closures of {
      name_mode : Name_mode.t;
      closure_vars : Var_in_binding_pos.t list;
    }
  | Symbols of symbols
  (* CR mshinwell: Add a case here for let-code and move it out of
     Symbols *)

include Container_types.Make (struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Singleton var -> Var_in_binding_pos.print ppf var
    | Set_of_closures { name_mode = _; closure_vars; } ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Var_in_binding_pos.print)
        closure_vars
    | Symbols { bound_symbols } ->
      Format.fprintf ppf "@[<hov 1>\
          @[(bound_symbols@ %a)@]\
          )@]"
        Bound_symbols.print bound_symbols

  (* The following would only be required if using
     [Name_abstraction.Make_map], which we don't with this module. *)

  let compare _ _ =
    Misc.fatal_error "Bindable_let_bound.compare not yet implemented"

  let equal _ _ =
    Misc.fatal_error "Bindable_let_bound.equal not yet implemented"

  let hash _ =
    Misc.fatal_error "Bindable_let_bound.hash not yet implemented"

  let output _ _ =
    Misc.fatal_error "Bindable_let_bound.output not yet implemented"
end)

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names t =
  match t with
  | Singleton var ->
    let var = Var_in_binding_pos.var var in
    Name_occurrences.singleton_variable var Name_mode.normal
  | Set_of_closures { name_mode = _; closure_vars; } ->
    List.fold_left (fun free_names var ->
        let var = Var_in_binding_pos.var var in
        Name_occurrences.add_variable free_names var
          Name_mode.normal)
      Name_occurrences.empty
      closure_vars
  | Symbols { bound_symbols; } ->
    Bound_symbols.free_names bound_symbols

let rec map_sharing f l0 =
  match l0 with
  | a::l ->
    let a' = f a in
    let l' = map_sharing f l in
    if a' == a && l' == l then l0 else a' :: l'
  | [] -> []

let apply_renaming t perm =
  match t with
  | Singleton var ->
    let var' = Var_in_binding_pos.apply_renaming var perm in
    if var == var' then t
    else Singleton var'
  | Set_of_closures { name_mode; closure_vars; } ->
    let closure_vars' =
      map_sharing (fun var ->
          Var_in_binding_pos.apply_renaming var perm)
        closure_vars
    in
    if closure_vars == closure_vars' then t
    else Set_of_closures { name_mode; closure_vars = closure_vars'; }
  | Symbols { bound_symbols; } ->
    let bound_symbols' =
      Bound_symbols.apply_renaming bound_symbols perm
    in
    if bound_symbols == bound_symbols' then t
    else Symbols { bound_symbols = bound_symbols'; }

let all_ids_for_export t =
  match t with
  | Singleton var ->
    Ids_for_export.add_variable Ids_for_export.empty
      (Var_in_binding_pos.var var)
  | Set_of_closures { name_mode = _; closure_vars; } ->
    List.fold_left (fun ids var ->
        Ids_for_export.add_variable ids (Var_in_binding_pos.var var))
      Ids_for_export.empty
      closure_vars
  | Symbols { bound_symbols; } ->
    Bound_symbols.all_ids_for_export bound_symbols

let rename t =
  match t with
  | Singleton var -> Singleton (Var_in_binding_pos.rename var)
  | Set_of_closures { name_mode; closure_vars; } ->
    let closure_vars =
      List.map (fun var -> Var_in_binding_pos.rename var) closure_vars
    in
    Set_of_closures { name_mode; closure_vars; }
  | Symbols _ -> t

let add_to_name_permutation t1 ~guaranteed_fresh:t2 perm =
  match t1, t2 with
  | Singleton var1, Singleton var2 ->
    Renaming.add_fresh_variable perm
      (Var_in_binding_pos.var var1)
      ~guaranteed_fresh:(Var_in_binding_pos.var var2)
  | Set_of_closures { name_mode = _; closure_vars = closure_vars1; },
      Set_of_closures { name_mode = _;
        closure_vars = closure_vars2; } ->
    if List.compare_lengths closure_vars1 closure_vars2 = 0
    then
      List.fold_left2
        (fun perm var1 var2 ->
          Renaming.add_fresh_variable perm
            (Var_in_binding_pos.var var1)
            ~guaranteed_fresh:(Var_in_binding_pos.var var2))
        perm
        closure_vars1
        closure_vars2
    else
      Misc.fatal_errorf "Mismatching closure vars:@ %a@ and@ %a"
        print t1
        print t2
  | Symbols _, Symbols _ -> perm
  | (Singleton _ | Set_of_closures _ | Symbols _), _ ->
    Misc.fatal_errorf "Kind mismatch:@ %a@ and@ %a"
      print t1
      print t2

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty

let singleton_occurrence_in_terms t = free_names t

let add_occurrence_in_terms t occs =
  Name_occurrences.union (free_names t) occs

let singleton var = Singleton var

let set_of_closures ~closure_vars =
  let name_modes =
    List.fold_left (fun name_modes var ->
        Name_mode.Set.add (Var_in_binding_pos.name_mode var)
          name_modes)
      Name_mode.Set.empty
      closure_vars
  in
  match Name_mode.Set.elements name_modes with
  | [] -> Misc.fatal_error "No closure IDs provided"
  | [name_mode] ->
    (* CR mshinwell: Check there are no duplicates in [closure_vars] *)
    Set_of_closures {
      name_mode;
      closure_vars;
    }
  | _ ->
    Misc.fatal_errorf "Inconsistent name occurrence kinds:@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Var_in_binding_pos.print)
      closure_vars

let symbols bound_symbols =
  Symbols { bound_symbols; }

let name_mode t =
  match t with
  | Singleton var -> Var_in_binding_pos.name_mode var
  | Set_of_closures { name_mode; _ } -> name_mode
  | Symbols _ -> Name_mode.normal

let with_name_mode t name_mode =
  match t with
  | Singleton var ->
    Singleton (Var_in_binding_pos.with_name_mode var name_mode)
  | Set_of_closures { name_mode = _; closure_vars; } ->
    Set_of_closures { name_mode; closure_vars; }
  | Symbols _ -> t

let must_be_singleton t =
  match t with
  | Singleton var -> var
  | Set_of_closures _ | Symbols _ ->
    Misc.fatal_errorf "Bound name is not a [Singleton]:@ %a" print t

let must_be_singleton_opt t =
  match t with
  | Singleton var -> Some var
  | Set_of_closures _ | Symbols _ -> None

let must_be_set_of_closures t =
  match t with
  | Set_of_closures { closure_vars; _ } -> closure_vars
  | Singleton _ | Symbols _ ->
    Misc.fatal_errorf "Bound name is not a [Set_of_closures]:@ %a" print t

let must_be_symbols t =
  match t with
  | Symbols symbols -> symbols
  | Singleton _ | Set_of_closures _ ->
    Misc.fatal_errorf "Bound name is not a [Set_of_closures]:@ %a" print t

let exists_all_bound_vars t ~f =
  match t with
  | Singleton var -> f var
  | Set_of_closures { closure_vars; _ } -> ListLabels.exists closure_vars ~f
  | Symbols _ -> false

let fold_all_bound_vars t ~init ~f =
  match t with
  | Singleton var -> f init var
  | Set_of_closures { closure_vars; _ } ->
    ListLabels.fold_left closure_vars ~init ~f
  | Symbols _ -> init

let all_bound_vars t =
  match t with
  | Singleton var -> Var_in_binding_pos.Set.singleton var
  | Set_of_closures { closure_vars; _ } ->
    Var_in_binding_pos.Set.of_list closure_vars
  | Symbols _ -> Var_in_binding_pos.Set.empty

let all_bound_vars' t =
  match t with
  | Singleton var -> Variable.Set.singleton (Var_in_binding_pos.var var)
  | Set_of_closures { closure_vars; _ } ->
    Variable.Set.of_list (List.map Var_in_binding_pos.var closure_vars)
  | Symbols _ -> Variable.Set.empty
