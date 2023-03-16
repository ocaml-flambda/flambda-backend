type t =
  | Singleton of Bound_var.t
  | Static of Bound_codelike.t

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Singleton bound_var -> Bound_var.print ppf bound_var
  | Static bound_static ->
    Format.fprintf ppf "@[<hov 1>%a@]"
      Bound_codelike.print bound_static

let free_names t =
  match t with
  | Singleton bound_var ->
    Name_occurrences.singleton_variable (Bound_var.var bound_var)
      Name_mode.normal
  | Static bound_static -> Bound_codelike.free_names bound_static

let apply_renaming t renaming =
  match t with
  | Singleton bound_var ->
    let bound_var' = Bound_var.apply_renaming bound_var renaming in
    if bound_var == bound_var' then t else Singleton bound_var'
  | Static bound_static ->
    let bound_static' = Bound_codelike.apply_renaming bound_static renaming in
    if bound_static == bound_static' then t else Static bound_static'

let ids_for_export t =
  match t with
  | Singleton bound_var -> Bound_var.ids_for_export bound_var
  | Static bound_static -> Bound_codelike.ids_for_export bound_static

let rename t =
  match t with
  | Singleton bound_var -> Singleton (Bound_var.rename bound_var)
  | Static _ -> t

let renaming t1 ~guaranteed_fresh:t2 =
  match t1, t2 with
  | Singleton bound_var1, Singleton bound_var2 ->
    Renaming.add_fresh_variable Renaming.empty (Bound_var.var bound_var1)
      ~guaranteed_fresh:(Bound_var.var bound_var2)
  | Static _, Static _ ->
    (* We never permute symbols or code IDs. *)
    Renaming.empty
  | (Singleton _ | Static _), _ ->
    Misc.fatal_errorf "Pattern mismatch:@ %a@ and@ %a" print t1 print t2

let name_mode t =
  match t with
  | Singleton bound_var ->
    Bound_var.name_mode bound_var
  | Static _ -> Name_mode.normal

let with_name_mode t name_mode =
  match t with
  | Singleton bound_var ->
    Singleton (Bound_var.with_name_mode bound_var name_mode)
  | Static _ -> t

let fold_all_bound_vars t ~init ~f =
  match t with
  | Singleton bound_var -> f init bound_var
  | Static _ -> init

let fold_all_bound_names t ~init ~var ~symbol ~code_id =
  match t with
  | Singleton bound_var -> var init bound_var
  | Static bound_static ->
    Code_id.Set.fold
      (fun cid acc -> code_id acc cid)
      (Bound_codelike.code_being_defined bound_static)
      init
    |> Symbol.Set.fold
         (fun s acc -> symbol acc s)
         (Bound_codelike.symbols_being_defined bound_static)
