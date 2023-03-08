[@@@ocaml.warning "+a-40-41-42"]

module Make (Atom : Identifiable.S) = struct
  type t = {
    head : Atom.t;
    args : (t * t) list;
  }

  type param = {
    head : Atom.t;
    args : (t * param) list;
    params : (t * param) list;
  }

  let rec print ppf ({ head; args } : t) =
    Format.fprintf ppf "@[<hov 1>%a%a@]"
      Atom.print head
      (print_pairs print_arg_pair) args
  and print_pairs : 'a. (_ -> 'a -> unit) -> _ -> 'a list -> unit =
    fun print_pair ppf pairs ->
      Format.pp_print_list ~pp_sep:Format.pp_print_cut print_pair ppf pairs
  and print_arg_pair ppf (name, arg) =
    Format.fprintf ppf "[%a:%a]" print name print arg

  include Identifiable.Make (struct
    type nonrec t = t = {
      head : Atom.t;
      args : (t * t) list;
    }

    let rec compare
        { head = head1; args = args1 }
        { head = head2; args = args2 } =
      match Atom.compare head1 head2 with
      | 0 -> List.compare compare_arg args1 args2
      | c -> c
    and compare_arg (name1, arg1) (name2, arg2) =
      match compare name1 name2 with
      | 0 -> compare arg1 arg2
      | c -> c

    let equal t1 t2 = compare t1 t2 = 0

    let print = print

    let output = Misc.output_of_print print

    let rec hash { head; args } =
      Hashtbl.hash (Atom.hash head, List.map hash_arg args)
    and hash_arg (name, param) =
      Hashtbl.hash (hash name, hash param)
  end)

  let rec print_param ppf ({ head; args; params } : param) =
    Format.fprintf ppf "@[<hov 1>%a%a%a@]"
      Atom.print head
      (print_pairs print_arg_pair_in_param) args
      (print_pairs print_param_pair) params
  and print_arg_pair_in_param ppf (name, value) =
    Format.fprintf ppf "[%a:%a]" print name print_param value
  and print_param_pair ppf (name, value) =
    Format.fprintf ppf "[%a\\%a]" print name print_param value

  type subst = param Map.t

  (* CR-someday lmaurer: Should try and make this unnecessary or at least cheap.
     Could do it by making [t] an unboxed existential so that converting from
     [param] is the identity. Or just have it wrap [param] and ignore
     [params]. *)
  let rec erase_param ({ head; args; params = _ }) =
    { head; args = List.map erase_arg args }
  and erase_arg (name, value) =
    name, erase_param value

  let union_args args1 args2 =
    (* [Misc.Stdlib.List.merge_map] of empty will iterate through the whole
       other list, so special-case that out here *)
    match args1, args2 with
    | [], args | args, [] -> args
    | _, _ ->
        Misc.Stdlib.List.merge_map args1 args2
          ~cmp:(fun (name1, _) (name2, _) -> compare name1 name2)
          ~left_only:(fun pair1 -> pair1)
          ~right_only:(fun pair2 -> pair2)
          ~both:
            (fun (param, value1) (_param, value2) ->
              Misc.fatal_errorf
                "Duplicate arguments for %a:@ %a@ vs. %a"
                print param
                print_param value1
                print_param value2)

  let rec subst ({ head; args; params } as param) (s : subst) =
    match Map.find_opt (erase_param param) s with
    | Some rhs -> rhs
    | None ->
        let matching_params, non_matching_params =
          List.partition_map
            (fun (name, value) ->
               match Map.find_opt (erase_param value) s with
               | Some rhs -> Left (name, rhs)
               | None -> Right (name, value))
            params
        in
        let args = List.map (fun pair -> subst_pair pair s) args in
        let params =
          List.map (fun pair -> subst_pair pair s) non_matching_params
        in
        let args = union_args args matching_params in
        { head; args; params }
  and subst_pair (name, value) s =
    name, subst value s

  let check s params =
    (* This could do more - say, check that the replacement (the argument) has
       all the parameters of the original (the parameter). (The subset rule
       requires this, since an argument has to refer to the parameter it
       implements, and thus the parameter's parameters must include the
       argument's parameters.) It would be redundant with the checks
       implemented elsewhere but could still be helpful. *)
    let param_set = List.map erase_param params |> Set.of_list in
    Set.subset (Map.keys s) param_set
end
