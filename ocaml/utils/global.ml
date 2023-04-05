[@@@ocaml.warning "+a-40-41-42"]

let pp_concat pp ppf list =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp ppf list

module Name = struct
  type t = {
    head : string;
    args : (t * t) list;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let rec compare
        { head = head1; args = args1 }
        { head = head2; args = args2 } =
      match String.compare head1 head2 with
      | 0 -> List.compare compare_arg args1 args2
      | c -> c
    and compare_arg (name1, arg1) (name2, arg2) =
      match compare name1 name2 with
      | 0 -> compare arg1 arg2
      | c -> c

    let equal t1 t2 = t1 == t2 || compare t1 t2 = 0

    let rec print ppf ({ head; args } : t) =
      Format.fprintf ppf "@[<hov 1>%s%a@]"
        head
        (pp_concat print_arg_pair) args
    and print_arg_pair ppf (name, arg) =
      Format.fprintf ppf "[%a:%a]" print name print arg

    let output = print |> Misc.output_of_print

    let rec hash { head; args } =
      Hashtbl.hash (head, List.map hash_arg args)
    and hash_arg (name, param) =
      Hashtbl.hash (hash name, hash param)
  end)
end

type t = {
  head : string;
  args : (Name.t * t) list;
  params : (Name.t * t) list;
}

include Identifiable.Make (struct
  type nonrec t = t

  let rec compare
      { head = head1; args = args1; params = params1 }
      { head = head2; args = args2; params = params2 } =
    match String.compare head1 head2 with
    | 0 -> begin
        match List.compare compare_pairs args1 args2 with
        | 0 -> List.compare compare_pairs params1 params2
        | c -> c
      end
    | c -> c
  and compare_pairs (param1, value1) (param2, value2) =
    match Name.compare param1 param2 with
    | 0 -> compare value1 value2
    | c -> c

  let equal t1 t2 = t1 == t2 || compare t1 t2 = 0

  let rec print ppf { head; args; params } =
    Format.fprintf ppf "@[<hov 1>%s%a%a@]"
      head
      (pp_concat print_arg_pair) args
      (pp_concat print_param_pair) params
  and print_arg_pair ppf (name, value) =
    Format.fprintf ppf "[%a:%a]" Name.print name print value
  and print_param_pair ppf (name, value) =
    Format.fprintf ppf "{%a:%a}" Name.print name print value

  let output = print |> Misc.output_of_print

  let rec hash { head; args; params } =
    Hashtbl.hash (head, List.map hash_pair args, List.map hash_pair params)
  and hash_pair (param, value) =
    Hashtbl.hash (Name.hash param, hash value)
end)

module Subst = Name.Map
type subst = t Subst.t

(* CR-someday lmaurer: Should try and make this unnecessary or at least cheap.
   Could do it by making [Name.t] an unboxed existential so that converting from
   [t] is the identity. Or just have [Name.t] wrap [t] and ignore [params]. *)
let rec to_name ({ head; args; params = _ }) : Name.t =
  { head; args = List.map arg_to_name args }
and arg_to_name (name, value) =
  name, to_name value

let union_args args1 args2 =
  (* [Misc.Stdlib.List.merge_map] of empty will iterate through the whole
      other list, so special-case that out here *)
  match args1, args2 with
  | [], args | args, [] -> args
  | _, _ ->
      Misc.Stdlib.List.merge_map args1 args2
        ~cmp:(fun (name1, _) (name2, _) -> Name.compare name1 name2)
        ~left_only:(fun pair1 -> pair1)
        ~right_only:(fun pair2 -> pair2)
        ~both:
          (fun (param, value1) (_param, value2) ->
            Misc.fatal_errorf
              "Duplicate arguments for %a:@ %a@ vs. %a"
              Name.print param
              print value1
              print value2)

let rec subst param (s : subst) =
  match Subst.find_opt (to_name param) s with
  | Some rhs -> rhs
  | None -> subst_inside param s
and subst_inside { head; args; params } (s : subst) =
  let matching_params, non_matching_params =
    List.partition_map
      (fun (name, value) ->
         match Subst.find_opt (to_name value) s with
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
  let param_set = List.map to_name params |> Name.Set.of_list in
  Name.Set.subset (Name.Map.keys s) param_set
