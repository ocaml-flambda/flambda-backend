[@@@ocaml.warning "+a-40-41-42"]

let pp_concat pp ppf list =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp ppf list

module Name : sig
  type t = private {
    head : string;
    args : (t * t) list;
  }

  val create : string -> (t * t) list -> t

  val unsafe_create_unchecked : string -> (t * t) list -> t

  include Identifiable.S with type t := t
end = struct
  type t = {
    head : string;
    args : (t * t) list;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let rec compare
        ({ head = head1; args = args1 } as t1)
        ({ head = head2; args = args2 } as t2) =
      if t1 == t2 then 0
      else
        match String.compare head1 head2 with
        | 0 -> List.compare compare_arg args1 args2
        | c -> c
    and compare_arg (name1, arg1) (name2, arg2) =
      match compare name1 name2 with
      | 0 -> compare arg1 arg2
      | c -> c

    let equal t1 t2 = compare t1 t2 = 0

    let rec print ppf ({ head; args } : t) =
      Format.fprintf ppf "@[<hov 1>%s%a@]"
        head
        (pp_concat print_arg_pair) args
    and print_arg_pair ppf (name, arg) =
      Format.fprintf ppf "[%a:%a]" print name print arg

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head args =
    let sorted_args =
      List.sort_uniq (fun (name1, _) (name2, _) -> compare name1 name2) args
    in
    let t = { head; args = sorted_args } in
    if List.length args != List.length sorted_args then
      Misc.fatal_errorf "Names of instance arguments must be unique:@ %a"
        print t;
    t

  let unsafe_create_unchecked head args = { head; args }
end

let compare_arg_name (name1, _) (name2, _) = Name.compare name1 name2

module T0 : sig
  type t = private {
    head : string;
    args : (Name.t * t) list;
    params : (Name.t * t) list;
  }

  include Identifiable.S with type t := t

  val create : string -> (Name.t * t) list -> params:(Name.t * t) list -> t

  val to_name : t -> Name.t
end = struct
  type t = {
    head : string;
    args : (Name.t * t) list;
    params : (Name.t * t) list;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let rec compare
        ({ head = head1; args = args1; params = params1 } as t1)
        ({ head = head2; args = args2; params = params2 } as t2) =
      if t1 == t2 then 0
      else
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

    let equal t1 t2 = compare t1 t2 = 0

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

    let hash = Hashtbl.hash
  end)

  let create head args ~params =
    let args_sorted = List.sort compare_arg_name args in
    let params_sorted = List.sort compare_arg_name params in
    let t = { head; args = args_sorted; params = params_sorted } in
    if
      List.length args != List.length args_sorted
      || List.length params != List.length params_sorted
    then
      Misc.fatal_errorf "Names of arguments and parameters must be unique:@ %a"
        print t;
    t

  (* CR-someday lmaurer: Should try and make this unnecessary or at least cheap.
     Could do it by making [Name.t] an unboxed existential so that converting from
     [t] is the identity. Or just have [Name.t] wrap [t] and ignore [params]. *)
  let rec to_name ({ head; args; params = _ }) : Name.t =
    (* Safe because we already checked the names in this exact argument list *)
    Name.unsafe_create_unchecked head (List.map arg_to_name args)
  and arg_to_name (name, value) =
    name, to_name value
end

include T0

module Subst = Name.Map
type subst = t Subst.t

let rec subst0 (t : t) (s : subst) ~changed =
  match Subst.find_opt (to_name t) s with
  | Some rhs -> changed := true; rhs
  | None -> subst0_inside t s ~changed
and subst0_inside { head; args; params } s ~changed =
  let matching_params, non_matching_params =
    List.partition_map
      (fun ((name, value) as pair) ->
          match Subst.find_opt (to_name value) s with
          | Some rhs -> changed := true; Left (name, rhs)
          | None -> Right pair)
      params
  in
  let args = subst0_alist args s ~changed in
  let params = subst0_alist non_matching_params s ~changed in
  let args = List.merge compare_arg_name args matching_params in
  create head args ~params
and subst0_alist l s ~changed =
  List.map (fun (name, value) -> name, subst0 value s ~changed) l

let subst t s =
  let changed = ref false in
  let new_t = subst0 t s ~changed in
  if !changed then new_t else t

let subst_inside t s =
  let changed = ref false in
  let new_t = subst0 t s ~changed in
  if !changed then new_t else t

let check s params =
  (* This could do more - say, check that the replacement (the argument) has
      all the parameters of the original (the parameter). (The subset rule
      requires this, since an argument has to refer to the parameter it
      implements, and thus the parameter's parameters must include the
      argument's parameters.) It would be redundant with the checks
      implemented elsewhere but could still be helpful. *)
  let param_set = List.map to_name params |> Name.Set.of_list in
  Name.Set.subset (Name.Map.keys s) param_set
