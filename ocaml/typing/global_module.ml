[@@@ocaml.warning "+a-40-41-42"]

let pp_concat pp ppf list =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp ppf list

type ('name, 'value) duplicate =
  | Duplicate of { name : 'name; value1 : 'value; value2 : 'value }

module Argument = struct
  type ('param, 'value) t = {
    param : 'param;
    value : 'value;
  }

  let compare cmp_param cmp_value
        { param = param1; value = value1 }
        { param = param2; value = value2 } =
    match cmp_param param1 param2 with
    | 0 -> cmp_value value1 value2
    | c -> c
end

let check_uniqueness_of_sorted l ~cmp =
  let rec loop n1 v1 l =
    match (l : (_, _) Argument.t list) with
    | [] -> Ok ()
    | { param = n2; value = v2 } :: l ->
      if cmp n1 n2 = 0 then
        Error (Duplicate { name = n1; value1 = v1; value2 = v2 })
      else
        loop n2 v2 l
  in
  match (l : (_, _) Argument.t list) with
  | [] -> Ok ()
  | { param = n1; value = v1 } :: l -> loop n1 v1 l

let sort_and_check_uniqueness l ~cmp =
  let open Argument in
  let l = List.stable_sort (fun arg1 arg2 -> cmp arg1.param arg2.param) l in
  check_uniqueness_of_sorted l ~cmp |> Result.map (fun () -> l)

module Name : sig
  type t = private {
    head : string;
    args : argument list;
  }
  and argument = (t, t) Argument.t

  val create : string -> argument list -> (t, (t, t) duplicate) Result.t

  val create_exn : string -> argument list -> t

  val unsafe_create_unchecked : string -> argument list -> t

  val to_string : t -> string

  include Identifiable.S with type t := t
end = struct
  type t = {
    head : string;
    args : argument list;
  }
  and argument = (t, t) Argument.t

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
    and compare_arg arg1 arg2 = Argument.compare compare compare arg1 arg2

    let equal t1 t2 = compare t1 t2 = 0

    let rec print ppf ({ head; args } : t) =
      match args with
      | [] ->
          (* Preserve simple non-wrapping behaviour in atomic case *)
          Format.fprintf ppf "%s" head
      | _ ->
          Format.fprintf ppf "@[<hov 1>%s%a@]"
            head
            (pp_concat print_arg_pair) args
    and print_arg_pair ppf ({ param = name; value = arg } : argument) =
      Format.fprintf ppf "[%a:%a]" print name print arg

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head args =
    sort_and_check_uniqueness args ~cmp:compare
    |> Result.map (fun args -> { head; args })

  let create_exn head args =
    match create head args with
    | Ok t -> t
    | Error (Duplicate _) ->
      Misc.fatal_errorf "Names of instance arguments must be unique:@ %a"
        print { head; args }

  let unsafe_create_unchecked head args = { head; args }

  let to_string = print |> Misc.to_string_of_print
end

let compare_arg_name arg1 arg2 =
   let open Argument in
   Name.compare arg1.param arg2.param

let rec list_similar f list1 list2 =
  match list1, list2 with
  | [], [] -> true
  | a :: list1, b :: list2 -> f a b && list_similar f list1 list2
  | (_ :: _), [] | [], (_ :: _) -> false

module T0 : sig
  type t = private {
    head : string;
    visible_args : argument list;
    hidden_args : argument list;
  }
  and argument = (Name.t, t) Argument.t

  include Identifiable.S with type t := t

  val create
     : string
    -> argument list
    -> hidden_args:argument list
    -> (t, (Name.t, t) duplicate) Result.t

  val create_exn
     : string
    -> argument list
    -> hidden_args:argument list
    -> t

  val to_name : t -> Name.t
end = struct
  type t = {
    head : string;
    visible_args : argument list;
    hidden_args : argument list;
  }
  and argument = (Name.t, t) Argument.t

  include Identifiable.Make (struct
    type nonrec t = t

    let rec compare
        ({ head = head1; visible_args = visible_args1; hidden_args = hidden_args1 } as t1)
        ({ head = head2; visible_args = visible_args2; hidden_args = hidden_args2 } as t2) =
      if t1 == t2 then 0
      else
        match String.compare head1 head2 with
        | 0 -> begin
            match List.compare compare_pairs visible_args1 visible_args2 with
            | 0 -> List.compare compare_pairs hidden_args1 hidden_args2
            | c -> c
          end
        | c -> c
    and compare_pairs arg1 arg2 = Argument.compare Name.compare compare arg1 arg2

    let equal t1 t2 = compare t1 t2 = 0

    let rec equal_looking t name =
      let { head; visible_args; hidden_args } = t in
      let { Name.head = name_head; args = name_args } = name in
      hidden_args = []
      && String.equal head name_head
      && list_similar equal_looking_args visible_args name_args
    and equal_looking_args
          ({ param = name1; value = value1 } : argument)
          ({ param = name2; value = value2 } : Name.argument) =
      Name.equal name1 name2 && equal_looking value1 value2

    let rec print ppf { head; visible_args; hidden_args } =
      Format.fprintf ppf "@[<hov 1>%s%a%a@]"
        head
        (pp_concat print_visible_pair) visible_args
        (pp_concat print_hidden_pair) hidden_args
    and print_visible_pair ppf ({ param = name; value } : argument) =
      Format.fprintf ppf "[%a:%a]" Name.print name print value
    and print_hidden_pair ppf ({ param = name; value } : argument) =
      if equal_looking value name then
        Format.fprintf ppf "{%a}" Name.print name
      else
        Format.fprintf ppf "{%a:%a}" Name.print name print value

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head visible_args ~hidden_args =
    let (let*) = Result.bind in
    let* visible_args = sort_and_check_uniqueness visible_args ~cmp:Name.compare in
    let* hidden_args = sort_and_check_uniqueness hidden_args ~cmp:Name.compare in
    (* This should check that visible and hidden args don't overlap.
       Fortunately, we don't ever parse these directly from the user, so there
       isn't much chance of an overlap actually happening. *)
    Ok { head; visible_args; hidden_args }

  let create_exn head visible_args ~hidden_args =
    match create head visible_args ~hidden_args with
    | Ok t -> t
    | Error (Duplicate _) ->
      Misc.fatal_errorf "Names of arguments and parameters must be unique:@ %a"
        print { head; visible_args; hidden_args }

  (* CR-someday lmaurer: Should try and make this unnecessary or at least cheap.
     Could do it by making [Name.t] an unboxed existential so that converting from
     [t] is the identity. Or just have [Name.t] wrap [t] and ignore [hidden_args]. *)
  let rec to_name ({ head; visible_args; hidden_args = _ }) : Name.t =
    (* Safe because we already checked the names in this exact argument list *)
    Name.unsafe_create_unchecked head (List.map arg_to_name visible_args)
  and arg_to_name ({ param = name; value } : argument) : Name.argument =
    { param = name; value = to_name value }
end

include T0

let to_string = print |> Misc.to_string_of_print

let all_args t = t.visible_args @ t.hidden_args

module Subst = Name.Map
type subst = t Subst.t

let rec subst0 (t : t) (s : subst) ~changed =
  match Subst.find_opt (to_name t) s with
  | Some rhs -> changed := true; rhs
  | None -> subst0_inside t s ~changed
and subst0_inside { head; visible_args; hidden_args } s ~changed =
  let matching_hidden_args, non_matching_hidden_args =
    List.partition_map
      (fun (({ param = name; value } : argument) as pair) ->
          match Subst.find_opt (to_name value) s with
          | Some rhs ->
            changed := true;
            Left ({ param = name; value = rhs } : argument)
          | None -> Right pair)
      hidden_args
  in
  let visible_args = subst0_alist visible_args s ~changed in
  let hidden_args = subst0_alist non_matching_hidden_args s ~changed in
  let visible_args =
    List.merge compare_arg_name visible_args matching_hidden_args
  in
  create_exn head visible_args ~hidden_args
and subst0_alist l s ~changed =
  List.map
    (fun (arg : argument) -> { arg with value = subst0 arg.value s ~changed })
  l

let subst t s =
  let changed = ref false in
  let new_t = subst0 t s ~changed in
  if !changed then new_t, `Changed else t, `Did_not_change

let subst_inside t s =
  let changed = ref false in
  let new_t = subst0_inside t s ~changed in
  if !changed then new_t else t

let check s args =
  (* This could do more - say, check that the replacement (the argument) has
      all the parameters of the original (the parameter). (The subset rule
      requires this, since an argument has to refer to the parameter it
      implements, and thus the parameter's parameters must include the
      argument's parameters.) It would be redundant with the checks
      implemented elsewhere but could still be helpful. *)
  let param_set = List.map to_name args |> Name.Set.of_list in
  Name.Set.subset (Name.Map.keys s) param_set

let rec is_complete t =
  let open Argument in
  match t.hidden_args with
  | [] -> List.for_all (fun { value; _ } -> is_complete value) t.visible_args
  | _ -> false

let has_arguments t =
  match t with
  | { head = _; visible_args = []; hidden_args = [] } -> false
  | _ -> true
