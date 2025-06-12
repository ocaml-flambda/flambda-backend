module Parameter_name = struct
  type t = string

  let of_string t = t

  let to_string t = t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = String.compare

    let equal a b = compare a b = 0

    let print = Format.pp_print_string

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)
end

let pp_concat pp ppf list =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp ppf list

type 'value duplicate =
  | Duplicate of { name : Parameter_name.t; value1 : 'value; value2 : 'value }

module Argument = struct
  type 'value t = {
    param : Parameter_name.t;
    value : 'value;
  }

  let compare cmp_value
        ({ param = param1; value = value1 } as t1)
        ({ param = param2; value = value2 } as t2) =
    if t1 == t2 then 0 else
      match Parameter_name.compare param1 param2 with
      | 0 -> cmp_value value1 value2
      | c -> c

  let compare_by_param t1 t2 = Parameter_name.compare t1.param t2.param
end

let check_uniqueness_of_sorted l =
  let rec loop n1 v1 l =
    match (l : _ Argument.t list) with
    | [] -> Ok ()
    | { param = n2; value = v2 } :: l ->
      if Parameter_name.compare n1 n2 = 0 then
        Error (Duplicate { name = n1; value1 = v1; value2 = v2 })
      else
        loop n2 v2 l
  in
  match (l : _ Argument.t list) with
  | [] -> Ok ()
  | { param = n1; value = v1 } :: l -> loop n1 v1 l

let sort_and_check_uniqueness l =
  let l = List.stable_sort Argument.compare_by_param l in
  check_uniqueness_of_sorted l |> Result.map (fun () -> l)

let check_uniqueness_of_merged (type v) l1 l2 =
  let open Argument in
  let exception Found_duplicate of v duplicate in
  match
    Misc.Stdlib.List.merge_iter l1 l2
      ~cmp:Argument.compare_by_param
      ~left_only:ignore
      ~right_only:ignore
      ~both:(fun { param = name; value = value1 } { value = value2; _ } ->
          raise (Found_duplicate (Duplicate { name; value1; value2 })))
  with
  | () -> Ok ()
  | exception Found_duplicate dup -> Error dup

module Name : sig
  type t = private {
    head : string;
    args : argument list;
  }
  and argument = t Argument.t

  val create : string -> argument list -> (t, t duplicate) Result.t

  val create_exn : string -> argument list -> t

  val create_no_args : string -> t

  val of_parameter_name : Parameter_name.t -> t

  val unsafe_create_unchecked : string -> argument list -> t

  val find_in_parameter_map : t -> 'a Parameter_name.Map.t -> 'a option

  val mem_parameter_set : t -> Parameter_name.Set.t -> bool

  val to_string : t -> string

  include Identifiable.S with type t := t
end = struct
  type t = {
    head : string;
    args : argument list;
  }
  and argument = t Argument.t

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

    and compare_arg arg1 arg2 = Argument.compare compare arg1 arg2

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
      Format.fprintf ppf "[%a:%a]" Parameter_name.print name print arg

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head args =
    sort_and_check_uniqueness args
    |> Result.map (fun args -> { head; args })

  let create_exn head args =
    match create head args with
    | Ok t -> t
    | Error (Duplicate _) ->
      Misc.fatal_errorf "Names of instance arguments must be unique:@ %a"
        print { head; args }

  let create_no_args head = create_exn head []

  let of_parameter_name param = create_no_args param

  let unsafe_create_unchecked head args = { head; args }

  let unsafe_to_parameter_name_opt t =
    (* Only safe for use as a lookup key, since it might actually not be a
       parameter name *)
    match t with
    | { head; args = [] } -> Some head
    | _ -> None

  let find_in_parameter_map t map =
    match unsafe_to_parameter_name_opt t with
    | Some param -> Parameter_name.Map.find_opt param map
    | None -> None

  let mem_parameter_set t set =
    match unsafe_to_parameter_name_opt t with
    | Some param -> Parameter_name.Set.mem param set
    | None -> false

  let to_string = print |> Misc.to_string_of_print
end

module T0 : sig
  type t = private {
    head : string;
    visible_args : argument list;
    (* CR-someday lmaurer: Could just be the parameter names *)
    hidden_args : argument list;
  }

  and argument = t Argument.t

  include Identifiable.S with type t := t

  val create
     : string
    -> argument list
    -> hidden_args:Parameter_name.t list
    -> (t, t duplicate) Result.t

  val create_exn
     : string
    -> argument list
    -> hidden_args:Parameter_name.t list
    -> t

  val to_name : t -> Name.t

  val unsafe_create_unchecked
     : string
    -> argument list
    -> hidden_args:argument list
    -> t
end = struct
  type t = {
    head : string;
    visible_args : argument list;
    hidden_args : argument list;
  }
  and argument = t Argument.t

  let rec print ppf { head; visible_args; hidden_args } =
    let hidden_args =
      (* Assume the value is just the name (because it is) *)
      List.map (fun ({ param; value = _ } : argument) -> param) hidden_args
    in
    print_syntax ppf ~head ~visible_args ~hidden_args
  and print_syntax ppf ~head ~visible_args ~hidden_args =
    Format.fprintf ppf "@[<hov 1>%s%a%a@]"
      head
      (pp_concat print_visible_pair) visible_args
      (pp_concat print_hidden_pair) hidden_args
  and print_visible_pair ppf ({ param = name; value } : argument) =
    Format.fprintf ppf "[%a:%a]" Parameter_name.print name print value
  and print_hidden_pair ppf name =
    Format.fprintf ppf "{%a}" Parameter_name.print name

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

    and compare_pairs arg1 arg2 = Argument.compare compare arg1 arg2

    let equal t1 t2 = compare t1 t2 = 0

    let print = print

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let of_parameter_name param = { head = param; hidden_args = []; visible_args = [] }

  let create head visible_args ~hidden_args =
    let hidden_args =
      List.map
        (fun param -> Argument.{ param; value = of_parameter_name param })
        hidden_args
    in
    let (let*) = Result.bind in
    let* visible_args = sort_and_check_uniqueness visible_args in
    let* hidden_args = sort_and_check_uniqueness hidden_args in
    let* () = check_uniqueness_of_merged visible_args hidden_args in
    Ok { head; visible_args; hidden_args }

  let create_exn head visible_args ~hidden_args =
    match create head visible_args ~hidden_args with
    | Ok t -> t
    | Error (Duplicate _) ->
      Misc.fatal_errorf "Names of arguments and parameters must be unique:@ %a"
        (fun ppf () -> print_syntax ppf ~head ~visible_args ~hidden_args) ()

  let unsafe_create_unchecked head visible_args ~hidden_args =
    { head; visible_args; hidden_args }

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

module Subst = Parameter_name.Map
type subst = t Subst.t

let find_in_parameter_map t map =
  match t with
  | { head; visible_args = []; hidden_args = []; } ->
    Parameter_name.Map.find_opt head map
  | _ -> None

let rec subst0 (t : t) (s : subst) ~changed =
  match find_in_parameter_map t s with
  | Some rhs -> changed := true; rhs
  | None -> subst0_inside t s ~changed
and subst0_inside { head; visible_args; hidden_args } s ~changed =
  let matching_hidden_args, non_matching_hidden_args =
    List.partition_map
      (fun (({ param = name; value } : argument) as pair) ->
          match find_in_parameter_map value s with
          | Some rhs ->
            changed := true;
            Left ({ param = name; value = rhs } : argument)
          | None -> Right pair)
      hidden_args
  in
  let visible_args = subst0_alist visible_args s ~changed in
  let visible_args =
    List.merge Argument.compare_by_param visible_args matching_hidden_args
  in
  let hidden_args =
    (* Don't bother substituting: these never have deeper structure *)
    non_matching_hidden_args
  in
  (* The [List.merge] preserved sorting so everything must still be valid *)
  unsafe_create_unchecked head visible_args ~hidden_args
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

let check s params =
  (* This could do more - say, check that the replacement (the argument) has
      all the parameters of the original (the parameter). (The subset rule
      requires this, since an argument has to refer to the parameter it
      implements, and thus the parameter's parameters must include the
      argument's parameters.) It would be redundant with the checks
      implemented elsewhere but could still be helpful. *)
  let param_set = Parameter_name.Set.of_list params in
  Parameter_name.Set.subset (Parameter_name.Map.keys s) param_set

let rec is_complete t =
  let open Argument in
  match t.hidden_args with
  | [] -> List.for_all (fun { value; _ } -> is_complete value) t.visible_args
  | _ -> false

let has_arguments t =
  match t with
  | { head = _; visible_args = []; hidden_args = [] } -> false
  | _ -> true

module Precision = struct
  type t = Exact | Approximate

  let print ppf = function
    | Exact -> Format.fprintf ppf "exact"
    | Approximate -> Format.fprintf ppf "approx"

  let output = Misc.output_of_print print

  let equal t1 t2 =
    match t1, t2 with
    | Exact, Exact
    | Approximate, Approximate -> true
    | (Exact | Approximate), _ -> false
end

module With_precision = struct
  type nonrec t = t * Precision.t

  let print ppf (t, prec) =
    match (prec : Precision.t) with
    | Exact -> print ppf t
    | Approximate -> Format.fprintf ppf "@[<hv 2>%a@ (approx)@]" print t

  let output = Misc.output_of_print print

  exception Inconsistent

  let meet_atom equal atom1 atom2 =
    if not (equal atom1 atom2) then raise Inconsistent

  let meet_approximate glob1 glob2 =
    (* Compute the meet, assuming the visible parts are equal *)
    let rec meet glob1 glob2 =
      let visible_args_rev =
        Misc.Stdlib.List.merge_fold glob1.visible_args glob2.visible_args
          ~cmp:Argument.compare_by_param
          ~init:[]
          ~left_only:(fun _ _ -> raise Inconsistent)
          ~right_only:(fun _ _ -> raise Inconsistent)
          ~both:(fun acc_rev arg1 arg2 -> meet_args arg1 arg2 :: acc_rev)
      in
      let hidden_args_rev =
        (* Keep only the hidden arguments that appear in both lists *)
        Misc.Stdlib.List.merge_fold glob1.hidden_args glob2.hidden_args
          ~cmp:Argument.compare_by_param
          ~init:[]
          ~left_only:(fun acc_rev _ -> acc_rev)
          ~right_only:(fun acc_rev _ -> acc_rev)
          ~both:(fun acc_rev arg1 arg2 -> meet_args arg1 arg2 :: acc_rev)
      in
      meet_atom String.equal glob1.head glob2.head;
      let visible_args = List.rev visible_args_rev in
      let hidden_args = List.rev hidden_args_rev in
      unsafe_create_unchecked glob1.head visible_args ~hidden_args
    and meet_args (arg1 : _ Argument.t) (arg2 : _ Argument.t) =
      meet_atom Parameter_name.equal arg1.param arg2.param;
      let value = meet arg1.value arg2.value in
      ({ param = arg1.param; value } : _ Argument.t)
    in
    meet glob1 glob2

  let meet (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | (glob1, Approximate), (glob2, Approximate) ->
        (meet_approximate glob1 glob2, Approximate)
    | (glob1, Exact), (glob2, Exact) ->
        begin match equal glob1 glob2 with
        | true -> t1
        | false -> raise Inconsistent
        end
    | ((exact, Exact) as t_exact), (approx, Approximate)
    | (approx, Approximate), ((exact, Exact) as t_exact) ->
        let exact' = meet_approximate exact approx in
        begin match equal exact exact' with
        | true -> t_exact
        | false -> raise Inconsistent
        end

  let equal (t1, prec1) (t2, prec2) =
    equal t1 t2 && Precision.equal prec1 prec2
end
