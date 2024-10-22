[@@@ocaml.warning "+a-40-41-42"]

module Parameter = struct
  type t = string

  let create t = t

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

type duplicate = | Duplicate of Parameter.t

module Argument = struct
  type 'value t = {
    param : Parameter.t;
    value : 'value;
  }

  let param t = t.param

  let compare cmp_value
        { param = param1; value = value1 }
        { param = param2; value = value2 } =
    match Parameter.compare param1 param2 with
    | 0 -> cmp_value value1 value2
    | c -> c
end

let check_uniqueness_of_sorted get_param l  =
  let rec loop n1 l =
    match l with
    | [] -> Ok ()
    | x :: l ->
      let n2 = get_param x in
      if Parameter.compare n1 n2 = 0 then
        Error (Duplicate n1)
      else
        loop n2 l
  in
  match l  with
  | [] -> Ok ()
  | x :: l -> loop (get_param x) l

let sort_and_check_uniqueness get_param l   =
  let l = List.stable_sort (fun arg1 arg2 -> Parameter.compare (get_param arg1) (get_param arg2)) l in
  check_uniqueness_of_sorted get_param l  |> Result.map (fun () -> l)

let check_uniqueness_of_merged (type v)
      (visible : v Argument.t list) hidden =
  let open Argument in
  let exception Found_duplicate of duplicate in
  match
    Misc.Stdlib.List.merge_iter visible hidden
      ~cmp:(fun vis hid -> Parameter.compare vis.param hid)
      ~left_only:ignore
      ~right_only:ignore
      ~both:(fun { param = name; _} _ ->
          raise (Found_duplicate (Duplicate name )))
  with
  | () -> Ok ()
  | exception Found_duplicate dup -> Error dup

module Name : sig
  type t = private {
    head : string;
    args : argument list;
  }
  and argument = t Argument.t

  val create : string -> argument list -> (t, duplicate) Result.t

  val create_exn : string -> argument list -> t

  val create_no_args : string -> t

  val unsafe_create_unchecked : string -> argument list -> t

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
      Format.fprintf ppf "[%a:%a]" Parameter.print name print arg

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head args =
    sort_and_check_uniqueness Argument.param args
    |> Result.map (fun args -> { head; args })

  let create_exn head args =
    match create head args with
    | Ok t -> t
    | Error (Duplicate _) ->
      Misc.fatal_errorf "Names of instance arguments must be unique:@ %a"
        print { head; args }

  let create_no_args head = create_exn head []

  let unsafe_create_unchecked head args = { head; args }

  let to_string = print |> Misc.to_string_of_print
end

let compare_arg_name arg1 arg2 =
   let open Argument in
   Parameter.compare arg1.param arg2.param

(* let rec list_similar f list1 list2 =
  match list1, list2 with
  | [], [] -> true
  | a :: list1, b :: list2 -> f a b && list_similar f list1 list2
  | (_ :: _), [] | [], (_ :: _) -> false *)

module T0 : sig
  type t = private {
    head : string;
    visible_args : argument list;
    hidden_args : Parameter.t list;
  }
  and argument = t Argument.t

  include Identifiable.S with type t := t

  val create
     : string
    -> argument list
    -> hidden_args:Parameter.t list
    -> (t, duplicate) Result.t

  val create_exn
     : string
    -> argument list
    -> hidden_args:Parameter.t list
    -> t

  val to_name : t -> Name.t
end = struct
  type t = {
    head : string;
    visible_args : argument list;
    hidden_args : Parameter.t list;
  }
  and argument = t Argument.t

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
            | 0 -> List.compare Parameter.compare hidden_args1 hidden_args2
            | c -> c
          end
        | c -> c
    and compare_pairs arg1 arg2 = Argument.compare compare arg1 arg2

    let equal t1 t2 = compare t1 t2 = 0

    (* let rec equal_looking t name =
      let { head; visible_args; hidden_args } = t in
      let { Name.head = name_head; args = name_args } = name in
      hidden_args = []
      && String.equal head name_head
      && list_similar equal_looking_args visible_args name_args
    and equal_looking_args
          ({ param = name1; value = value1 } : argument)
          ({ param = name2; value = value2 } : Name.argument) =
      Parameter.equal name1 name2 && equal_looking value1 value2 *)

    let rec print ppf { head; visible_args; hidden_args } =
      Format.fprintf ppf "@[<hov 1>%s%a%a@]"
        head
        (pp_concat print_visible_pair) visible_args
        (pp_concat print_hidden_pair) hidden_args
    and print_visible_pair ppf ({ param = name; value } : argument) =
      Format.fprintf ppf "[%a:%a]" Parameter.print name print value
    and print_hidden_pair ppf (name : Parameter.t) =
      Format.fprintf ppf "{%a}" Parameter.print name

    let output = print |> Misc.output_of_print

    let hash = Hashtbl.hash
  end)

  let create head visible_args ~hidden_args =
    let (let*) = Result.bind in
    let* visible_args = sort_and_check_uniqueness Argument.param visible_args in
    let* hidden_args = sort_and_check_uniqueness Fun.id hidden_args in
    let* () = check_uniqueness_of_merged visible_args hidden_args in
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


module Subst = Parameter.Map
type subst = t Subst.t

let rec subst0 {head; visible_args; hidden_args} (s : subst) ~changed =
  match Subst.find_opt head s with
  | Some rhs ->
    begin match visible_args, hidden_args with
    | [], [] -> changed := true; rhs
    | _, _ -> Misc.fatal_error "ill-formed global module"
    end
  | None ->
      let visible_args, hidden_args = subst0_args visible_args hidden_args s ~changed in
      create_exn head visible_args ~hidden_args
and subst0_args visible_args hidden_args s ~changed =
  let matching_hidden_args, non_matching_hidden_args =
    List.partition_map
      (fun name ->
          match Subst.find_opt name s with
          | Some rhs ->
            changed := true;
            Left ({ param = name; value = rhs } : argument)
          | None -> Right name)
      hidden_args
  in
  let visible_args = subst0_visible_args visible_args s ~changed in
  let visible_args =
    List.merge compare_arg_name visible_args matching_hidden_args
  in
  visible_args, non_matching_hidden_args
and subst0_visible_args l s ~changed =
  List.map
    (fun (arg : argument) -> { arg with value = subst0 arg.value s ~changed })
  l

let subst t s =
  let changed = ref false in
  let new_t = subst0 t s ~changed in
  if !changed then new_t, `Changed else t, `Did_not_change

let rec is_complete t =
  let open Argument in
  match t.hidden_args with
  | [] -> List.for_all (fun { value; _ } -> is_complete value) t.visible_args
  | _ -> false

let has_arguments t =
  match t with
  | { head = _; visible_args = []; hidden_args = [] } -> false
  | _ -> true
