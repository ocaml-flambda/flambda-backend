(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Pierrick Couderc, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-40-41-42"]

open! Int_replace_polymorphic_compare
module List = Misc.Stdlib.List
module String = Misc.Stdlib.String

type error =
  | Invalid_character of char * string
  | Bad_compilation_unit_name of string
  | Child_of_instance of { child_name : string }
  | Packed_instance of { name : string }
  | Already_an_instance of { name : string }

exception Error of error

module Name : sig
  type t

  include Identifiable.S with type t := t

  val dummy : t

  val predef_exn : t

  val of_string : string -> t

  val to_string : t -> string

  val of_head_of_global_name : Global_module.Name.t -> t

  val of_global_name_no_args_exn : Global_module.Name.t -> t

  val to_global_name : t -> Global_module.Name.t

  val check_as_path_component : t -> unit
end = struct
  (* Be VERY careful changing this. Anything not equivalent to [string] will
     require bumping magic numbers due to changes in file formats, in addition
     to breaking the (somewhat horrifying) invariant on
     [Cmm_helpers.globals_map]. Furthermore there are uses of polymorphic
     compare hidden in [List.mem], [List.assoc] etc. *)
  type t = string

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = String.compare

    let equal = String.equal

    let hash = Hashtbl.hash

    let print = String.print

    let output = Misc.output_of_print print
  end)

  let isupper chr = Char.equal (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.equal str ""
    then raise (Error (Bad_compilation_unit_name str))
    else str

  let of_head_of_global_name (name : Global_module.Name.t) = of_string name.head

  let of_global_name_no_args_exn (name : Global_module.Name.t) =
    match name.args with
    | [] -> of_head_of_global_name name
    | _ ->
      (* This is a wart. We should have a separate type
         [Global_module.Parameter_name.t] that is known not to have arguments,
         and then we can convert without runtime checks here. Note that we can't
         actually be specific in this message about why the thing isn't supposed
         to have arguments. *)
      Misc.fatal_errorf "Arguments not allowed in name:@ %a"
        Global_module.Name.print name

  let to_global_name t = Global_module.Name.create_no_args t

  (* This is so called (and separate from [of_string]) because we only want to
     check a name if it has a prefix. In particular, this allows single-module
     executables to have names like ".cinaps" that aren't valid module names. *)
  let check_as_path_component t =
    if String.length t < 1
       || (not (isupper (String.get t 0)))
       || String.contains t '.'
    then raise (Error (Bad_compilation_unit_name t))

  let dummy = "*dummy*"

  let predef_exn = "*predef*"

  let to_string t = t
end

module Prefix : sig
  type t

  include Identifiable.S with type t := t

  val parse_for_pack : string -> t

  val from_clflags : unit -> t

  val of_list : Name.t list -> t

  val to_list : t -> Name.t list

  val to_string : t -> string

  val empty : t

  val is_empty : t -> bool
end = struct
  (* As with [Name.t], changing this will change several file formats, requiring
     bumps of magic numbers. *)
  type t = Name.t list

  include Identifiable.Make (struct
    type nonrec t = t

    let equal = List.equal Name.equal

    let compare = List.compare Name.compare

    let hash = Hashtbl.hash

    let print ppf p =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ".")
        Name.print ppf p

    let output = Misc.output_of_print print
  end)

  let is_valid_character first_char c =
    match c with
    | 'A' .. 'Z' -> true
    | '_' | '0' .. '9' | 'a' .. 'z' -> not first_char
    | _ -> false

  let parse_for_pack pack =
    let prefix = String.split_on_char '.' pack in
    ListLabels.iter prefix ~f:(fun module_name ->
        String.iteri
          (fun i c ->
            if not (is_valid_character (i = 0) c)
            then raise (Error (Invalid_character (c, module_name))))
          module_name);
    ListLabels.map prefix ~f:Name.of_string

  let from_clflags () =
    match !Clflags.for_package with
    | None -> []
    | Some pack -> parse_for_pack pack

  let to_string p = Format.asprintf "%a" print p

  let empty = []

  let is_empty t = match t with [] -> true | _ :: _ -> false

  let of_list t = t

  let to_list t = t
end

module T0 : sig
  type descr = private
    { name : Name.t;
      for_pack_prefix : Prefix.t;
      arguments : argument list
    }

  and argument =
    { param : t;
      value : t
    }

  and t

  val descr : t -> descr

  val name : t -> Name.t

  val is_plain_name : t -> bool

  val for_pack_prefix : t -> Prefix.t

  val instance_arguments : t -> argument list

  val to_global_name : t -> Global_module.Name.t option

  val to_global_name_exn : t -> Global_module.Name.t

  val to_global_name_without_prefix : t -> Global_module.Name.t

  val create_full : Prefix.t -> Name.t -> argument list -> t

  val of_global_name : Global_module.Name.t -> t

  val compare : t -> t -> int
end = struct
  (* As with [Name.t], changing [descr] or [t] requires bumping magic
     numbers. *)
  type descr =
    { name : Name.t;
      for_pack_prefix : Prefix.t;
      arguments : argument list
    }

  and argument =
    { param : t;
      value : t
    }

  and full =
    | With_prefix of
        { name : Name.t;
          for_pack_prefix : Prefix.t
        }
    | Global of Global_module.Name.t

  (* type t = Bare_name of Name.t [@@unboxed] | With_prefix of with_prefix |
     Global of Global.Name.t *)
  and t = Obj.t

  (* Some manual inlining is done here to ensure good performance under
     Closure. *)

  let is_plain_name t =
    let tag = Obj.tag t in
    tag = Obj.string_tag

  let of_plain_name name : t = Obj.repr (name : Name.t)

  let of_full full : t = Obj.repr (full : full)

  let of_global_name (glob : Global_module.Name.t) =
    match glob with
    | { head; args = [] } -> of_plain_name (Name.of_string head)
    | _ -> of_full (Global glob)

  let convert_arguments l =
    ListLabels.map
      ~f:(fun ({ param; value } : Global_module.Name.argument) ->
        { param = of_global_name param; value = of_global_name value })
      l

  let descr t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag
    then
      { name = Sys.opaque_identity (Obj.obj t : Name.t);
        for_pack_prefix = Prefix.empty;
        arguments = []
      }
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix { name; for_pack_prefix } ->
        { name; for_pack_prefix; arguments = [] }
      | Global { head; args } ->
        let name = Name.of_string head in
        let arguments = convert_arguments args in
        { name; arguments; for_pack_prefix = Prefix.empty }

  let name t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag
    then Sys.opaque_identity (Obj.obj t : Name.t)
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix { name; _ } -> name
      | Global { head; _ } -> Name.of_string head

  let for_pack_prefix t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag
    then Prefix.empty
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix { for_pack_prefix; _ } -> for_pack_prefix
      | Global _ -> Prefix.empty

  let instance_arguments t =
    let tag = Obj.tag t in
    assert (tag < 2 || tag = Obj.string_tag);
    if tag = Obj.string_tag
    then []
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix _ -> []
      | Global { args; _ } -> convert_arguments args

  let rec compare t1 t2 =
    if t1 == t2
    then 0
    else
      let { for_pack_prefix = for_pack_prefix1;
            name = name1;
            arguments = args1
          } =
        descr t1
      in
      let { for_pack_prefix = for_pack_prefix2;
            name = name2;
            arguments = args2
          } =
        descr t2
      in
      let c = Name.compare name1 name2 in
      if c <> 0
      then c
      else
        let c = Prefix.compare for_pack_prefix1 for_pack_prefix2 in
        if c <> 0 then c else List.compare compare_instance_arg args1 args2

  and compare_instance_arg { param = param1; value = value1 }
      { param = param2; value = value2 } =
    let c = compare param1 param2 in
    if c <> 0 then c else compare value1 value2

  let compare_argument_by_name arg1 arg2 = compare arg1.param arg2.param

  let to_global_name_exn t =
    if is_plain_name t
    then
      let name = Sys.opaque_identity (Obj.obj t : Name.t) in
      Global_module.Name.create_no_args (Name.to_string name)
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix { name; _ } ->
        raise (Error (Packed_instance { name = name |> Name.to_string }))
      | Global glob -> glob

  let to_global_name t =
    try Some (to_global_name_exn t) with Error (Packed_instance _) -> None

  let to_global_name_without_prefix t =
    if is_plain_name t
    then
      let name = Sys.opaque_identity (Obj.obj t : Name.t) in
      Global_module.Name.create_no_args (Name.to_string name)
    else
      let full = Sys.opaque_identity (Obj.obj t : full) in
      match full with
      | With_prefix { name; _ } ->
        Global_module.Name.create_no_args (Name.to_string name)
      | Global glob -> glob

  let of_global_name (name : Global_module.Name.t) =
    match name with
    | { head; args = [] } -> of_plain_name (head |> Name.of_string)
    | _ -> of_full (Global name)

  let create_full for_pack_prefix name arguments =
    let empty_prefix = Prefix.is_empty for_pack_prefix in
    let empty_arguments = match arguments with [] -> true | _ -> false in
    let () =
      if not empty_prefix
      then (
        if not empty_arguments
        then
          (* CR-someday lmaurer: [for_pack_prefix] and [arguments] would make
             for better output but it doesn't seem worth moving both [error] and
             [print] to before this point *)
          raise (Error (Packed_instance { name = name |> Name.to_string }));
        Name.check_as_path_component name;
        ListLabels.iter ~f:Name.check_as_path_component
          (for_pack_prefix |> Prefix.to_list))
    in
    let arguments = ListLabels.sort arguments ~cmp:compare_argument_by_name in
    if empty_prefix && empty_arguments
    then of_plain_name name
    else if empty_prefix
    then
      let head = Name.to_string name in
      let arguments =
        ListLabels.map
          ~f:(fun { param; value } : Global_module.Name.argument ->
            { param = to_global_name_exn param;
              value = to_global_name_exn value
            })
          arguments
      in
      of_full (Global (Global_module.Name.create_exn head arguments))
    else of_full (With_prefix { for_pack_prefix; name })
end

include T0

let create prefix name = create_full prefix name []

let create_child parent name_ =
  if not (Prefix.is_empty (for_pack_prefix parent))
  then
    (* CR-someday lmaurer: Same as for [create_full] *)
    raise (Error (Child_of_instance { child_name = name_ |> Name.to_string }));
  let prefix =
    (for_pack_prefix parent |> Prefix.to_list) @ [name parent] |> Prefix.of_list
  in
  create prefix name_

let of_string str =
  let for_pack_prefix, name =
    (* Also see [Name.check_as_path_component] *)
    if String.equal str ".cinaps" || String.equal str "(.cinaps)"
    then Prefix.empty, Name.of_string str
    else
      match String.rindex_opt str '.' with
      | None -> Prefix.empty, Name.of_string str
      | Some _ ->
        Misc.fatal_errorf "[of_string] does not parse qualified names: %s" str
  in
  create for_pack_prefix name

let dummy = create Prefix.empty (Name.of_string "*none*")

let predef_exn = create Prefix.empty Name.predef_exn

let name_as_string t = name t |> Name.to_string

let equal_to_name t other_name =
  is_plain_name t && Name.equal other_name (name t)

let with_for_pack_prefix t for_pack_prefix =
  create_full for_pack_prefix (name t) (instance_arguments t)

let is_packed t = not (Prefix.is_empty (for_pack_prefix t))

include Identifiable.Make (struct
  type nonrec t = t

  let compare = compare

  let equal x y = if x == y then true else compare x y = 0

  let rec print fmt t =
    let { for_pack_prefix; name; arguments } = descr t in
    let () =
      if Prefix.is_empty for_pack_prefix
      then Format.fprintf fmt "%a" Name.print name
      else
        Format.fprintf fmt "%a.%a" Prefix.print for_pack_prefix Name.print name
    in
    ListLabels.iter ~f:(print_arg fmt) arguments

  and print_arg fmt { param; value } =
    Format.fprintf fmt "[%a:%a]" print param print value

  let output = Misc.output_of_print print

  let rec hash t =
    let { for_pack_prefix; name; arguments } = descr t in
    Hashtbl.hash
      ( Name.hash name,
        Prefix.hash for_pack_prefix,
        ListLabels.map ~f:hash_arg arguments )

  and hash_arg { param; value } = Hashtbl.hash (hash param, hash value)
end)

let full_path_as_string t =
  (* We take care not to break sharing when the prefix is empty. However we
     can't share in the case where there is a prefix. *)
  if Prefix.is_empty (for_pack_prefix t)
  then Name.to_string (name t)
  else Format.asprintf "%a" print t

let is_instance t =
  match instance_arguments t with [] -> false | _ :: _ -> true

let create_instance t arguments =
  let { for_pack_prefix; name; arguments = existing_arguments } = descr t in
  let () =
    match existing_arguments with
    | [] -> ()
    | _ :: _ ->
      raise (Error (Already_an_instance { name = full_path_as_string t }))
  in
  create_full for_pack_prefix name arguments

let split_instance_exn t =
  match descr t with
  | { arguments = []; _ } ->
    Misc.fatal_errorf "@[<hov 1>Not an instance:@ %a@]" print t
  | { name; for_pack_prefix; arguments } ->
    create_full for_pack_prefix name [], arguments

let full_path t = Prefix.to_list (for_pack_prefix t) @ [name t]

let flatten t =
  let rec flatten_arg { param; value } ~depth =
    assert (not (is_packed value));
    let param_name = name param in
    let { name; arguments; _ } = descr value in
    (depth, param_name, name)
    :: ListLabels.concat_map ~f:(flatten_arg ~depth:(depth + 1)) arguments
  in
  let { for_pack_prefix; name; arguments } = descr t in
  ( for_pack_prefix,
    name,
    ListLabels.concat_map ~f:(flatten_arg ~depth:0) arguments )

let base_filename t =
  (* This is a one-way function. Please don't parse anything out of it. Consider
     the following formatting details to be a state secret (shared only with
     Dune). *)
  let _prefix, name, arguments = flatten t in
  let arg_segments =
    ListLabels.map arguments ~f:(fun (depth, _param, value) ->
        (* Dropping the parameter names in the hopes of keeping the filenames
           _extremely_ long rather than _excruciatingly_ long. I will not be
           surprised if we decide that we're better off with the parameter names
           because the filenames are beyond hope anyway. *)
        String.make (depth + 1) '-' ^ (value |> Name.to_string))
  in
  String.concat "" ((name |> Name.to_string) :: arg_segments)

let is_parent t ~child =
  List.equal Name.equal (full_path t) (Prefix.to_list (for_pack_prefix child))

let is_strict_prefix list1 ~of_:list2 ~equal =
  (not (List.equal equal list1 list2)) && List.is_prefix list1 ~of_:list2 ~equal

let can_access_by_name t ~accessed_by:me =
  let my_path = full_path me in
  (* Criterion 1 in .mli *)
  let t's_prefix_is_my_ancestor =
    List.is_prefix
      (for_pack_prefix t |> Prefix.to_list)
      ~of_:my_path ~equal:Name.equal
  in
  (* Criterion 2 *)
  let t_is_not_my_strict_ancestor =
    not (is_strict_prefix (full_path t) ~of_:my_path ~equal:Name.equal)
  in
  t's_prefix_is_my_ancestor && t_is_not_my_strict_ancestor

let can_access_cmx_file = can_access_by_name

let which_cmx_file desired_comp_unit ~accessed_by : t =
  let desired_prefix = for_pack_prefix desired_comp_unit in
  if Prefix.is_empty desired_prefix
  then
    (* If the unit we're looking for is not in a pack, then the correct .cmx
       file is the one with the same name as the unit, irrespective of any
       current pack. *)
    desired_comp_unit
  else
    let () = assert (not (is_instance desired_comp_unit)) in
    (* This lines up the full paths as described above. *)
    let rec match_components ~current ~desired ~acc_rev =
      match current, desired with
      | current_name :: current, desired_name :: desired ->
        if Name.equal current_name desired_name
        then
          (* The full paths are equal up to the current point; keep going. *)
          let acc_rev = current_name :: acc_rev in
          match_components ~current ~desired ~acc_rev
        else
          (* The paths have diverged. The next component of the desired path is
             the .cmx file to load. *)
          acc_rev, desired_name
      | [], desired_name :: _desired ->
        (* The whole of the current unit's full path (including the name of the
           unit itself) is now known to be a prefix of the desired unit's pack
           *prefix*. This means we must be making a pack. The .cmx file to load
           is named after the next component of the desired unit's path (which
           may in turn be a pack). *)
        acc_rev, desired_name
      | [], [] ->
        (* The paths were equal, so the desired compilation unit is just the
           current one. *)
        acc_rev, name desired_comp_unit
      | _ :: _, [] ->
        (* The current path is longer than the desired unit's path, which means
           we're attempting to go back up the pack hierarchy. This is an
           error. *)
        Misc.fatal_errorf
          "Compilation unit@ %a@ is inaccessible when compiling compilation \
           unit@ %a"
          print desired_comp_unit print accessed_by
    in
    let prefix_rev, name =
      match_components ~current:(full_path accessed_by)
        ~desired:(full_path desired_comp_unit)
        ~acc_rev:[]
    in
    (* CR lmaurer: It's silly to be writing `ListLabels` out everywhere,
       especially here. *)
    create (ListLabels.rev prefix_rev |> Prefix.of_list) name

let print_name ppf t = Format.fprintf ppf "%a" Name.print (name t)

let to_global_ident_for_bytecode t =
  Ident.create_persistent (full_path_as_string t)

let print_debug ppf t =
  let name = name t in
  let for_pack_prefix = for_pack_prefix t in
  if Prefix.is_empty for_pack_prefix
  then Format.fprintf ppf "@[<hov 1>(@[<hov 1>(id@ %a)@])@]" Name.print name
  else
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(for_pack_prefix@ %a)@]@;@[<hov 1>(name@ %a)@]"
      Prefix.print for_pack_prefix Name.print name

let current = ref None

let set_current t_opt = current := t_opt

let get_current () = !current

let get_current_or_dummy () = Option.value !current ~default:dummy

let get_current_exn () =
  match !current with
  | Some t -> t
  | None -> Misc.fatal_error "No compilation unit set"

let is_current t = match !current with None -> false | Some t' -> equal t t'
