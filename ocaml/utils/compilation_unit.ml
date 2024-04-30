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

exception Error of error

(* CR-someday lmaurer: Move this to [Identifiable] and change /all/ definitions
   of [output] that delegate to [print] to use it. Yes, they're all broken. *)
let output_of_print print =
  let output out_channel t =
    let ppf = Format.formatter_of_out_channel out_channel in
    print ppf t;
    (* Must flush the formatter immediately because it has a buffer separate
       from the output channel's buffer *)
    Format.pp_print_flush ppf ()
  in
  output

module Name : sig
  type t

  include Identifiable.S with type t := t

  val dummy : t

  val predef_exn : t

  val of_string : string -> t

  val to_string : t -> string

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

    let output = output_of_print print
  end)

  let isupper chr = Char.equal (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.equal str ""
    then raise (Error (Bad_compilation_unit_name str))
    else str

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

    let output = output_of_print print
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
  type t

  val for_pack_prefix_and_name : t -> Prefix.t * Name.t

  val name : t -> Name.t

  val for_pack_prefix : t -> Prefix.t

  val create : Prefix.t -> Name.t -> t
end = struct
  (* As with [Name.t], changing [with_prefix] or [t] requires bumping magic
     numbers. *)
  type with_prefix =
    { name : Name.t;
      for_pack_prefix : Prefix.t
    }

  (* type t = Without_prefix of Name.t [@@unboxed] | With_prefix of
     with_prefix *)
  type t = Obj.t

  (* Some manual inlining is done here to ensure good performance under
     Closure. *)

  let for_pack_prefix_and_name t =
    let tag = Obj.tag t in
    assert (tag = 0 || tag = Obj.string_tag);
    if tag <> 0
    then Prefix.empty, Sys.opaque_identity (Obj.obj t : Name.t)
    else
      let with_prefix = Sys.opaque_identity (Obj.obj t : with_prefix) in
      with_prefix.for_pack_prefix, with_prefix.name

  let name t =
    let tag = Obj.tag t in
    assert (tag = 0 || tag = Obj.string_tag);
    if tag <> 0
    then Sys.opaque_identity (Obj.obj t : Name.t)
    else
      let with_prefix = Sys.opaque_identity (Obj.obj t : with_prefix) in
      with_prefix.name

  let for_pack_prefix t =
    let tag = Obj.tag t in
    assert (tag = 0 || tag = Obj.string_tag);
    if tag <> 0
    then Prefix.empty
    else
      let with_prefix = Sys.opaque_identity (Obj.obj t : with_prefix) in
      with_prefix.for_pack_prefix

  let create for_pack_prefix name =
    let empty_prefix = Prefix.is_empty for_pack_prefix in
    let () =
      if not empty_prefix
      then (
        Name.check_as_path_component name;
        ListLabels.iter ~f:Name.check_as_path_component
          (for_pack_prefix |> Prefix.to_list))
    in
    if empty_prefix
    then Sys.opaque_identity (Obj.repr name)
    else Sys.opaque_identity (Obj.repr { for_pack_prefix; name })
end

include T0

let create_child parent name_ =
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

let with_for_pack_prefix t for_pack_prefix = create for_pack_prefix (name t)

let is_packed t = not (Prefix.is_empty (for_pack_prefix t))

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2
    then 0
    else
      let for_pack_prefix1, name1 = for_pack_prefix_and_name t1 in
      let for_pack_prefix2, name2 = for_pack_prefix_and_name t2 in
      let c = Name.compare name1 name2 in
      if c <> 0 then c else Prefix.compare for_pack_prefix1 for_pack_prefix2

  let equal x y = if x == y then true else compare x y = 0

  let print fmt t =
    let for_pack_prefix, name = for_pack_prefix_and_name t in
    if Prefix.is_empty for_pack_prefix
    then Format.fprintf fmt "%a" Name.print name
    else Format.fprintf fmt "%a.%a" Prefix.print for_pack_prefix Name.print name

  let output = output_of_print print

  let hash t =
    let for_pack_prefix, name = for_pack_prefix_and_name t in
    Hashtbl.hash (Name.hash name, Prefix.hash for_pack_prefix)
end)

let full_path t = Prefix.to_list (for_pack_prefix t) @ [name t]

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

let full_path_as_string t =
  (* We take care not to break sharing when the prefix is empty. However we
     can't share in the case where there is a prefix. *)
  if Prefix.is_empty (for_pack_prefix t)
  then Name.to_string (name t)
  else Format.asprintf "%a" print t

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
