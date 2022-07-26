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
  | Invalid_character of char
  | Bad_compilation_unit_name of string

exception Error of error

(* CR mshinwell: add sig here to hide "= string" *)
module Name : sig
  type t
  include Identifiable.S with type t := t
  val dummy : t
  val of_string : string -> t
  val to_string : t -> string
  val check_as_path_component : t -> unit
end = struct
  type t = string

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = String.compare
    let equal = String.equal
    let hash = Hashtbl.hash
    let print = String.print
    let output chan t = print (Format.formatter_of_out_channel chan) t
  end)

  let isupper chr =
    Char.equal (Char.uppercase_ascii chr) chr

  let of_string str = str

  (* This is so called (and separate from [of_string]) because we only want to
     check a name if it has a prefix. In particular, this allows single-module
     executables to have names like ".cinaps" that aren't valid module names. *)
  let check_as_path_component t =
    if String.length t < 1
      || not (isupper (String.get t 0))
      || String.contains t '.'
    then raise (Error (Bad_compilation_unit_name t))

  let dummy = ""

  let to_string t = t
end

module Prefix : sig
  type t
  include Identifiable.S with type t := t
  val parse_for_pack : string option -> t
  val to_list : t -> Name.t list
  val to_string : t -> string
  val empty : t
  val is_empty : t -> bool
end = struct
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

    let output chan t = print (Format.formatter_of_out_channel chan) t
  end)

  let is_valid_character first_char c =
    let code = Char.code c in
    if first_char then
      code >= 65 && code <= 90 (* [A-Z] *)
    else
      Char.equal c '_'
      || code >= 48 && 57 <= 90 (* [0-9] *)
      || code >= 65 && code <= 90 (* [A-Z] *)
      || code >= 97 && code <= 122 (* [a-z] *)

  let parse pack =
    let prefix = String.split_on_char '.' pack in
    ListLabels.iter prefix ~f:(fun module_name ->
      String.iteri (fun i c ->
          if not (is_valid_character (i=0) c) then
            raise (Error (Invalid_character c)))
        module_name);
    ListLabels.map prefix ~f:Name.of_string

  let parse_for_pack = function
    | None -> []
    | Some pack -> parse pack

  let to_string p =
    Format.asprintf "%a" print p

  let empty = []

  let is_empty t =
    match t with
    | [] -> true
    | _::_ -> false

  let to_list t = t
end

type t = {
  name : Name.t;
  for_pack_prefix : Prefix.t;
  hash : int;
}

let create ?(for_pack_prefix = Prefix.empty) name =
  if not (Prefix.is_empty for_pack_prefix) then begin
    Name.check_as_path_component name;
    ListLabels.iter ~f:Name.check_as_path_component
      (for_pack_prefix |> Prefix.to_list)
  end;
  { name;
    for_pack_prefix;
    hash = Hashtbl.hash (name, for_pack_prefix)
  }

let of_string str =
  let for_pack_prefix, name =
    match String.rindex_opt str '.' with
    | None -> Prefix.empty, Name.of_string str
    | Some pos ->
        Prefix.parse_for_pack (Some (String.sub str 0 (pos+1))),
        Name.of_string (String.sub str (pos+1) (String.length str - pos - 1))
  in
  create ~for_pack_prefix name

let dummy = create (Name.of_string "*none*")

let predef_exn = create (Name.of_string "*predef*")

let name t = t.name

let for_pack_prefix t = t.for_pack_prefix

let with_for_pack_prefix t for_pack_prefix = { t with for_pack_prefix; }

let is_packed t = not (Prefix.is_empty t.for_pack_prefix)

let full_path t =
  (Prefix.to_list t.for_pack_prefix) @ [ t.name ]

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ name = name1; for_pack_prefix = for_pack_prefix1;
           hash = hash1; _} as t1)
        ({ name = name2; for_pack_prefix = for_pack_prefix2;
           hash = hash2; _} as t2) =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare hash1 hash2 in
      if c <> 0 then c
      else
        let c = Name.compare name1 name2 in
        if c <> 0 then c
        else Prefix.compare for_pack_prefix1 for_pack_prefix2

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf { for_pack_prefix; hash = _; name } =
    if Prefix.is_empty for_pack_prefix then
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %a)@])@]"
        Name.print name
    else
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(for_pack_prefix@ %a)@]@;\
          @[<hov 1>(name@ %a)@]"
        Prefix.print for_pack_prefix
        Name.print name

  let output oc t =
    print (Format.formatter_of_out_channel oc) t

  let hash t = t.hash
end)

let print_name ppf t =
  Format.fprintf ppf "%a" Name.print t.name

let print_full_path fmt t =
  if Prefix.is_empty t.for_pack_prefix then
    Format.fprintf fmt "%a" Name.print t.name
  else
    Format.fprintf fmt "%a.%a"
      Prefix.print t.for_pack_prefix
      Name.print t.name

let full_path_as_string t =
  Format.asprintf "%a" print_full_path t

let current = ref None

let set_current t =
  current := Some t

let get_current_exn () =
  match !current with
  | Some t -> t
  | None -> Misc.fatal_error "No compilation unit set"

let is_current t =
  match !current with
  | None -> false
  | Some t' -> equal t t'
