(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  { name : string;
    linkage_name : Linkage_name.t;
    hash : int
  }

let string_for_printing t = t.name

(* Multiple units can have the same [id] if they come from different packs. To
   distinguish these we also keep the linkage name, which contains the name of
   the pack. *)
let compare v1 v2 =
  if v1 == v2
  then 0
  else
    let c = v1.hash - v2.hash in
    if c <> 0
    then c
    else if c = 0
    then
      (* We don't need to compare [name], [linkage_name] has everything. *)
      Linkage_name.compare v1.linkage_name v2.linkage_name
    else c

let equal t1 t2 = compare t1 t2 = 0

include Container_types.Make (struct
  type nonrec t = t

  let compare = compare

  let equal = equal

  let print ppf t = Format.pp_print_string ppf (string_for_printing t)

  let hash x = x.hash
end)

let create ~name linkage_name =
  { name; linkage_name; hash = Linkage_name.hash linkage_name }

let get_persistent_ident t = Ident.create_persistent t.name

let name t = t.name

let get_linkage_name t = t.linkage_name

let current = ref None

let set_current t = current := Some t

let get_current () = !current

let get_current_exn () =
  match !current with
  | Some current -> current
  | None ->
    Misc.fatal_error
      "Compilation_unit.get_current_exn: current compilation unit not set"

let is_current cu =
  match !current with None -> false | Some current -> equal cu current

let predefined_exception_t =
  create ~name:".predef_exn" (Linkage_name.create ".predef_exn")

let predefined_exception () = predefined_exception_t

let is_predefined_exception t = equal t predefined_exception_t

let external_symbols_t = create ~name:".extern" (Linkage_name.create ".extern")

let external_symbols () = external_symbols_t

let is_external_symbols t = equal t external_symbols_t
