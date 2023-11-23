(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-30-40-41-42"]

module CU = Compilation_unit

type t = {
  compilation_unit : Compilation_unit.t;
  linkage_name : Linkage_name.t;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else
        (* Linkage names are unique across a whole project, so just comparing
           those is sufficient. *)
        Linkage_name.compare t1.linkage_name t2.linkage_name

  let equal t1 t2 = compare t1 t2 = 0
  let output chan t = Linkage_name.output chan t.linkage_name
  let hash { hash; } = hash

  (* CR mshinwell: maybe print all fields *)
  let print ppf t = Linkage_name.print ppf t.linkage_name
end)

let caml_symbol_prefix = "caml"

(* CR ocaml 5 runtime: Remove this_is_ocamlc and force_runtime4_symbols once
   fully on runtime5 *)
let this_is_ocamlc = ref false
let force_runtime4_symbols = ref false

let separator () =
  if !this_is_ocamlc then
    Misc.fatal_error "Didn't expect utils/symbol.ml to be used in ocamlc";
  if Config.runtime5 && not !force_runtime4_symbols then "." else "__"

let this_is_ocamlc () = this_is_ocamlc := true
let force_runtime4_symbols () = force_runtime4_symbols := true

let linkage_name t = t.linkage_name

let linkage_name_for_ocamlobjinfo t =
  (* For legacy compatibility, even though displaying "Foo.Bar" is nicer
     than "Foo__Bar" *)
  let linkage_name = linkage_name t |> Linkage_name.to_string in
  assert (Misc.Stdlib.String.begins_with linkage_name
            ~prefix:caml_symbol_prefix);
  let prefix_len = String.length caml_symbol_prefix in
  String.sub linkage_name prefix_len (String.length linkage_name - prefix_len)

let compilation_unit t = t.compilation_unit

(* CR-someday lmaurer: Would be nicer to have some of this logic in
   [Linkage_name]; among other things, we could then define
   [Linkage_name.for_current_unit] *)

let linkage_name_for_compilation_unit comp_unit =
  let name = CU.Name.to_string (CU.name comp_unit) in
  let for_pack_prefix = CU.for_pack_prefix comp_unit in
  let suffix =
    if CU.Prefix.is_empty for_pack_prefix then name
    else
      let pack_names =
        CU.Prefix.to_list for_pack_prefix |> List.map CU.Name.to_string
      in
      String.concat (separator ()) (pack_names @ [name])
  in
  caml_symbol_prefix ^ suffix
  |> Linkage_name.of_string

let for_predef_ident id =
  assert (Ident.is_predef id);
  let linkage_name = "caml_exn_" ^ Ident.name id |> Linkage_name.of_string in
  let compilation_unit = CU.predef_exn in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let unsafe_create compilation_unit linkage_name =
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name; }

let for_name compilation_unit name =
  let prefix =
    linkage_name_for_compilation_unit compilation_unit |> Linkage_name.to_string
  in
  let linkage_name =
    prefix ^ (separator ()) ^ name |> Linkage_name.of_string
  in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name; }

let for_local_ident id =
  assert (not (Ident.is_global_or_predef id));
  let compilation_unit = CU.get_current_exn () in
  for_name compilation_unit (Ident.unique_name id)

let for_compilation_unit compilation_unit =
  let linkage_name = linkage_name_for_compilation_unit compilation_unit in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let for_current_unit () =
  for_compilation_unit (CU.get_current_exn ())

let const_label = ref 0

let for_new_const_in_current_unit () =
  incr const_label;
  for_name (Compilation_unit.get_current_exn ()) (Int.to_string !const_label)

let is_predef_exn t =
  CU.equal t.compilation_unit CU.predef_exn
