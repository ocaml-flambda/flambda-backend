(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Ident

type backend_var = t

let name_for_debugger t =
  let prefix = "*opt*" in
  let prefix_len = String.length prefix in
  let name = name t in
  if String.starts_with ~prefix name
     && String.length name > prefix_len
  then (String.sub name prefix_len (String.length name - prefix_len)) ^ "_opt"
  else name

let unique_name_for_debugger t =
  Printf.sprintf "%s/%d" (name_for_debugger t) (stamp t)

module Provenance = struct
  type t = {
    module_path : Path.t;
    location : Debuginfo.t;
    original_ident : Ident.t;
  }

  let print ppf { module_path; location; original_ident; } =
    let printf fmt = Format.fprintf ppf fmt in
    printf "@[<hov 1>(";
    printf "@[<hov 1>(module_path@ %a)@]@ "
      Path.print module_path;
    if !Clflags.locations then
      printf "@[<hov 1>(location@ %a)@]@ "
        Debuginfo.print_compact location;
    printf "@[<hov 1>(original_ident@ %a)@]"
      Ident.print original_ident;
    printf ")@]"

  let create ~module_path ~location ~original_ident =
    { module_path;
      location;
      original_ident;
    }

  let module_path t = t.module_path
  let location t = t.location
  let original_ident t = t.original_ident

  let equal t1 t2 = Stdlib.compare t1 t2 = 0
end

module With_provenance = struct
  type t =
    | Without_provenance of backend_var
    | With_provenance of {
        var : backend_var;
        provenance : Provenance.t;
      }

  let create ?provenance var =
    match provenance with
    | None -> Without_provenance var
    | Some provenance -> With_provenance { var; provenance; }

  let var t =
    match t with
    | Without_provenance var
    | With_provenance { var; provenance = _; } -> var

  let provenance t =
    match t with
    | Without_provenance _ -> None
    | With_provenance { var = _; provenance; } -> Some provenance

  let name t = name (var t)

  let rename t =
    let var = rename (var t) in
    match provenance t with
    | None -> Without_provenance var
    | Some provenance -> With_provenance { var; provenance; }

  let print ppf t =
    match provenance t with
    | None -> print ppf (var t)
    | Some provenance ->
      Format.fprintf ppf "%a[%a]"
        print (var t)
        Provenance.print provenance
end
