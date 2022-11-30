(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Consistency tables: for checking consistency of module CRCs *)

open Misc

module Make (Module_name : sig
  type t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Tbl : Hashtbl.S with type key = t
  val compare : t -> t -> int
end) (Data : sig
  type t
end) = struct
  type t = (Data.t * Digest.t * filepath) Module_name.Tbl.t

  let create () = Module_name.Tbl.create 13

  let clear = Module_name.Tbl.clear

  exception Inconsistency of {
    unit_name : Module_name.t;
    inconsistent_source : string;
    original_source : string;
    inconsistent_data : Data.t;
    original_data : Data.t;
  }

  exception Not_available of Module_name.t

  let check_ tbl name data crc source =
    let (old_data, old_crc, old_source) = Module_name.Tbl.find tbl name in
    if not (Digest.equal crc old_crc)
    then raise(Inconsistency {
        unit_name = name;
        inconsistent_source = source;
        original_source = old_source;
        inconsistent_data = data;
        original_data = old_data;
      })

  let check tbl name data crc source =
    try check_ tbl name data crc source
    with Not_found ->
      Module_name.Tbl.add tbl name (data, crc, source)

  let check_noadd tbl name data crc source =
    try check_ tbl name data crc source
    with Not_found ->
      raise (Not_available name)

  let set tbl name data crc source = Module_name.Tbl.add tbl name (data, crc, source)

  let source tbl name = thd3 (Module_name.Tbl.find tbl name)

  let find t name =
    match Module_name.Tbl.find t name with
    | exception Not_found -> None
    | (data, crc, _) -> Some (data, crc)

  let extract l tbl =
    let l = List.sort_uniq Module_name.compare l in
    List.fold_left (fun assc name -> (name, find tbl name) :: assc) [] l

  let extract_map mod_names tbl =
    Module_name.Set.fold
      (fun name result -> Module_name.Map.add name (find tbl name) result)
      mod_names
      Module_name.Map.empty

  let filter p tbl =
    let to_remove = ref [] in
    Module_name.Tbl.iter
      (fun name _ ->
        if not (p name) then to_remove := name :: !to_remove)
      tbl;
    List.iter
      (fun name ->
         while Module_name.Tbl.mem tbl name do
           Module_name.Tbl.remove tbl name
         done)
      !to_remove
end
