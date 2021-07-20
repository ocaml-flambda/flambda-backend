(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module String = struct
  include String
  module Map = Map.Make (String)
end

module One_cache = struct
  type t = {
    prefix : string;
    mutable next_id : int;
    mutable printed : (string * Obj.t) list;
  }

  let create prefix =
    { prefix;
      next_id = 0;
      printed = [];
    }

  let with_cache t ppf obj printer =
    let obj = Obj.repr obj in
    let rec find = function
      | (name, obj')::printed ->
        if obj == obj' then Format.fprintf ppf "*%s" name
        else find printed
      | [] ->
        let name = Printf.sprintf "%s%d" t.prefix t.next_id in
        t.next_id <- t.next_id + 1;
        t.printed <- (name, obj) :: t.printed;
        Format.fprintf ppf "@[<hv 1>&%s =@ %a@]" name printer ()
    in
    find t.printed
end

type t = {
  mutable by_prefix : One_cache.t String.Map.t;
}

let create () = {
  by_prefix = String.Map.empty;
}

let with_cache t ppf prefix obj printer =
  match String.Map.find prefix t.by_prefix with
  | exception Not_found ->
    let cache = One_cache.create prefix in
    t.by_prefix <- String.Map.add prefix cache t.by_prefix;
    One_cache.with_cache cache ppf obj printer
  | cache ->
    One_cache.with_cache cache ppf obj printer
