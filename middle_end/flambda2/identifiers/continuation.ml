(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Id = Table_by_int_id.Id

let continuation_flags = 0

let next_stamp =
  let next_stamp = ref 0 in
  fun () ->
    incr next_stamp;
    !next_stamp

module Sort = struct
  type t =
    | Normal_or_exn
    | Return
    | Define_root_symbol
    | Toplevel_return

  let to_string t =
    match t with
    | Normal_or_exn -> "Normal_or_exn"
    | Return -> "Return"
    | Define_root_symbol -> "Define_root_symbol"
    | Toplevel_return -> "Toplevel_return"

  let print ppf t = Format.pp_print_string ppf (to_string t)

  let equal t1 t2 =
    match t1, t2 with
    | Normal_or_exn, Normal_or_exn
    | Return, Return
    | Define_root_symbol, Define_root_symbol
    | Toplevel_return, Toplevel_return ->
      true
    | Normal_or_exn, _ | Return, _ | Define_root_symbol, _ | Toplevel_return, _
      ->
      false
end

module Data = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      name_stamp : int;
      sort : Sort.t
    }

  let flags = continuation_flags

  let [@ocamlformat "disable"] print ppf
      { compilation_unit; name; name_stamp; sort; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        @[<hov 1>(sort@ %a)@]\
        )@]"
      Compilation_unit.print_debug compilation_unit
      name
      name_stamp
      Sort.print sort

  let hash { compilation_unit; name = _; name_stamp; sort = _ } =
    Hashtbl.hash (Compilation_unit.hash compilation_unit, name_stamp)

  let equal t1 t2 =
    if t1 == t2
    then true
    else
      let { compilation_unit = compilation_unit1;
            name_stamp = name_stamp1;
            name = _;
            sort = _
          } =
        t1
      in
      let { compilation_unit = compilation_unit2;
            name_stamp = name_stamp2;
            name = _;
            sort = _
          } =
        t2
      in
      Int.equal name_stamp1 name_stamp2
      && Compilation_unit.equal compilation_unit1 compilation_unit2
end

type t = Id.t

type exported = Data.t

module Table = Table_by_int_id.Make (Data)

let grand_table_of_continuations = ref (Table.create ())

let initialise () = grand_table_of_continuations := Table.create ()

let reset () = initialise ()

let create ?sort ?name () : t =
  let sort = Option.value sort ~default:Sort.Normal_or_exn in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let name_stamp = next_stamp () in
  let name =
    let default =
      if Flambda_features.debug_flambda2 ()
      then Format.asprintf "k%d" name_stamp
      else "k"
    in
    Option.value name ~default
  in
  let data : Data.t = { compilation_unit; name; name_stamp; sort } in
  Table.add !grand_table_of_continuations data

let find_data t = Table.find !grand_table_of_continuations t

let rename t =
  let { Data.name; sort; name_stamp = _; compilation_unit = _ } = find_data t in
  create ~sort ~name ()

let is_renamed_version_of t t' =
  let data = find_data t in
  let data' = find_data t' in
  Sort.equal data.sort data'.sort && String.equal data.name data'.name

let name t = (find_data t).name

let name_stamp t = (find_data t).name_stamp

let sort t = (find_data t).sort

module T0 = struct
  let compare t1 t2 = Id.compare t1 t2

  let equal t1 t2 = Id.equal t1 t2

  let hash t = Hashtbl.hash t

  let print ppf t =
    Format.fprintf ppf "%t" Flambda_colours.continuation;
    if String.equal (name t) "k"
    then Format.fprintf ppf "k%d" (name_stamp t)
    else Format.fprintf ppf "%s/%d" (name t) (name_stamp t);
    Format.fprintf ppf "%t" Flambda_colours.pop
end

include T0

module T = struct
  type nonrec t = t

  include T0
end

module Tree = Patricia_tree.Make (T)
module Set = Tree.Set
module Map = Tree.Map
module Lmap = Lmap.Make (T)

let export t = find_data t

let import data = Table.add !grand_table_of_continuations data
