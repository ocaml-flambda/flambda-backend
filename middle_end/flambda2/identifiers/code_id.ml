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

(* CR mshinwell: Move to Reg_width_things so we can then do Code_id_or_symbol
   for free *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Id = Table_by_int_id.Id

module Code_id_data = struct
  type t =
    { compilation_unit : Compilation_unit.t;
      name : string;
      linkage_name : Linkage_name.t
    }

  let flags = 0

  let [@ocamlformat "disable"] print ppf { compilation_unit; name; linkage_name; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(linkage_name@ %a)@]@ \
        )@]"
      Compilation_unit.print compilation_unit
      name
      Linkage_name.print linkage_name

  let hash { compilation_unit; name = _; linkage_name } =
    Hashtbl.hash
      (Compilation_unit.hash compilation_unit, Linkage_name.hash linkage_name)

  let equal
      { compilation_unit = compilation_unit1;
        name = _;
        linkage_name = linkage_name1
      }
      { compilation_unit = compilation_unit2;
        name = _;
        linkage_name = linkage_name2
      } =
    Linkage_name.equal linkage_name1 linkage_name2
    && Compilation_unit.equal compilation_unit1 compilation_unit2
end

type t = Id.t

type exported = Code_id_data.t

module Table = Table_by_int_id.Make (Code_id_data)

let grand_table_of_code_ids = ref (Table.create ())

let initialise () = grand_table_of_code_ids := Table.create ()

let find_data t = Table.find !grand_table_of_code_ids t

let get_compilation_unit t = (find_data t).compilation_unit

let linkage_name t = (find_data t).linkage_name

let name t = (find_data t).name

let previous_name_stamp = ref (-1)

let create ~name compilation_unit =
  let name_stamp =
    (* CR mshinwell: check for overflow on 32 bit *)
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let linkage_name =
    let unique_name = Printf.sprintf "%s_%d" name name_stamp in
    let unit_linkage_name =
      Linkage_name.to_string
        (Compilation_unit.get_linkage_name compilation_unit)
    in
    Linkage_name.create (unit_linkage_name ^ "__" ^ unique_name ^ "_code")
  in
  let data : Code_id_data.t = { compilation_unit; name; linkage_name } in
  Table.add !grand_table_of_code_ids data

let rename t = create ~name:(name t) (Compilation_unit.get_current_exn ())

let in_compilation_unit t comp_unit =
  Compilation_unit.equal (get_compilation_unit t) comp_unit

let code_symbol t =
  let data = find_data t in
  Symbol.unsafe_create data.compilation_unit data.linkage_name

module T0 = struct
  let compare = Id.compare

  let equal = Id.equal

  let hash = Id.hash

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@<0>%s%a@<0>%s"
      (Flambda_colours.code_id ())
      Linkage_name.print (linkage_name t)
      (Flambda_colours.normal ())
end

include T0

module T = struct
  type nonrec t = t

  include T0
end

module Set = Patricia_tree.Make_set (struct
  let print = print
end)

module Map =
  Patricia_tree.Make_map
    (struct
      let print = print
    end)
    (Set)

module Tbl = Container_types.Make_tbl (Numeric_types.Int) (Map)
module Lmap = Lmap.Make (T)

let invert_map map =
  Map.fold
    (fun older newer invert_map -> Map.add newer older invert_map)
    map Map.empty

let export t = find_data t

let import (data : exported) = Table.add !grand_table_of_code_ids data

let map_compilation_unit f (data : exported) : exported =
  { data with compilation_unit = f data.compilation_unit }
