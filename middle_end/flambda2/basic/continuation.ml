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

[@@@ocaml.warning "+a-30-40-41-42"]

module Id = Table_by_int_id.Id

let continuation_flags = 0

let raise_count = ref 0

let next_raise_count () =
(*
if !raise_count = 52 then begin
Format.eprintf "Creation of continuation %d:\n%s\n%!"
  (!raise_count + 1)
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100))
end;
*)
  incr raise_count;
  !raise_count

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

  let [@ocamlformat "disable"] print ppf t = Format.pp_print_string ppf (to_string t)

  let equal t1 t2 =
    match t1, t2 with
    | Normal_or_exn, Normal_or_exn
    | Return, Return
    | Define_root_symbol, Define_root_symbol
    | Toplevel_return, Toplevel_return -> true
    | Normal_or_exn, _
    | Return, _
    | Define_root_symbol, _
    | Toplevel_return, _ -> false
end

module Data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    previous_compilation_units : Compilation_unit.t list;
    name : string;
    name_stamp : int;
    sort : Sort.t;
  }

  let flags = continuation_flags

  let [@ocamlformat "disable"] print ppf { compilation_unit; name; name_stamp; sort;
                  previous_compilation_units = _; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        @[<hov 1>(sort@ %a)@]\
        )@]"
      Compilation_unit.print compilation_unit
      name
      name_stamp
      Sort.print sort

  let hash { compilation_unit; previous_compilation_units;
             name = _; name_stamp; sort = _; } =
    Hashtbl.hash (List.map Compilation_unit.hash
                    (compilation_unit :: previous_compilation_units),
                  name_stamp)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { compilation_unit = compilation_unit1; name_stamp = name_stamp1;
            previous_compilation_units = previous_compilation_units1;
            name = _; sort = _
          } = t1
      in
      let { compilation_unit = compilation_unit2; name_stamp = name_stamp2;
            previous_compilation_units = previous_compilation_units2;
            name = _; sort = _
          } = t2
      in
      let rec previous_compilation_units_match l1 l2 =
        match l1, l2 with
        | [], [] -> true
        | [], _ :: _ | _ :: _, [] -> false
        | unit1 :: tl1, unit2 :: tl2 ->
          Compilation_unit.equal unit1 unit2
          && previous_compilation_units_match tl1 tl2
      in
      Int.equal name_stamp1 name_stamp2
        && Compilation_unit.equal compilation_unit1 compilation_unit2
        && previous_compilation_units_match
             previous_compilation_units1
             previous_compilation_units2
end

type t = Id.t
type exported = Data.t

module Table = Table_by_int_id.Make(Data)
let grand_table_of_continuations = ref (Table.create ())

let initialise () = grand_table_of_continuations := Table.create ()

(* CR mshinwell: Document why this uses [next_raise_count].  Does it need
   to?  It would be better if it didn't. *)
let create ?sort ?name () : t =
  let sort = Option.value sort ~default:Sort.Normal_or_exn in
  let name = Option.value name ~default:"k" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let previous_compilation_units = [] in
  let name_stamp = next_raise_count () in
  let data : Data.t =
    { compilation_unit; previous_compilation_units; name; name_stamp; sort }
  in
  Table.add !grand_table_of_continuations data

let find_data t = Table.find !grand_table_of_continuations t

let rename t =
  let { Data.name; sort; name_stamp = _; compilation_unit = _;
        previous_compilation_units = _;
      } = find_data t
  in
  create ~sort ~name ()

let name t = (find_data t).name

let name_stamp t = (find_data t).name_stamp

let sort t = (find_data t).sort

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    Id.compare t1 t2

  let equal t1 t2 =
    t1 == t2

  let hash t =
    Hashtbl.hash t

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.continuation ());
    if String.equal (name t) "k"
    then Format.fprintf ppf "k%d" (name_stamp t)
    else Format.fprintf ppf "%s/%d" (name t) (name_stamp t);
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

module Set = Patricia_tree.Make_set (struct let print = print end)
module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
(* CR mshinwell: The [Tbl]s will still print integers! *)
module Tbl = Container_types.Make_tbl (Numeric_types.Int) (Map)

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

let export t = find_data t

let import data = Table.add !grand_table_of_continuations data

let map_compilation_unit f (data : Data.t) : Data.t =
  let new_compilation_unit = f data.compilation_unit in
  if Compilation_unit.equal new_compilation_unit data.compilation_unit
  then data
  else
    { data with compilation_unit = new_compilation_unit;
                previous_compilation_units =
                  data.compilation_unit :: data.previous_compilation_units;
    }

module With_args = struct
  type nonrec t = t * Variable.t list

  include Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      let c = compare (fst t1) (fst t2) in
      if c <> 0 then c
      else Variable.compare_lists (snd t1) (snd t2)

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (hash (fst t),
        List.map Variable.hash (snd t))

    let [@ocamlformat "disable"] print ppf (cont, vars) =
      Format.fprintf ppf "@[(%a, %a)@]"
        print cont
        Variable.print_list vars

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)
end
