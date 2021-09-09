(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module RWC = Reg_width_const

include Reg_width_things.Simple

let const_bool b = const (if b then RWC.const_true else RWC.const_false)

let const_true = const_bool true
let const_false = const_bool false

let untagged_const_true = const RWC.untagged_const_true
let untagged_const_false = const RWC.untagged_const_false

let untagged_const_zero = const RWC.untagged_const_zero

let untagged_const_int i = const (RWC.untagged_const_int i)

let const_int i = const (RWC.const_int i)
let const_zero = const RWC.const_zero
let const_one = const RWC.const_one

let const_unit = const RWC.const_unit

let [@inline always] is_var t =
  pattern_match t
    ~name:(fun name ~coercion:_ -> Name.is_var name)
    ~const:(fun _ -> false)

let [@inline always] is_symbol t =
  pattern_match t
    ~name:(fun name ~coercion:_ -> Name.is_symbol name)
    ~const:(fun _ -> false)

let [@inline always] is_const t =
  pattern_match t ~name:(fun _ ~coercion:_ -> false) ~const:(fun _ -> true)

let is_imported_or_constant t =
  pattern_match t
    ~const:(fun _ -> true)
    ~name:(fun name ~coercion:_ -> Name.is_imported name)

let pattern_match' t ~var ~symbol ~const =
  pattern_match t ~const
    ~name:(fun name -> Name.pattern_match name ~var ~symbol)

let const_from_descr descr = const (RWC.of_descr descr)

let without_coercion t = pattern_match t ~name:(fun n ~coercion:_ -> name n) ~const

let apply_coercion t applied_coercion =
  if Coercion.is_id applied_coercion then Some t
  else
    Coercion.compose (coercion t) ~then_:applied_coercion
    |> Option.map (fun coercion -> with_coercion (without_coercion t) coercion)

let apply_coercion_exn t applied_coercion =
  match apply_coercion t applied_coercion with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Cannot@ apply@ coercion@ %a@ to@ %a"
      Coercion.print applied_coercion
      print t

(* CR mshinwell: Make naming consistent with [Name] re. the option type *)

(* CR mshinwell: Careful that coercions don't get dropped using the
   following *)

let [@inline always] must_be_var t =
  pattern_match t
    ~name:(fun name ~coercion ->
        Name.must_be_var_opt name
        |> Option.map (fun var -> var, coercion)
      )
    ~const:(fun _ -> None)

let [@inline always] must_be_symbol t =
  pattern_match t
    ~name:(fun name ~coercion ->
        Name.must_be_symbol_opt name
        |> Option.map (fun symbol -> symbol, coercion)
      )
    ~const:(fun _ -> None)

let [@inline always] must_be_name t =
  pattern_match t
    ~name:(fun name ~coercion -> Some (name, coercion))
    ~const:(fun _ -> None)

let free_names_with_mode t mode =
  pattern_match t
    ~name:(fun name ~coercion ->
        Name_occurrences.add_name (Coercion.free_names coercion) name mode)
    ~const:(fun _ -> Name_occurrences.empty)

let free_names t = free_names_with_mode t Name_mode.normal
let free_names_in_types t = free_names_with_mode t Name_mode.in_types

let apply_renaming t perm =
  Renaming.apply_simple perm t

module List = struct
  type nonrec t = t list

  include Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      Misc.Stdlib.List.compare compare t1 t2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (List.map hash t)

    let print ppf t =
      (Format.pp_print_list print ~pp_sep:Format.pp_print_space) ppf t

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names t =
    List.fold_left (fun free t ->
        Name_occurrences.union free (free_names t))
      (Name_occurrences.empty)
      t

  let apply_renaming t perm =
    let changed = ref false in
    let result =
      List.map (fun simple ->
          let simple' = apply_renaming simple perm in
          if not (simple == simple') then begin
            changed := true
          end;
          simple')
        t
    in
    if not !changed then t
    else result
end

module With_kind = struct
  type nonrec t = t * Flambda_kind.t

  include Container_types.Make (struct
    type nonrec t = t

    let compare (s1, k1) (s2, k2) =
      let c = compare s1 s2 in
      if c <> 0 then c
      else Flambda_kind.compare k1 k2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash (s, k) =
      Hashtbl.hash (hash s, Flambda_kind.hash k)

    let print ppf (s, k) =
      Format.fprintf ppf "@[(%a@ @<1>\u{2237}@ %a)@]"
        print s
        Flambda_kind.print k

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names (simple, _kind) = free_names simple

  let apply_renaming ((simple, kind) as t) perm =
    let simple' = apply_renaming simple perm in
    if simple == simple' then t
    else simple', kind
end
