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

include Int_ids.Name

let is_var t = pattern_match t ~var:(fun _ -> true) ~symbol:(fun _ -> false)

let is_symbol t = pattern_match t ~var:(fun _ -> false) ~symbol:(fun _ -> true)

let to_var t =
  pattern_match t ~var:(fun var -> Some var) ~symbol:(fun _ -> None)

let to_symbol t =
  pattern_match t ~var:(fun _ -> None) ~symbol:(fun symbol -> Some symbol)

let set_of_var_set vars =
  Variable.Set.fold (fun v t_set -> Set.add (var v) t_set) vars Set.empty

let set_of_symbol_set symbols =
  Symbol.Set.fold
    (fun sym t_set -> Set.add (symbol sym) t_set)
    symbols Set.empty

let set_to_var_set t =
  Set.fold
    (fun name vars ->
      match to_var name with
      | None -> vars
      | Some var -> Variable.Set.add var vars)
    t Variable.Set.empty

let set_to_symbol_set t =
  Set.fold
    (fun name syms ->
      match to_symbol name with
      | None -> syms
      | Some sym -> Symbol.Set.add sym syms)
    t Symbol.Set.empty

let compilation_unit t =
  pattern_match t
    ~var:(fun var -> Variable.compilation_unit var)
    ~symbol:(fun sym -> Symbol.compilation_unit sym)

let is_imported t =
  let current = Compilation_unit.get_current_exn () in
  not (Compilation_unit.equal current (compilation_unit t))

let must_be_var_opt t =
  pattern_match t ~var:(fun var -> Some var) ~symbol:(fun _ -> None)

let must_be_symbol t =
  pattern_match t
    ~var:(fun _ -> Misc.fatal_errorf "Must be a symbol:@ %a" print t)
    ~symbol:(fun sym -> sym)

let must_be_symbol_opt t =
  pattern_match t ~var:(fun _ -> None) ~symbol:(fun sym -> Some sym)

module Pair = struct
  include Container_types.Make_pair (Int_ids.Name) (Int_ids.Name)

  type nonrec t = t * t
end
