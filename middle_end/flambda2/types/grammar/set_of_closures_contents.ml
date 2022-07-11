(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { closures : Function_slot.Set.t;
    value_slots : Value_slot.Set.t
  }

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf { closures; value_slots; } =
    Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(closures@ %a)@]@ \
          @[<hov 1>(value_slots@ %a)@]\
          )@]"
      Function_slot.Set.print closures
      Value_slot.Set.print value_slots

  let compare { closures = closures1; value_slots = value_slots1 }
      { closures = closures2; value_slots = value_slots2 } =
    let c = Function_slot.Set.compare closures1 closures2 in
    if c <> 0 then c else Value_slot.Set.compare value_slots1 value_slots2

  let equal t1 t2 = compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"
end)

let create closures value_slots = { closures; value_slots }

let closures t = t.closures

let value_slots t = t.value_slots

let subset { closures = closures1; value_slots = value_slots1 }
    { closures = closures2; value_slots = value_slots2 } =
  Function_slot.Set.subset closures1 closures2
  && Value_slot.Set.subset value_slots1 value_slots2

let union { closures = closures1; value_slots = value_slots1 }
    { closures = closures2; value_slots = value_slots2 } =
  let closures = Function_slot.Set.union closures1 closures2 in
  let value_slots = Value_slot.Set.union value_slots1 value_slots2 in
  { closures; value_slots }

let inter { closures = closures1; value_slots = value_slots1 }
    { closures = closures2; value_slots = value_slots2 } =
  let closures = Function_slot.Set.inter closures1 closures2 in
  let value_slots = Value_slot.Set.inter value_slots1 value_slots2 in
  { closures; value_slots }

let apply_renaming { closures; value_slots } renaming =
  let value_slots =
    Value_slot.Set.filter
      (fun var -> Renaming.value_slot_is_used renaming var)
      value_slots
  in
  { closures; value_slots }

let free_names { closures = _; value_slots } =
  Value_slot.Set.fold
    (fun value_slot free_names ->
      Name_occurrences.add_value_slot_in_types free_names value_slot)
    value_slots Name_occurrences.empty

let remove_unused_value_slots { closures; value_slots } ~used_value_slots =
  let value_slots =
    Value_slot.Set.filter
      (fun var ->
        (not
           (Value_slot.in_compilation_unit var
              (Compilation_unit.get_current_exn ())))
        || Value_slot.Set.mem var used_value_slots)
      value_slots
  in
  { closures; value_slots }

module With_function_slot = struct
  type nonrec t = Function_slot.t * t

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf (function_slot, contents) =
      Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
        Function_slot.print function_slot
        print contents

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare (function_slot1, contents1) (function_slot2, contents2) =
      let c = Function_slot.compare function_slot1 function_slot2 in
      if c <> 0 then c else compare contents1 contents2

    let equal t1 t2 = compare t1 t2 = 0
  end)
end

module With_function_slot_or_unknown = struct
  type nonrec t = Function_slot.t Or_unknown.t * t

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf (function_slot_or_unknown, contents) =
      Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
        (Or_unknown.print Function_slot.print) function_slot_or_unknown
        print contents

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare (function_slot1, contents1) (function_slot2, contents2) =
      let c =
        Or_unknown.compare Function_slot.compare function_slot1 function_slot2
      in
      if c <> 0 then c else compare contents1 contents2

    let equal t1 t2 = compare t1 t2 = 0
  end)
end
