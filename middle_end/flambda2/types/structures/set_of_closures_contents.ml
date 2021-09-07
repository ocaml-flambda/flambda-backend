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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  { closures : Closure_id.Set.t; closure_vars : Var_within_closure.Set.t }

include Container_types.Make (struct
  type nonrec t = t

  let [@ocamlformat "disable"] print ppf { closures; closure_vars; } =
    Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(closures@ %a)@]@ \
          @[<hov 1>(closure_vars@ %a)@]\
          )@]"
      Closure_id.Set.print closures
      Var_within_closure.Set.print closure_vars

  let compare { closures = closures1; closure_vars = closure_vars1 }
      { closures = closures2; closure_vars = closure_vars2 } =
    let c = Closure_id.Set.compare closures1 closures2 in
    if c <> 0
    then c
    else Var_within_closure.Set.compare closure_vars1 closure_vars2

  let equal t1 t2 = compare t1 t2 = 0

  let output _ _ = Misc.fatal_error "Not yet implemented"

  let hash _ = Misc.fatal_error "Not yet implemented"
end)

let create closures closure_vars = { closures; closure_vars }

let closures t = t.closures

let closure_vars t = t.closure_vars

let subset { closures = closures1; closure_vars = closure_vars1 }
    { closures = closures2; closure_vars = closure_vars2 } =
  Closure_id.Set.subset closures1 closures2
  && Var_within_closure.Set.subset closure_vars1 closure_vars2

let union { closures = closures1; closure_vars = closure_vars1 }
    { closures = closures2; closure_vars = closure_vars2 } =
  let closures = Closure_id.Set.union closures1 closures2 in
  let closure_vars = Var_within_closure.Set.union closure_vars1 closure_vars2 in
  { closures; closure_vars }

let inter { closures = closures1; closure_vars = closure_vars1 }
    { closures = closures2; closure_vars = closure_vars2 } =
  let closures = Closure_id.Set.inter closures1 closures2 in
  let closure_vars = Var_within_closure.Set.inter closure_vars1 closure_vars2 in
  { closures; closure_vars }

let apply_renaming { closures; closure_vars } renaming =
  let closure_vars =
    Var_within_closure.Set.filter
      (fun var -> Renaming.closure_var_is_used renaming var)
      closure_vars
  in
  { closures; closure_vars }

module With_closure_id = struct
  type nonrec t = Closure_id.t * t

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf (closure_id, contents) =
      Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
        Closure_id.print closure_id
        print contents

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare (closure_id1, contents1) (closure_id2, contents2) =
      let c = Closure_id.compare closure_id1 closure_id2 in
      if c <> 0 then c else compare contents1 contents2

    let equal t1 t2 = compare t1 t2 = 0
  end)
end

module With_closure_id_or_unknown = struct
  type nonrec t = Closure_id.t Or_unknown.t * t

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf (closure_id_or_unknown, contents) =
      Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
        (Or_unknown.print Closure_id.print) closure_id_or_unknown
        print contents

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare (closure_id1, contents1) (closure_id2, contents2) =
      let c = Or_unknown.compare Closure_id.compare closure_id1 closure_id2 in
      if c <> 0 then c else compare contents1 contents2

    let equal t1 t2 = compare t1 t2 = 0
  end)
end
