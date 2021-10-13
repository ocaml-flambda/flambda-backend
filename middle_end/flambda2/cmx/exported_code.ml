(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Flambda.Code
module P = Flambda.Function_params_and_body

module Calling_convention = struct
  type t =
    { needs_closure_arg : bool;
      params_arity : Flambda_arity.t;
      is_tupled : bool
    }

  let needs_closure_arg t = t.needs_closure_arg

  let params_arity t = t.params_arity

  let is_tupled t = t.is_tupled

  let [@ocamlformat "disable"] print ppf { needs_closure_arg; params_arity; is_tupled } =
    Format.fprintf ppf
      "@[<hov 1>(needs_closure_arg@ %b)@] \
       @[<hov 1>(is_tupled@ %b)@] \
       @[<hov 1>(params_arity@ %a)@]"
      needs_closure_arg
      is_tupled
      Flambda_arity.print params_arity

  let equal
      { needs_closure_arg = needs_closure_arg1;
        params_arity = params_arity1;
        is_tupled = is_tupled1
      }
      { needs_closure_arg = needs_closure_arg2;
        params_arity = params_arity2;
        is_tupled = is_tupled2
      } =
    Bool.equal needs_closure_arg1 needs_closure_arg2
    && Flambda_arity.equal params_arity1 params_arity2
    && Bool.equal is_tupled1 is_tupled2

  let compute ~params_and_body ~is_tupled =
    let f ~return_continuation:_ ~exn_continuation:_ params ~body:_
        ~my_closure:_ ~(is_my_closure_used : _ Or_unknown.t) ~my_depth:_
        ~free_names_of_body:_ =
      let is_my_closure_used =
        match is_my_closure_used with
        | Unknown -> true
        | Known is_my_closure_used -> is_my_closure_used
      in
      let params_arity = Bound_parameter.List.arity params in
      { needs_closure_arg = is_my_closure_used; params_arity; is_tupled }
    in
    P.pattern_match params_and_body ~f
end

type t0 =
  | Present of
      { code : C.t;
        calling_convention : Calling_convention.t
      }
  | Imported of { calling_convention : Calling_convention.t }

type t = t0 Code_id.Map.t

let print0 ppf t0 =
  match t0 with
  | Present { code; calling_convention } ->
    Format.fprintf ppf
      "@[<hov 1>(Present@ (@[<hov 1>(code@ %a)@]@[<hov 1>(calling_convention@ \
       %a)@]))@]"
      C.print code Calling_convention.print calling_convention
  | Imported { calling_convention } ->
    Format.fprintf ppf "@[<hov 1>(Imported@ (calling_convention@ %a))@]"
      Calling_convention.print calling_convention

let [@ocamlformat "disable"] print ppf t =
  Code_id.Map.print print0 ppf t

let empty = Code_id.Map.empty

let add_code code t =
  let with_calling_convention =
    Code_id.Map.filter_map
      (fun _code_id code ->
        match C.params_and_body code with
        | Present params_and_body ->
          let is_tupled = C.is_tupled code in
          let calling_convention =
            Calling_convention.compute ~params_and_body ~is_tupled
          in
          Some (Present { code; calling_convention })
        | Deleted ->
          (* CR lmaurer for vlaviron: Okay to just ignore deleted code? *)
          None)
      code
  in
  Code_id.Map.disjoint_union with_calling_convention t

let mark_as_imported t =
  let forget_params_and_body t0 =
    match t0 with
    | Imported _ -> t0
    | Present { code = _; calling_convention } ->
      Imported { calling_convention }
  in
  Code_id.Map.map forget_params_and_body t

let merge t1 t2 =
  let merge_one code_id t01 t02 =
    match t01, t02 with
    | ( Imported { calling_convention = cc1 },
        Imported { calling_convention = cc2 } ) ->
      if Calling_convention.equal cc1 cc2
      then Some t01
      else
        Misc.fatal_errorf
          "Code id %a is imported with different calling conventions(%a and %a)"
          Code_id.print code_id Calling_convention.print cc1
          Calling_convention.print cc2
    | Present _, Present _ ->
      Misc.fatal_errorf "Cannot merge two definitions for code id %a"
        Code_id.print code_id
    | ( Imported { calling_convention = cc_imported },
        (Present { calling_convention = cc_present; code = _ } as t0) )
    | ( (Present { calling_convention = cc_present; code = _ } as t0),
        Imported { calling_convention = cc_imported } ) ->
      if Calling_convention.equal cc_present cc_imported
      then Some t0
      else
        Misc.fatal_errorf
          "Code_id %a is present with calling convention %abut imported with \
           calling convention %a"
          Code_id.print code_id Calling_convention.print cc_present
          Calling_convention.print cc_imported
  in
  Code_id.Map.union merge_one t1 t2

let mem code_id t = Code_id.Map.mem code_id t

let find_code t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { code; calling_convention = _ } -> Some code
  | Imported _ -> None

let find_code_if_not_imported t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    (* In some cases a code ID is created, the corresponding closure stored into
       another closure, but the corresponding closure variable ends up never
       being used and so the initial code ID and closure are removed, but the
       type of the second closure still mentions its closure variable and its
       contents (eventually pointing to the code ID). Ideally the type should be
       patched to remove the unused closure variable before computing
       reachability, but for now this is done during import instead so we can
       end up with missing code IDs during the reachability computation, and
       have to assume that it fits the above case. *)
    None
  | Present { code; calling_convention = _ } -> Some code
  | Imported _ -> None

let find_calling_convention t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { code = _; calling_convention } -> calling_convention
  | Imported { calling_convention } -> calling_convention

let remove_unreachable t ~reachable_names =
  Code_id.Map.filter
    (fun code_id _exported_code ->
      Name_occurrences.mem_code_id reachable_names code_id)
    t

let all_ids_for_export t =
  Code_id.Map.fold
    (fun code_id code_data all_ids ->
      let all_ids = Ids_for_export.add_code_id all_ids code_id in
      match code_data with
      | Present { code; calling_convention = _ } ->
        Ids_for_export.union all_ids (C.all_ids_for_export code)
      | Imported { calling_convention = _ } -> all_ids)
    t Ids_for_export.empty

let apply_renaming code_id_map renaming t =
  if Renaming.is_empty renaming && Code_id.Map.is_empty code_id_map
  then t
  else
    Code_id.Map.fold
      (fun code_id code_data all_code ->
        let code_id =
          match Code_id.Map.find code_id code_id_map with
          | exception Not_found -> code_id
          | code_id -> code_id
        in
        let code_data =
          match code_data with
          | Present { calling_convention; code } ->
            let code = C.apply_renaming code renaming in
            Present { calling_convention; code }
          | Imported { calling_convention } -> Imported { calling_convention }
        in
        Code_id.Map.add code_id code_data all_code)
      t Code_id.Map.empty

let iter t f =
  Code_id.Map.iter
    (fun id -> function Present { code; _ } -> f id code | _ -> ())
    t
