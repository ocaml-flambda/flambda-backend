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

[@@@ocaml.warning "+a-30-40-41-42"]

(* CR-someday xclerc: we could add annotations to external declarations (akin to
   [@@noalloc]) in order to be able to refine the computation of
   effects/coeffects for such functions. *)

let check_arity arity =
  match arity with [] -> Misc.fatal_error "Invalid empty arity" | _ :: _ -> ()

let fprintf = Format.fprintf

module Function_call = struct
  type t =
    | Direct of
        { code_id : Code_id.t;
          closure_id : Closure_id.t;
          return_arity : Flambda_arity.With_subkinds.t
        }
    | Indirect_unknown_arity
    | Indirect_known_arity of
        { param_arity : Flambda_arity.With_subkinds.t;
          return_arity : Flambda_arity.With_subkinds.t
        }

  let [@ocamlformat "disable"] print ppf call =
    match call with
    | Direct { code_id; closure_id; return_arity; } ->
      fprintf ppf "@[<hov 1>(Direct@ \
          @[<hov 1>(code_id@ %a)@]@ \
          @[<hov 1>(closure_id@ %a)@]@ \
          @[<hov 1>(return_arity@ %a)@]\
          )@]"
        Code_id.print code_id
        Closure_id.print closure_id
        Flambda_arity.With_subkinds.print return_arity
    | Indirect_unknown_arity ->
      fprintf ppf "Indirect_unknown_arity"
    | Indirect_known_arity { param_arity; return_arity; } ->
      fprintf ppf "@[(Indirect_known_arity %a \u{2192} %a)@]"
        Flambda_arity.With_subkinds.print param_arity
        Flambda_arity.With_subkinds.print return_arity

  let return_arity call =
    match call with
    | Direct { return_arity; _ } | Indirect_known_arity { return_arity; _ } ->
      return_arity
    | Indirect_unknown_arity -> [Flambda_kind.With_subkind.any_value]
end

type method_kind =
  | Self
  | Public
  | Cached

let print_method_kind ppf kind =
  match kind with
  | Self -> fprintf ppf "Self"
  | Public -> fprintf ppf "Public"
  | Cached -> fprintf ppf "Cached"

type t =
  | Function of Function_call.t
  | Method of
      { kind : method_kind;
        obj : Simple.t
      }
  | C_call of
      { alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
        is_c_builtin : bool
      }

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Function call -> Function_call.print ppf call
  | Method { kind; obj; } ->
    fprintf ppf "@[(Method %a : %a)@]"
      Simple.print obj
      print_method_kind kind
  | C_call { alloc; param_arity; return_arity; is_c_builtin; } ->
    fprintf ppf "@[(C@ @[(alloc %b)@]@ @[(is_c_builtin %b)@]@ \
        @<0>%s@<1>\u{2237}@<0>%s %a @<1>\u{2192} %a)@]"
      alloc
      is_c_builtin
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      Flambda_arity.print param_arity
      Flambda_arity.print return_arity

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t =
  print ppf t

let direct_function_call code_id closure_id ~return_arity =
  check_arity return_arity;
  Function (Direct { code_id; closure_id; return_arity })

let indirect_function_call_unknown_arity () = Function Indirect_unknown_arity

let indirect_function_call_known_arity ~param_arity ~return_arity =
  check_arity return_arity;
  Function (Indirect_known_arity { param_arity; return_arity })

let method_call kind ~obj = Method { kind; obj }

let c_call ~alloc ~param_arity ~return_arity ~is_c_builtin =
  begin
    match return_arity with
    | [] | [_] -> ()
    | _ :: _ :: _ ->
      Misc.fatal_errorf "Illegal return arity for C call: %a"
        Flambda_arity.print return_arity
  end;
  C_call { alloc; param_arity; return_arity; is_c_builtin }

let return_arity t =
  match t with
  | Function call -> Function_call.return_arity call
  | Method _ -> [Flambda_kind.With_subkind.any_value]
  | C_call { return_arity; _ } ->
    List.map
      (fun kind -> Flambda_kind.With_subkind.create kind Anything)
      return_arity

let free_names t =
  match t with
  | Function (Direct { code_id; closure_id = _; return_arity = _ }) ->
    Name_occurrences.add_code_id Name_occurrences.empty code_id Name_mode.normal
  | Function Indirect_unknown_arity
  | Function (Indirect_known_arity { param_arity = _; return_arity = _ })
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Name_occurrences.empty
  | Method { kind = _; obj } ->
    Simple.pattern_match obj
      ~name:(fun obj ~coercion:_ ->
        Name_occurrences.singleton_name obj Name_mode.normal)
      ~const:(fun _ -> Name_occurrences.empty)

let apply_renaming t perm =
  match t with
  | Function (Direct { code_id; closure_id; return_arity }) ->
    let code_id' = Renaming.apply_code_id perm code_id in
    if code_id == code_id'
    then t
    else Function (Direct { code_id = code_id'; closure_id; return_arity })
  | Function Indirect_unknown_arity
  | Function (Indirect_known_arity { param_arity = _; return_arity = _ })
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    t
  | Method { kind; obj } ->
    let obj' = Simple.apply_renaming obj perm in
    if obj == obj' then t else Method { kind; obj = obj' }

let all_ids_for_export t =
  match t with
  | Function (Direct { code_id; closure_id = _; return_arity = _ }) ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id
  | Function Indirect_unknown_arity
  | Function (Indirect_known_arity { param_arity = _; return_arity = _ })
  | C_call { alloc = _; param_arity = _; return_arity = _; is_c_builtin = _ } ->
    Ids_for_export.empty
  | Method { kind = _; obj } -> Ids_for_export.from_simple obj
