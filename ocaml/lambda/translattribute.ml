(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda
open Location


let is_inline_attribute =
  [ ["inline"; "ocaml.inline"],true ]

let is_inlined_attribute =
  [ ["inlined"; "ocaml.inlined"], true
  ; ["unrolled"; "ocaml.unrolled"], (Config.flambda || Config.flambda2)
  ]

let is_specialise_attribute =
  [ ["specialise"; "ocaml.specialise"], Config.flambda ]

let is_specialised_attribute =
  [ ["specialised"; "ocaml.specialised"], Config.flambda ]

let is_local_attribute =
  [ ["local"; "ocaml.local"], true ]

let is_tailcall_attribute =
  [ ["tailcall"; "ocaml.tailcall"], true ]

let is_property_attribute = function
  | Noalloc -> [ ["noalloc"; "ocaml.noalloc"], true ]

let is_tmc_attribute =
  [ ["tail_mod_cons"; "ocaml.tail_mod_cons"], true ]

let is_poll_attribute =
  [ ["poll"; "ocaml.poll"], true ]

let is_loop_attribute =
  [ ["loop"; "ocaml.loop"], true ]

let find_attribute p attributes =
  let inline_attribute = Builtin_attributes.filter_attributes p attributes in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | attr :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some attr
  in
  attr

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false

let get_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [{pstr_desc = Pstr_eval (exp, [])}] -> get_from_exp exp
  | _ -> Result.Error ()

let get_optional_payload get_from_exp =
  let open Parsetree in
  function
  | PStr [] -> Result.Ok None
  | other -> Result.map Option.some (get_payload get_from_exp other)

let get_id_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok id
  | _ -> Result.Error ()

let get_int_from_exp =
  let open Parsetree in
  function
    | { pexp_desc = Pexp_constant (Pconst_integer(s, None)) } ->
        begin match Misc.Int_literal_converter.int s with
        | n -> Result.Ok n
        | exception (Failure _) -> Result.Error ()
        end
    | _ -> Result.Error ()

let get_construct_from_exp =
  let open Parsetree in
  function
    | { pexp_desc =
          Pexp_construct ({ txt = Longident.Lident constr }, None) } ->
        Result.Ok constr
    | _ -> Result.Error ()

let get_bool_from_exp exp =
  Result.bind (get_construct_from_exp exp)
    (function
      | "true" -> Result.Ok true
      | "false" -> Result.Ok false
      | _ -> Result.Error ())

let parse_id_payload txt loc ~default ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    default
  in
  match get_optional_payload get_id_from_exp payload with
  | Error () -> warn ()
  | Ok None -> empty
  | Ok (Some id) ->
      match List.assoc_opt id cases with
      | Some r -> r
      | None -> warn ()

let parse_inline_attribute attr : inline_attribute =
  match attr with
  | None -> Default_inline
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inline
    end else
      parse_id_payload txt loc
        ~default:Default_inline
        ~empty:Always_inline
        [
          "never", Never_inline;
          "always", Always_inline;
          "available", Available_inline;
        ]
        payload

let parse_inlined_attribute attr : inlined_attribute =
  match attr with
  | None -> Default_inlined
  | Some {Parsetree.attr_name = {txt;loc} as id; attr_payload = payload} ->
    if is_unrolled id then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match get_payload get_int_from_exp payload with
      | Ok n -> Unroll n
      | Error () ->
        Location.prerr_warning loc (warning txt);
        Default_inlined
    end else
      parse_id_payload txt loc
        ~default:Default_inlined
        ~empty:Always_inlined
        [
          "never", Never_inlined;
          "always", Always_inlined;
          "hint", Hint_inlined;
        ]
        payload

let parse_specialise_attribute attr =
  match attr with
  | None -> Default_specialise
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_specialise
        ~empty:Always_specialise
        [
          "never", Never_specialise;
          "always", Always_specialise;
        ]
        payload

let parse_local_attribute attr =
  match attr with
  | None -> Default_local
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_local
        ~empty:Always_local
        [
          "never", Never_local;
          "always", Always_local;
          "maybe", Default_local;
        ]
        payload

let parse_property_attribute attr p =
  match attr with
  | None -> Default_check
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload}->
      parse_id_payload txt loc
        ~default:Default_check
        ~empty:(Assert p)
        [
          "assume", Assume p;
        ]
        payload

let parse_poll_attribute attr =
  match attr with
  | None -> Default_poll
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_poll
        ~empty:Default_poll
        [
          "error", Error_poll;
        ]
        payload

let parse_loop_attribute attr =
  match attr with
  | None -> Default_loop
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:Default_loop
        ~empty:Always_loop
        [
          "never", Never_loop;
          "always", Always_loop;
        ]
        payload

let get_inline_attribute l =
  let attr = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let get_specialise_attribute l =
  let attr = find_attribute is_specialise_attribute l in
  parse_specialise_attribute attr

let get_local_attribute l =
  let attr = find_attribute is_local_attribute l in
  parse_local_attribute attr

let get_property_attribute l p =
  let attr = find_attribute (is_property_attribute p) l in
  parse_property_attribute attr p

let get_check_attribute l =
  List.filter_map (fun p ->
    match get_property_attribute l p with
    | Default_check -> None
    | a -> Some a)
    [Noalloc]

let get_poll_attribute l =
  let attr = find_attribute is_poll_attribute l in
  parse_poll_attribute attr

let get_loop_attribute l =
  let attr = find_attribute is_loop_attribute l in
  parse_loop_attribute attr

let check_local_inline loc attr =
  match attr.local, attr.inline with
  | Always_local, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Duplicated_attribute "local/inline")
  | _ ->
      ()

let check_poll_inline loc attr =
  match attr.poll, attr.inline with
  | Error_poll, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with inlining")
  | _ ->
      ()

let check_poll_local loc attr =
  match attr.poll, attr.local with
  | Error_poll, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
          "[@poll error] is incompatible with local function optimization")
  | _ ->
      ()

let lfunction_with_attr ~attr
  { kind; params; return; body; attr=_; loc; mode; region } =
  lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~region

let add_inline_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_inline_attribute attributes with
      | Default_inline -> expr
      | (Always_inline | Available_inline | Never_inline | Unroll _)
          as inline ->
        begin match attr.inline with
          | Default_inline -> ()
          | Always_inline | Available_inline | Never_inline | Unroll _ ->
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "inline")
        end;
        let attr = { attr with inline } in
        check_local_inline loc attr;
        check_poll_inline loc attr;
        lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_specialise_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_specialise_attribute attributes with
    | Default_specialise -> expr
    | (Always_specialise | Never_specialise) as specialise ->
      begin match attr.specialise with
      | Default_specialise -> ()
      | Always_specialise | Never_specialise ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "specialise")
      end;
      let attr = { attr with specialise } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_local_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_local_attribute attributes with
    | Default_local -> expr
    | (Always_local | Never_local) as local ->
      begin match attr.local with
      | Default_local -> ()
      | Always_local | Never_local ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "local")
      end;
      let attr = { attr with local } in
      check_local_inline loc attr;
      check_poll_local loc attr;
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_check_attribute expr loc attributes =
  let to_string = function
    | Noalloc -> "noalloc"
  in
  let to_string = function
    | Assert p -> to_string p
    | Assume p -> Printf.sprintf "%s assume" (to_string p)
    | Default_check -> assert false
  in
  match expr, get_check_attribute attributes with
  | expr, [] -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), [check] ->
      begin match attr.check with
      | Default_check -> ()
      | Assert Noalloc | Assume Noalloc ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute (to_string check))
      end;
      let attr = { attr with check } in
      lfunction_with_attr ~attr funct
  | expr, [check] ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute (to_string check));
      expr
  | expr, a::b::_ ->
    Location.prerr_warning loc
      (Warnings.Duplicated_attribute
         (Printf.sprintf "%s/%s"(to_string a) (to_string b)));
    expr

let add_loop_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_loop_attribute attributes with
    | Default_loop -> expr
    | (Always_loop | Never_loop) as loop ->
      begin match attr.loop with
      | Default_loop -> ()
      | Always_loop | Never_loop ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "loop")
      end;
      let attr = { attr with loop } in
      lfunction_with_attr ~attr funct
    end
  | _ -> expr

let add_tmc_attribute expr loc attributes =
  match expr with
  | Lfunction funct ->
     let attr = find_attribute is_tmc_attribute attributes in
     begin match attr with
     | None -> expr
     | Some _ ->
        if funct.attr.tmc_candidate then
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "tail_mod_cons");
        let attr = { funct.attr with tmc_candidate = true } in
        lfunction_with_attr ~attr funct
     end
  | _ -> expr

let add_poll_attribute expr loc attributes =
  match expr, get_poll_attribute attributes with
  | expr, Default_poll -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), poll ->
      begin match attr.poll with
      | Default_poll -> ()
      | Error_poll ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "error_poll")
      end;
      let attr = { attr with poll } in
      check_poll_inline loc attr;
      check_poll_local loc attr;
      let attr = { attr with inline = Never_inline; local = Never_local } in
      lfunction_with_attr ~attr funct
  | expr, Error_poll ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "error_poll");
      expr

(* Get the [@inlined] attribute payload (or default if not present). *)
let get_inlined_attribute e =
  let attr = find_attribute is_inlined_attribute e.exp_attributes in
  parse_inlined_attribute attr

let get_inlined_attribute_on_module e =
  let rec get mod_expr =
    let attr = find_attribute is_inlined_attribute mod_expr.mod_attributes in
    let attr = parse_inlined_attribute attr in
    let attr =
      match mod_expr.Typedtree.mod_desc with
      | Tmod_constraint (me, _, _, _) ->
        let inner_attr = get me in
        begin match attr with
        | Always_inlined | Hint_inlined | Never_inlined | Unroll _ -> attr
        | Default_inlined -> inner_attr
        end
      | _ -> attr
    in
    attr
  in
  get e

let get_specialised_attribute e =
  let attr = find_attribute is_specialised_attribute e.exp_attributes in
  parse_specialise_attribute attr

let get_tailcall_attribute e =
  let attr = find_attribute is_tailcall_attribute e.exp_attributes in
  match attr with
  | None -> Default_tailcall
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
    match get_optional_payload get_bool_from_exp payload with
    | Ok (None | Some true) -> Tailcall_expectation true
    | Ok (Some false) -> Tailcall_expectation false
    | Error () ->
        let msg = "Only an optional boolean literal is supported." in
        Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
        Default_tailcall

let add_function_attributes lam loc attr =
  let lam =
    add_inline_attribute lam loc attr
  in
  let lam =
    add_specialise_attribute lam loc attr
  in
  let lam =
    add_local_attribute lam loc attr
  in
  let lam =
    add_check_attribute lam loc attr
  in
  let lam =
    add_loop_attribute lam loc attr
  in
  let lam =
    add_tmc_attribute lam loc attr
  in
  let lam =
    (* last because poll overrides inline and local *)
    add_poll_attribute lam loc attr
  in
  lam
