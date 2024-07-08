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
open Builtin_attributes

<<<<<<< HEAD
let is_inline_attribute =
  [ ["inline"; "ocaml.inline"],true ]
||||||| 121bedcfd2
let is_inline_attribute = function
  | {txt=("inline"|"ocaml.inline")} -> true
  | _ -> false
=======
let return_if_flambda =
  if Config.flambda then Return else Mark_used_only
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_inlined_attribute =
  [ ["inlined"; "ocaml.inlined"], true
  ; ["unrolled"; "ocaml.unrolled"], (Config.flambda || Config.flambda2)
  ]
||||||| 121bedcfd2
let is_inlined_attribute = function
  | {txt=("inlined"|"ocaml.inlined")} -> true
  | {txt=("unrolled"|"ocaml.unrolled")} when Config.flambda -> true
  | _ -> false
=======
let is_inline_attribute =
  [ "inline", Return ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_specialise_attribute =
  [ ["specialise"; "ocaml.specialise"], Config.flambda ]
||||||| 121bedcfd2
let is_specialise_attribute = function
  | {txt=("specialise"|"ocaml.specialise")} when Config.flambda -> true
  | _ -> false
=======
let is_inlined_attribute =
  [ "inlined", Return
  ; "unrolled", return_if_flambda
  ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_specialised_attribute =
  [ ["specialised"; "ocaml.specialised"], Config.flambda ]
||||||| 121bedcfd2
let is_specialised_attribute = function
  | {txt=("specialised"|"ocaml.specialised")} when Config.flambda -> true
  | _ -> false
=======
let is_specialise_attribute =
  [ "specialise", return_if_flambda ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_local_attribute =
  [ ["local"; "ocaml.local"], true ]
||||||| 121bedcfd2
let is_local_attribute = function
  | {txt=("local"|"ocaml.local")} -> true
  | _ -> false
=======
let is_specialised_attribute =
  [ "specialised", return_if_flambda ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_tailcall_attribute =
  [ ["tailcall"; "ocaml.tailcall"], true ]
||||||| 121bedcfd2
let is_tmc_attribute = function
  | {txt=("tail_mod_cons"|"ocaml.tail_mod_cons")} -> true
  | _ -> false
=======
let is_local_attribute =
  [ "local", Return ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_tmc_attribute =
  [ ["tail_mod_cons"; "ocaml.tail_mod_cons"], true ]
||||||| 121bedcfd2
let is_poll_attribute = function
  | {txt=("poll")} -> true
  | _ -> false
=======
let is_tailcall_attribute =
  [ "tailcall", Return ]

let is_tmc_attribute =
  [ "tail_mod_cons", Return ]

let is_poll_attribute =
  [ "poll", Return ]
>>>>>>> 5.2.0

<<<<<<< HEAD
let is_poll_attribute =
  [ ["poll"; "ocaml.poll"], true ]

let is_loop_attribute =
  [ ["loop"; "ocaml.loop"], true ]

let is_opaque_attribute =
  [ ["opaque"; "ocaml.opaque"], true ]

let is_unboxable_attribute =
  [ ["unboxable"; "ocaml.unboxable"], true ]

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false
||||||| 121bedcfd2
let find_attribute p attributes =
  let inline_attribute, other_attributes =
    List.partition (fun a -> p a.Parsetree.attr_name) attributes
  in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | _ :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      None
  in
  attr, other_attributes

let is_unrolled = function
  | {txt="unrolled"|"ocaml.unrolled"} -> true
  | {txt="inline"|"ocaml.inline"|"inlined"|"ocaml.inlined"} -> false
  | _ -> assert false
=======
let find_attribute p attributes =
  let inline_attribute = select_attributes p attributes in
  let attr =
    match inline_attribute with
    | [] -> None
    | [attr] -> Some attr
    | attr :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some attr
  in
  attr
>>>>>>> 5.2.0

let parse_id_payload txt loc options ~default ~empty payload =
  match
    Builtin_attributes.parse_optional_id_payload txt loc options ~empty payload
  with
  | Ok a -> a
  | Error () -> default

let parse_inline_attribute attr : inline_attribute =
  match attr with
  | None -> Default_inline
  | Some ({Parsetree.attr_name = {txt;loc}; attr_payload = payload} as attr) ->
    if attr_equals_builtin attr "unrolled" then begin
      (* the 'unrolled' attributes must be used as [@unrolled n]. *)
      let warning txt = Warnings.Attribute_payload
          (txt, "It must be an integer literal")
      in
      match Builtin_attributes.get_int_payload payload with
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
      match Builtin_attributes.get_int_payload payload with
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

let parse_opaque_attribute attr =
  match attr with
  | None -> false
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
      parse_id_payload txt loc
        ~default:false
        ~empty:true
        []
        payload

let find_attribute p l =
  Builtin_attributes.(find_attribute (Attributes_filter.create p) l)

let get_inline_attribute l =
  let attr = find_attribute is_inline_attribute l in
  parse_inline_attribute attr

let get_specialise_attribute l =
  let attr = find_attribute is_specialise_attribute l in
  parse_specialise_attribute attr

let get_local_attribute l =
  let attr = find_attribute is_local_attribute l in
  parse_local_attribute attr

let get_opaque_attribute l =
  let attr = find_attribute is_opaque_attribute l in
  parse_opaque_attribute attr

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

let check_opaque_inline loc attr =
  match attr.is_opaque, attr.inline with
  | true, (Always_inline | Available_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
           "[@opaque] is incompatible with inlining")
  | _ ->
      ()

let check_opaque_local loc attr =
  match attr.is_opaque, attr.local with
  | true, Always_local ->
      Location.prerr_warning loc
        (Warnings.Inlining_impossible
           "[@opaque] is incompatible with local function optimization")
  | _ ->
      ()


let lfunction_with_attr ~attr
  { kind; params; return; body; attr=_; loc; mode; ret_mode; region } =
  lfunction ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode ~region

let add_inline_attribute expr loc attributes =
<<<<<<< HEAD
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
||||||| 121bedcfd2
  match expr, get_inline_attribute attributes with
  | expr, Default_inline -> expr
  | Lfunction({ attr = { stub = false } as attr } as funct), inline ->
      begin match attr.inline with
      | Default_inline -> ()
      | Always_inline | Hint_inline | Never_inline | Unroll _ ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "inline")
      end;
      let attr = { attr with inline } in
      check_local_inline loc attr;
      check_poll_inline loc attr;
      lfunction_with_attr ~attr funct
  | expr, (Always_inline | Hint_inline | Never_inline | Unroll _) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "inline");
      expr
=======
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_inline_attribute attributes with
      | Default_inline -> expr
      | (Always_inline | Hint_inline | Never_inline | Unroll _)
          as inline ->
        begin match attr.inline with
          | Default_inline -> ()
          | Always_inline | Hint_inline | Never_inline | Unroll _ ->
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "inline")
        end;
        let attr = { attr with inline } in
        check_local_inline loc attr;
        check_poll_inline loc attr;
      lfunction_with_attr ~attr funct
    end
  | _ -> expr
>>>>>>> 5.2.0

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
<<<<<<< HEAD
    end
  | _ -> expr

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
||||||| 121bedcfd2
  | expr, (Always_local | Never_local) ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute "local");
      expr
=======
    end
  | _ -> expr
>>>>>>> 5.2.0

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
  match expr with
  | Lfunction({ attr = { stub = false } as attr } as funct) ->
    begin match get_poll_attribute attributes with
    | Default_poll -> expr
    | Error_poll as poll ->
      begin match attr.poll with
      | Default_poll -> ()
      | Error_poll ->
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "poll error")
      end;
      let attr = { attr with poll } in
      check_poll_inline loc attr;
      check_poll_local loc attr;
      let attr = { attr with inline = Never_inline; local = Never_local } in
      lfunction_with_attr ~attr funct
    end
  | expr -> expr

<<<<<<< HEAD
let add_opaque_attribute expr loc attributes =
  match expr with
  | Lfunction({ attr } as funct) ->
      if not (get_opaque_attribute attributes) then
        expr
      else begin
        if attr.is_opaque then
          Location.prerr_warning loc
            (Warnings.Duplicated_attribute "opaque");
        let attr = { attr with is_opaque = true } in
        check_opaque_inline loc attr;
        check_opaque_local loc attr;
        let attr = { attr with inline = Never_inline; local = Never_local } in
        lfunction_with_attr ~attr funct
      end
  | _ -> expr
||||||| 121bedcfd2
(* Get the [@inlined] attribute payload (or default if not present).
   It also returns the expression without this attribute. This is
   used to ensure that this attribute is not misplaced: If it
   appears on any expression, it is an error, otherwise it would
   have been removed by this function *)
let get_and_remove_inlined_attribute e =
  let attr, exp_attributes =
    find_attribute is_inlined_attribute e.exp_attributes
  in
  let inlined = parse_inline_attribute attr in
  inlined, { e with exp_attributes }
=======
(* Get the [@inlined] attribute payload (or default if not present). *)
let get_inlined_attribute e =
  let attr = find_attribute is_inlined_attribute e.exp_attributes in
  parse_inline_attribute attr
>>>>>>> 5.2.0

<<<<<<< HEAD
let add_unbox_return_attribute expr loc attributes =
  match expr with
  | Lfunction funct ->
      let attr = find_attribute is_unboxable_attribute attributes in
      begin match attr with
      | None -> expr
      | Some _ ->
          if funct.attr.unbox_return then
            Location.prerr_warning loc
              (Warnings.Duplicated_attribute "unboxable");
          let attr = { funct.attr with unbox_return = true } in
          lfunction_with_attr ~attr funct
      end
  | _ -> expr


(* Get the [@inlined] attribute payload (or default if not present). *)
let get_inlined_attribute e =
  let attr = find_attribute is_inlined_attribute e.exp_attributes in
  parse_inlined_attribute attr

let get_inlined_attribute_on_module e =
  let rec get mod_expr =
    let attr = find_attribute is_inlined_attribute mod_expr.mod_attributes in
    let attr = parse_inlined_attribute attr in
    let attr =
||||||| 121bedcfd2
let get_and_remove_inlined_attribute_on_module e =
  let rec get_and_remove mod_expr =
    let attr, mod_attributes =
      find_attribute is_inlined_attribute mod_expr.mod_attributes
    in
    let attr = parse_inline_attribute attr in
    let attr, mod_desc =
=======
let get_inlined_attribute_on_module e =
  let rec get mod_expr =
    let attr = find_attribute is_inlined_attribute mod_expr.mod_attributes in
    let attr = parse_inline_attribute attr in
    let attr =
>>>>>>> 5.2.0
      match mod_expr.Typedtree.mod_desc with
<<<<<<< HEAD
      | Tmod_constraint (me, _, _, _) ->
        let inner_attr = get me in
        begin match attr with
        | Always_inlined | Hint_inlined | Never_inlined | Unroll _ -> attr
        | Default_inlined -> inner_attr
        end
      | _ -> attr
||||||| 121bedcfd2
      | Tmod_constraint (me, mt, mtc, mc) ->
        let inner_attr, me = get_and_remove me in
        let attr =
          match attr with
          | Always_inline | Hint_inline | Never_inline | Unroll _ -> attr
          | Default_inline -> inner_attr
        in
        attr, Tmod_constraint (me, mt, mtc, mc)
      | md -> attr, md
=======
      | Tmod_constraint (me, _, _, _) ->
        let inner_attr = get me in
        begin match attr with
        | Always_inline | Hint_inline | Never_inline | Unroll _ -> attr
        | Default_inline -> inner_attr
        end
      | _ -> attr
>>>>>>> 5.2.0
    in
    attr
  in
  get e

let get_specialised_attribute e =
  let attr = find_attribute is_specialised_attribute e.exp_attributes in
  parse_specialise_attribute attr

let get_tailcall_attribute e =
<<<<<<< HEAD
  let attr = find_attribute is_tailcall_attribute e.exp_attributes in
  match attr with
  | None -> Default_tailcall
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
    match Builtin_attributes.get_optional_bool_payload payload with
    | Ok (None | Some true) -> Tailcall_expectation true
    | Ok (Some false) -> Tailcall_expectation false
    | Error () ->
        let msg = "Only an optional boolean literal is supported." in
        Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
        Default_tailcall
||||||| 121bedcfd2
  let is_tailcall_attribute = function
    | {Parsetree.attr_name = {txt=("tailcall"|"ocaml.tailcall")}; _} -> true
    | _ -> false
  in
  let tailcalls, other_attributes =
    List.partition is_tailcall_attribute e.exp_attributes
  in
  let tailcall_attribute = match tailcalls with
    | [] -> Default_tailcall
    | {Parsetree.attr_name = {txt; loc}; attr_payload = payload} :: r ->
        begin match r with
        | [] -> ()
        | {Parsetree.attr_name = {txt;loc}; _} :: _ ->
            Location.prerr_warning loc (Warnings.Duplicated_attribute txt)
        end;
        match get_optional_payload get_bool_from_exp payload with
        | Ok (None | Some true) -> Tailcall_expectation true
        | Ok (Some false) -> Tailcall_expectation false
        | Error () ->
            let msg = "Only an optional boolean literal is supported." in
            Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
            Default_tailcall
      in
      tailcall_attribute, { e with exp_attributes = other_attributes }

let check_attribute e {Parsetree.attr_name = { txt; loc }; _} =
  match txt with
  | "inline" | "ocaml.inline"
  | "specialise" | "ocaml.specialise"
  | "poll" -> begin
      match e.exp_desc with
      | Texp_function _ -> ()
      | _ ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute txt)
    end
  | "inlined" | "ocaml.inlined"
  | "specialised" | "ocaml.specialised"
  | "tailcall" | "ocaml.tailcall" ->
      (* Removed by the Texp_apply cases *)
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute txt)
  | _ -> ()

let check_attribute_on_module e {Parsetree.attr_name = { txt; loc }; _} =
  match txt with
  | "inline" | "ocaml.inline" ->  begin
      match e.mod_desc with
      | Tmod_functor _ -> ()
      | _ ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute txt)
    end
  | "inlined" | "ocaml.inlined" ->
      (* Removed by the Texp_apply cases *)
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute txt)
  | _ -> ()
=======
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
>>>>>>> 5.2.0

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
    add_loop_attribute lam loc attr
  in
  let lam =
    add_tmc_attribute lam loc attr
  in
  let lam =
    add_unbox_return_attribute lam loc attr
  in
  (* last because poll and opaque overrides inline and local *)
  let lam =
    add_poll_attribute lam loc attr
  in
  let lam =
    add_opaque_attribute lam loc attr
  in
  lam

let transl_param_attributes pat =
  let attrs = pat.pat_attributes in
  let unbox_param =
    Option.is_some (find_attribute is_unboxable_attribute attrs)
  in
  { unbox_param }
