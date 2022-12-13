(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_helper


module Attribute_table = Hashtbl.Make (struct
  type t = string with_loc

  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = (=)
end)
let unused_attrs = Attribute_table.create 128
let mark_used t = Attribute_table.remove unused_attrs t

(* [attr_order] is used to issue unused attribute warnings in the order the
   attributes occur in the file rather than the random order of the hash table
*)
let attr_order a1 a2 =
  match String.compare a1.loc.loc_start.pos_fname a2.loc.loc_start.pos_fname
  with
  | 0 -> Int.compare a1.loc.loc_start.pos_lnum a2.loc.loc_start.pos_lnum
  | n -> n

let warn_unused () =
  let keys = List.of_seq (Attribute_table.to_seq_keys unused_attrs) in
  let keys = List.sort attr_order keys in
  List.iter (fun sloc ->
    Location.prerr_warning sloc.loc (Warnings.Misplaced_attribute sloc.txt))
    keys

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings.  Explicitly excluded is [deprecated_mutable],
   which is currently broken in the compiler *)
let builtin_attrs =
  [ "inline"; "ocaml.inline"
  ; "inlined"; "ocaml.inlined"
  ; "specialise"; "ocaml.specialise"
  ; "specialised"; "ocaml.specialised"
  ; "tailcall"; "ocaml.tailcall"
  ; "unrolled"; "ocaml.unrolled"
  ; "error"; "ocaml.error"
  ; "alert"; "ocaml.alert"
  ; "deprecated"; "ocaml.deprecated"
  ; "warning"; "ocaml.warning"
  ; "warnerror"; "ocaml.warnerror"
  ; "ppwarning"; "ocaml.ppwarning"
  ; "explicit_arity"; "ocaml.explicit_arity"
  ; "warn_on_literal_pattern"; "ocaml.warn_on_literal_pattern"
  ; "immediate"; "ocaml.immediate"
  ; "immediate64"; "ocaml.immediate64"
  ; "boxed"; "ocaml.boxed"
  ; "unboxed"; "ocaml.unboxed"
  ; "principal"; "ocaml.principal"
  ; "noprincipal"; "ocaml.noprincipal"
  ; "nolabels"; "ocaml.nolabels"
  ; "flambda_oclassic"; "ocaml.flambda_oclassic"
  ; "flambda_o3"; "ocaml.flambda_o3"
  ; "afl_inst_ratio"; "ocaml.afl_inst_ratio"
  ; "local_opt"; "ocaml.local_opt"
  ; "curry"; "ocaml.curry"; "extension.curry"
  ; "global"; "ocaml.global"; "extension.global"
  ; "nonlocal"; "ocaml.nonlocal"; "extension.nonlocal"
  ; "local"; "ocaml.local"; "extension.local"
  ; "nontail"; "ocaml.nontail"; "extension.nontail"
  ; "tail"; "ocaml.tail"; "extension.tail"
  ; "include_functor"; "ocaml.include_functor"; "extension.include_functor"
  ; "noalloc"; "ocaml.noalloc"
  ; "untagged"; "ocaml.untagged"
  ; "poll"; "ocaml.poll"
  ; "loop"; "ocaml.loop"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

let is_builtin_attr s = Hashtbl.mem builtin_attrs s

let mk_internal ?(loc= !default_loc) name payload =
  if is_builtin_attr name.txt
  then Attribute_table.add unused_attrs name ();
  Attr.mk ~loc name payload


let ident_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_ident {txt=Lident id}},_)}] ->
     Some id
  | _ -> None

let string_of_cst = function
  | Pconst_string(s, _, _) -> Some s
  | _ -> None

let int_of_cst = function
  | Pconst_integer(i, None) -> Some (int_of_string i)
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let int_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      int_of_cst c
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = ("ocaml.error"|"error"); loc}, p), _)} ->
        begin match p with
        | PStr([{pstr_desc=Pstr_eval
                     ({pexp_desc=Pexp_constant(Pconst_string(msg,_,_))}, _)}
               ]) ->
            { Location.loc; txt = fun ppf -> Format.pp_print_text ppf msg }
        | _ ->
            { Location.loc; txt = fun ppf ->
                Format.fprintf ppf
                  "Invalid syntax for sub-message of extension '%s'." main_txt }
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        { Location.loc; txt = fun ppf ->
            Format.fprintf ppf "Uninterpreted extension '%s'." txt }
    | _ ->
        { Location.loc = main_loc; txt = fun ppf ->
            Format.fprintf ppf
              "Invalid syntax for sub-message of extension '%s'." main_txt }
  in
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
      begin match p with
      | PStr [] -> raise Location.Already_displayed_error
      | PStr({pstr_desc=Pstr_eval
                  ({pexp_desc=Pexp_constant(Pconst_string(msg,_,_))}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let kind_and_message = function
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_apply
                 ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},
                  [Nolabel,{pexp_desc=Pexp_constant (Pconst_string(s,_,_))}])
            },_)}] ->
      Some (id, s)
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},_)}] ->
      Some (id, "")
  | _ -> None

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let alert_attr x =
  match x.attr_name.txt with
  | "ocaml.deprecated"|"deprecated" -> begin
      mark_used x.attr_name;
      Some (x, "deprecated", string_of_opt_payload x.attr_payload)
    end
  | "ocaml.alert"|"alert" ->
      begin match kind_and_message x.attr_payload with
      | Some (kind, message) -> begin
        mark_used x.attr_name;
        Some (x, kind, message)
      end
      | None -> None (* note: bad payloads detected by warning_attribute *)
      end
  | _ -> None

let alert_attrs l =
  List.filter_map alert_attr l

let mark_alert_used a =
  match a.attr_name.txt with
  | "ocaml.deprecated"|"deprecated"|"ocaml.alert"|"alert" ->
    mark_used a.attr_name
  | _ -> ()

let mark_alerts_used l = List.iter mark_alert_used l

let mark_warn_on_literal_pattern_used l =
  List.iter (fun a ->
    match a.attr_name.txt with
    | "ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern" ->
      mark_used a.attr_name
    | _ -> ())
    l

let alerts_of_attrs l =
  List.fold_left
    (fun acc (_, kind, message) ->
       let upd = function
         | None | Some "" -> Some message
         | Some s -> Some (cat s message)
       in
       Misc.Stdlib.String.Map.update kind upd acc
    )
    Misc.Stdlib.String.Map.empty
    (alert_attrs l)

let check_alerts loc attrs s =
  Misc.Stdlib.String.Map.iter
    (fun kind message -> Location.alert loc ~kind (cat s message))
    (alerts_of_attrs attrs)

let check_alerts_inclusion ~def ~use loc attrs1 attrs2 s =
  let m2 = alerts_of_attrs attrs2 in
  Misc.Stdlib.String.Map.iter
    (fun kind msg ->
       if not (Misc.Stdlib.String.Map.mem kind m2) then
         Location.alert ~def ~use ~kind loc (cat s msg)
    )
    (alerts_of_attrs attrs1)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | {attr_name =  {txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _};
     attr_payload = p} :: _ ->
     Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec attrs_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      a :: attrs_of_sig tl
  | _ ->
      []

let alerts_of_sig sg = alerts_of_attrs (attrs_of_sig sg)

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []

let alerts_of_str str = alerts_of_attrs (attrs_of_str str)

let warn_payload loc txt msg =
  Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg))

let warning_attribute ?(ppwarning = true) =
  let process loc name errflag payload =
    mark_used name;
    match string_of_payload payload with
    | Some s ->
        begin try
          Option.iter (Location.prerr_alert loc)
            (Warnings.parse_options errflag s)
        with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | None ->
        warn_payload loc name.txt "A single string literal is expected"
  in
  let process_alert loc txt = function
    | PStr[{pstr_desc=
              Pstr_eval(
                {pexp_desc=Pexp_constant(Pconst_string(s,_,_))},
                _)
           }] ->
        begin try Warnings.parse_alert_option s
        with Arg.Bad msg -> warn_payload loc txt msg
        end
    | k ->
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc txt "The alert name 'all' is reserved"
        | Some _ -> ()
        | None -> warn_payload loc txt "Invalid payload"
  in
  function
  | {attr_name = {txt = ("ocaml.warning"|"warning"); _} as name;
     attr_loc;
     attr_payload;
     } ->
      process attr_loc name false attr_payload
  | {attr_name = {txt = ("ocaml.warnerror"|"warnerror"); _} as name;
     attr_loc;
     attr_payload
    } ->
      process attr_loc name true attr_payload
  | {attr_name = {txt="ocaml.ppwarning"|"ppwarning"; _} as name;
     attr_loc = _;
     attr_payload =
       PStr [
         { pstr_desc=
             Pstr_eval({pexp_desc=Pexp_constant (Pconst_string (s, _, _))},_);
           pstr_loc }
       ];
    } when ppwarning ->
    (mark_used name;
     Location.prerr_warning pstr_loc (Warnings.Preprocessor s))
  | {attr_name = {txt = ("ocaml.alert"|"alert"); _} as name;
     attr_loc;
     attr_payload;
     } ->
      (mark_used name;
       process_alert attr_loc name.txt attr_payload)
  | _ ->
     ()

let warning_scope ?ppwarning attrs f =
  let prev = Warnings.backup () in
  try
    List.iter (warning_attribute ?ppwarning) (List.rev attrs);
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn

let has_attribute nms attrs =
  List.exists
    (fun a ->
       if List.mem a.attr_name.txt nms
       then (mark_used a.attr_name; true)
       else false)
    attrs

let filter_attributes nms_and_conds attrs =
  List.filter (fun a ->
    List.exists (fun (nms, cond) ->
      if List.mem a.attr_name.txt nms
      then (mark_used a.attr_name; cond)
      else false)
      nms_and_conds
  ) attrs

let when_attribute_is nms attr ~f =
  if List.mem attr.attr_name.txt nms then begin
    mark_used attr.attr_name;
    f ()
  end

let warn_on_literal_pattern attrs =
  has_attribute ["ocaml.warn_on_literal_pattern"; "warn_on_literal_pattern"]
    attrs

let explicit_arity attrs =
  has_attribute ["ocaml.explicit_arity"; "explicit_arity"] attrs

let immediate attrs = has_attribute ["ocaml.immediate"; "immediate"] attrs

let immediate64 attrs = has_attribute ["ocaml.immediate64"; "immediate64"] attrs

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let has_unboxed attrs = has_attribute ["ocaml.unboxed"; "unboxed"] attrs

let has_boxed attrs = has_attribute ["ocaml.boxed"; "boxed"] attrs

let parse_empty_payload attr =
  match attr.attr_payload with
  | PStr [] -> Some ()
  | _ ->
    warn_payload attr.attr_loc attr.attr_name.txt
      "No attribute payload was expected";
    None

let parse_int_payload attr =
  match int_of_payload attr.attr_payload with
  | Some i -> Some i
  | None ->
    warn_payload attr.attr_loc attr.attr_name.txt
      "A constant payload of type int was expected";
    None

let clflags_attribute_without_payload attr ~name clflags_ref =
  when_attribute_is [name; "ocaml." ^ name] attr ~f:(fun () ->
    match parse_empty_payload attr with
    | Some () -> clflags_ref := true
    | None -> ())

let clflags_attribute_without_payload' attr ~name ~f =
  when_attribute_is [name; "ocaml." ^ name] attr ~f:(fun () ->
    Option.iter f (parse_empty_payload attr))

let clflags_attribute_with_int_payload attr ~name clflags_ref =
  when_attribute_is [name; "ocaml." ^ name] attr ~f:(fun () ->
    match parse_int_payload attr with
    | Some i -> clflags_ref := i
    | None -> ())

let principal_attribute attr =
  clflags_attribute_without_payload attr
    ~name:"principal" Clflags.principal

let noprincipal_attribute attr =
  clflags_attribute_without_payload' attr
    ~name:"noprincipal"
    ~f:(fun () -> Clflags.principal := false)

let nolabels_attribute attr =
  clflags_attribute_without_payload attr
    ~name:"nolabels" Clflags.classic

let flambda_oclassic_attribute attr =
  clflags_attribute_without_payload' attr
    ~name:"flambda_oclassic"
    ~f:(fun () ->
      if Config.flambda || Config.flambda2 then Clflags.set_oclassic ())

let flambda_o3_attribute attr =
  clflags_attribute_without_payload' attr
    ~name:"flambda_o3"
    ~f:(fun () -> if Config.flambda || Config.flambda2 then Clflags.set_o3 ())

let inline_attribute attr =
  when_attribute_is ["inline"; "ocaml.inline"] attr ~f:(fun () ->
    let err_msg =
      "Either specify an integer, or the form accepted by '-inline' in quotes"
    in
    match string_of_payload attr.attr_payload with
    | Some s ->
      Clflags.Float_arg_helper.parse s err_msg Clflags.inline_threshold
    | None ->
      match int_of_payload attr.attr_payload with
      | Some i ->
        let s = string_of_int i in
        Clflags.Float_arg_helper.parse s err_msg Clflags.inline_threshold
      | None -> warn_payload attr.attr_loc attr.attr_name.txt err_msg)

let afl_inst_ratio_attribute attr =
  clflags_attribute_with_int_payload attr
    ~name:"afl_inst_ratio" Clflags.afl_inst_ratio

let parse_standard_interface_attributes attr =
  warning_attribute attr;
  principal_attribute attr;
  noprincipal_attribute attr;
  nolabels_attribute attr

let parse_standard_implementation_attributes attr =
  warning_attribute attr;
  principal_attribute attr;
  noprincipal_attribute attr;
  nolabels_attribute attr;
  inline_attribute attr;
  afl_inst_ratio_attribute attr;
  flambda_o3_attribute attr;
  flambda_oclassic_attribute attr

let has_local_opt attrs =
  has_attribute ["ocaml.local_opt"; "local_opt"] attrs

let has_curry attrs =
  has_attribute ["extension.curry"; "ocaml.curry"; "curry"] attrs

(* extension.* is generated by the parser and not usually written directly,
   so does not have a short form. An error is reported if it is seen when
   the extension is disabled *)

let check_local ext_names other_names attr =
  if has_attribute ext_names attr then
    if not (Clflags.Extension.is_enabled Local) then
      Error ()
    else
      Ok true
  else
    Ok (has_attribute other_names attr)

let has_local attr =
  check_local ["extension.local"] ["ocaml.local"; "local"] attr

let has_global attrs =
  check_local ["extension.global"] ["ocaml.global"; "global"] attrs

let has_nonlocal attrs =
  check_local ["extension.nonlocal"] ["ocaml.nonlocal"; "nonlocal"] attrs

let tailcall attr =
  let has_nontail = has_attribute ["ocaml.nontail"; "nontail"] attr in
  let tail_attrs = filter_attributes [["ocaml.tail";"tail"], true] attr in
  match has_nontail, tail_attrs with
  | true, (_ :: _) -> Error `Conflict
  | _, (_ :: _ :: _) -> Error `Conflict
  | false, [] -> Ok None
  | true, [] -> Ok (Some `Nontail)
  | false, [{attr_payload = PStr []}] -> Ok (Some `Tail)
  | false, [t] ->
     match ident_of_payload t.attr_payload with
     | Some "hint" -> Ok (Some `Tail_if_possible)
     | _ ->
        Location.prerr_warning t.attr_loc
          (Warnings.Attribute_payload
             (t.attr_name.txt, "Only 'hint' is supported"));
        Ok (Some `Tail_if_possible)

let has_include_functor attr =
  if has_attribute ["extension.include_functor"] attr then
    if not (Clflags.Extension.is_enabled Include_functor) then
      Error ()
    else
      Ok true
  else
    Ok false
