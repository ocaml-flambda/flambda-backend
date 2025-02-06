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
let attr_order a1 a2 = Location.compare a1.loc a2.loc

let compiler_stops_before_attributes_consumed () =
  let stops_before_lambda =
    match !Clflags.stop_after with
    | None -> false
    | Some pass -> Clflags.Compiler_pass.(compare pass Lambda) < 0
  in
  stops_before_lambda || !Clflags.print_types

let unchecked_zero_alloc_attributes = Attribute_table.create 1
let mark_zero_alloc_attribute_checked txt loc =
  Attribute_table.remove unchecked_zero_alloc_attributes { txt; loc }
let register_zero_alloc_attribute attr =
    Attribute_table.replace unchecked_zero_alloc_attributes attr ()
let warn_unchecked_zero_alloc_attribute () =
    (* When using -i, attributes will not have been translated, so we can't
     warn about missing ones. *)
  if !Clflags.print_types then ()
  else
  let keys = List.of_seq (Attribute_table.to_seq_keys unchecked_zero_alloc_attributes) in
  let keys = List.sort attr_order keys in
  List.iter (fun sloc ->
    Location.prerr_warning sloc.loc (Warnings.Unchecked_zero_alloc_attribute))
    keys

let warn_unused () =
  let keys = List.of_seq (Attribute_table.to_seq_keys unused_attrs) in
  Attribute_table.clear unused_attrs;
  if not (compiler_stops_before_attributes_consumed ()) then
    let keys = List.sort attr_order keys in
    List.iter (fun sloc ->
      Location.prerr_warning sloc.loc (Warnings.Misplaced_attribute sloc.txt))
      keys

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings. *)
let builtin_attrs =
  [ "inline"
  ; "inlined"
  ; "specialise"
  ; "specialised"
  ; "tailcall"
  ; "unrolled"
  ; "error"
  ; "alert"
  ; "deprecated"
  ; "deprecated_mutable"
  ; "warning"
  ; "warnerror"
  ; "ppwarning"
  ; "explicit_arity"
  ; "warn_on_literal_pattern"
  ; "immediate"
  ; "immediate64"
  ; "boxed"
  ; "unboxed"
  ; "principal"
  ; "noprincipal"
  ; "nolabels"
  ; "flambda_oclassic"
  ; "flambda_o3"
  ; "afl_inst_ratio"
  ; "local_opt"
  ; "curry"; "extension.curry"
  (* [local] and [global] are never used and always trigger warning 53 *)
  ; "global"; "extension.global"
  ; "local"; "extension.local"
  ; "nontail"; "extension.nontail"
  ; "tail"; "extension.tail"
  ; "noalloc"
  ; "zero_alloc"
  ; "untagged"
  ; "poll"
  ; "loop"
  ; "tail_mod_cons"
  ; "unaliasable"
  ; "builtin"
  ; "no_effects"
  ; "no_coeffects"
  ; "only_generative_effects"
  ; "error_message"
  ; "layout_poly"
  ; "no_mutable_implied_modalities"
  ; "or_null_reexport"
  ; "no_recursive_modalities"
  ; "jane.non_erasable.instances"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

let drop_ocaml_attr_prefix s =
  let len = String.length s in
  if String.starts_with ~prefix:"ocaml." s && len > 6 then
    String.sub s 6 (len - 6)
  else
    s

let is_builtin_attr s = Hashtbl.mem builtin_attrs (drop_ocaml_attr_prefix s)

type current_phase = Parser | Invariant_check

let register_attr current_phase name =
  match current_phase with
  | Parser when !Clflags.all_ppx <> [] -> ()
  | Parser | Invariant_check ->
    if is_builtin_attr name.txt then
      Attribute_table.replace unused_attrs name ()

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
  | ({txt = "call_pos"; loc}, _) ->
      Location.errorf ~loc "[%%call_pos] can only exist as the type of a labelled argument"
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let attr_equals_builtin {attr_name = {txt; _}; _} s =
  (* Check for attribute s or ocaml.s.  Avoid allocating a fresh string. *)
  txt = s ||
  (   String.length txt = 6 + String.length s
   && String.starts_with ~prefix:"ocaml." txt
   && String.ends_with ~suffix:s txt)

let mark_alert_used a =
  if attr_equals_builtin a "deprecated" || attr_equals_builtin a "alert"
  then mark_used a.attr_name

let mark_alerts_used l = List.iter mark_alert_used l

let mark_warn_on_literal_pattern_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "warn_on_literal_pattern"
    then mark_used a.attr_name)
    l

let mark_deprecated_mutable_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "deprecated_mutable"
    then mark_used a.attr_name)
    l

let mark_payload_attrs_used payload =
  let iter =
    { Ast_iterator.default_iterator
      with attribute = fun self a ->
        mark_used a.attr_name;
        Ast_iterator.default_iterator.attribute self a
    }
  in
  iter.payload iter payload

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
  if attr_equals_builtin x "deprecated" then
    Some (x, "deprecated", string_of_opt_payload x.attr_payload)
  else if attr_equals_builtin x "alert" then
    begin match kind_and_message x.attr_payload with
    | Some (kind, message) -> Some (x, kind, message)
    | None -> None (* note: bad payloads detected by warning_attribute *)
    end
  else None

let alert_attrs l =
  List.filter_map alert_attr l

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
  | attr :: _ when attr_equals_builtin attr "deprecated_mutable" ->
    Some (string_of_opt_payload attr.attr_payload)
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

let rec attrs_of_sig_items = function
  | {psig_desc = Psig_attribute a} :: tl ->
      a :: attrs_of_sig_items tl
  | _ ->
      []

let alerts_of_sig ~mark {psg_items; _} =
  let a = attrs_of_sig_items psg_items in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []

let alerts_of_str ~mark str =
  let a = attrs_of_str str in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

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
  let process_alert loc name = function
    | PStr[{pstr_desc=
              Pstr_eval(
                {pexp_desc=Pexp_constant(Pconst_string(s,_,_))},
                _)
           }] ->
        begin
          mark_used name;
          try Warnings.parse_alert_option s
          with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | k ->
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc name.txt "The alert name 'all' is reserved"
        | Some _ ->
            (* Do [mark_used] in the [Some] case only if Warning 53 is
               disabled. Later, they will be marked used (provided they are in a
               valid place) in [compile_common], when they are extracted to be
               persisted inside the [.cmi] file. *)
            if not (Warnings.is_active (Misplaced_attribute ""))
            then mark_used name
        | None -> begin
            (* Do [mark_used] in the [None] case, which is just malformed and
               covered by the "Invalid payload" warning. *)
            mark_used name;
            warn_payload loc name.txt "Invalid payload"
          end
  in
  fun ({attr_name; attr_loc; attr_payload} as attr) ->
    if attr_equals_builtin attr "warning" then
      process attr_loc attr_name false attr_payload
    else if attr_equals_builtin attr "warnerror" then
      process attr_loc attr_name true attr_payload
    else if attr_equals_builtin attr "alert" then
      process_alert attr_loc attr_name attr_payload
    else if ppwarning && attr_equals_builtin attr "ppwarning" then
      begin match attr_payload with
      | PStr [{ pstr_desc=
                  Pstr_eval({pexp_desc=Pexp_constant
                                         (Pconst_string (s, _, _))},_);
                pstr_loc }] ->
        (mark_used attr_name;
         Location.prerr_warning pstr_loc (Warnings.Preprocessor s))
      | _ ->
        (mark_used attr_name;
         warn_payload attr_loc attr_name.txt
           "A single string literal is expected")
      end

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

let has_attribute nm attrs =
  List.exists
    (fun a ->
       if attr_equals_builtin a nm
       then (mark_used a.attr_name; true)
       else false)
    attrs

type attr_action = Mark_used_only | Return
let select_attributes actions attrs =
  List.filter (fun a ->
    List.exists (fun (nm, action) ->
      attr_equals_builtin a nm &&
      begin
        mark_used a.attr_name;
        action = Return
      end)
      actions
  ) attrs

let select_attribute p attributes =
  let attributes = select_attributes p attributes in
  let attr =
    match attributes with
    | [] -> None
    | [attr] -> Some attr
    | attr :: {Parsetree.attr_name = {txt;loc}; _} :: _ ->
      Location.prerr_warning loc (Warnings.Duplicated_attribute txt);
      Some attr
  in
  attr

let when_attribute_is nms attr ~f =
  if List.mem attr.attr_name.txt nms then begin
    mark_used attr.attr_name;
    f ()
  end

type jkind_attribute =
  | Immediate64
  | Immediate

let jkind_attribute_of_string = function
  | "ocaml.immediate64" | "immediate64" -> Some Immediate64
  | "ocaml.immediate" | "immediate" -> Some Immediate
  | _ -> None

let jkind_attribute_to_string = function
  | Immediate64 -> "immediate64"
  | Immediate -> "immediate"

let jkind attrs =
  let jkind =
    List.find_map
      (fun a ->
         match jkind_attribute_of_string a.attr_name.txt with
         | Some attr -> Some (a, attr)
         | None -> None
      ) attrs
  in
  match jkind with
  | None -> None
  | Some (a, l) ->
     mark_used a.attr_name;
     Some (Location.mkloc l a.attr_loc)

let warn_on_literal_pattern attrs =
  has_attribute "warn_on_literal_pattern" attrs

let explicit_arity attrs = has_attribute "explicit_arity" attrs

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let has_unboxed attrs = has_attribute "unboxed" attrs

let has_boxed attrs = has_attribute "boxed" attrs

let has_unsafe_allow_any_mode_crossing attrs =
  has_attribute "unsafe_allow_any_mode_crossing" attrs

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

let parse_ident_payload attr =
  match ident_of_payload attr.attr_payload with
  | Some i -> Some i
  | None ->
    warn_payload attr.attr_loc attr.attr_name.txt
      "A constant payload of type ident was expected";
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

let parse_attribute_with_ident_payload attr ~name ~f =
  when_attribute_is [name; "ocaml." ^ name] attr ~f:(fun () ->
    match parse_ident_payload attr with
    | Some i -> f i
    | None -> ())

let zero_alloc_attribute ~in_signature (attr : Parsetree.attribute)  =
  let module A = Zero_alloc_annotations in
  let msg =
    if in_signature then
      "Only 'all' and 'all_opt' are supported"
    else
      "Only 'all', 'all_opt', 'check', 'check_opt', 'check_all', and 'check_none' are supported"
  in
  let warn () =
    warn_payload attr.attr_loc attr.attr_name.txt msg
  in
  let set_if_not_in_sig r v =
    if not in_signature then
      r := v
    else
      warn ()
  in
  parse_attribute_with_ident_payload attr
    ~name:"zero_alloc" ~f:(function
      | "check" -> set_if_not_in_sig Clflags.zero_alloc_check A.Check.Check_default
      | "check_opt" -> set_if_not_in_sig Clflags.zero_alloc_check A.Check.Check_opt_only
      | "check_all" -> set_if_not_in_sig Clflags.zero_alloc_check A.Check.Check_all
      | "check_none" -> set_if_not_in_sig Clflags.zero_alloc_check A.Check.No_check
      | "all" -> Clflags.zero_alloc_assert := A.Assert.Assert_all
      | "all_opt" -> Clflags.zero_alloc_assert := A.Assert.Assert_all_opt
      | _ ->
        warn ())

let attribute_with_ignored_payload name attr =
  when_attribute_is [name; "ocaml." ^ name] attr ~f:(fun () -> ())

let unsafe_allow_any_mode_crossing_attribute =
  attribute_with_ignored_payload "unsafe_allow_any_mode_crossing"

let afl_inst_ratio_attribute attr =
  clflags_attribute_with_int_payload attr
    ~name:"afl_inst_ratio" Clflags.afl_inst_ratio

let parse_standard_interface_attributes attr =
  warning_attribute attr;
  principal_attribute attr;
  noprincipal_attribute attr;
  nolabels_attribute attr;
  zero_alloc_attribute ~in_signature:true attr;
  unsafe_allow_any_mode_crossing_attribute attr

let parse_standard_implementation_attributes attr =
  warning_attribute attr;
  principal_attribute attr;
  noprincipal_attribute attr;
  nolabels_attribute attr;
  inline_attribute attr;
  afl_inst_ratio_attribute attr;
  flambda_o3_attribute attr;
  flambda_oclassic_attribute attr;
  zero_alloc_attribute ~in_signature:false attr;
  unsafe_allow_any_mode_crossing_attribute attr

let has_no_mutable_implied_modalities attrs =
  has_attribute "no_mutable_implied_modalities" attrs

let has_local_opt attrs =
  has_attribute "local_opt" attrs

let has_layout_poly attrs =
  has_attribute "layout_poly" attrs

let curry_attr_name = "extension.curry"

let has_curry attrs =
  has_attribute curry_attr_name attrs
  || has_attribute "curry" attrs

let has_or_null_reexport attrs =
  has_attribute "or_null_reexport" attrs

let curry_attr loc =
  Ast_helper.Attr.mk ~loc:Location.none (Location.mkloc curry_attr_name loc) (PStr [])
;;

let tailcall attr =
  let has_nontail = has_attribute "nontail" attr in
  let tail_attrs = select_attributes ["tail", Return] attr in
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

let error_message_attr l =
  let inner x =
    match x.attr_name.txt with
    | "ocaml.error_message"|"error_message" ->
      begin match string_of_payload x.attr_payload with
      | Some _ as r ->
        mark_used x.attr_name;
        r
      | None -> warn_payload x.attr_loc x.attr_name.txt
                  "error_message attribute expects a string argument";
        None
      end
    | _ -> None in
  List.find_map inner l

type zero_alloc_check =
  { strict: bool;
    opt: bool;
    arity: int;
    loc: Location.t;
    custom_error_msg : string option;
  }

type zero_alloc_assume =
  { strict: bool;
    never_returns_normally: bool;
    never_raises: bool;
    arity: int;
    loc: Location.t;
  }

type zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of zero_alloc_check
  | Assume of zero_alloc_assume

let is_zero_alloc_check_enabled ~opt =
  match !Clflags.zero_alloc_check with
  | No_check -> false
  | Check_all -> true
  | Check_default -> not opt
  | Check_opt_only -> opt

let is_zero_alloc_attribute =
  [ "zero_alloc", Return ]

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

let get_int_payload = get_payload get_int_from_exp
let get_optional_bool_payload = get_optional_payload get_bool_from_exp

let get_id_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok id
  | _ -> Result.Error ()

type parsed_payload =
  | Ident
  | Const_int
  | Const_string

let get_id_or_constant_from_exp =
  let open Parsetree in
  function
  | { pexp_desc = Pexp_ident { txt = Longident.Lident id } } -> Result.Ok (Ident, id)
  | { pexp_desc = Pexp_constant (Pconst_integer (s,None)) } -> Result.Ok (Const_int, s)
  | { pexp_desc = Pexp_constant (Pconst_string (s,_loc,_so)) } -> Result.Ok (Const_string, s)
  | _ -> Result.Error ()

let get_ids_and_constants_from_exp exp =
  let open Parsetree in
  (match exp with
   | { pexp_desc = Pexp_apply (exp, args) } ->
     get_id_or_constant_from_exp exp ::
     List.map (function
       | (Asttypes.Nolabel, arg) -> get_id_or_constant_from_exp arg
       | (_, _) -> Result.Error ())
       args
   | _ -> [get_id_or_constant_from_exp exp])
  |> List.fold_left (fun acc r ->
    match acc, r with
    | Result.Ok ids, Ok id -> Result.Ok (id::ids)
    | (Result.Error _ | Ok _), _ -> Result.Error ())
    (Ok [])
  |> Result.map List.rev

let parse_optional_id_payload txt loc ~empty cases payload =
  let[@local] warn () =
    let ( %> ) f g x = g (f x) in
    let msg =
      cases
      |> List.map (fst %> Printf.sprintf "'%s'")
      |> String.concat ", "
      |> Printf.sprintf "It must be either %s or empty"
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg));
    Error ()
  in
  match get_optional_payload get_id_from_exp payload with
  | Error () -> warn ()
  | Ok None -> Ok empty
  | Ok (Some id) ->
      match List.assoc_opt id cases with
      | Some r -> Ok r
      | None -> warn ()

(* Looks for `custom_error_message msg` in payload.
   If present, this returns `msg` and an updated payload
   with `customer_error_message msg` removed.
   Preserves the order of the payload. *)
let filter_custom_error_message payload =
  let rec find_msg acc payload =
    match payload with
    | [] | [_] -> None
    | (Ident, "custom_error_message")::(Const_string, msg)::payload ->
      Some (msg, (List.rev acc) @ payload)
    | s1::payload -> find_msg (s1 :: acc) payload
  in
  find_msg [] payload

(* Looks for `arity n` in payload. If present, this returns `n` and an updated
   payload with `arity n` removed. Note it may change the order of the payload,
   which is fine because we sort it later. *)
let filter_arity payload =
  let rec find_arity acc payload =
    match payload with
    | [] | [_] -> None
    | (Ident, "arity") as s1 :: ((Const_int, n) :: payload) as payload' ->
      (match int_of_string_opt n with
       | Some n -> Some (n, acc @ payload)
       | None -> find_arity (s1 :: acc) payload')
    | s1::payload' -> find_arity (s1 :: acc) payload'
  in
  find_arity [] payload

(* If "assume_unless_opt" is not found returns None, otherwise
   returns the rest of the payload. Note it may change the order of the payload,
   which is fine because we sort it later.  *)
let filter_assume_unless_opt payload =
  let rec find acc payload =
    match payload with
    | [] -> None
    | "assume_unless_opt"::tl -> Some (acc @ tl)
    | hd::tl -> find (hd::acc) tl
  in
  find [] payload

let zero_alloc_lookup_table =
  (* These are the possible payloads (sans arity) paired with a function that
     returns the corresponding check_attribute, given the arity and the loc. *)
  [
    (["assume"],
     fun arity loc _ ->
       Assume { strict = false; never_returns_normally = false;
                never_raises = false;
                arity; loc; });
    (["assume_unless_opt"],
     fun arity loc _ ->
       (* same as "assume" *)
       Assume { strict = false; never_returns_normally = false;
                never_raises = false;
                arity; loc; });
    (["strict"],
     fun arity loc custom_error_msg ->
       Check { strict = true; opt = false; arity; loc; custom_error_msg; });
    (["opt"],
     fun arity loc custom_error_msg ->
       Check { strict = false; opt = true; arity; loc; custom_error_msg; });
    (["opt"; "strict"; ],
     fun arity loc custom_error_msg ->
       Check { strict = true; opt = true; arity; loc; custom_error_msg; });
    (["assume"; "strict"],
     fun arity loc _ ->
       Assume { strict = true; never_returns_normally = false;
                never_raises = false;
                arity; loc; });
    (["assume"; "never_returns_normally"],
     fun arity loc _ ->
       Assume {  strict = false; never_returns_normally = true;
                never_raises = false;
                arity; loc; });
    (["assume"; "never_returns_normally"; "strict"],
     fun arity loc _ ->
       Assume { strict = true; never_returns_normally = true;
                never_raises = false;
                arity; loc; });
    (["assume"; "error"],
     fun arity loc _ ->
       Assume { strict = true; never_returns_normally = true;
                never_raises = true;
                arity; loc; });
    (["ignore"], fun _ _ _ -> Ignore_assert_all)
  ]

let parse_zero_alloc_payload ~loc ~arity ~custom_error_message
      ~warn ~empty payload =
  (* This parses the remainder of the payload after arity has been parsed
     out. *)
  match payload with
  | [] -> empty
  | _ :: _ ->
    let payload = List.sort String.compare payload in
    match List.assoc_opt payload zero_alloc_lookup_table with
    | None -> warn ();  Default_zero_alloc
    | Some ca -> ca arity loc custom_error_message

let parse_zero_alloc_attribute ~in_signature ~on_application ~default_arity attr =
  match attr with
  | None -> Default_zero_alloc
  | Some {Parsetree.attr_name = {txt; loc}; attr_payload = payload} ->
    let warn () =
      let ( %> ) f g x = g (f x) in
      let msg =
        let custom_payloads =
          let fail _ _ _ = assert false in
          [
            (["arity <int_constant>"], fail);
            (["custom_error_message <string_constant>"], fail)
          ]
        in
        (zero_alloc_lookup_table@custom_payloads)
        |> List.map (fst %> String.concat " " %> Printf.sprintf "'%s'")
        |> String.concat ", "
        |> Printf.sprintf "It must be either %s or empty"
      in
      Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg))
    in
    let empty arity custom_error_msg =
      Check { strict = false; opt = false; arity; loc; custom_error_msg; }
    in
    match get_optional_payload get_ids_and_constants_from_exp payload with
    | Error () -> warn (); Default_zero_alloc
    | Ok None -> empty default_arity None
    | Ok (Some payload) ->
      let custom_error_message, payload =
        match filter_custom_error_message payload with
        | None -> None, payload
        | Some (custom_error_message, payload) ->
          let is_assume = function
            | (Ident, ("assume" | "assume_unless_opt")) -> true
            | _ -> false
          in
          if List.exists is_assume payload then
            (warn_payload loc txt
               "The \"custom_error_message\" payload is not supported with \"assume\".";
             None, payload)
          else
            Some custom_error_message, payload
      in
      let arity, payload =
        match filter_arity payload with
        | None -> default_arity, payload
        | Some (user_arity, payload) ->
          if in_signature then
            user_arity, payload
          else
            (warn_payload loc txt
               "The \"arity\" field is only supported on \"zero_alloc\" in \
                signatures";
             default_arity, payload)
      in
      let _, payload = List.split payload in
      let parse p =
        let empty = empty arity custom_error_message in
        parse_zero_alloc_payload ~loc ~arity ~custom_error_message ~warn ~empty p
      in
      match filter_assume_unless_opt payload with
      | None -> parse payload
      | Some rest ->
        if in_signature then
          (warn_payload loc txt
             "The payload \"assume_unless_opt\" is not supported \
              in signatures.";
           (* Treat [@zero_alloc assume_unless_opt] as [@zero_alloc] in signatures. *)
           parse rest)
        else
          let no_other_payload = List.compare_length_with rest 0 = 0 in
          if no_other_payload then (
            if is_zero_alloc_check_enabled ~opt:true then
              (if on_application then
                 (* Treat as if there is no attribute.
                    Check is not allowed on applications. *)
                 Default_zero_alloc
               else
                 (* Treat [@zero_alloc assume_unless_opt] as [@zero_alloc],
                    forcing the function to be checked.
                    Setting [opt = false] to satisfy [@zero_alloc]
                    and not only [@zero_alloc opt] on the corresponding signatures. *)
                 empty arity custom_error_message)
            else
              (* Treat "assume_unless_opt" as "assume".
                 Reuse standard parsing for better error messages. *)
              parse payload)
          else (
            (* No support for other payloads with "assume_unless_opt". *)
            warn ();
            Default_zero_alloc)


let get_zero_alloc_attribute ~in_signature ~on_application ~default_arity l =
  let attr = select_attribute is_zero_alloc_attribute l in
  let res =
      parse_zero_alloc_attribute ~in_signature ~on_application ~default_arity
        attr
  in
  (match attr, res with
   | None, Default_zero_alloc -> ()
   | _, Default_zero_alloc -> ()
   | None, (Check _ | Assume _ | Ignore_assert_all) -> assert false
   | Some _, Ignore_assert_all -> ()
   | Some _, Assume _ -> ()
   | Some attr, Check { opt; _ } ->
     if not in_signature && is_zero_alloc_check_enabled ~opt && !Clflags.native_code then
       (* The warning for unchecked functions will not trigger if the check is
          requested through the [@@@zero_alloc all] top-level annotation rather
          than through the function annotation [@zero_alloc]. *)
       register_zero_alloc_attribute attr.attr_name);
   res

let zero_alloc_attribute_only_assume_allowed za =
  match za with
  | Assume assume -> Some assume
  | Default_zero_alloc | Ignore_assert_all -> None
  | Check { loc; _ } ->
    let name = "zero_alloc" in
    let msg = "Only the following combinations are supported in this context: \
               'zero_alloc assume', \
               'zero_alloc assume_unless_opt', \
               `zero_alloc assume strict`, \
               `zero_alloc assume error`,\
               `zero_alloc assume never_returns_normally`,\
               `zero_alloc assume never_returns_normally strict`."
    in
    Location.prerr_warning loc (Warnings.Attribute_payload (name, msg));
    None

let assume_zero_alloc ~inferred assume : Zero_alloc_utils.Assume_info.t =
  match assume with
  | { strict; never_returns_normally; never_raises; } ->
    Zero_alloc_utils.Assume_info.create ~strict ~never_returns_normally ~never_raises ~inferred

type tracing_probe =
  { name : string;
    name_loc : Location.t;
    enabled_at_init : bool;
    arg : Parsetree.expression;
  }

let get_tracing_probe_payload (payload : Parsetree.payload) =
  let ( let* ) = Result.bind in
  let* name, name_loc, args =
    match payload with
    | PStr
        ([{ pstr_desc =
              Pstr_eval
                ({ pexp_desc =
                      (Pexp_apply
                        ({ pexp_desc=
                              (Pexp_constant (Pconst_string(name,_,None)));
                            pexp_loc = name_loc;
                            _ }
                        , args))
                  ; _ }
                , _)}]) -> Ok (name, name_loc, args)
    | _ -> Error ()
  in
  let bool_of_string = function
    | "true" -> Ok true
    | "false" -> Ok false
    | _ -> Error ()
  in
  let* arg, enabled_at_init =
    match args with
    | [Nolabel, arg] -> Ok (arg, false)
    | [Labelled "enabled_at_init",
        { pexp_desc =
            Pexp_construct({ txt = Longident.Lident b; _ },
                          None); _ };
        Nolabel, arg] ->
          let* enabled_at_init = bool_of_string b in
          Ok (arg, enabled_at_init)
    | _ -> Error ()
  in
  Ok { name; name_loc; enabled_at_init; arg }
