(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Blambda
open Format

let structured_constant = Printlambda.structured_constant

let tailcall ppf = function
  | Tailcall -> pp_print_string ppf "tailcall"
  | Nontail -> pp_print_string ppf "nontail"

let comparison ppf = function
  | Eq -> pp_print_string ppf "eq"
  | Neq -> pp_print_string ppf "neq"
  | Ltint -> pp_print_string ppf "ltint"
  | Leint -> pp_print_string ppf "leint"
  | Gtint -> pp_print_string ppf "gtint"
  | Geint -> pp_print_string ppf "geint"
  | Ultint -> pp_print_string ppf "ultint"
  | Ugeint -> pp_print_string ppf "ugeint"

let primitive ppf = function
  | Getglobal c -> fprintf ppf "getglobal %a" Compilation_unit.print c
  | Getpredef i -> fprintf ppf "getpredef %a" Ident.print i
  | Boolnot -> pp_print_string ppf "boolnot"
  | Isint -> pp_print_string ppf "isint"
  | Vectlength -> pp_print_string ppf "vectlength"
  | Setglobal c -> fprintf ppf "setglobal %a" Compilation_unit.print c
  | Getfield i -> fprintf ppf "getfield %d" i
  | Getfloatfield i -> fprintf ppf "getfloatfield %d" i
  | Raise Raise_regular -> pp_print_string ppf "raise"
  | Raise Raise_notrace -> pp_print_string ppf "raise_notrace"
  | Raise Raise_reraise -> pp_print_string ppf "reraise"
  | Offsetint i -> fprintf ppf "offsetint %d" i
  | Offsetref i -> fprintf ppf "offsetref %d" i
  | Negint -> pp_print_string ppf "negint"
  | Addint -> pp_print_string ppf "addint"
  | Subint -> pp_print_string ppf "subint"
  | Mulint -> pp_print_string ppf "mulint"
  | Divint -> pp_print_string ppf "divint"
  | Modint -> pp_print_string ppf "modint"
  | Andint -> pp_print_string ppf "andint"
  | Orint -> pp_print_string ppf "orint"
  | Xorint -> pp_print_string ppf "xorint"
  | Lslint -> pp_print_string ppf "lslint"
  | Lsrint -> pp_print_string ppf "lsrint"
  | Asrint -> pp_print_string ppf "asrint"
  | Intcomp cmp -> comparison ppf cmp
  | Getbyteschar -> pp_print_string ppf "getbyteschar"
  | Getvectitem -> pp_print_string ppf "getvectitem"
  | Setfield i -> fprintf ppf "setfield %d" i
  | Setfloatfield i -> fprintf ppf "setfloatfield %d" i
  | Setvectitem -> pp_print_string ppf "setvectitem"
  | Setbyteschar -> pp_print_string ppf "setbyteschar"
  | Ccall s -> fprintf ppf "ccall %s" s
  | Makeblock { tag } -> fprintf ppf "makeblock %d" tag
  | Makefloatblock -> pp_print_string ppf "makefloatblock"
  | Make_faux_mixedblock { total_len; tag } ->
    fprintf ppf "make_faux_mixedblock {total_len=%d; tag=%d}" total_len tag
  | Check_signals -> pp_print_string ppf "check_signals"

let location ppf (loc : Debuginfo.Scoped_location.t) =
  match loc with
  | Loc_unknown -> pp_print_string ppf "<unknown location>"
  | Loc_known { scopes; loc } ->
    fprintf ppf "%s %s(%i)%s:%i-%i"
      (Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:true
         scopes)
      loc.loc_start.pos_fname loc.loc_start.pos_lnum
      (if loc.loc_ghost then "<ghost>" else "")
      loc.loc_start.pos_cnum loc.loc_end.pos_cnum

let rec blambda ppf = function
  | Var id -> Ident.print ppf id
  | Const cst -> structured_constant ppf cst
  | Apply { func; args; tailcall = t } ->
    fprintf ppf "@[<2>(apply %a@ %a %a)@]" tailcall t blambda func
      (pp_print_list ~pp_sep:pp_print_space blambda)
      args
  | Assign (id, expr) ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id blambda expr
  | Function f -> bfunction ppf f
  | Let _ as expr ->
    let rec bindings = function
      | Let { id; arg; body } ->
        let bindings, body = bindings body in
        (id, arg) :: bindings, body
      | expr -> [], expr
    in
    let bindings, expr = bindings expr in
    fprintf ppf "@[<2>(let@ @[<hv 1>(%a)@]@ %a)@]"
      (pp_print_list ~pp_sep:pp_print_space (fun ppf (id, arg) ->
           fprintf ppf "@[<2>%a =@ %a@]" Ident.print id blambda arg))
      bindings blambda expr
  | Letrec { decls; body; free_variables_of_decls = _ } ->
    fprintf ppf "@[<2>(letrec@ @[%a@]@ in@ %a)@]"
      (pp_print_list ~pp_sep:pp_print_space rec_binding)
      decls blambda body
  | Prim (p, args) ->
    fprintf ppf "@[<2>(%%%a@ %a)@]" primitive p
      (pp_print_list ~pp_sep:pp_print_space blambda)
      args
  | Switch { arg; const_cases; block_cases; cases } ->
    let const_cases = Array.to_list const_cases in
    let block_cases = Array.to_list block_cases in
    let cases = Array.to_list cases in
    let matching_cases cases i =
      ListLabels.mapi cases ~f:(fun n j -> n, j)
      |> ListLabels.filter_map ~f:(fun (n, j) ->
             if i <> j then None else Some n)
    in
    let or_pattern ppf = function
      | [] -> ()
      | [x] -> pp_print_int ppf x
      | values ->
        fprintf ppf "@[<h>(%a)@]"
          (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;|") pp_print_int)
          values
    in
    let labels ~name ~cases i =
      match matching_cases cases i with
      | [] -> None
      | _ :: _ as cases ->
        Some (fun ppf -> fprintf ppf "case %s %a:" name or_pattern cases)
    in
    let switch_cases ppf =
      let case_labels i ppf =
        let formatters =
          List.filter_map
            (fun f -> f i)
            [ labels ~name:"const" ~cases:const_cases;
              labels ~name:"block" ~cases:block_cases ]
        in
        pp_print_list ~pp_sep:pp_print_cut (fun ppf f -> f ppf) ppf formatters
      in
      let cases = List.mapi (fun i x -> i, x) cases in
      pp_print_list ~pp_sep:pp_print_cut
        (fun ppf (i, x) ->
          fprintf ppf "@[<2>%t@ %a@]" (case_labels i) blambda x)
        ppf cases
    in
    fprintf ppf "@[(@[switch@ %a@] %t)@]" blambda arg switch_cases
  | Staticraise (id, args) ->
    fprintf ppf "@[<2>(staticraise %d@ %a)@]" id
      (pp_print_list ~pp_sep:pp_print_space blambda)
      args
  | Staticcatch { id; args; handler; body } ->
    fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]" blambda body id
      (fun ppf vars ->
        List.iter (fun x -> fprintf ppf " %a" Ident.print x) vars)
      args blambda handler
  | Trywith { body; param; handler } ->
    fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]" blambda body Ident.print
      param blambda handler
  | Ifthenelse { cond; ifso; ifnot } ->
    fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" blambda cond blambda ifso blambda
      ifnot
  | Sequence (e1, e2) ->
    fprintf ppf "@[<2>(seq@ %a@ %a)@]" blambda e1 blambda e2
  | While { cond; body } ->
    fprintf ppf "@[<2>(while@ %a@ %a)@]" blambda cond blambda body
  | For
      { id = for_id;
        from = for_from;
        to_ = for_to;
        dir = for_dir;
        body = for_body
      } ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]" Ident.print for_id blambda
      for_from
      (match for_dir with Upto -> "to" | Downto -> "downto")
      blambda for_to blambda for_body
  | Send { method_kind; met; obj; args; tailcall = t } ->
    let name = match method_kind with Self -> "sendself" | Public -> "send" in
    fprintf ppf "@[<2>(%s%a@ met=%a@ %a)@]" name tailcall t blambda met
      (pp_print_list ~pp_sep:pp_print_space blambda)
      (obj :: args)
  | Event (expr, ev) ->
    if not !Clflags.locations
    then blambda ppf expr
    else
      let kind =
        match ev.lev_kind with
        | Lev_before -> "before"
        | Lev_after _ -> "after"
        | Lev_function -> "funct-body"
        | Lev_pseudo -> "pseudo"
      in
      fprintf ppf "@[<2>(%s %a@ %a)@]" kind location ev.lev_loc blambda expr
  | Pseudo_event (expr, loc) ->
    if not !Clflags.locations
    then blambda ppf expr
    else fprintf ppf "@[<2>(pseudo %a@ %a)@]" location loc blambda expr
  | Context_switch (op, tc, args) ->
    let op =
      match op with
      | Perform -> "perform"
      | Reperform -> "reperform"
      | Runstack -> "runstack"
      | Resume -> "resume"
    in
    fprintf ppf "@[<2>(%s %a@ %a)@]" op tailcall tc
      (pp_print_list ~pp_sep:pp_print_space blambda)
      args
  | Sequand (x, y) -> fprintf ppf "@[<2>(&&@ %a@ %a)@]" blambda x blambda y
  | Sequor (x, y) -> fprintf ppf "@[<2>(||@ %a@ %a)@]" blambda x blambda y

and rec_binding ppf { id; def } =
  fprintf ppf "@[<2>%a@ =@ %a@]" Ident.print id bfunction def

and bfunction ppf { params; body; free_variables = _ } =
  fprintf ppf "@[<2>(fun@ @[%a@]@ ->@ %a)@]"
    (pp_print_list ~pp_sep:pp_print_space Ident.print)
    params blambda body
