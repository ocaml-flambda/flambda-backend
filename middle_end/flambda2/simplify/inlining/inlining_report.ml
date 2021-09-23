(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020--2020 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Decisions related to inlining,

   This includes deciding not to consider some function for inlining (e.g.
   method, c-calls, or function with no precise enough type to inline). *)

type at_call_site =
  | Known_function of
      { code_id : Code_id.exported;
        decision : Call_site_inlining_decision.t
      }
  | Unknown_function

type fundecl_pass =
  | Before_simplify of { dbg_including_inlining_stack : Debuginfo.t }
  | After_simplify

type at_function_declaration =
  { pass : fundecl_pass;
    code_id : Code_id.exported;
    decision : Function_decl_inlining_decision_type.t
  }

type decision =
  | At_call_site of at_call_site
  | At_function_declaration of at_function_declaration

(* Log for a whole round

   Currently, we make use of the precise ordering of decisions to print the log
   in a somewhat hierarchical manner. More precisely, we rely on the fact that
   there are two decisions for a function declarations: before and after
   simplify. This make it possible to treat those two decisions as "entering"
   and "exiting" the function's body, considering all decisions that occur in
   between to pertain to the body of the function, using the order of the list
   insertions of the {record_decision} function.

   On the other hand, flambda1 had a non-mutable way of storing this information
   as a closure_stack (see trunk:middle_end/flambda/inlining_stats.ml) in the
   environment of the simplification process. This could be replicated here,
   i.e. we could store something akin to flambda1's Closure_stack.t inside the
   denv if the current mutable way proves to be too annoying. *)

(* Individual decisions, with debuginfo *)
type t =
  { dbg : Debuginfo.t;
    decision : decision
  }

type metadata = { compilation_unit : Compilation_unit.t }

type report = [`Flambda2_1_0_0 of metadata * t list]

(* Actual log storage. During simplification, in order to be more efficient,
   decisions are stored from the most recent one (in the head of the list), to
   the oldest one (at the end of the list).

   This means that one should rev the list before printing. *)
let log : t list ref = ref []

let stars fmt depth = Format.fprintf fmt "%s" (String.make (depth + 1) '*')

let print_debuginfo ppf dbg =
  if Debuginfo.is_none dbg
  then Format.pp_print_string ppf "None"
  else Debuginfo.print_compact ppf dbg

let [@ocamlformat "disable"] rec print ~depth fmt = function
  (* end of report log *)
  | [] ->
    if depth <> 0 then
      Misc.fatal_errorf "incoherent depth at end of inlining report"

  (* Entering a function declaration (possibly nested) *)
  | { dbg; decision = At_function_declaration {
      pass = Before_simplify _; code_id; decision; } } :: r ->
    Format.fprintf fmt "%a Definition of %s{%a}@\n"
      stars depth Code_id.(name (import code_id)) print_debuginfo dbg;
    Format.fprintf fmt "%a @[<v>Before simplification:@ @ %a@]@\n@\n"
      stars (depth + 1)
      Function_decl_inlining_decision_type.report decision;
    print ~depth:(depth + 1) fmt r

  (* Exiting a function_declaration (possibly nested) *)
  | { dbg ; decision = At_function_declaration {
      pass = After_simplify; code_id; decision; } } :: r ->
    Format.fprintf fmt "%a @[<v>After simplification of %s{%a}:@ @ %a@]@\n@\n@\n"
      stars depth Code_id.(name (import code_id)) print_debuginfo dbg
      Function_decl_inlining_decision_type.report decision;
    print ~depth:(depth - 1) fmt r

  (* Function call *)
  | { decision = At_call_site Unknown_function; dbg; } :: r ->
    Format.fprintf fmt "%a @[<v>%s of %s{%a}@ @ %s@ %s@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      "<unknown function>" print_debuginfo dbg
      "The function call has not been inlined"
      "because the optimizer had not enough information about the function";
    print ~depth fmt r
  | { decision = At_call_site (Known_function { code_id; decision; });
      dbg; } :: r ->
    Format.fprintf fmt "%a @[<v>%s of %s{%a}@ @ %a@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      Code_id.(name (import code_id)) print_debuginfo dbg
      Call_site_inlining_decision.report decision;
    print ~depth fmt r

(* Exposed interface *)

let record_decision ~dbg decision =
  if Flambda_features.inlining_report ()
     || Flambda_features.inlining_report_bin ()
  then log := { dbg; decision } :: !log

let output_then_forget_decisions ~output_prefix =
  Misc.try_finally
    ~always:(fun () -> log := [])
    ~exceptionally:(fun () ->
      (* CR gbury: Is there a more appropritate function to report a warning
         (that is not one of the numbered warnings *)
      Format.eprintf "WARNING: inlining report output failed@.")
    (fun () ->
      let l = lazy (List.rev !log) in
      if Flambda_features.inlining_report ()
      then begin
        let out_channel = open_out (output_prefix ^ ".inlining.org") in
        let fmt = Format.formatter_of_out_channel out_channel in
        Format.fprintf fmt "%a@." (print ~depth:0) (Lazy.force l);
        close_out out_channel
      end;
      if Flambda_features.inlining_report_bin ()
      then begin
        let ch = open_out_bin (output_prefix ^ ".inlining") in
        let metadata =
          { compilation_unit = Compilation_unit.get_current_exn () }
        in
        let report : report = `Flambda2_1_0_0 (metadata, Lazy.force l) in
        Marshal.to_channel ch report [];
        close_out ch
      end)
