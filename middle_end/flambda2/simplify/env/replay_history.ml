(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Guillaume Bury, Pierre Chambart and Nathanaëlle Courant, OCamlPro    *)
(*                                                                        *)
(*   Copyright 2013--2024 OCamlPro SAS                                    *)
(*   Copyright 2014--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* List of actions that should be replayed, currently only cares about bound
   variables and continuations. In the future, we may want to also store
   inlining decisions.

   CR gbury: move to a [Replay_action.ml] file ? *)
module Action = struct
  type t =
    | Bound_variable of Variable.t
    | Bound_continuations of Continuation.t list

  let[@ocamlformat "disable"] print ppf = function
    | Bound_variable v -> Variable.print ppf v
    | Bound_continuations l ->
        Format.pp_print_list ~pp_sep:Format.pp_print_space Continuation.print ppf l
end

(* Type def *)
(* ******** *)

type t =
  | First_pass of
      { history : Action.t list
            (* Bindables opened, with the head of the list being the
               innermost/last binder opened *)
      }
  | Replaying of
      { always_inline : bool;
        (* Force inlining of all function calls during replay. This is for
           instance set by the match-in-match specialization. *)
        previous_history : Action.t list;
        (* Bindables opened during the first pass, with the head of the list
           being the outermost/first one opened. *)
        variables : Variable.t Variable.Map.t;
        (* Map from the variables of the current pass to the corresponding
           variable from the first pass. *)
        continuations : Continuation.t Continuation.Map.t
            (* Map from the continuations of the current pass to the
               corresponding variable from the first pass. *)
      }

let[@ocamlformat "disable"] print ppf = function
  | First_pass { history; } ->
      Format.fprintf ppf "@[<hov 1>(first_pass@ \
        @[<hov 1>(history@ %a)@]\
        )@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Action.print) history
  | Replaying { always_inline; previous_history; variables; continuations; } ->
      Format.fprintf ppf "@[<hov 1>(replaying@ \
        @[<hov 1>(always_inline@ %b)@]@ \
        @[<hov 1>(previous_history@ %a)@]@ \
        @[<hov 1>(variables@ %a)@]@ \
        @[<hov 1>(continuations@ %a)@]\
        )@]"
        always_inline
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Action.print) previous_history
        (Variable.Map.print Variable.print) variables
        (Continuation.Map.print Continuation.print) continuations

(* Creating API *)
(* ************ *)

let first_pass = First_pass { history = [] }

let replay ~always_inline = function
  | First_pass { history } ->
    let previous_history = List.rev history in
    Replaying
      { always_inline;
        previous_history;
        variables = Variable.Map.empty;
        continuations = Continuation.Map.empty
      }
  | Replaying _ ->
    (* CR gbury: we could support this use-case (which might be useful for
       widening in loops) in the future by also storing the original history in
       the [Replaying] case *)
    Misc.fatal_errorf "Cannot replay an already replayed binding history"

let error_empty_history replay action =
  Misc.fatal_errorf
    "@[<v>@[<hov>Found an empty replay history when replaying action:@ %a.@]@ \
     Replay: %a@]"
    Action.print action print replay

let error_mismatched_action replay old_action new_action =
  Misc.fatal_errorf
    "@[<v>Action mismatch when replaying history:@ old action: %a@ new action: \
     %a@ replay: %a"
    Action.print old_action Action.print new_action print replay

let error_not_renamed_version_of replay old_action new_action =
  Misc.fatal_errorf
    "@[<v>Actions are not renamed versions when replaying history:@ old \
     action: %a@ new action: %a@ replay: %a"
    Action.print old_action Action.print new_action print replay

let define_variable var replay =
  let action : Action.t = Bound_variable var in
  match replay with
  | First_pass { history } -> First_pass { history = action :: history }
  | Replaying
      { previous_history = prev_bound :: previous_history;
        variables;
        continuations;
        always_inline
      } -> (
    match prev_bound with
    | Bound_variable prev_var ->
      if not (Variable.is_renamed_version_of prev_var var)
      then error_not_renamed_version_of replay prev_bound action
      else
        let variables = Variable.Map.add var prev_var variables in
        Replaying { previous_history; variables; continuations; always_inline }
    | Bound_continuations _ -> error_mismatched_action replay prev_bound action)
  | Replaying { previous_history = []; _ } -> error_empty_history replay action

let define_continuations conts replay =
  let action : Action.t = Bound_continuations conts in
  match replay with
  | First_pass { history } -> First_pass { history = action :: history }
  | Replaying
      { previous_history = prev_bound :: previous_history;
        variables;
        continuations;
        always_inline
      } -> (
    match prev_bound with
    | Bound_continuations prev_conts ->
      if not
           (List.compare_lengths prev_conts conts = 0
           && Misc.Stdlib.List.equal Continuation.is_renamed_version_of
                prev_conts conts)
      then error_not_renamed_version_of replay prev_bound action
      else
        let continuations =
          List.fold_left2
            (fun acc prev_cont cont -> Continuation.Map.add cont prev_cont acc)
            continuations prev_conts conts
        in
        Replaying { previous_history; variables; continuations; always_inline }
    | Bound_variable _ -> error_mismatched_action replay prev_bound action)
  | Replaying { previous_history = []; _ } -> error_empty_history replay action

(* Inspection API *)
(* ************** *)

let must_inline = function
  | First_pass _ -> false
  | Replaying { always_inline; _ } -> always_inline

type 'a replay_result =
  | Still_recording
  | Replayed of 'a

let replay_variable_mapping = function
  | First_pass _ -> Still_recording
  | Replaying
      { variables; previous_history = _; continuations = _; always_inline = _ }
    ->
    Replayed variables

let replay_continuation_mapping = function
  | First_pass _ -> Still_recording
  | Replaying
      { continuations; previous_history = _; variables = _; always_inline = _ }
    ->
    Replayed continuations
