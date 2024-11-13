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

module Action = struct
  type t =
    | Bound_variable of Variable.t
    | Bound_continuations of Continuation.t list

  let[@ocamlformat "disable"] print ppf = function
    | Bound_variable v -> Variable.print ppf v
    | Bound_continuations l ->
        Format.pp_print_list ~pp_sep:Format.pp_print_space Continuation.print ppf l
end

type t =
  | First_pass of
      { history : Action.t list
            (* Bindables opened, with the head of the list being the
               innermost/last binder opened *)
      }
  | Replaying of
      { always_inline : bool;
            (** Force inlining of all function calls during replay. This is for instance
          set by the match-in-match specialization. *)
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
      Format.fprintf ppf "@[<hov 1>(first_pass@ \
        @[<hov 1>(always_inline@ %b)@]@ \
        @[<hov 1>(previous_history@ %a)@]@ \
        @[<hov 1>(variables@ %a)@]@ \
        @[<hov 1>(continuations@ %a)@]\
        )@]"
        always_inline
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Action.print) previous_history
        (Variable.Map.print Variable.print) variables
        (Continuation.Map.print Continuation.print) continuations

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

let must_inline = function
  | First_pass _ -> false
  | Replaying { always_inline; _ } -> always_inline

let define_variable var = function
  | First_pass { history } ->
    let bound : Action.t = Bound_variable var in
    First_pass { history = bound :: history }
  | Replaying
      { previous_history = prev_bound :: previous_history;
        variables;
        continuations;
        always_inline
      } -> (
    match prev_bound with
    | Bound_variable prev_var ->
      if not (Variable.is_renamed_version_of prev_var var)
      then
        Misc.fatal_errorf
          "Mismatch in binding history beetween the first and second pass"
      else
        let variables = Variable.Map.add var prev_var variables in
        Replaying { previous_history; variables; continuations; always_inline }
    | Bound_continuations _ ->
      Misc.fatal_errorf
        "Mismatch in binding history beetween the first and second pass")
  | Replaying { previous_history = []; _ } ->
    Misc.fatal_errorf
      "Mismatch in binding history beetween the first and second pass"

let define_continuations conts = function
  | First_pass { history } ->
    let bound : Action.t = Bound_continuations conts in
    First_pass { history = bound :: history }
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
      then
        Misc.fatal_errorf
          "Mismatch in binding history beetween the first and second pass"
      else
        let continuations =
          List.fold_left2
            (fun acc prev_cont cont -> Continuation.Map.add cont prev_cont acc)
            continuations prev_conts conts
        in
        Replaying { previous_history; variables; continuations; always_inline }
    | Bound_variable _ ->
      Misc.fatal_errorf
        "Mismatch in binding history beetween the first and second pass")
  | Replaying { previous_history = []; _ } ->
    Misc.fatal_errorf
      "Mismatch in binding history beetween the first and second pass"

let replay_variable_mapping = function
  | Replaying
      { variables; previous_history = _; continuations = _; always_inline = _ }
    ->
    variables
  | First_pass _ ->
    Misc.fatal_errorf
      "Cannot access the variable mapping when this is the first pass"

let replay_continuation_mapping = function
  | Replaying
      { continuations; previous_history = _; variables = _; always_inline = _ }
    ->
    continuations
  | First_pass _ ->
    Misc.fatal_errorf
      "Cannot access the variable mapping when this is the first pass"
