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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T0 = Name_abstraction.Make_list (Kinded_parameter) (Expr)

(* CR mshinwell: This should use [Bindable_continuation]. [Exn_continuation]
   involves extra args, but we never have extra args here! *)
module T1 = Name_abstraction.Make (Bindable_exn_continuation) (T0)
module T2 = Name_abstraction.Make (Bindable_continuation) (T1)

(* CR lmaurer: It would be good to avoid the extra abstraction when a function
   is known to be non-recursive. Maybe we should flatten all of these into one
   big [Bindable]? *)
module A = Name_abstraction.Make (Bindable_variable_in_terms) (T2)

type t =
  { abst : A.t;
    dbg : Debuginfo.t;
    params_arity : Flambda_arity.t;
    is_my_closure_used : bool Or_unknown.t;
    free_names_of_body : Name_occurrences.t Or_unknown.t
  }

let create ~return_continuation exn_continuation params ~dbg ~body
    ~free_names_of_body ~my_closure ~my_depth =
  let is_my_closure_used =
    Or_unknown.map free_names_of_body ~f:(fun free_names_of_body ->
        Name_occurrences.mem_var free_names_of_body my_closure)
  in
  let my_closure =
    Kinded_parameter.create my_closure (K.With_subkind.create K.value Anything)
  in
  let t0 = T0.create (params @ [my_closure]) body in
  let t1 = T1.create exn_continuation t0 in
  let t2 = T2.create return_continuation t1 in
  let abst = A.create my_depth t2 in
  { abst;
    dbg;
    params_arity = Kinded_parameter.List.arity params;
    is_my_closure_used;
    free_names_of_body
  }

let extract_my_closure params_and_my_closure =
  match List.rev params_and_my_closure with
  | my_closure :: params_rev ->
    List.rev params_rev, Kinded_parameter.var my_closure
  | [] -> assert false
(* see [create], above. *)

let pattern_match t ~f =
  A.pattern_match t.abst ~f:(fun my_depth t2 ->
      T2.pattern_match t2 ~f:(fun return_continuation t1 ->
          T1.pattern_match t1 ~f:(fun exn_continuation t0 ->
              T0.pattern_match t0 ~f:(fun params_and_my_closure body ->
                  let params, my_closure =
                    extract_my_closure params_and_my_closure
                  in
                  f ~return_continuation exn_continuation params ~body
                    ~my_closure ~is_my_closure_used:t.is_my_closure_used
                    ~my_depth))))

let pattern_match_pair t1 t2 ~f =
  A.pattern_match_pair t1.abst t2.abst ~f:(fun my_depth t2_1 t2_2 ->
      T2.pattern_match_pair t2_1 t2_2 ~f:(fun return_continuation t1_1 t1_2 ->
          T1.pattern_match_pair t1_1 t1_2 ~f:(fun exn_continuation t0_1 t0_2 ->
              T0.pattern_match_pair t0_1 t0_2
                ~f:(fun params_and_my_closure body1 body2 ->
                  let params, my_closure =
                    extract_my_closure params_and_my_closure
                  in
                  f ~return_continuation exn_continuation params ~body1 ~body2
                    ~my_closure ~my_depth))))

let [@ocamlformat "disable"] print ppf t =
  pattern_match t
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure
            ~is_my_closure_used:_ ~my_depth ->
      let my_closure =
        Kinded_parameter.create my_closure
          (K.With_subkind.create K.value Anything)
      in
      fprintf ppf
        "@[<hov 1>(@<0>%s@<1>\u{03bb}@<0>%s@[<hov 1>\
         @<1>\u{3008}%a@<1>\u{3009}@<1>\u{300a}%a@<1>\u{300b}\
         %a %a @<0>%s%a @<0>%s.@<0>%s@]@ %a))@]"
        (Flambda_colours.lambda ())
        (Flambda_colours.normal ())
        Continuation.print return_continuation
        Exn_continuation.print exn_continuation
        Kinded_parameter.List.print params
        Kinded_parameter.print my_closure
        (Flambda_colours.depth_variable ())
        Bindable_variable_in_terms.print my_depth
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        Expr.print body)

let params_arity t = t.params_arity

let apply_renaming
    ({ abst; dbg; params_arity; is_my_closure_used; free_names_of_body } as t)
    perm =
  let abst' = A.apply_renaming abst perm in
  if abst == abst'
  then t
  else
    let free_names_of_body' =
      Or_unknown.map free_names_of_body ~f:(fun free_names_of_body ->
          Name_occurrences.apply_renaming free_names_of_body perm)
    in
    { abst = abst';
      dbg;
      params_arity;
      is_my_closure_used;
      free_names_of_body = free_names_of_body'
    }

let free_names
    { abst;
      params_arity = _;
      dbg = _;
      is_my_closure_used = _;
      free_names_of_body = _
    } =
  A.free_names abst

let free_names_of_body
    { abst = _;
      params_arity = _;
      dbg = _;
      is_my_closure_used = _;
      free_names_of_body
    } =
  free_names_of_body

let debuginfo { dbg; _ } = dbg

let all_ids_for_export
    { abst;
      params_arity = _;
      dbg = _;
      is_my_closure_used = _;
      free_names_of_body = _
    } =
  A.all_ids_for_export abst
