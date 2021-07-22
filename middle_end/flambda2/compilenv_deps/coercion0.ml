(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  type variable
  type rec_info_expr

  type t = private
    | Id
    | Change_depth of {
        from : rec_info_expr;
        to_ : rec_info_expr;
      }

  val change_depth
    : from:rec_info_expr
    -> to_:rec_info_expr
    -> t

  val id : t

  val is_id : t -> bool

  val inverse : t -> t

  val compose : t -> then_:t -> t option

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  val map_depth_variables : t -> f:(variable -> variable) -> t
end

module Make(Rec_info_expr : Rec_info_expr0.S)
  : S with type variable = Rec_info_expr.variable
       and type rec_info_expr = Rec_info_expr.t
= struct
  type variable = Rec_info_expr.variable
  type rec_info_expr = Rec_info_expr.t

  type t =
    | Id
    | Change_depth of {
        from : rec_info_expr;
        to_ : rec_info_expr;
      }

  let id = Id

  let change_depth ~from ~to_ =
    (* The special case here is actually very common; it appears, for instance,
       when something gets composed when its own inverse, which happens often
       in the back-and-forth in [Typing_env.add_equation] for instance *)
    if from == to_ then Id else Change_depth { from; to_ }

  let is_id = function
    | Id -> true
    | Change_depth _ -> false

  let inverse = function
    | Id -> Id
    | Change_depth { from; to_ } -> Change_depth { from = to_; to_ = from }

  let compose t1 ~then_:t2 =
    match t1, t2 with
    | Id, _ -> Some t2
    | _, Id -> Some t1
    | Change_depth { from = from1; to_ = to_1 },
      Change_depth { from = from2; to_ = to_2 } ->
      (* CR lmaurer: We would like to check that [to_1] equals [from_2], but we
         can't do that without an environment. We've considered making this
         function purely syntactic (and thus total) before; that would solve
         the problem. Perhaps there should be a Coercion kind for semantic
         operations? *)
      ignore (to_1, from2);
      Some (change_depth ~from:from1 ~to_:to_2)

  let print ppf = function
    | Id ->
      Format.fprintf ppf "@<0>%sid@<0>%s"
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
    | Change_depth { from; to_; } ->
      Format.fprintf ppf "@<0>%s@[<hov 1>(depth@ %a@<0>%s ->@ %a@<0>%s)@]@<0>%s"
        (Flambda_colours.coercion ())
        Rec_info_expr.print from
        (Flambda_colours.coercion ())
        Rec_info_expr.print to_
        (Flambda_colours.coercion ())
        (Flambda_colours.normal ())

  let equal t1 t2 =
    t1 == t2
    ||
    match t1, t2 with
    | Id, Id -> true
    | Change_depth { from = from1; to_ = to_1 },
      Change_depth { from = from2; to_ = to_2 } ->
      Rec_info_expr.equal from1 from2 &&
      Rec_info_expr.equal to_1 to_2
    | (Id | Change_depth _), _ -> false

  let hash = function
    | Id ->
      Hashtbl.hash 0
    | Change_depth { from; to_ } ->
      Hashtbl.hash
        (1, Rec_info_expr.hash from, Rec_info_expr.hash to_)

  let map_depth_variables t ~f =
    match t with
    | Id -> t
    | Change_depth { from = old_from; to_ = old_to } ->
      let new_from = Rec_info_expr.map_depth_variables ~f old_from in
      let new_to = Rec_info_expr.map_depth_variables ~f old_to in
      if new_from == old_from && new_to == old_to then t else
        change_depth ~from:new_from ~to_: new_to
end
