(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type variable

  module Unrolling_state : sig
    type t = private
      | Not_unrolling
      | Unrolling of { remaining_depth : int }
      | Do_not_unroll

    val not_unrolling : t

    val unrolling : remaining_depth:int -> t

    val do_not_unroll : t

    val print : Format.formatter -> t -> unit

    val equal : t -> t -> bool

    val hash : t -> int
  end

  type t = private
    | Const of
        { depth : int Or_infinity.t;
          unrolling : Unrolling_state.t
        }
    | Var of variable
    | Succ of t
    | Unroll_to of int * t

  val initial : t

  val unknown : t

  val do_not_inline : t

  val const : depth:int Or_infinity.t -> unrolling:Unrolling_state.t -> t

  val var : variable -> t

  val succ : t -> t

  val unroll_to : int -> t -> t

  val is_obviously_initial : t -> bool

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  val map_depth_variables : t -> f:(variable -> variable) -> t

  val erase_variables : t -> t
end

module Make (Variable : Container_types.S) : S with type variable = Variable.t =
struct
  type variable = Variable.t

  module Unrolling_state = struct
    type t =
      | Not_unrolling
      | Unrolling of { remaining_depth : int }
      | Do_not_unroll

    let not_unrolling = Not_unrolling

    let unrolling ~remaining_depth = Unrolling { remaining_depth }

    let do_not_unroll = Do_not_unroll

    let [@ocamlformat "disable"] print ppf = function
      | Not_unrolling ->
        Format.pp_print_string ppf "Not_unrolling"
      | Unrolling { remaining_depth } ->
        Format.fprintf ppf
          "@[<hov 1>(Unrolling@ \
           @[<hov 1>(remaining_depth@ %d)@])@]"
          remaining_depth
      | Do_not_unroll ->
        Format.pp_print_string ppf "Do_not_unroll"

    let equal t1 t2 =
      match t1, t2 with
      | Not_unrolling, Not_unrolling -> true
      | ( Unrolling { remaining_depth = remaining_depth1 },
          Unrolling { remaining_depth = remaining_depth2 } ) ->
        remaining_depth1 = remaining_depth2
      | Do_not_unroll, Do_not_unroll -> true
      | (Not_unrolling | Unrolling _ | Do_not_unroll), _ -> false

    let hash = function
      | Not_unrolling -> Hashtbl.hash 0
      | Unrolling { remaining_depth } -> Hashtbl.hash (1, remaining_depth)
      | Do_not_unroll -> Hashtbl.hash 2
  end

  type t =
    | Const of
        { depth : int Or_infinity.t;
          unrolling : Unrolling_state.t
        }
    | Var of Variable.t
    | Succ of t
    | Unroll_to of int * t

  let initial = Const { depth = Finite 0; unrolling = Not_unrolling }

  let unknown = Const { depth = Infinity; unrolling = Not_unrolling }

  let do_not_inline = Const { depth = Infinity; unrolling = Do_not_unroll }

  let const ~depth ~unrolling = Const { depth; unrolling }

  let var dv = Var dv

  let succ t = Succ t

  let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

  let is_obviously_initial = function
    | Const { depth = Finite 0; unrolling = Not_unrolling } -> true
    | Const
        { depth = Finite _ | Infinity;
          unrolling = Not_unrolling | Unrolling _ | Do_not_unroll
        }
    | Var _ | Succ _ | Unroll_to _ ->
      false

  let [@ocamlformat "disable"] rec print ppf = function
    | Const { depth; unrolling } ->
      begin match unrolling with
      | Not_unrolling ->
        Format.fprintf ppf "%t%a%t"
          Flambda_colours.rec_info
          (Or_infinity.print ~f:Format.pp_print_int) depth
          Flambda_colours.pop
      | Unrolling _ | Do_not_unroll ->
        Format.fprintf ppf
          "%t@[<hov 1>(%a@ %a)@]%t"
        Flambda_colours.rec_info
        (Or_infinity.print ~f:Format.pp_print_int) depth
        Unrolling_state.print unrolling
        Flambda_colours.pop
      end
    | Var dv ->
      Format.fprintf ppf "%t%a%t"
        Flambda_colours.depth_variable
        Variable.print dv
        Flambda_colours.pop
    | Succ t ->
      Format.fprintf ppf "@[<hov 1>(succ@ %a)@]" print t
    | Unroll_to (unroll_depth, t) ->
      Format.fprintf ppf "@[<hov 1>(unroll_to@ %d@ %a)@]" unroll_depth print t

  let rec equal t1 t2 =
    t1 == t2
    ||
    match t1, t2 with
    | ( Const { depth = depth1; unrolling = unrolling1 },
        Const { depth = depth2; unrolling = unrolling2 } ) ->
      Or_infinity.equal ~f:Int.equal depth1 depth2
      && Unrolling_state.equal unrolling1 unrolling2
    | Var dv1, Var dv2 -> Variable.equal dv1 dv2
    | Succ t1, Succ t2 -> equal t1 t2
    | Unroll_to (unroll_depth1, t1), Unroll_to (unroll_depth2, t2) ->
      unroll_depth1 = unroll_depth2 && equal t1 t2
    | (Const _ | Var _ | Succ _ | Unroll_to _), _ -> false

  let rec hash = function
    | Const { depth; unrolling } ->
      Hashtbl.hash
        ( 0,
          Or_infinity.hash ~f:Hashtbl.hash depth,
          Unrolling_state.hash unrolling )
    | Var dv -> Hashtbl.hash (1, Variable.hash dv)
    | Succ t -> Hashtbl.hash (2, hash t)
    | Unroll_to (unroll_depth, t) -> Hashtbl.hash (3, unroll_depth, hash t)

  let rec map_depth_variables t ~f =
    match t with
    | Const _ -> t
    | Var dv ->
      let new_dv = f dv in
      if new_dv == dv then t else Var new_dv
    | Succ t0 ->
      let new_t0 = map_depth_variables t0 ~f in
      if new_t0 == t0 then t else Succ new_t0
    | Unroll_to (depth, t0) ->
      let new_t0 = map_depth_variables t0 ~f in
      if new_t0 == t0 then t else Unroll_to (depth, new_t0)

  let rec erase_variables t =
    match t with
    | Const _ -> t
    | Var _ -> unknown
    | Succ t0 ->
      let new_t0 = erase_variables t0 in
      if new_t0 == t0 then t else Succ new_t0
    | Unroll_to (depth, t0) ->
      let new_t0 = erase_variables t0 in
      if new_t0 == t0 then t else Unroll_to (depth, new_t0)
end
