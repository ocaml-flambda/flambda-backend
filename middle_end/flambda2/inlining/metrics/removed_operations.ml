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

(* Operations are fine grained metrics (number of calls / allocations) about
     the size of an expression.
 *)

type t = {
    call : int;
    alloc : int;
    prim : int;
    branch : int;
    (* CR-someday pchambart: branch : t list; *)
    direct_call_of_indirect : int;
    specialized_poly_compare : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

let zero = {
    call = 0;
    alloc = 0;
    prim = 0;
    branch = 0;
    direct_call_of_indirect = 0;
    specialized_poly_compare = 0;
    requested_inline = 0;
  }

let call = { zero with call = 1; }
let alloc = { zero with alloc = 1; }

let prim (prim:Flambda_primitive.t) =
  match prim with
  | Unary (prim, _) -> begin
      match prim with
      | Duplicate_block _ | Duplicate_array _ | Box_number _ | Unbox_number _ ->
         alloc
      | _ ->
        (* Some allocating primitives ([Num_conv] to naked_int64 on arch32 for example) are
        not counted here. *)
         { zero with prim = 1 }
    end
  | Nullary _ ->
     zero
  | Binary (_, _, _)
  | Ternary (_, _, _, _) ->
     { zero with prim = 1 }
  | Variadic (prim, _) -> begin
      match prim with
      | Make_block _ | Make_array _ ->
         alloc
    end

let branch = { zero with branch = 1; }

let direct_call_of_indirect =
  { zero with direct_call_of_indirect = 1; }

let specialized_poly_compare =
  { zero with specialized_poly_compare = 1; }

let [@ocamlformat "disable"] print ppf b =
  Format.fprintf ppf "@[call: %i@ alloc: %i@ \
                      prim: %i@ branch: %i@ \
                      direct: %i@ poly_cmp: %i@ \
                      requested: %i@]"
    b.call
    b.alloc
    b.prim
    b.branch
    b.direct_call_of_indirect
    b.specialized_poly_compare
    b.requested_inline

let (+) t1 t2 = {
    call = t1.call + t2.call;
    alloc = t1.alloc + t2.alloc;
    prim = t1.prim + t2.prim;
    branch = t1.branch + t2.branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    specialized_poly_compare =
      t1.specialized_poly_compare + t2.specialized_poly_compare;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

let evaluate ~args (t : t) =
  (float_of_int t.call *. Inlining_arguments.call_cost args)
   +. (float_of_int t.alloc *. Inlining_arguments.alloc_cost args)
   +. (float_of_int t.prim *. Inlining_arguments.prim_cost args)
   +. (float_of_int t.branch *. Inlining_arguments.branch_cost args)
   +. (float_of_int t.direct_call_of_indirect
      *. Inlining_arguments.indirect_call_cost args)
   +. (float_of_int t.specialized_poly_compare
       *. Inlining_arguments.poly_compare_cost args)

