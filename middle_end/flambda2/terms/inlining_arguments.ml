(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR lmaurer: Flatten this into the top-level module. *)
module Args = struct
  type t =
    { max_inlining_depth : int;
      max_rec_depth : int;
      call_cost : float;
      alloc_cost : float;
      prim_cost : float;
      branch_cost : float;
      indirect_call_cost : float;
      poly_compare_cost : float;
      small_function_size : int;
      large_function_size : int;
      threshold : float
    }

  let matches_opt_level
      { max_inlining_depth;
        max_rec_depth;
        call_cost;
        alloc_cost;
        prim_cost;
        branch_cost;
        indirect_call_cost;
        poly_compare_cost;
        small_function_size;
        large_function_size;
        threshold
      } opt_level =
    let module I = Flambda_features.Inlining in
    let round_or_default : I.round_or_default = Default opt_level in
    Int.equal max_inlining_depth (I.max_depth round_or_default)
    && Int.equal max_rec_depth (I.max_rec_depth round_or_default)
    && Float.equal call_cost (I.call_cost round_or_default)
    && Float.equal alloc_cost (I.alloc_cost round_or_default)
    && Float.equal prim_cost (I.prim_cost round_or_default)
    && Float.equal branch_cost (I.branch_cost round_or_default)
    && Float.equal indirect_call_cost (I.indirect_call_cost round_or_default)
    && Float.equal poly_compare_cost (I.poly_compare_cost round_or_default)
    && Int.equal small_function_size (I.small_function_size round_or_default)
    && Int.equal large_function_size (I.large_function_size round_or_default)
    && Float.equal threshold (I.threshold round_or_default)

  let[@ocamlformat "disable"] print ppf t =
    let { max_inlining_depth; max_rec_depth;
          call_cost; alloc_cost; prim_cost; branch_cost;
          indirect_call_cost; poly_compare_cost;
          small_function_size; large_function_size;
          threshold;
        } = t
    in
    let module C = Flambda_colours in
    if matches_opt_level t Oclassic
    then Format.fprintf ppf "%tOclassic%t" C.elide C.pop
    else if matches_opt_level t O2
    then Format.fprintf ppf "%tO2%t" C.elide C.pop
    else if matches_opt_level t O3
    then Format.fprintf ppf "%tO3%t" C.elide C.pop
    else
      Format.fprintf ppf
        "@[<hov 1>(\
         @[<hov 1>(max_inlining_depth@ %d)@]@ \
         @[<hov 1>(max_rec_depth@ %d)@]@ \
         @[<hov 1>(call_cost@ %f)@]@ \
         @[<hov 1>(alloc_cost@ %f)@]@ \
         @[<hov 1>(prim_cost@ %f)@]@ \
         @[<hov 1>(branch_cost@ %f)@]@ \
         @[<hov 1>(indirect_call_cost@ %f)@]@ \
         @[<hov 1>(poly_compare_cost@ %f)@]@ \
         @[<hov 1>(small_function_size@ %d)@]@ \
         @[<hov 1>(large_function_size@ %d)@]@ \
         @[<hov 1>(threshold@ %f)@]\
         )@]"
        max_inlining_depth
        max_rec_depth
        call_cost
        alloc_cost
        prim_cost
        branch_cost
        indirect_call_cost
        poly_compare_cost
        small_function_size
        large_function_size
        threshold

  let equal t1 t2 =
    let { max_inlining_depth = t1_max_inlining_depth;
          max_rec_depth = t1_max_rec_depth;
          call_cost = t1_call_cost;
          alloc_cost = t1_alloc_cost;
          prim_cost = t1_prim_cost;
          branch_cost = t1_branch_cost;
          indirect_call_cost = t1_indirect_call_cost;
          poly_compare_cost = t1_poly_compare_cost;
          small_function_size = t1_small_function_size;
          large_function_size = t1_large_function_size;
          threshold = t1_threshold
        } =
      t1
    in
    let { max_inlining_depth = t2_max_inlining_depth;
          max_rec_depth = t2_max_rec_depth;
          call_cost = t2_call_cost;
          alloc_cost = t2_alloc_cost;
          prim_cost = t2_prim_cost;
          branch_cost = t2_branch_cost;
          indirect_call_cost = t2_indirect_call_cost;
          poly_compare_cost = t2_poly_compare_cost;
          small_function_size = t2_small_function_size;
          large_function_size = t2_large_function_size;
          threshold = t2_threshold
        } =
      t2
    in
    t1_max_inlining_depth = t2_max_inlining_depth
    && t1_max_rec_depth = t2_max_rec_depth
    && Float.compare t1_call_cost t2_call_cost = 0
    && Float.compare t1_alloc_cost t2_alloc_cost = 0
    && Float.compare t1_prim_cost t2_prim_cost = 0
    && Float.compare t1_branch_cost t2_branch_cost = 0
    && Float.compare t1_indirect_call_cost t2_indirect_call_cost = 0
    && Float.compare t1_poly_compare_cost t2_poly_compare_cost = 0
    && t1_small_function_size = t2_small_function_size
    && t1_large_function_size = t2_large_function_size
    && Float.compare t1_threshold t2_threshold = 0

  let ( <= ) t1 t2 =
    (* The comparison of two [Args.t] is a pointwise comparison. It is a
     * partial order so it may happen that both [t1 <= t2] and [t2 <= t1] are
     * false. For example we could have:
     * t1 = { call_cost = 2; alloc_cost = 2 }
     * t2 = { call_cost = 4; alloc_cost = 1 }
     * In that case [(<=) t1 t2 = false] as [t1.alloc_cost > t2.alloc_cost] and
     * [(<=) t2 t1 = false] as [t2.call_cost > t1.call_cost]
     *)
    let { max_inlining_depth = t1_max_inlining_depth;
          max_rec_depth = t1_max_rec_depth;
          call_cost = t1_call_cost;
          alloc_cost = t1_alloc_cost;
          prim_cost = t1_prim_cost;
          branch_cost = t1_branch_cost;
          indirect_call_cost = t1_indirect_call_cost;
          poly_compare_cost = t1_poly_compare_cost;
          small_function_size = t1_small_function_size;
          large_function_size = t1_large_function_size;
          threshold = t1_threshold
        } =
      t1
    in
    let { max_inlining_depth = t2_max_inlining_depth;
          max_rec_depth = t2_max_rec_depth;
          call_cost = t2_call_cost;
          alloc_cost = t2_alloc_cost;
          prim_cost = t2_prim_cost;
          branch_cost = t2_branch_cost;
          indirect_call_cost = t2_indirect_call_cost;
          poly_compare_cost = t2_poly_compare_cost;
          small_function_size = t2_small_function_size;
          large_function_size = t2_large_function_size;
          threshold = t2_threshold
        } =
      t2
    in
    t1_max_inlining_depth <= t2_max_inlining_depth
    && t1_max_rec_depth <= t2_max_rec_depth
    && Float.compare t1_call_cost t2_call_cost <= 0
    && Float.compare t1_alloc_cost t2_alloc_cost <= 0
    && Float.compare t1_prim_cost t2_prim_cost <= 0
    && Float.compare t1_branch_cost t2_branch_cost <= 0
    && Float.compare t1_indirect_call_cost t2_indirect_call_cost <= 0
    && Float.compare t1_poly_compare_cost t2_poly_compare_cost <= 0
    && t1_small_function_size <= t2_small_function_size
    && t1_large_function_size <= t2_large_function_size
    && Float.compare t1_threshold t2_threshold <= 0

  let meet t1 t2 =
    let { max_inlining_depth = t1_max_inlining_depth;
          max_rec_depth = t1_max_rec_depth;
          call_cost = t1_call_cost;
          alloc_cost = t1_alloc_cost;
          prim_cost = t1_prim_cost;
          branch_cost = t1_branch_cost;
          indirect_call_cost = t1_indirect_call_cost;
          poly_compare_cost = t1_poly_compare_cost;
          small_function_size = t1_small_function_size;
          large_function_size = t1_large_function_size;
          threshold = t1_threshold
        } =
      t1
    in
    let { max_inlining_depth = t2_max_inlining_depth;
          max_rec_depth = t2_max_rec_depth;
          call_cost = t2_call_cost;
          alloc_cost = t2_alloc_cost;
          prim_cost = t2_prim_cost;
          branch_cost = t2_branch_cost;
          indirect_call_cost = t2_indirect_call_cost;
          poly_compare_cost = t2_poly_compare_cost;
          small_function_size = t2_small_function_size;
          large_function_size = t2_large_function_size;
          threshold = t2_threshold
        } =
      t2
    in
    { max_inlining_depth = min t1_max_inlining_depth t2_max_inlining_depth;
      max_rec_depth = min t1_max_rec_depth t2_max_rec_depth;
      call_cost = Float.min t1_call_cost t2_call_cost;
      alloc_cost = Float.min t1_alloc_cost t2_alloc_cost;
      prim_cost = Float.min t1_prim_cost t2_prim_cost;
      branch_cost = Float.min t1_branch_cost t2_branch_cost;
      indirect_call_cost = Float.min t1_indirect_call_cost t2_indirect_call_cost;
      poly_compare_cost = Float.min t1_poly_compare_cost t2_poly_compare_cost;
      small_function_size = min t1_small_function_size t2_small_function_size;
      large_function_size = min t1_large_function_size t2_large_function_size;
      threshold = Float.min t1_threshold t2_threshold
    }

  let create ~round =
    let module I = Flambda_features.Inlining in
    { max_inlining_depth = I.max_depth (Round round);
      max_rec_depth = I.max_rec_depth (Round round);
      call_cost = I.call_cost (Round round);
      alloc_cost = I.alloc_cost (Round round);
      prim_cost = I.prim_cost (Round round);
      branch_cost = I.branch_cost (Round round);
      indirect_call_cost = I.indirect_call_cost (Round round);
      poly_compare_cost = I.poly_compare_cost (Round round);
      small_function_size = I.small_function_size (Round round);
      large_function_size = I.large_function_size (Round round);
      threshold = I.threshold (Round round)
    }
end

type t = Args.t

let [@ocamlformat "disable"] print ppf = Args.print ppf

let max_inlining_depth t = t.Args.max_inlining_depth

let call_cost t = t.Args.call_cost

let alloc_cost t = t.Args.alloc_cost

let prim_cost t = t.Args.prim_cost

let branch_cost t = t.Args.branch_cost

let indirect_call_cost t = t.Args.indirect_call_cost

let poly_compare_cost t = t.Args.poly_compare_cost

let small_function_size t = t.Args.small_function_size

let large_function_size t = t.Args.large_function_size

let threshold t = t.Args.threshold

let meet t1 t2 =
  (* If we are sure that args1 is lower than args2 then
   * [meet args1 args2 = args1]. In that case we can avoid calling
   * [Args.meet] and reuse args1 to avoid having to allocate a new Args.t.
   * The same goes if the are sure that args2 is lower than args1.
   *)
  if Args.( <= ) t1 t2
  then t1
  else if Args.( <= ) t2 t1
  then t2
  else Args.meet t1 t2

let create ~round = Args.create ~round

let equal t1 t2 = Args.equal t1 t2

let is_oclassic t = Args.matches_opt_level t Flambda_backend_flags.Oclassic

let is_o2 t = Args.matches_opt_level t Flambda_backend_flags.O2

let is_o3 t = Args.matches_opt_level t Flambda_backend_flags.O3
