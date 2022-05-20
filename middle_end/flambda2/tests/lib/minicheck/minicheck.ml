(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Splittable_random = Splittable_random
module Type = Type

let seed = 0

let something_has_failed = ref false

let check0 ~type_ ~f ?(n = 1000) ?(verbose = false) ~name () =
  Format.eprintf "%s: " name;
  let start = Sys.time () in
  let r = Splittable_random.of_int seed in
  let i = ref 1 in
  let failed = ref false in
  while (not !failed) && !i <= n do
    let a = Type.generate type_ r in
    if verbose
    then
      Format.eprintf "@[<hov 2>Attempt %d/%d:@ %a@]@." !i n (Type.print type_) a;
    if not (f a)
    then begin
      Format.eprintf "FAILED after %d/%d attempts@." !i n;
      Format.eprintf "@[<hov 2>Counterexample:@ %a@]@." (Type.print type_) a;
      failed := true;
      something_has_failed := true
    end
    else incr i
  done;
  if not !failed
  then
    let finish = Sys.time () in
    let duration = finish -. start in
    Format.eprintf "PASSED %d attempts (%f s)@." n duration

let rec call : type a b. a -> (a, b) Type.Tuple.Value.t -> b =
 fun f tup -> match tup with [] -> f | a :: tup -> call (f a) tup

let check :
    type a.
    types:(a, bool) Type.Tuple.t ->
    f:a ->
    ?n:int ->
    ?verbose:bool ->
    name:string ->
    unit ->
    unit =
 fun ~types ~f ?n ?verbose ~name () ->
  let run type_ f = check0 ~type_ ~f ?n ?verbose ~name () in

  match types with
  | [] -> run Type.unit (fun () -> f)
  | [ty] -> run ty f
  | [ty1; ty2] -> run (Type.pair ty1 ty2) (fun (a, b) -> f a b)
  | [ty1; ty2; ty3] -> run (Type.triple ty1 ty2 ty3) (fun (a, b, c) -> f a b c)
  | [ty1; ty2; ty3; ty4] ->
    run (Type.quad ty1 ty2 ty3 ty4) (fun (a, b, c, d) -> f a b c d)
  | ty1 :: ty2 :: ty3 :: types ->
    (* Could do more to pack the remaining arguments together but I've
       over-engineered this enough as is *)
    run
      (Type.quad ty1 ty2 ty3 (Type.tuple types))
      (fun (a, b, c, tup) -> call (f a b c) tup)

let something_has_failed () = !something_has_failed
